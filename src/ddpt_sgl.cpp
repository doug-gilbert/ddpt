/*
 * Copyright (c) 2008-2018, Douglas Gilbert
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

/* ddpt_sgl is a helper for ddpt which is a dd clone and thus a utility
 * program for copying files.
 */

#include <iostream>
#include <vector>
#include <map>
#include <list>
#include <algorithm>
#include <system_error>
#include <thread>
#include <mutex>
#include <chrono>
#include <atomic>
#include <random>

/* Need _GNU_SOURCE for O_DIRECT */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>
#include <errno.h>
#include <limits.h>
#include <fcntl.h>
#include <time.h>
#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>
#include <sys/stat.h>

/* N.B. config.h must precede anything that depends on HAVE_*  */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


static const char * ddpt_sgl_version_str = "0.96 20180611 [svn: r358]";

#include "ddpt.h"
#include "sg_lib.h"
#include "sg_unaligned.h"
#include "sg_pr2serr.h"

using namespace std;
using namespace std::chrono;

# define ACT_DEF 0
# define ACT_NONE 1
# define ACT_APPEND_B2A 2
# define ACT_ENUMERATE 99


typedef vector<struct scat_gath_elem> sgl_vect;


struct sgl_stats {
    bool last_degen;
    bool not_mono_asc;  /* negated so zeroed default is mono_asc */
    bool not_mono_desc; /* LBAn+1 < (LBAn + NUMn - 1) while (NUMn > 0) */
    bool fragmented;    /* false if monotonic and all LBAs between highest
                         * and lowest are transferred once (i.e no holes) */
    int num_degen;
    int elems;  /* because of degen_mask may have < sge_p array elements */
    int64_t sum;        /* of number_of_blocks */
    uint64_t lowest_lba;
    uint64_t highest_lba;
};

struct cl_sgl_stats {
    struct sgl_stats a_sgl;
    struct sgl_stats b_sgl;
};

struct chs_t {
    uint16_t cyls;      /* EIDE+ATA-2 CHS: (28 bits overall): 16 bits */
    uint16_t heads;     /* EIDE+ATA-2 CHS: (28 bits overall): 4 bits */
    uint16_t sects;     /* EIDE+ATA-2 CHS: (28 bits overall): 8 bits */
};

struct my_opts_t {
    bool append2out_f;
    bool out2stdout;
    bool do_overlap_check;
    bool pr_stats;
    int act_val;
    int degen_mask;
    int do_hex;
    int dry_run;
    int help;
    int verbose;
    const char * b_sgl_arg;
    const char * out_fn;
    struct chs_t chs;
    struct cl_sgl_stats ab_sgl_stats;
    vector<const char *> dev_names;
};


static struct option long_options[] = {
    {"action", required_argument, 0, 'a'},
    {"a-sgl", required_argument, 0, 'A'},
    {"a_sgl", required_argument, 0, 'A'},
    {"b-sgl", required_argument, 0, 'B'},
    {"b_sgl", required_argument, 0, 'B'},
    {"chs", required_argument, 0, 'C'},
    {"c-h-s", required_argument, 0, 'C'},
    {"c_h_s", required_argument, 0, 'C'},
    {"degen", required_argument, 0, 'D'},
    {"dry-run", no_argument, 0, 'd'},
    {"dry_run", no_argument, 0, 'd'},
    {"help", no_argument, 0, 'h'},
    {"hex", no_argument, 0, 'H'},
    {"out", required_argument, 0, 'o'},
    {"overlap-sgl", no_argument, 0, 'O'},
    {"overlap_sgl", no_argument, 0, 'O'},
    {"stats", no_argument, 0, 's'},
    {"verbose", no_argument, 0, 'v'},
    {"version", no_argument, 0, 'V'},
    {0, 0, 0, 0},
};

struct val_name_t {
    int val;
    const char * name;
};

struct val_name_t act_array[] = {
    {ACT_NONE, "no_action"},
    {ACT_NONE, "none"},
    {ACT_APPEND_B2A, "append"},    /* just giving 'append' is sufficient */
    /* more to go here */
    {ACT_ENUMERATE, "xxx"},
    {ACT_ENUMERATE, "enum"},
    {0, NULL},                     /* trailing array sentinel */
};


static void
usage()
{
    pr2serr("Usage: "
            "ddpt_sgl [--action=ACT] [--a-sgl=SGL] [--b-sgl=SGL] "
            "[--chs=CHS]\n"
            "                [--degen=DV] [--dry-run] [--help] [--hex] "
            "[--out=O_SGL]\n"
            "                [-overlap-sgl] [--stats] [--verbose] "
            "[--version]\n"
            "  where:\n"
            "    --action=ACT|-a ACT    ACT is action to take, use 'xxx' "
            "to list\n"
            "                           what is available\n"
            "    --a_sgl=SGL|-A SGL    input the 'A' scatter gather list "
            "(sgl)\n"
            "    --b_sgl=SGL|-B SGL    input the 'B' scatter gather list\n"
            "    --chs=CHS|-C CHS      CHS is '<cyls>,<heads>,<sects>'\n"
            "    --degen=DV|-D DV      DV indicates degenerate element "
            "treatment\n"
            "                          0: ignore all but last degen "
            "element (def)\n"
            "                          1: take in account for highest and "
            "lowest\n"
            "                          2: take in account for monotonic\n"
            "    --dry-run|-d          parse and prepare, then exit\n"
            "    --help|-h             print out usage message then exit\n"
            "    --hex|-H              output sgl is in decimal when '-H' "
            "not given;\n"
            "                          given once: each value prefixed by "
            "'0x'\n"
            "                          given twice: each value in hex and "
            "'HEX' at\n"
            "                          start of output/file\n"
            "    --out=O_SGL|-o O_SGL    O_SGL is name of file to output "
            "sgl to; prefix\n"
            "                            with '+' for append to file; '-' "
            "for stdout\n"
            "    --overlap-sgl|-O      check if either (or both) given SGLs "
            "overlap\n"
            "    --stats|-s            print given sgl(s) statistics\n"
            "    --verbose|-v          increase verbosity\n"
            "    --version|-V          print version string and exit\n\n"
            "A SGL can be a single value (a starting_LBA) or an even "
            "number of comma\nseparated values parsed into a sequence of "
            " starting_LBA,number_of_blocks\npairs. Alternatively SGL can "
            "be a filename prefixed by '@' or 'H@'. For\n'@' that file "
            "contains decimal values by default unless prefixed by '0x'\nor "
            "with a trailing 'h'. For 'H@' that file contains hexadecimal "
            "values\nwith the string 'HEX' at the start of that file.\n"
           );
}

static void
action_enum(void)
{
    pr2serr("Actions available with --action=ACT are:\n"
            "   append-b2a    append b-sgl to end of a-sgl\n"
            "   none          take no action, placeholder\n"
            "   xxxx          more to follow ...\n"
           );
}

static void
pr_statistics(const sgl_stats & sst)
{
    printf("Number of elements: %d, number of degenerates: %d\n", sst.elems,
           sst.num_degen);
    if (sst.not_mono_desc && sst.not_mono_asc)
        printf("  not monotonic, ");
    else if ((! sst.not_mono_desc) && (! sst.not_mono_asc))
        printf("  monotonic (both), ");
    else if (sst.not_mono_desc)
        printf("  monotonic ascending, %s, ",
               sst.fragmented ? "fragmented" : "linear");
    else
        printf("  monotonic descending, %s, ",
               sst.fragmented ? "fragmented" : "linear");
    printf("last degenerate: %s\n", sst.last_degen ? "yes" : "no");
    printf("  lowest,highest LBA: 0x%" PRIx64 ",0x%" PRIx64 "  block "
           "sum: %" PRId64 "\n", sst.lowest_lba, sst.highest_lba, sst.sum);
}

/* definition for function object to do indirect comparson via index_arr */
struct indir_comp_t {
    indir_comp_t(const sgl_vect & a_sgl) : sgl(a_sgl) {}
    const sgl_vect & sgl;

    bool operator()(const int & lhs, const int & rhs ) const
                { return sgl[lhs].lba < sgl[rhs].lba; }
};

static bool
sort_chk_overlap(const sgl_vect & sgl, const char * id_str)
{
    int k, ind, prev_ind;
    int sz = sgl.size();
    uint64_t prev_lba, ll, c_lba;
    uint32_t prev_num;

    if (sz < 2) {
        pr2serr("%s: sg elements do not overlap (sz=%d)\n", id_str, sz);
        return false;
    }
    vector<int> index_arr(sgl.size());
    iota(index_arr.begin(), index_arr.end(), 0);        /* origin 0 */
    indir_comp_t compare_obj(sgl);
    stable_sort(index_arr.begin(), index_arr.end(), compare_obj);

    ind = index_arr[0];
    prev_lba = sgl[ind].lba;
    prev_num = sgl[ind].num;
    prev_ind = 0;
    for (k = 1; k < sz; ++k) {
        ind = index_arr[k];
        c_lba = sgl[ind].lba;
        ll = prev_lba + prev_num;
        if (ll > c_lba)
            break;
        if (ll < (c_lba + sgl[ind].num)) {
            prev_lba = c_lba;
            prev_num = sgl[ind].num;
            prev_ind = ind;
        }
    }
    if (k >= sz) {
        pr2serr("%s: elements do not overlap\n", id_str);
        return false;
    } else {
        pr2serr("%s: elements DO overlap, elements %d and %d clash\n",
                id_str, prev_ind, ind);
        return true;
    }
}

static int
cl_sgl_parse(const char * key, const char * buf, int degen_mask,
             sgl_vect & sgl, struct scat_gath_elem * & sge_p,
             int & num_elems, struct sgl_stats & stats,
             int append_xlen, int vb)
{
    bool def_hex = false;
    bool prev_lba_valid = false;
    int len, err, orig_sz, k, ret;
    uint32_t num;
    int64_t ll;
    uint64_t lba;
    uint64_t prev_lba_plus = 0; /* prev LBA plus prev nm */
    uint64_t prev_lba = 0;
    const char * cp;
    const char * method = "";
    struct scat_gath_elem * ep;
    struct scat_gath_elem * hold_ep = NULL;

    orig_sz = sgl.size();
    if (orig_sz > 0) {
        k = orig_sz - 1;
        prev_lba_plus = stats.not_mono_asc ? 0 : sgl[k].lba + sgl[k].num;
        prev_lba = stats.not_mono_desc ? 0 : sgl[k].lba;
        prev_lba_valid = true;
    }
    if ((append_xlen > 0) && sge_p) {
        hold_ep = (struct scat_gath_elem *)calloc(sizeof(*hold_ep),
                                                  orig_sz + append_xlen);
        if (NULL == hold_ep) {
            pr2serr("%s: unable to allocate memory\n", key);
            return sg_convert_errno(ENOMEM);
        }
        memcpy(hold_ep, sge_p, sizeof(*hold_ep) * append_xlen);
        free(sge_p);
        sge_p = NULL;
    }

    len = (int)strlen(buf);
    if ((('-' == buf[0]) && (1 == len)) || ((len > 1) && ('@' == buf[0])) ||
        ((len > 2) && ('H' == toupper(buf[0])) && ('@' == buf[1]))) {
        if ('H' == toupper(buf[0])) {
            cp = buf + 2;
            def_hex = true;
        } else if ('-' == buf[0])
            cp = buf;
        else
            cp = buf + 1;
        sge_p = file2sgl(cp, def_hex, &num_elems, &err, true);
        if (NULL == sge_p) {
            pr2serr("%s: bad argument, err=%d\n", key, err);
            ret = err ? err : SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        method = "in file";
    } else if (num_either_ch_in_str(buf, len, ',', ' ') > 0) {
        sge_p = cl2sgl(buf, &num_elems, vb > 0);
        if (NULL == sge_p) {
            pr2serr("%s: bad command line argument\n", key);
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        method = "on cl";
    } else {    /* single number on command line (e.g. skip=1234) */
        sge_p = (struct scat_gath_elem *)calloc(1, sizeof(*sge_p));
        if (NULL == sge_p) {
            pr2serr("%s: unable to allocate memory\n", key);
            ret = sg_convert_errno(ENOMEM);
            goto fini;
        }
        ll = sg_get_llnum(buf);
        if (-1LL == ll) {
            pr2serr("%s: unable to decode number\n", key);
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        sge_p->lba = (uint64_t)ll;
        sge_p->num = 0;
        num_elems = 1;
        method = "singleton on cl";
    }

    sgl.reserve(orig_sz + num_elems);
    if (0 == stats.elems)
        stats.lowest_lba = UINT64_MAX;

    for (ep = sge_p, k = 0; k < num_elems; ++k, ++ep) {
        num = ep->num;
        lba = ep->lba;
        if (0 == num) {
            ++stats.num_degen;
            if (k == (num_elems - 1))
                stats.last_degen = true;   /* always append last degen */
            else if (0 == degen_mask)
                continue;       /* so not placed in vector */

            if (0x1 & degen_mask) {
                if (lba < stats.lowest_lba)
                    stats.lowest_lba = lba;
                if (lba > 0) {
                    if ((lba - 1) > stats.highest_lba)
                        stats.highest_lba = lba - 1;
                }
            }
            if (0x2 & degen_mask) {
                if ((k > 0) || prev_lba_valid) {
                    if (! stats.not_mono_asc) {
                        if (lba < prev_lba_plus)
                            stats.not_mono_asc = true;
                        else if (lba > prev_lba_plus)
                            stats.fragmented = true;
                        prev_lba_plus = lba;
                    }
                    if (! stats.not_mono_desc) {
                        if (lba > prev_lba)
                            stats.not_mono_desc = true;
                        else if (lba < prev_lba)
                            stats.fragmented = true;
                        prev_lba = lba;
                    }
                } else {
                     prev_lba_plus = lba;
                     prev_lba = lba;
                }
            }
        } else {        /* normal (i.e. (num > 0), not degenerate) element */
            stats.sum += num;
            if ((k > 0) || prev_lba_valid) {
                if (! stats.not_mono_asc) {
                    if (lba < prev_lba_plus)
                        stats.not_mono_asc = true;
                    else if (lba > prev_lba_plus)
                        stats.fragmented = true;
                    prev_lba_plus = lba + num;
                }
                if (! stats.not_mono_desc) {
                    if ((lba + num) > prev_lba)
                        stats.not_mono_desc = true;
                    else if ((lba + num) < prev_lba)
                        stats.fragmented = true;
                    prev_lba = lba;
                }
            } else {
                prev_lba_plus = lba + num;
                prev_lba = lba;
            }

            if (lba < stats.lowest_lba)
                stats.lowest_lba = lba;
            if ((lba + num) > 0) {
                if ((lba + (num - 1)) > stats.highest_lba)
                    stats.highest_lba = lba + (num - 1);
            }
        }
        sgl.push_back(*ep);
        ++stats.elems;
    }           /* end of for loop over sgl elements (lba,num pairs) */
    if (hold_ep) {      /* vector handles append ok, sge_p needs work here */
        struct scat_gath_elem * sw_ep;

        ep = hold_ep + orig_sz;
        memcpy(ep, sge_p, sizeof(*ep) * num_elems);
        sw_ep = sge_p;  /* swap sge_p <--> hold_ep (use tmp sw_rp) */
        sge_p = hold_ep;
        hold_ep = sw_ep;
        if (hold_ep) {
            free(hold_ep);
            hold_ep = NULL; /* don't want double free at fini */
        }
        num_elems += orig_sz;
    }
    if (vb > 1)
        pr2serr("%s: %s, %d sgl elements\n", key, method, (int)sgl.size());
    ret = 0;

fini:
    if (hold_ep)
        free(hold_ep);
    return ret;
}


int
main(int argc, char * argv[])
{
    int k, n, c, res, err;
    int a_num_elems = 0;
    int b_num_elems = 0;
    int force = 0;
    int ret = 0;
    int64_t ll;
    char b[128];
    struct my_opts_t opts;
    struct my_opts_t * op;
    const char * a_sgl_arg = NULL;
    const char * b_sgl_arg = NULL;
    const char * cp;
    const char * dev_name;
    struct scat_gath_elem * a_sge_p = NULL;  /* has a_num_elems elements */
    struct scat_gath_elem * b_sge_p = NULL;
    struct cl_sgl_stats * ssp;
    struct val_name_t * vnp;
    struct scat_gath_elem sge;
    struct sgl_info_t sgli;
    sgl_vect a_sgl;     /* depending on degen_mask may have < a_num_elems */
    sgl_vect b_sgl;     /* depending on degen_mask may have < b_num_elems */

    op = &opts;
    memset(op, 0, sizeof(opts));
    ssp = &op->ab_sgl_stats;
    ssp->a_sgl.lowest_lba = INT64_MAX;
    ssp->b_sgl.lowest_lba = INT64_MAX;

    while (1) {
        int option_index = 0;

        c = getopt_long(argc, argv, "a:A:B:C:dD:hHo:OsvV",
                        long_options, &option_index);
        if (c == -1)
            break;

        switch (c) {
        case 'a':
            for (vnp = act_array; vnp->name; ++vnp) {
                if (0 == memcmp(optarg, vnp->name, strlen(vnp->name))) {
                    op->act_val = vnp->val;
                    break;
                }
            }
            if (NULL == vnp->name) {
                pr2serr("action: %s not found, try '--action=xxx' to list "
                        "available\n", optarg);
                return SG_LIB_SYNTAX_ERROR;
            }
            break;
        case 'A':
            a_sgl_arg = optarg;
            break;
        case 'B':
            b_sgl_arg = optarg;
            op->b_sgl_arg = optarg;
            break;
        case 'C':
            n = sg_get_num(optarg);
            if ((n < 1) || (n > 0xffff)) {
                pr2serr("--chs= expects <cyls> from 1 to 16383 inclusive\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->chs.cyls = n;
            cp = strchr(optarg, ',');
            if (NULL == cp)
                goto missing_comma;
            n = sg_get_num(cp + 1);
            if ((n < 1) || (n > 0xffff)) {
                pr2serr("--chs= expects <heads> from 1 to 16383 inclusive\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->chs.heads = n;
            cp = strchr(cp + 1, ',');
            if (NULL == cp) {
missing_comma:
                pr2serr("--chs= expects two commas: "
                        "<cyls>,<heads>,<sects>\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            n = sg_get_num(cp + 1);
            if ((n < 1) || (n > 0xffff)) {
                pr2serr("--chs= expects <sects> from 1 to 16383 inclusive\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->chs.sects = n;
            break;
        case 'd':
            ++op->dry_run;
            break;
        case 'D':
            n = sg_get_num(optarg);
            if ((n < 0) || (n > 3)) {
                pr2serr("for --degen= expect a value from 0 to 3 inclusive\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->degen_mask = n;
            break;
        case 'h':
        case '?':
            ++op->help;
            break;
        case 'H':
            ++op->do_hex;
            break;
        case 'o':
            if ('+' == optarg[0]) {
                op->out_fn = optarg + 1;
                op->append2out_f = true;
            } else if ('-' == optarg[0]) {
                op->out2stdout = true;
                op->out_fn = optarg ;
            } else
                op->out_fn = optarg ;
            break;
        case 'O':
            op->do_overlap_check = true;
            break;
        case 's':
            op->pr_stats = true;
            break;
        case 'v':
            ++op->verbose;
            break;
        case 'V':
            pr2serr("version: %s\n", ddpt_sgl_version_str);
            return 0;
        default:
            pr2serr("unrecognised option code 0x%x ??\n", c);
            usage();
            return 1;
        }
    }
    if (optind < argc) {
        if (optind < argc) {
            for (; optind < argc; ++optind)
                op->dev_names.push_back(argv[optind]);
        }
    }

    if (op->help > 0) {
        usage();
        return 0;
    }
    if (ACT_ENUMERATE == op->act_val) {
        action_enum();
        return 0;
    }
    if (a_sgl_arg) {
        res = cl_sgl_parse("a-sgl", a_sgl_arg, op->degen_mask, a_sgl,
                           a_sge_p, a_num_elems, ssp->a_sgl, 0, op->verbose);
        if (res) {
            ret = res;
            goto fini;
        }
        if (op->pr_stats) {
            memset(&sgli, 0, sizeof(sgli));
            sgli.elems = a_num_elems;
            sgli.sglp = a_sge_p;
            pr_statistics(ssp->a_sgl);
            sgl_print(&sgli, true, "a_sgl", true);
        }
    }
    if (b_sgl_arg) {
        res = cl_sgl_parse("b-sgl", b_sgl_arg, op->degen_mask, b_sgl,
                           b_sge_p, b_num_elems, ssp->b_sgl, 0, op->verbose);
        if (res) {
            ret = res;
            goto fini;
        }
        if (op->pr_stats) {
            memset(&sgli, 0, sizeof(sgli));
            sgli.elems = b_num_elems;
            sgli.sglp = b_sge_p;
            pr_statistics(ssp->b_sgl);
            sgl_print(&sgli, true, "b_sgl", true);
        }
    }

    switch (op->act_val) {
    case ACT_DEF:
    case ACT_NONE:
        if (op->do_overlap_check) {
            if (a_sgl_arg)
                sort_chk_overlap(a_sgl, "a_sgl");
            if (b_sgl_arg)
                sort_chk_overlap(b_sgl, "b_sgl");

        } else if ((ACT_DEF == op->act_val) && (NULL == a_sgl_arg) &&
                   (NULL == b_sgl_arg))
            pr2serr("No scatter gather lists or actions given, nothing to "
                    "do, use '-h' for help\n");

        if (ACT_NONE == op->act_val) {
            if ((NULL == a_sgl_arg) && (NULL == b_sgl_arg))
                pr2serr("--action=none given, but nothing to to\n");
        }
        break;
    case ACT_APPEND_B2A:
        if (! (a_sge_p && b_sge_p)) {
            pr2serr("to do append, need both --a-sgl=SGL and --b-sgl=SGL "
                    "to be given\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        res = cl_sgl_parse("append_b2a", op->b_sgl_arg, op->degen_mask,
                           a_sgl, a_sge_p, a_num_elems, ssp->a_sgl,
                           b_num_elems, op->verbose);
        if (res) {
            ret = res;
            goto fini;
        }
        if (op->pr_stats) {
            memset(&sgli, 0, sizeof(sgli));
            sgli.elems = a_num_elems;
            sgli.sglp = a_sge_p;
            pr_statistics(ssp->a_sgl);
            sgl_print(&sgli, true, "append_b2a", true);
        }
        if (a_sgl_arg)
            sort_chk_overlap(a_sgl, "a_append_b");
        break;
    default:
        pr2serr("Unknown action (value=%d) selected\n", op->act_val);
        break;
    }

    if (op->out_fn && op->out_fn[0]) {
        FILE * fp;

        if (op->out2stdout)
            fp = stdout;
        else {
            fp = fopen(op->out_fn, (op->append2out_f ? "a" : "w"));
            if (NULL == fp) {
                err = errno;
                pr2serr("Unable to open %s, error: %s\n", op->out_fn,
                        strerror(err));
                ret = sg_convert_errno(err);
                goto fini;
            } else {
                time_t t = time(NULL);
                struct tm *tm = localtime(&t);
                char s[64];

                strftime(s, sizeof(s), "%c", tm);
                fprintf(fp, "# Scatter gather list generated by "
                        "ddpt_sgl  %s\n\n", s);
            }
            if (op->do_hex > 1)
                fprintf(fp, "HEX\n\n");

            n = a_sgl.size();
            fprintf(fp, "# %d sgl element%s, one element (LBA,NUM) per line\n",
                    n, (1 == n ? "" : "s"));
            for (k = 0; k < n; ++k) {
                const struct scat_gath_elem & sge_r = a_sgl[k];
                if (0 == op->do_hex)
                    fprintf(fp, "%" PRIu64 ",%u\n", sge_r.lba, sge_r.num);
                else if (1 == op->do_hex)
                    fprintf(fp, "0x%" PRIx64 ",0x%u\n", sge_r.lba, sge_r.num);
                else if (op->do_hex > 1)
                    fprintf(fp, "%" PRIx64 ",%u\n", sge_r.lba, sge_r.num);
            }

            if (stdout != fp)
                fclose(fp);
        }
    }

    ret = 0;

fini:
    if (a_sge_p)
        free(a_sge_p);
    if (b_sge_p)
        free(b_sge_p);
    return ret;
}


