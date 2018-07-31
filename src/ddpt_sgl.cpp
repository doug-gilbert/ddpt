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
#include <iterator>
// #include <map>
// #include <list>
#include <algorithm>
#include <system_error>
// #include <thread>
// #include <mutex>
// #include <chrono>
// #include <atomic>
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


static const char * ddpt_sgl_version_str = "0.96 20180719 [svn: r361]";

#include "ddpt.h"
#include "sg_lib.h"
#include "sg_unaligned.h"
#include "sg_pr2serr.h"

using namespace std;
// using namespace std::chrono;

# define ACT_DEF 0
# define ACT_NONE 1
# define ACT_APPEND_B2A 2
# define ACT_TO_CHS 3
# define ACT_SPLIT_N 4
# define ACT_SORT 5
# define ACT_EQUAL 6    /* lists must be same length */
# define ACT_PART_EQUAL 7 /* equality up to shorter of 2 lists */
# define ACT_PART_SAME 9
# define ACT_SAME 8     /* sort both lists then check for equality */
# define ACT_TSPLIT_N 10 /* act in parallel on a-sgl and b-sgl */
# define ACT_DIVISIBLE_N 11 /* on a-sgl, boolean result */
# define ACT_SCALE_N 12 /* on a-sgl --> O_SGL; N>0 multiply, n<0 divide */
# define ACT_ENUMERATE 99


typedef vector<struct scat_gath_elem> sgl_vect;


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
    {"document", no_argument, 0, 'd'},
    {"extension", required_argument, 0, 'e'},
    {"help", no_argument, 0, 'h'},
    {"hex", no_argument, 0, 'H'},
    {"interleave", required_argument, 0, 'i'},
    {"non-overlap", no_argument, 0, 'N'},
    {"non_overlap", no_argument, 0, 'N'},
    {"out", required_argument, 0, 'o'},
    {"quiet", no_argument, 0, 'q'},
    {"round", required_argument, 0, 'r'},
    {"stats", no_argument, 0, 's'},
    {"verbose", no_argument, 0, 'v'},
    {"version", no_argument, 0, 'V'},
    {0, 0, 0, 0},
};

struct val_name_t {
    int val;
    const char * name;
};

/* '*' in a name is expected to be a number */
struct val_name_t act_array[] = {
    {ACT_NONE, "no_action"},
    {ACT_NONE, "none"},
    {ACT_APPEND_B2A, "append"},    /* just giving 'append' is sufficient */
    {ACT_DIVISIBLE_N, "divisible_*"},   /* longer match string first ... */
    {ACT_DIVISIBLE_N, "divisible*"},
    {ACT_ENUMERATE, "xxx"},
    {ACT_ENUMERATE, "enum"},
    {ACT_EQUAL, "equal"},
    {ACT_PART_EQUAL, "part-equal"},
    {ACT_PART_EQUAL, "part_equal"},
    {ACT_PART_SAME, "part-same"},
    {ACT_PART_EQUAL, "part_same"},
    {ACT_SAME, "same"},
    {ACT_SCALE_N, "scale_*"},
    {ACT_SCALE_N, "scale*"},
    {ACT_SORT, "sort"},
    {ACT_SPLIT_N, "split_*"},
    {ACT_SPLIT_N, "split*"},
    {ACT_TO_CHS, "to-chs"},
    {ACT_TO_CHS, "to_chs"},
    {ACT_TSPLIT_N, "tsplit_*"},
    {ACT_TSPLIT_N, "tsplit*"},
    {0, NULL},                     /* trailing array sentinel */
};


static void
usage()
{
    pr2serr("Usage: "
            "ddpt_sgl [--action=ACT] [--a-sgl=SGL] [--b-sgl=SGL] "
            "[--chs=CHS]\n"
            "                [--degen=DV] [--document] [--extension=FNE] "
            "[--help]\n"
            "                [--hex] [--interleave=IL] [--non-overlap] "
            "[--out=O_SGL]\n"
            "                [--quiet] [--round=RB] [--stats] [--verbose] "
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
            "    --document|-d         place copy of command line as "
            "comment in\n"
            "                          start of O_SGL(s)\n"
            "    --extension=FNE|-e FNE    filename extension (i.e. used "
            "after '.')\n"
            "    --help|-h             print out usage message then exit\n"
            "    --hex|-H              output sgl is in decimal when '-H' "
            "not given;\n"
            "                          given once: each value prefixed by "
            "'0x'\n"
            "                          given twice: each value in hex and "
            "'HEX' at\n"
            "                          start of output/file\n"
            "    --interleave=IL|-i IL    move to next output sgl when "
            "splitting\n"
            "                             (def: 0 --> no interleave)\n"
            "    --non-overlap|-N      check if either (or both) given SGLs "
            "overlap\n"
            "    --out=O_SGL|-o O_SGL    O_SGL is name of file to output "
            "sgl to; prefix\n"
            "                            with '+' for append to file; '-' "
            "for stdout\n"
            "    --quiet|-q            suppress output to stderr\n"
            "    --round=RB|-r RB      RB is maximum number of blocks to "
            "round to\n"
            "                          nearest sge boundary when splitting "
            "(def: 0)\n"
            "    --stats|-s            print given sgl(s) statistics\n"
            "    --verbose|-v          increase verbosity\n"
            "    --version|-V          print version string and exit\n\n"
            "A SGL can be a single value (a starting_LBA) or an even "
            "number of comma\nseparated values parsed into a sequence of "
            " starting_LBA,number_of_blocks\npairs. Alternatively SGL can "
            "be a filename prefixed by '@' or 'H@'. For\n'@' that file "
            "contains decimal values by default unless prefixed by '0x'\nor "
            "with a trailing 'h'. For 'H@' that file contains hexadecimal "
            "values\nwith the string 'HEX' at the start of that file. When "
            "'-e FNE' is\ngiven then the form of created filenames is "
            "'O_SGL<n>.FNE' when\n<n> starts at 1.\n"
           );
}

/* Enumerate actions (to stderr) */
static void
action_enum(void)
{
    pr2serr("Actions available with --action=ACT are:\n"
            "   append-b2a    append b-sgl to end of a-sgl, output to O_SGL\n"
            "   divisible<n>[,L|N]    returns true if all starting LBAs "
            "and/or NUMs\n"
            "                         in a-sgl are divisible by <n>; "
            "otherwise\n"
            "                         returns false. If ',L' appended only "
            "checks LBAs\n"
            "   enum          print this list...\n"
            "   equal         compare a-sgl to b-sgl; require same order "
            "and length\n"
            "   none          take no action, placeholder\n"
            "   part-equal    compare a-sgl to b-sgl\n"
            "   part-same     compare a-sgl to b-sgl\n"
            "   same          compare a-sgl to b-sgl, ignoring order\n"
            "   sort          sort a-sgl and output to O_SGL\n"
            "   scale<n>      multiply all starting LBAs in a_sgl by <n> "
            "(if > 0)\n"
            "                 and divide all NUMs by <n>, result --> "
            "O_SGL;\n"
            "                 if <n> < 0 then divide all ...\n"
            "   split<n>      divide a-sgl into 'n' O_SGLs\n"
            "   to-chs        assume a-sgl is flat sgl, convert to CHS in "
            "O_SGL\n"
            "   tsplit<n>     twin split: divide a-sgl and b-sgl into two "
            "series\n"
            "   xxx           print this list...\n"
           );
}

/* Print statistics (to stdout) */
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

/* Print scatter gather list (to stdout) */
static void
pr_sgl(const struct scat_gath_elem * first_elemp, int num_elems, bool in_hex)
{
    const struct scat_gath_elem * sgep = first_elemp;
    char lba_str[20];
    char num_str[20];

    printf("Scatter gather list, number of elements: %d\n", num_elems);
    printf("    Logical Block Address   Number of blocks\n");
    for (int k = 0; k < num_elems; ++k, ++sgep) {
        if (in_hex) {
            snprintf(lba_str, sizeof(lba_str), "0x%" PRIx64, sgep->lba);
            snprintf(num_str, sizeof(num_str), "0x%x", sgep->num);
            printf("    %-14s          %-12s\n", lba_str, num_str);
        } else
            printf("    %-14" PRIu64 "          %-12u\n", sgep->lba,
                   sgep->num);
    }
}

/* definition for function object to do indirect comparson via index_arr */
struct indir_comp_t {
    indir_comp_t(const sgl_vect & a_sgl) : sgl(a_sgl) {}
    const sgl_vect & sgl;

    bool operator()(const int & lhs, const int & rhs ) const
                { return sgl[lhs].lba < sgl[rhs].lba; }
};

/* Check if 'sgl' has any overlapping blocks. First the sgl is sorted
 * by LBA; then each LBA segment is checked with the one following it
 * to see if they overlap (with the number of blocks of the first/lower
 * one taken into account. Returns true if the overlap, false if they
 * don't. 'sgl' is not modified. */
static bool
non_overlap_check(const sgl_vect & sgl, const char * id_str, bool quiet)
{
    int k, ind, prev_ind;
    int sz = sgl.size();
    uint64_t prev_lba, ll, c_lba;
    uint32_t prev_num;

    if (sz < 2) {
        if (! quiet)
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
        if (! quiet)
            pr2serr("%s: elements do not overlap\n", id_str);
        return true;
    } else {
        if (! quiet)
            pr2serr("%s: elements DO overlap, elements %d and %d clash\n",
                    id_str, prev_ind, ind);
        return false;
    }
}

static void
sort2o_sgl(const sgl_vect & sgl, sgl_vect & out_sgl)
{
    int k, ind;
    int sz = sgl.size();
    struct scat_gath_elem a_sge;
    vector<int> index_arr(sz);

    iota(index_arr.begin(), index_arr.end(), 0);        /* origin 0 */
    if (sz > 1) {
        indir_comp_t compare_obj(sgl);
        stable_sort(index_arr.begin(), index_arr.end(), compare_obj);
    }
    for (k = 0; k < sz; ++k) {
        ind = index_arr[k];
        a_sge.lba = sgl[ind].lba;
        a_sge.num = sgl[ind].num;
        out_sgl.push_back(a_sge);
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
        method = "on command line";
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
            pr2serr("If '%s' is a filename containing a sgl then prefix it "
                    "with a '@'\n", buf);
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        sge_p->lba = (uint64_t)ll;
        sge_p->num = 0;
        num_elems = 1;
        method = "singleton on command line";
    }

    sgl.reserve(orig_sz + num_elems);
    if (0 == stats.elems)
        stats.lowest_lba = UINT64_MAX;

    for (ep = sge_p, k = 0; k < num_elems; ++k, ++ep) {
        num = ep->num;
        lba = ep->lba;
        if (0 == num) {
            ++stats.num_degen;
            if (k == (num_elems - 1)) {
                stats.last_degen = true;   /* always append last degen */
                if (stats.lowest_lba > stats.highest_lba)
                    stats.lowest_lba = stats.highest_lba;
            } else if (0 == degen_mask)
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
    if (vb > 1) {
        k = (int)sgl.size();
        pr2serr("%s: %s, %d sgl element%s\n", key, method, k,
                (1 == k) ? "" : "s");
    }
    ret = 0;

fini:
    if (hold_ep)
        free(hold_ep);
    return ret;
}

/* hold limiting values of CHS or the mapping a a flat LBA */
struct work_chs_t {
    uint32_t cyl;
    uint32_t head;
    uint32_t sect;
};

static inline struct work_chs_t
calc_chs(uint64_t flat_lba, const struct work_chs_t & max_w_chs)
{
    struct work_chs_t res_chs;

    res_chs.cyl = (uint32_t)(flat_lba / (max_w_chs.sect * max_w_chs.head));
    res_chs.head = (uint32_t)((flat_lba / max_w_chs.sect) % max_w_chs.head);
    res_chs.sect = (uint32_t)(flat_lba % max_w_chs.sect);
    return res_chs;
}

static int
convert2chs(const sgl_vect & in_sgl, sgl_vect & out_sgl, int max_bpt,
            struct sgl_opts_t * op)
{
    int k;
    uint32_t num_blks, total_sectors, n;
    uint64_t lba;
    struct work_chs_t max_chs, a_chs;
    struct scat_gath_elem a_sge;

    max_chs.cyl = (uint32_t)op->chs.cyls + 1;
    max_chs.head = (uint32_t)op->chs.heads + 1;
    max_chs.sect = (uint32_t)op->chs.sects;
    total_sectors = max_chs.cyl * max_chs.head * max_chs.sect;

    for (k = 0; k < (int)in_sgl.size(); ++k) {
        const struct scat_gath_elem & sge = in_sgl[k];

        lba = sge.lba;
        num_blks = sge.num;
        if (0 == num_blks)
            continue;
        if ((lba + num_blks) > (uint64_t)total_sectors) {
            pr2serr("%s: total chs sectors (%u) exceeded in in_sgl element "
                    "%d\n", __func__, total_sectors, k + 1);
            return SG_LIB_LBA_OUT_OF_RANGE;
        }
        for (n = 0; num_blks > 0; num_blks -= n, lba += n) {
            a_chs = calc_chs(lba, max_chs);
            a_sge.lba = (a_chs.sect + 1) | ((0xffff & a_chs.cyl) << 8) |
                        (a_chs.head << 24);
            if ((num_blks + a_chs.sect) <= max_chs.sect)
                n = num_blks;
            else
                n = max_chs.sect - a_chs.sect;
            if (n > (uint32_t)max_bpt)
                n = max_bpt;
            if (0 == n)
                break;
            a_sge.num = n;
            out_sgl.push_back(a_sge);
        }
    }
    return 0;
}

static int
do_split(const sgl_vect & in_sgl, const struct scat_gath_elem * in_sgep,
         const struct sgl_stats & in_stats, struct sgl_opts_t * op)
{
    bool chk_last = false;
    int k, n, m, d, rem, rblks;
    int res = 0;
    int vb = op->verbose;
    uint32_t nblks;
    int64_t nn;
    const char * fnamep;
    FILE * fp;
    struct sgl_iter_t iter, hold_iter;

    if (in_stats.sum < 1) {
        pr2serr("%s: need to know blocks in sgl, to split\n", __func__);
        return SG_LIB_SYNTAX_ERROR;
    }
    if (op->split_n > in_stats.sum) {
        pr2serr("split_n action value (%d) exceeds sum of blocks (%" PRId64
                ")\n", op->split_n, in_stats.sum);
        return SG_LIB_CONTRADICT;
    }
    if (vb > 3)
        pr2serr("%s: sum=%" PRId64 ", interleave=%d, split_n=%d\n", __func__,
                in_stats.sum, op->interleave, op->split_n);

    if (op->interleave > 0) {
        nn = in_stats.sum / op->interleave;
        if (nn > INT32_MAX) {
            pr2serr("%s: for sanity limit number of interleaved segments to "
                    "2**31-1\n", __func__);
            pr2serr("%s: try increasing IL given to --interleave=IL\n",
                    __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        nblks = op->interleave;
        n = (int32_t)nn;
    } else {
        nn = in_stats.sum / op->split_n;
        if (nn > INT32_MAX) {
            pr2serr("%s: for sanity limit blocks in each split file to "
                    "2**31-1\n", __func__);
            if (nn < (2LL * INT32_MAX))
                pr2serr("%s: try increasing the number of split files\n",
                        __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        nblks = (int)nn;
        n = op->split_n;
    }
    rem = (in_stats.sum % nblks);
    chk_last = !! rem;
    memset(&iter, 0, sizeof(iter));
    iter.sglp = (struct scat_gath_elem *)in_sgep;       /* won't modify */
    iter.elems = in_sgl.size();
    if (op->round_blks > 0) {
        if (nblks < 10) {
            if (! op->quiet)
                pr2serr("--round=%d ignored when average blocks per segment "
                        "less than 10\n", op->round_blks);
            op->round_blks = 0;
        } else if ((uint32_t)op->round_blks > (nblks / 3)) {
            if (! op->quiet)
                pr2serr("--round=%d ignored when > one third of average "
                        "blocks per segment\n", op->round_blks);
            op->round_blks = 0;
        }
    }
    for (k = 0; k < n; ++k) {
        m = (k % op->split_n);
        fp = op->split_out_fns[m].fp;
        fnamep = op->split_out_fns[m].out_fn.c_str();
        if (vb > 3)
            pr2serr("Writing to %s {k=%d}:\n", fnamep, k);
        if (chk_last && (k == (n - 1)))
            nblks += rem;
        if (op->round_blks > 0) {
            hold_iter = iter;   /* rounding can cause nblks to be modified */
            rblks = (op->round_blks < ((int)nblks / 2)) ? op->round_blks : 0;
        } else
            rblks = 0;
        res = iter_add_process(&iter, nblks, output_sge, fp, op->do_hex,
                               rblks, vb);
        if (res) {
            pr2serr("%s: error while writing to %s: [0x%x], k=%d, n=%d\n",
                    __func__, fnamep, res, k, n);
            sg_if_can2stderr("", res);
            break;
        }
        if (op->round_blks > 0) {
            if (vb > 5) {
                sgl_iter_print(&iter, "current iter (lhs)", true);
                sgl_iter_print(&hold_iter, "prior iter", true);
            }
            d = sgl_iter_diff(&iter, &hold_iter);
            if (nblks != (uint32_t)d) {
                if (vb > 3)
                    pr2serr("rounding, adjusting nblks=%u to %d, k=%d\n",
                            nblks, d, k);
                nblks = d;
            }
        }
    }
    return res;
}

static int
do_tsplit(const sgl_vect & a_sgl, const struct scat_gath_elem * a_sgep,
          const sgl_vect & b_sgl, const struct scat_gath_elem * b_sgep,
          struct sgl_opts_t * op)
{
    bool chk_last = false;
    int k, n, m, d, rem, rblks;
    int res = 0;
    int vb = op->verbose;
    uint32_t nblks;
    int64_t nn, a_sum, b_sum;
    const char * fnamep;
    FILE * fp;
    const struct sgl_stats * a_statsp = &op->ab_sgl_stats.a_stats;
    const struct sgl_stats * b_statsp = &op->ab_sgl_stats.b_stats;
    struct sgl_iter_t iter, b_iter, hold_iter;

    a_sum = a_statsp->sum;
    b_sum = b_statsp->sum;
    memset(&iter, 0, sizeof(iter));
    memset(&b_iter, 0, sizeof(b_iter));
    if ((a_sum < 1) && (b_sum < 1)) {
        pr2serr("%s: no block count in both a_sgl and b_sgl, unable to "
                "tsplit\n", __func__);
        return SG_LIB_SYNTAX_ERROR;
    } else if (a_sum && b_sum) {
        if (a_sum == b_sum)
            ;   /* this is fine */
        else if (a_sum < b_sum) {
            pr2serr("%s: 'A' sgl sum_blks (%" PRIu64 ") < 'B' sgl sum_blks "
                    "(%" PRIu64 ")\n", __func__, a_sum, b_sum);
            return SG_LIB_CONTRADICT;
        } else {        /* so a_sum > b_sum */
            if (! b_statsp->last_degen) {
                pr2serr("%s: sum_blks: 'A' sgl > 'B'; but trailing 'B' "
                        "element not degenerate\n", __func__);
                return SG_LIB_SYNTAX_ERROR;
            }
            b_iter.extend_last = true;
        }
    } else if (b_sum < 1) {
        if (! b_statsp->last_degen) {
            pr2serr("%s: sum_blks: 'A' sgl > 'B'; but trailing 'B' "
                    "element not degenerate\n", __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        b_iter.extend_last = true;
    } else {    /* so a_sum < 1 */
        pr2serr("%s: no block count in a_sgl\n", __func__);
        return SG_LIB_SYNTAX_ERROR;
    }

    if (op->split_n > a_sum) {
        pr2serr("tsplit_n action value (%d) exceeds sum of 'A' blocks (%"
                PRId64 ")\n", op->split_n, a_sum);
        return SG_LIB_CONTRADICT;
    }
    if (vb > 3)
        pr2serr("%s: a_sum=%" PRId64 ", b_sum=%" PRId64 ", interleave=%d, "
                "split_n=%d\n", __func__, a_sum, b_sum, op->interleave,
                op->split_n);

    if (op->interleave > 0) {
        nn = a_sum / op->interleave;
        if (nn > INT32_MAX) {
            pr2serr("%s: for sanity limit number of interleaved segments to "
                    "2**31-1\n", __func__);
            pr2serr("%s: try increasing IL given to --interleave=IL\n",
                    __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        nblks = op->interleave;
        n = (int32_t)nn;
    } else {
        nn = a_sum / op->split_n;
        if (nn > INT32_MAX) {
            pr2serr("%s: for sanity limit blocks in each split file to "
                    "2**31-1\n", __func__);
            if (nn < (2LL * INT32_MAX))
                pr2serr("%s: try increasing the number of split files\n",
                        __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        nblks = (int)nn;
        n = op->split_n;
    }
    rem = (a_sum % nblks);
    chk_last = !! rem;
    iter.sglp = (struct scat_gath_elem *)a_sgep;       /* won't modify */
    iter.elems = a_sgl.size();
    b_iter.sglp = (struct scat_gath_elem *)b_sgep;       /* won't modify */
    b_iter.elems = b_sgl.size();
    if (op->round_blks > 0) {
        if (nblks < 10) {
            if (! op->quiet)
                pr2serr("--round=%d ignored when average blocks per segment "
                        "less than 10\n", op->round_blks);
            op->round_blks = 0;
        } else if ((uint32_t)op->round_blks > (nblks / 3)) {
            if (! op->quiet)
                pr2serr("--round=%d ignored when > one third of average "
                        "blocks per segment\n", op->round_blks);
            op->round_blks = 0;
        }
    }
    for (k = 0; k < n; ++k) {
        m = (k % op->split_n);
        fp = op->split_out_fns[m].fp;
        fnamep = op->split_out_fns[m].out_fn.c_str();
        if (vb > 3)
            pr2serr("Writing to %s {k=%d}:\n", fnamep, k);
        if (chk_last && (k == (n - 1)))
            nblks += rem;
        if (op->round_blks > 0) {
            hold_iter = iter;   /* rounding can cause nblks to be modified */
            rblks = (op->round_blks < ((int)nblks / 2)) ? op->round_blks : 0;
        } else
            rblks = 0;
        res = iter_add_process(&iter, nblks, output_sge, fp, op->do_hex,
                               rblks, vb);
        if (res) {
            pr2serr("%s: error while writing to %s: [0x%x], k=%d, n=%d\n",
                    __func__, fnamep, res, k, n);
            sg_if_can2stderr("", res);
            break;
        }
        if (op->round_blks > 0) {
            if (vb > 5) {
                sgl_iter_print(&iter, "current iter (lhs)", true);
                sgl_iter_print(&hold_iter, "prior iter", true);
            }
            d = sgl_iter_diff(&iter, &hold_iter);
            if (nblks != (uint32_t)d) {
                if (vb > 3)
                    pr2serr("rounding, adjusting nblks=%u to %d, k=%d\n",
                            nblks, d, k);
                nblks = d;
            }
        }
        /* Now split b_sgl using same nblks; ignoring interleave and round */
        fp = op->b_split_out_fns[m].fp;
        fnamep = op->b_split_out_fns[m].out_fn.c_str();
        if (vb > 3)
            pr2serr("Writing to %s {k=%d}:\n", fnamep, k);
        res = iter_add_process(&b_iter, nblks, output_sge, fp, op->do_hex,
                               0, vb);
        if (res) {
            pr2serr("%s: error while writing to %s: [0x%x], k=%d, n=%d\n",
                    __func__, fnamep, res, k, n);
            sg_if_can2stderr("", res);
            break;
        }
    }
    return res;
}

static int
parse_action(const char * optarg, struct sgl_opts_t * op)
{
    bool ok;
    int len, n, off;
    const char * cp;
    const char * commap;
    const char * comma2p;
    struct val_name_t * vnp;

    for (vnp = act_array, ok = false; vnp->name; ++vnp) {
        len = strlen(vnp->name);
        commap = strchr(optarg, ',');
        if ((cp = strchr(vnp->name, '*'))) {  /* action with numeric suffix */
            if (cp == vnp->name)
                break;  /* don't accept '*' in 1st position */
            off = cp - vnp->name;       /* offset corresponding to '*' */
            if (0 == memcmp(optarg, vnp->name, off)) {
                if ((op->act_val > 0) && (op->act_val == vnp->val))
                    return 0;   /* already processed this action */
                op->act_val = vnp->val;
                if ('-' == optarg[off]) {
                    if (0 == strcmp("-1", optarg + off))
                        n = -1;
                    else {      /* decode starting after '-' */
                        n = sg_get_num(optarg + off + 1);
                        if (-1 == n) {
                            pr2serr("'--action=%s' unable to decode "
                                    "negative number\n", optarg);
                            return SG_LIB_SYNTAX_ERROR;
                        }
                        n = -n;         /* now turn negative */
                    }
                } else {
                    n = sg_get_num(optarg + off);
                    if (-1 == n) {
                        pr2serr("'--action=%s' unable to decode number\n",
                                optarg);
                        return SG_LIB_SYNTAX_ERROR;
                    }
                }
                if (0 == n) {
                    pr2serr("'--action=%s' value of 0 not permitted\n",
                            optarg);
                    return SG_LIB_SYNTAX_ERROR;
                }
                switch (vnp->val) {
                case ACT_DIVISIBLE_N:
                    if (n < 0) {
                        pr2serr("'--action=%s' negative value not "
                                "permitted\n", optarg);
                        return SG_LIB_SYNTAX_ERROR;
                    }
                    op->div_scale_n = n;
                    if (commap) { /* <n>,L,n taken as both */
                        if ('L' == toupper(*(commap + 1))) {
                            comma2p = strchr(commap + 1, ',');
                            if (comma2p) {
                                if ('N' != toupper(*(comma2p + 1)))
                                    op->div_lba_only = true;
                            } else
                                op->div_lba_only = true;
                        } else if ('N' == toupper(*(commap + 1))) {
                            comma2p = strchr(commap + 1, ',');
                            if (comma2p) {
                                if ('L' != toupper(*(comma2p + 1)))
                                    op->div_num_only = true;
                            } else
                                op->div_num_only = true;
                        }
                    }
                    break;
                case ACT_SCALE_N:
                    op->div_scale_n = n;
                    break;
                case ACT_SPLIT_N:
                case ACT_TSPLIT_N:
                    if (n <= 0) {
                        pr2serr("'--action=%s' zero or negative value not "
                                "permitted\n", optarg);
                        return SG_LIB_SYNTAX_ERROR;
                    }
                    op->split_n = n;
                    break;
                default:
                    pr2serr("'--action=%s' unexpected\n", optarg);
                    return SG_LIB_LOGIC_ERROR;
                }
            }
        } else if (0 == memcmp(optarg, vnp->name, len)) {
            ok = true;
            op->act_val = vnp->val;
            break;
        }
    }
    if (! ok) {
        pr2serr("'--action=%s' not found or ill-formed, try "
                "'--action=xxx' to list available\n", optarg);
        return SG_LIB_SYNTAX_ERROR;
    }
    return 0;
}


int
main(int argc, char * argv[])
{
    bool lba_side;
    bool ret_bool = true;
    bool use_res_sgl = false;
    bool verbose_given = false;
    bool version_given = false;
    bool write2o_sgl = false;
    int a, k, j, n, c, res, err, vb;
    int a_num_elems = 0;        /* elements in a_sge_p */
    int b_num_elems = 0;        /* elements in b_sge_p */
    int ret = 0;
    char b[256];
    static const int blen = sizeof(b);
    struct sgl_opts_t opts;
    struct sgl_opts_t * op;
    const char * a_sgl_arg = NULL;
    const char * b_sgl_arg = NULL;
    const char * cp;
    FILE * fp;
    sgl_vect::iterator sgvi;
    struct scat_gath_elem * a_sge_p = NULL;  /* has a_num_elems elements */
    struct scat_gath_elem * b_sge_p = NULL;
    struct scat_gath_elem * sgep;
    struct cl_sgl_stats * ssp;
    string cmd_line;
    struct sgl_info_t sgli;
    sgl_vect a_sgl;     /* depending on degen_mask may have < a_num_elems */
    sgl_vect b_sgl;     /* depending on degen_mask may have < b_num_elems */
    sgl_vect res_sgl;
    sgl_vect res_b_sgl;

    for (k = 0; k < argc; ++k) {
        if (k)
            cmd_line.push_back(' ');
        cmd_line += argv[k];
    }
    op = &opts;
    memset((void *)op, 0, sizeof(opts));  // sizeof(struct sgl_opts_t));
    ssp = &op->ab_sgl_stats;
    ssp->a_stats.lowest_lba = INT64_MAX;
    ssp->b_stats.lowest_lba = INT64_MAX;

    while (1) {
        int option_index = 0;

        c = getopt_long(argc, argv, "a:A:B:C:dD:e:hHi:No:qr:svV",
                        long_options, &option_index);
        if (c == -1)
            break;

        switch (c) {
        case 'a':
            res = parse_action(optarg, op);
            if (res)
                return res;
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
            if ((n < 1) || (n > 0x10000)) {
                pr2serr("--chs= expects <cyls> from 1 to 65536 inclusive\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            /* store in cyls 0-based (so maximum (65536) fits in 16 bits) */
            op->chs.cyls = n - 1;
            cp = strchr(optarg, ',');
            if (NULL == cp)
                goto missing_comma;
            n = sg_get_num(cp + 1);
            if ((n < 1) || (n > 16)) {
                pr2serr("--chs= expects <heads> from 1 to 16 inclusive\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            /* store in heads 0-based */
            op->chs.heads = n - 1;
            cp = strchr(cp + 1, ',');
            if (NULL == cp) {
missing_comma:
                pr2serr("--chs= expects two commas: "
                        "<cyls>,<heads>,<sects>\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            n = sg_get_num(cp + 1);
            if ((n < 1) || (n > 255)) {
                pr2serr("--chs= expects <sects> per track from 1 to 255 "
                        "inclusive\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            /* sectors per track, leave 1-based */
            op->chs.sects = n;
            op->chs_given = true;
            break;
        case 'd':
            op->document = true;
            break;
        case 'D':
            n = sg_get_num(optarg);
            if ((n < 0) || (n > 3)) {
                pr2serr("for --degen= expect a value from 0 to 3 inclusive\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->degen_mask = n;
            break;
        case 'e':
            op->fne = optarg;   /* FNE: filename extension */
            break;
        case 'h':
        case '?':
            ++op->help;
            break;
        case 'H':
            ++op->do_hex;
            break;
        case 'i':
            n = sg_get_num(optarg);
            if (n < 0) {
                pr2serr("for --interleave= expect a value of 0 or greater\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->interleave = n;
            break;
        case 'N':
            op->non_overlap_chk = true;
            break;
        case 'o':
            if ('+' == optarg[0]) {
                op->out_fn = optarg + 1;
                op->append2out_f = true;
            } else if ('-' == optarg[0]) {
                op->out2stdout = true;
                op->out_fn = optarg;
            } else
                op->out_fn = optarg;
            break;
        case 'q':
            op->quiet = true;
            break;
        case 'r':
            n = sg_get_num(optarg);
            if (n < 0) {
                pr2serr("for --round= expect a value of 0 or greater\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->round_blks = n;
            break;
        case 's':
            op->pr_stats = true;
            break;
        case 'v':
            verbose_given = true;
            ++op->verbose;
            break;
        case 'V':
            version_given = true;
            break;
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
#ifdef DEBUG
    pr2serr("In DEBUG mode, ");
    if (verbose_given && version_given) {
        pr2serr("but override: '-vV' given, zero verbose and continue\n");
        verbose_given = false;
        version_given = false;
        op->verbose = 0;
    } else if (! verbose_given) {
        pr2serr("set '-vv'\n");
        op->verbose = 2;
        op->quiet = false;      /* override --quiet if given */
    } else
        pr2serr("keep verbose=%d\n", op->verbose);
#else
    if (verbose_given && version_given)
        pr2serr("Not in DEBUG mode, so '-vV' has no special action\n");
#endif
    if (version_given) {
        pr2serr("version: %s\n", ddpt_sgl_version_str);
        return 0;
    }
    vb = op->verbose;

    if (ACT_ENUMERATE == op->act_val) {
        action_enum();
        return 0;
    }
    if (a_sgl_arg) {
        res = cl_sgl_parse("a-sgl", a_sgl_arg, op->degen_mask, a_sgl,
                           a_sge_p, a_num_elems, ssp->a_stats, 0, vb);
        if (res) {
            ret = res;
            goto fini;
        }
        if (op->pr_stats) {
            memset(&sgli, 0, sizeof(sgli));
            sgli.elems = a_num_elems;
            sgli.sglp = a_sge_p;
            pr_statistics(ssp->a_stats);
            sgl_print(&sgli, true, "a_sgl", true);
        }
    }
    if (b_sgl_arg) {
        res = cl_sgl_parse("b-sgl", b_sgl_arg, op->degen_mask, b_sgl,
                           b_sge_p, b_num_elems, ssp->b_stats, 0, vb);
        if (res) {
            ret = res;
            goto fini;
        }
        if (op->pr_stats) {
            memset(&sgli, 0, sizeof(sgli));
            sgli.elems = b_num_elems;
            sgli.sglp = b_sge_p;
            pr_statistics(ssp->b_stats);
            sgl_print(&sgli, true, "b_sgl", true);
        }
    }

    switch (op->act_val) {
    case ACT_DEF:
    case ACT_NONE:
        if (op->non_overlap_chk) {
            if (ret_bool && a_sgl_arg)
                ret_bool = non_overlap_check(a_sgl, "a_sgl", op->quiet);
            if (ret_bool && b_sgl_arg)
                ret_bool = non_overlap_check(b_sgl, "b_sgl", op->quiet);
        } else if ((ACT_DEF == op->act_val) && (NULL == a_sgl_arg) &&
                   (NULL == b_sgl_arg))
            pr2serr("No scatter gather lists or actions given, nothing to "
                    "do, use '-h' for help\n");

        if (a_sgl_arg && (NULL == b_sgl_arg) && op->out_fn) {
            if (! op->quiet)
                pr2serr("As a convenience copying --a-sgl=SGL to "
                        "--out=O_SGL\n");
            write2o_sgl = true;
        } else {
            if (ACT_NONE == op->act_val) {
                if ((NULL == a_sgl_arg) && (NULL == b_sgl_arg))
                    pr2serr("--action=none given, but nothing to to\n");
                if (! op->quiet) {
                    if (a_sgl_arg)
                        printf("a_sgl_elems=%d\n", (int)a_sgl.size());
                    if (b_sgl_arg)
                        printf("b_sgl_elems=%d\n", (int)b_sgl.size());
                }
            }
            write2o_sgl = false;
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
                           a_sgl, a_sge_p, a_num_elems, ssp->a_stats,
                           b_num_elems, vb);
        if (res) {
            ret = res;
            goto fini;
        }
        if (op->pr_stats) {
            memset(&sgli, 0, sizeof(sgli));
            sgli.elems = a_num_elems;
            sgli.sglp = a_sge_p;
            pr_statistics(ssp->a_stats);
            sgl_print(&sgli, true, "append_b2a", true);
        }
        if (op->non_overlap_chk && a_sgl_arg && ret_bool)
            ret_bool = non_overlap_check(a_sgl, "a_append_b", op->quiet);
        use_res_sgl = false;    /* so write out a_sgl */
        write2o_sgl = true;
        break;
    case ACT_DIVISIBLE_N:
        if (! a_sge_p) {
            pr2serr("to do divisble<n>, need --a-sgl=SGL to be given\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        n = op->div_scale_n;
        write2o_sgl = false;
        lba_side = false;
        for (k = 0, sgep = a_sge_p; k < a_num_elems; ++k, ++sgep) {
            if (! op->div_num_only) {
                if (sgep->lba % n) {
                    ret_bool = false;
                    lba_side = true;
                    break;
                }
            }
            if (! op->div_lba_only) {
                if (sgep->num % n) {
                    ret_bool = false;
                    break;
                }
            }
        }
        if (! op->quiet) {
            if (op->div_lba_only)
                cp = "LBAs";
            else if (op->div_num_only)
                cp = "NUMs";
            else
                cp = "LBAs and NUMs";
            pr2serr("--a-sgl=SGL, all %s are %sdivisible by %d\n",
                    cp, (ret_bool ? "" : "_not_ "), n);
            if (vb && (false == ret_bool))
                pr2serr("  first false: sgl elem=%d (origin 0) %s\n",
                        k, (lba_side ? "LBA" : "NUM"));
        }
        break;
    case ACT_EQUAL:
        if (! (a_sge_p && b_sge_p)) {
            pr2serr("to do equal, need both --a-sgl=SGL and --b-sgl=SGL "
                    "to be given\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        ret_bool = sgl_eq(a_sge_p, a_num_elems, 0, b_sge_p, b_num_elems, 0,
                          false);
        if (! op->quiet)
            pr2serr("--a-sgl=SGL and --b-sgl=SGL are %sequal\n",
                    ret_bool ? "" : "_not_ ");
        write2o_sgl = false;
        break;
    case ACT_PART_EQUAL:
        if (! (a_sge_p && b_sge_p)) {
            pr2serr("to do part-equal, need both --a-sgl=SGL and --b-sgl=SGL "
                    "to be given\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        ret_bool = sgl_eq(a_sge_p, a_num_elems, 0, b_sge_p, b_num_elems, 0,
                          true);
        if (! op->quiet)
            pr2serr("--a-sgl=SGL and --b-sgl=SGL are %spart equal\n",
                    ret_bool ? "" : "_not_ ");
        write2o_sgl = false;
        break;
    case ACT_PART_SAME:
        if (! (a_sge_p && b_sge_p)) {
            pr2serr("to do part-same, need both --a-sgl=SGL and --b-sgl=SGL "
                    "to be given\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        sort2o_sgl(a_sgl, res_sgl);
        sort2o_sgl(b_sgl, res_b_sgl);
        ret_bool = sgl_eq(res_sgl.data(), res_sgl.size(), 0,
                          res_b_sgl.data(), res_b_sgl.size(), 0, true);
        if (! op->quiet)
            pr2serr("--a-sgl=SGL and --b-sgl=SGL are %spart same%s\n",
                    (ret_bool ? "" : "_not_ "),
                    (vb ? " (part equal after sorting both)" : ""));
        write2o_sgl = false;
        break;
    case ACT_SAME:
        if (! (a_sge_p && b_sge_p)) {
            pr2serr("to do same, need both --a-sgl=SGL and --b-sgl=SGL "
                    "to be given\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        sort2o_sgl(a_sgl, res_sgl);
        sort2o_sgl(b_sgl, res_b_sgl);
        ret_bool = sgl_eq(res_sgl.data(), res_sgl.size(), 0,
                          res_b_sgl.data(), res_b_sgl.size(), 0, false);
        if (! op->quiet)
            pr2serr("--a-sgl=SGL and --b-sgl=SGL are %sthe same%s\n",
                    (ret_bool ? "" : "_not_ "),
                    (vb ? " (equal after sorting both)" : ""));
        write2o_sgl = false;
        break;
    case ACT_SCALE_N:
        n = op->div_scale_n;
        if (! (a_sge_p && op->out_fn)) {
            pr2serr("--action=scale_%d needs --a_sgl= and --out= options\n",
                    n);
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        n = op->div_scale_n;
        if (0 == n) {
            pr2serr("somehow div_scale_n is 0\n");
            ret = SG_LIB_LOGIC_ERROR;
            goto fini;
        }
        a = (n >= 0) ? n : -n;
        sgvi = a_sgl.begin();
        for (k = 0; k < (int)a_sgl.size(); ++k, ++sgvi) {
            if (n > 0) {
                sgvi->lba *= a;
                sgvi->num /= a;
            } else {
                sgvi->lba /= a;
                sgvi->num *= a;
            }
        }
        use_res_sgl = false;
        write2o_sgl = true;
        break;
    case ACT_SORT:
        if (! (a_sge_p && op->out_fn)) {
            pr2serr("--action=sort needs --a_sgl= and --out= options\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        sort2o_sgl(a_sgl, res_sgl);
        use_res_sgl = true;
        write2o_sgl = true;
        break;
    case ACT_SPLIT_N:
        if (! (a_sge_p && op->out_fn)) {
            pr2serr("--action=split_%d needs --a_sgl= and --out= options\n",
                    op->split_n);
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        for (k = 0; k < op->split_n; ++k) {

            if ((1 == strlen(op->out_fn)) && ('-' == op->out_fn[0]))
                fp = stdout;
            else {
                if (op->fne)
                    snprintf(b, blen, "%s%d.%s", op->out_fn, k + 1, op->fne);
                else
                    snprintf(b, blen, "%s%d", op->out_fn, k + 1);
                fp = fopen(b, (op->append2out_f ? "a" : "w"));
                if (NULL == fp) {
                    err = errno;
                    pr2serr("Unable to open %s, error: %s\n", b,
                            strerror(err));
                    ret = sg_convert_errno(err);
                    goto fini;
                }
            }
            if (op->document) {
                fprintf(fp, "# k=%d\n", k + 1); /* origin 1 like filename */
                fprintf(fp, "# %s\n", cmd_line.c_str());
                fprintf(fp, "#\n");
            }
            if (op->do_hex > 1)
                 fprintf(fp, "HEX\n\n");
            op->split_out_fns.push_back(split_fn_fp(b, fp));
            if (vb > 1)
                pr2serr("split out filename: %s\n", b);
        }
        if (vb > 4) {
            pr2serr("sgl 'A' statistics:\n");
            pr_statistics(ssp->a_stats);
        }

        ret = do_split(a_sgl, a_sge_p, op->ab_sgl_stats.a_stats, op);

        for (k = 0; k < (int)op->split_out_fns.size(); ++k) {
            fp = op->split_out_fns[k].fp;
            if (stdout != fp)
                fclose(fp);
        }
        if (ret)
            goto fini;
        write2o_sgl = false;    /* do_split writes out multiple o_sgls */
        break;
    case ACT_TO_CHS:
        if (! (a_sge_p && op->chs_given && op->out_fn)) {
            pr2serr("--action=to-chs needs --a_sgl=, --chs= and --out= "
                    "options\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        ret = convert2chs(a_sgl, res_sgl, 255, op);
        if (ret)
            goto fini;
        if ((NULL == op->out_fn) && (! op->quiet))
            pr2serr("Warning convert2chs but no --out=O_SGL given, so throw "
                    "away\n");
        if (vb)
            pr_sgl(res_sgl.data(), res_sgl.size(), op->do_hex > 0);
        use_res_sgl = true;
        write2o_sgl = true;
        break;
    case ACT_TSPLIT_N:
        if (! (a_sge_p && b_sge_p && op->out_fn)) {
            pr2serr("--action=tsplit_%d needs --a_sgl=, --b_sgl=  and "
                    "--out= options\n", op->split_n);
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        for (k = 0, j = 0; k < op->split_n; ++j, k += ((j % 2) ? 0 : 1)) {
            const char * out_ind = (j % 2) ? "_b" : "";

            if ((1 == strlen(op->out_fn)) && ('-' == op->out_fn[0]))
                fp = stdout;
            else {
                /* filename counting is origin 1 */
                if (op->fne)
                    snprintf(b, blen, "%s%d%s.%s", op->out_fn, k + 1, out_ind,
                             op->fne);
                else
                    snprintf(b, blen, "%s%d%s", op->out_fn, k + 1, out_ind);
                fp = fopen(b, (op->append2out_f ? "a" : "w"));
                if (NULL == fp) {
                    err = errno;
                    pr2serr("Unable to open %s, error: %s\n", b,
                            strerror(err));
                    ret = sg_convert_errno(err);
                    goto fini;
                }
            }
            if (op->document) {
                fprintf(fp, "# k=%d %s\n", k + 1, out_ind); /* also origin 1 */
                fprintf(fp, "# %s\n", cmd_line.c_str());
                fprintf(fp, "#\n");
            }
            if (op->do_hex > 1)
                 fprintf(fp, "HEX\n\n");
            if (j % 2)
                op->b_split_out_fns.push_back(split_fn_fp(b, fp));
            else
                op->split_out_fns.push_back(split_fn_fp(b, fp));
            if (vb > 1)
                pr2serr("split out filename: %s\n", b);
        }
        if (vb > 4) {
            pr2serr("sgl 'A' statistics:\n");
            pr_statistics(ssp->a_stats);
            pr2serr("sgl 'B' statistics:\n");
            pr_statistics(ssp->b_stats);
        }

        ret = do_tsplit(a_sgl, a_sge_p, b_sgl, b_sge_p, op);

        for (k = 0; k < (int)op->split_out_fns.size(); ++k) {
            fp = op->split_out_fns[k].fp;
            if (stdout != fp)
                fclose(fp);
        }
        for (k = 0; k < (int)op->b_split_out_fns.size(); ++k) {
            fp = op->b_split_out_fns[k].fp;
            if (stdout != fp)
                fclose(fp);
        }
        if (ret)
            goto fini;
        write2o_sgl = false;    /* do_tsplit writes out multiple o_sgls */
        break;
    default:
        pr2serr("Unknown action (value=%d) selected\n", op->act_val);
        write2o_sgl = false;
        break;
    }

    if (write2o_sgl && op->out_fn && op->out_fn[0]) {
        sgl_vect & o_sgl = use_res_sgl ? res_sgl : a_sgl;
        time_t t = time(NULL);
        FILE * fp;
        struct tm *tm = localtime(&t);
        char s[64];

        if (op->out2stdout)
            fp = stdout;
        else {
            char bb[256];
            int bblen = sizeof(bb);

            if (op->fne)
                snprintf(bb, bblen, "%s.%s", op->out_fn, op->fne);
            else
                snprintf(bb, bblen, "%s", op->out_fn);
            fp = fopen(bb, (op->append2out_f ? "a" : "w"));
            if (NULL == fp) {
                err = errno;
                pr2serr("Unable to open %s, error: %s\n", bb, strerror(err));
                ret = sg_convert_errno(err);
                goto fini;
            }
        }
        if (op->document) {
            strftime(s, sizeof(s), "%c", tm);
            fprintf(fp, "# Scatter gather list generated by ddpt_sgl  %s\n\n",
                    s);
        }
        if (op->do_hex > 1)
            fprintf(fp, "HEX\n\n");

        n = o_sgl.size();
        if (op->document)
            fprintf(fp, "# %d sgl element%s, one element (LBA,NUM) per "
                    "line\n", n, (1 == n ? "" : "s"));
        for (k = 0; k < n; ++k)
            output_sge(fp, &o_sgl[k], op->do_hex, vb);

        if (stdout != fp)
            fclose(fp);
    }

    ret = 0;

fini:
    if (a_sge_p)
        free(a_sge_p);
    if (b_sge_p)
        free(b_sge_p);
    /* SG_LIB_OK_FALSE(36) indicates at least one ret_bool check failed */
    return ((0 == ret) && (false == ret_bool)) ? SG_LIB_OK_FALSE : ret;
}


