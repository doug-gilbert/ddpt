/*
 * Copyright (c) 2020-2023, Douglas Gilbert
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
#include <random>       /* iota() needs this */

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


static const char * ddpt_sgl_version_str = "0.98 20230513 [svn: r410]";

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
# define ACT_TSORT 13    /* sort a-sgl, move b-sgl in unison */
# define ACT_DIVISIBLE_N 11 /* on a-sgl, boolean result */
# define ACT_SCALE_N 12 /* on a-sgl; N>0 multiply LBA, n<0 divide LBA */
# define ACT_SELECT 14
# define ACT_TSELECT 15
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
    {"elem", required_argument, 0, 'E'},
    {"extension", required_argument, 0, 'e'},
    {"flexible", no_argument, 0, 'f'},
    {"help", no_argument, 0, 'h'},
    {"hex", no_argument, 0, 'H'},
    {"iaf", required_argument, 0, 'I'},
    {"index", required_argument, 0, 'x'},
    {"interleave", required_argument, 0, 'i'},
    {"non-overlap", no_argument, 0, 'N'},
    {"non_overlap", no_argument, 0, 'N'},
    {"out", required_argument, 0, 'o'},
    {"quiet", no_argument, 0, 'q'},
    {"round", required_argument, 0, 'r'},
    {"sort-cmp", required_argument, 0, 'S'},
    {"sort_cmp", required_argument, 0, 'S'},
    {"stats", no_argument, 0, 's'},
    {"statistics", no_argument, 0, 's'},
    {"verbose", no_argument, 0, 'v'},
    {"version", no_argument, 0, 'V'},
    {0, 0, 0, 0},
};

struct val_name_t {
    int val;
    const char * name;
};

/* '*' in a name expects a number in that position */
struct val_name_t act_array[] = {
    {ACT_APPEND_B2A, "append"},    /* just giving 'append' is sufficient */
    {ACT_DIVISIBLE_N, "divisible_*"},   /* longer match string first ... */
    {ACT_DIVISIBLE_N, "divisible*"},
    {ACT_ENUMERATE, "xxx"},
    {ACT_ENUMERATE, "enum"},
    {ACT_EQUAL, "equal"},
    {ACT_NONE, "none"},
    {ACT_PART_EQUAL, "part-equal"},
    {ACT_PART_EQUAL, "part_equal"},
    {ACT_PART_SAME, "part-same"},
    {ACT_PART_EQUAL, "part_same"},
    {ACT_SAME, "same"},
    {ACT_SCALE_N, "scale_*"},
    {ACT_SCALE_N, "scale*"},
    {ACT_SELECT, "select"},
    {ACT_SORT, "sort"},
    {ACT_SPLIT_N, "split_*"},
    {ACT_SPLIT_N, "split*"},
    {ACT_TO_CHS, "to-chs"},
    {ACT_TO_CHS, "to_chs"},
    {ACT_TSELECT, "tselect"},
    {ACT_TSORT, "tsort"},
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
            "                [--degen=DV] [--document] [--elem=SE[,LE]] "
            "[--extension=FNE]\n"
            "                [--flexible] [--help] [--hex] [--iaf=IAF] "
            "[--index=IA]\n"
            "                [--interleave=IL] [--non-overlap] "
            "[--out=O_SGL] [--quiet]\n"
            "                [--round=RB] [--sort-cmp=SC] [--stats] "
            "[--verbose]\n"
            "                [--version]\n"
            "  where:\n"
            "    --action=ACT|-a ACT    ACT is action to take, use 'xxx' "
            "to list\n"
            "                           what is available\n"
            "    --a-sgl=SGL|-A SGL    input the 'A' scatter gather list "
            "(sgl)\n"
            "    --b-sgl=SGL|-B SGL    input the 'B' scatter gather list\n"
            "    --chs=CHS|-C CHS      CHS is '<cyls>,<heads>,<sects>' (e.g. "
            "768,16,255)\n"
            "    --degen=DV|-D DV      DV indicates degenerate element "
            "treatment\n"
            "                            0: ignore all but last degen "
            "element (def)\n"
            "                            1: take in account for highest and "
            "lowest\n"
            "                            2: take in account for monotonic\n"
            "    --document|-d         place datetime stamp and other "
            "info at start\n"
            "                          of O_SGL(s); >1: add invoking command "
            "line\n"
            "    --elem=SE[,LE]|-E SE[,LE]    write sgl element SE (origin "
            "0) to O_SGL\n"
            "                                 if LE given then write out "
            "SE...LE\n"
            "                                 elements inclusive; LE,0 is "
            "LE to end\n"
            "    --extension=FNE|-e FNE    filename extension (i.e. used "
            "after '.')\n"
            "    --flexible|-f         relax rule about hex sgl\n"
            "    --help|-h             print out usage message then exit\n"
            "    --hex|-H              output sgl is in decimal when '-H' "
            "not given;\n"
            "                          given once: each value prefixed by "
            "'0x'\n"
            "                          given twice: each value in hex and "
            "'HEX' at\n"
            "                          start of output/file\n"
            "    --iaf=IAF|-I IAF      index array filename, for output\n"
            "    --index=IA|-x IA      input index array, inline or "
            "'@IAF'\n"
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
            "    --sort-cmp_SC|-S SC    sort comparison, 0: LBA ascending "
            "(def);\n"
            "                           1: LBA descending; 2 NUM asc; 3 "
            "NUM des\n"
            "    --stats|-s            print given sgl(s) statistics\n"
            "    --verbose|-v          increase verbosity\n"
            "    --version|-V          print version string and exit\n\n"
            "A SGL can be a single value (a starting_LBA) or an even "
            "number of comma\nseparated values parsed into a sequence of "
            "starting_LBA,NUM pairs.\nAlternatively SGL can be a filename "
            "prefixed by '@' or 'H@'. For '@' that\nfile contains decimal "
            "values by default unless prefixed by '0x' or with\na trailing "
            "'h'. For 'H@' that file contains hexadecimal values with the\n"
            "string 'HEX' at the start of that file. When '-e FNE' is given "
            "then\nthe form of created filenames is 'O_SGL<n>.FNE' where "
            "<n> starts at 1.\n"
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
            "   scale<n>      multiply all starting LBAs and NUMS in a_sgl "
            "by <n>\n "
            "                 (when <n> > 0), result --> O_SGL; if <n> < 0 "
            "then\n"
            "                 divide all LBAs and NUMs by |n|\n"
            "   select        from a-sgl based on index array, output to "
            "O_SGL\n"
            "   sort          sort a-sgl and output to O_SGL\n"
            "   split<n>      divide a-sgl into 'n' O_SGLs\n"
            "   to-chs        assume a-sgl is flat sgl, convert to CHS in "
            "O_SGL\n"
            "   tselect       select from a-sgl, move b-sgl in unison to "
            "O_SGL_t\n"
            "   tsort         sort a-sgl to O_SGL, move b-sgl in unison to "
            "O_SGL_t\n"
            "   tsplit<n>     twin split: divide a-sgl and b-sgl into two "
            "series\n"
            "   xxx           print this list...\n"
           );
}

/* Print statistics (to stdout) */
static void
pr_statistics(const sgl_stats & sst, FILE * fp)
{
    fprintf(fp, "Number of elements: %d, number of degenerates: %d\n",
            sst.elems, sst.num_degen);
    if (sst.not_mono_desc && sst.not_mono_asc)
        fprintf(fp, "  not monotonic, ");
    else if ((! sst.not_mono_desc) && (! sst.not_mono_asc))
        fprintf(fp, "  monotonic (both), ");
    else if (sst.not_mono_desc)
        fprintf(fp, "  monotonic ascending, %s, ",
                sst.fragmented ? "fragmented" : "linear");
    else
        fprintf(fp, "  monotonic descending, %s, ",
                sst.fragmented ? "fragmented" : "linear");
    fprintf(fp, "last degenerate: %s\n", sst.last_degen ? "yes" : "no");
    fprintf(fp, "  lowest,highest LBA: 0x%" PRIx64 ",0x%" PRIx64 "  block "
            "sum: %" PRId64 "\n", sst.lowest_lba, sst.highest_lba, sst.sum);
}

/* Print scatter gather list (to stdout if fp is NULL) */
static void
pr_sgl(const struct scat_gath_elem * first_elemp, int num_elems, bool in_hex,
       FILE * fp)
{
    const struct scat_gath_elem * sgep = first_elemp;
    char lba_str[20];
    char num_str[20];

    if (NULL == fp)
        fp = stdout;
    fprintf(fp, "Scatter gather list, number of elements: %d\n", num_elems);
    fprintf(fp, "    Logical Block Address   Number of blocks\n");
    for (int k = 0; k < num_elems; ++k, ++sgep) {
        if (in_hex) {
            snprintf(lba_str, sizeof(lba_str), "0x%" PRIx64, sgep->lba);
            snprintf(num_str, sizeof(num_str), "0x%x", sgep->num);
            fprintf(fp, "    %-14s          %-12s\n", lba_str, num_str);
        } else
            fprintf(fp, "    %-14" PRIu64 "          %-12u\n", sgep->lba,
                    sgep->num);
    }
}

typedef bool (*cmp_f_t)(const sgl_vect & sgl, int lhs, int rhs);

static bool
cmp_asc_lba(const sgl_vect & sgl, int lhs, int rhs)
{
    return sgl[lhs].lba < sgl[rhs].lba;
}

static bool
cmp_des_lba(const sgl_vect & sgl, int lhs, int rhs)
{
    return sgl[lhs].lba > sgl[rhs].lba;
}

static bool
cmp_asc_num(const sgl_vect & sgl, int lhs, int rhs)
{
    return sgl[lhs].num < sgl[rhs].num;
}

static bool
cmp_des_num(const sgl_vect & sgl, int lhs, int rhs)
{
    return sgl[lhs].num > sgl[rhs].num;
}

/* definition for function object to do indirect comparson via index_arr */
struct indir_comp_t {

    indir_comp_t(const sgl_vect & a_sgl, int typ) : sgl(a_sgl)
        {
            switch (typ) {
#if 1
            case 1: cmp_f = cmp_des_lba; break;
            case 2: cmp_f = cmp_asc_num; break;
            case 3: cmp_f = cmp_des_num; break;
            case 0: default: cmp_f = cmp_asc_lba; break;

#else
            case 1: cmp_f = &indir_comp_t::des_lba; break;
            case 2: cmp_f = &indir_comp_t::asc_num; break;
            case 3: cmp_f = &indir_comp_t::des_num; break;
            case 0: default: cmp_f = &indir_comp_t::asc_lba; break;
#endif
            }
        }

    bool operator()(int lhs, int rhs) const
#if 1
        { return cmp_f(sgl, lhs, rhs); }
#else
        { return (this->*cmp_f)(lhs, rhs); }
#endif
private:
    const sgl_vect & sgl;

#if 1

    cmp_f_t cmp_f;

#else

    // typedef bool (indir_comp_t::*cmp_f_t)(int lhs, int rhs) const;

    bool (indir_comp_t:: * cmp_f)(int lhs, int rhs) const;

    bool asc_lba(int lhs, int rhs) const
        { return sgl[lhs].lba < sgl[rhs].lba; }
    bool des_lba(int lhs, int rhs) const
        { return sgl[lhs].lba > sgl[rhs].lba; }
    bool asc_num(int lhs, int rhs) const
        { return sgl[lhs].num < sgl[rhs].num; }
    bool des_num(int lhs, int rhs) const
        { return sgl[lhs].num > sgl[rhs].num; }
#endif
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
    indir_comp_t compare_obj(sgl, 0 /* ascending based on LBA */);
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

static int
do_write2o_sgl(const char * bname, const char * extra, bool use_elem_opt,
               sgl_vect & o_sgl, struct sgl_opts_t * op)
{
    int k, n, err;
    FILE * fp;
    char s[64];

    if (op->out2stdout)
        fp = stdout;
    else {
        char bb[256];
        int bblen = sizeof(bb);

        if (op->fne)
            snprintf(bb, bblen, "%s%s.%s", bname, (extra ? extra : ""),
                     op->fne);
        else
            snprintf(bb, bblen, "%s%s", bname, (extra ? extra : ""));
        fp = fopen(bb, (op->append2out_f ? "a" : "w"));
        if (NULL == fp) {
            err = errno;
            pr2serr("Unable to open %s, error: %s\n", bb, strerror(err));
            return sg_convert_errno(err);
        }
    }
    if (op->document) {
        time_t t = time(NULL);
        struct tm *tm = localtime(&t);

        strftime(s, sizeof(s), "%c", tm);
        fprintf(fp, "# Scatter gather list generated by ddpt_sgl  %s\n", s);
        if (op->document > 1) {
            fprintf(fp, "# with this command line:\n");
            fprintf(fp, "#   %s\n", op->cmd_line.c_str());
        }
        fprintf(fp, "#\n");
    }
    if (op->do_hex > 1)
        fprintf(fp, "HEX\n\n");

    n = o_sgl.size();
    if ((n > 0) && use_elem_opt && op->elem_given) {
        int se = op->start_elem;
        int le = op->last_elem;

        if (se < 0)
            se += n;
        if (le < 0)
            le += n;
        if ((se < 0) || (le < 0)) {
            pr2serr("%s: --elem=%d,%d inconsistent with %d elements in "
                    "o_sgl\n", __func__, op->start_elem, op->last_elem, n);
            return SG_LIB_CONTRADICT;
        }
        if ((se >= n) && (le >= n)) {
            pr2serr("%s: --elem=%d,%d too large for o_sgl with %d "
                    "elements\n", __func__, se, le, n);
            return SG_LIB_CONTRADICT;
        }
        if (se == le)   /* just one element output, must be in range */
            output_sge_f(fp, &o_sgl[se], op->do_hex, op->verbose);
        else if (le > se) {     /* normal, forward scan [SE ... LE] */
            if (le >= n) {
                le = n - 1;
                pr2serr("Reducing to --elem=%d,%d to avoid overrun\n",
                        le, se);
            }
            for ( ; se <= le; ++se)
                output_sge_f(fp, &o_sgl[se], op->do_hex, op->verbose);
        } else { /* reversal scan: se down to le */
            if (se >= n) {
                se = n - 1;
                pr2serr("Reducing to --elem=%d,%d to avoid overrun\n",
                        le, se);
            }
            for ( ; se >= le; --se)
                output_sge_f(fp, &o_sgl[se], op->do_hex, op->verbose);
        }
    } else {    /* simply output all of o_sgl to file */
        if (op->document)
            fprintf(fp, "# %d sgl element%s, one element (LBA,NUM) per "
                    "line\n", n, (1 == n ? "" : "s"));
        for (k = 0; k < n; ++k)
            output_sge_f(fp, &o_sgl[k], op->do_hex, op->verbose);
    }
    if (stdout != fp)
        fclose(fp);
    return 0;
}

static int
output_iaf_f(FILE * fp, int index, int do_hex, int vb)
{
    if (do_hex < 1)
        fprintf(fp, "%d\n", index);
    else if (do_hex < 2)
        fprintf(fp, "0x%x\n", index);
    else
        fprintf(fp, "%x\n", index);
    if (ferror(fp)) {
        if (vb)
            pr2serr("%s: failed during formatted write to index array file\n",
                    __func__);
        clearerr(fp);   /* error flag stays set unless .... */
        return SG_LIB_FILE_ERROR;
    } else
        return 0;
}

static int
do_write2iaf(const vector<int> & index_arr, struct sgl_opts_t * op)
{
    int k, n, err;
    int res = 0;
    FILE * fp;
    char s[64];

    if (op->iaf2stdout)
        fp = stdout;
    else {
        if ((NULL == op->iaf) || ('\0' == op->iaf[0])) {
            pr2serr("%s: bad index array filename\n", __func__);
            return SG_LIB_LOGIC_ERROR;
        }
        fp = fopen(op->iaf, (op->append2iaf ? "a" : "w"));
        if (NULL == fp) {
            err = errno;
            pr2serr("Unable to open %s, error: %s\n", op->iaf, strerror(err));
            return sg_convert_errno(err);
        }
    }
    if (op->document) {
        time_t t = time(NULL);
        struct tm *tm = localtime(&t);

        strftime(s, sizeof(s), "%c", tm);
        fprintf(fp, "# Index array generated by ddpt_sgl  %s\n", s);
        if (op->document > 1) {
            fprintf(fp, "# with this command line:\n");
            fprintf(fp, "#   %s\n", op->cmd_line.c_str());
        }
        fprintf(fp, "#\n");
    }
    if (op->do_hex > 1)
        fprintf(fp, "HEX\n\n");

    n = index_arr.size();
    if ((n > 0) && op->elem_given) {
        int se = op->start_elem;
        int le = op->last_elem;

        if (se < 0)
            se += n;
        if (le < 0)
            le += n;
        if ((se < 0) || (le < 0)) {
            pr2serr("%s: --elem=%d,%d inconsistent with %d elements in "
                    "o_sgl\n", __func__, op->start_elem, op->last_elem, n);
            return SG_LIB_CONTRADICT;
        }
        if ((se >= n) && (le >= n)) {
            pr2serr("%s: --elem=%d,%d too large for o_sgl with %d "
                    "elements\n", __func__, se, le, n);
            return SG_LIB_CONTRADICT;
        }
        if (se == le)   /* just one element output, must be in range */
            res = output_iaf_f(fp, index_arr[se], op->do_hex, op->verbose);
        else if (le > se) {     /* normal, forward scan [SE ... LE] */
            if (le >= n) {
                le = n - 1;
                pr2serr("Reducing to --elem=%d,%d to avoid overrun\n",
                        le, se);
            }
            for ( ; se <= le; ++se) {
                if ((res = output_iaf_f(fp, index_arr[se], op->do_hex,
                                        op->verbose)))
                    break;
            }
        } else { /* reversal scan: se down to le */
            if (se >= n) {
                se = n - 1;
                pr2serr("Reducing to --elem=%d,%d to avoid overrun\n",
                        le, se);
            }
            for ( ; se >= le; --se) {
                if ((res = output_iaf_f(fp, index_arr[se], op->do_hex,
                                        op->verbose)))
                    break;
            }
        }
    } else {    /* simply output all of index_arr to file */
        for (k = 0; k < n; ++k) {
            if ((res = output_iaf_f(fp, index_arr[k], op->do_hex,
                                    op->verbose)))
                break;
        }
    }
    if (stdout != fp)
        fclose(fp);
    return res;
}

/* Ascending stable sort of sort_sgl based on LBA with result as an index
 * array that can be used elsewhere to re-arrange the elements of sort_sgl. */
static void
sort_based_on(const sgl_vect & sort_sgl, vector<int> & index_arr,
              int sort_cmp_val, int vb)
{
    int k;
    int sort_sz = sort_sgl.size();

    iota(index_arr.begin(), index_arr.end(), 0);   /* 0, 1, 2, ... n-1 */
    if (sort_sz > 1) {
        indir_comp_t compare_obj(sort_sgl, sort_cmp_val);
        /* STL stable_sort() used */
        stable_sort(index_arr.begin(), index_arr.end(), compare_obj);
        if (vb > 3) {
            pr2serr("%s: iota array (size=%d) transformed by stable sort "
                    "to:\n   ", __func__, sort_sz);
            for (k = 0; k < sort_sz; ++k) {
                pr2serr("%d ", index_arr[k]);
                if ((k > 0) && (0 == (k % 16)))
                    pr2serr("\n   ");
            }
            pr2serr("\n");
        }
    }
}

/* Assume index_arr contains valid indexes of in_sgl (i.e.
 * 0 .. in_sgl.size()-1). N.B. doesn't clear
 * out_sgl, just appends to the end of it. */
static void
rearrange_sgl(const sgl_vect & in_sgl, const vector<int> & index_arr,
              sgl_vect & out_sgl, bool b_vb)
{
    int indx;
    int in_sz = in_sgl.size();
    int index_arr_sz = index_arr.size();
    struct scat_gath_elem a_sge;

    for (int k = 0; k < index_arr_sz; ++k) {
        indx = index_arr[k];
        if (indx >= in_sz) {
            if (b_vb)
                pr2serr("%s: index %d exceeds in_sgl.size=%d, ignore\n",
                        __func__, indx, in_sz);
        } else if (indx < 0) {
            indx = -indx;
            if (indx > in_sz) {
                if (b_vb)
                    pr2serr("%s: negative index %d exceeds in_sgl.size=%d, "
                            "ignore\n", __func__, indx, in_sz);
            } else {
                a_sge.lba = in_sgl[in_sz - indx].lba;
                a_sge.num = in_sgl[in_sz - indx].num;
                out_sgl.push_back(a_sge);
            }
        } else {
            a_sge.lba = in_sgl[indx].lba;
            a_sge.num = in_sgl[indx].num;
            out_sgl.push_back(a_sge);
        }
    }
}

/* Assume ref_index_arr contains valid indexes of ref_sgl (i.e.
 * 0 .. ref_sgl.size()-1) and same number of elements. Assumes twin_sgl
 * is a twin of ref_sgl. Re-arranges twin_sgl into out_t_sgl.
 * N.B. doesn't clear out_t_sgl, just appends to the end of it. */
static void
rearrange_twin_sgl(const sgl_vect & twin_sgl, const sgl_vect & ref_sgl,
                   const vector<int> & ref_index_arr, sgl_vect & out_t_sgl,
                   int do_hex, int vb)
{
    int k, j, indx;
    const int ref_sz = ref_sgl.size();
    const int twin_sz = twin_sgl.size();
    uint32_t num, i_num, t_num, i_elem_ind;
    uint64_t lba;
    const struct scat_gath_elem * t_sgep;
    sgl_vect indir_t_sgl;
    struct sgl_iter_t iter;
    struct scat_gath_elem w_sge;

    /* Build indirect sgl that points into twin_sgl based on breaks in the
     * ref_sgl. indir_t_sgl will have one more element than ref_sgl. Range
     * in twin_sgl corresponding to ref_sgl[n] is
     *     [indir_t_sgl[n] ... indir_t_sgl[n+1])
     * That is a half open interval. */
    t_sgep = twin_sgl.data();
    sgl_iter_init(&iter, (struct scat_gath_elem *)t_sgep, twin_sz);
    w_sge.lba = 0;
    w_sge.num = 0;
    indir_t_sgl.push_back(w_sge);
    for (k = 0; k < ref_sz; ++k) {
        num = ref_sgl[k].num;
        if (! sgl_iter_add(&iter, num, true)) {
            pr2serr("%s: iterator explodes at k=%d, num=%u\n", __func__,
                    k, num);
            pr2serr(" ... ignore twin re-arrange\n");
            return;
        }
        if (vb > 3)
            sgl_iter_print(&iter, "twin setup iterator", false, false,
                           stderr);
        w_sge.lba = iter.it_e_ind;
        w_sge.num = iter.it_bk_off;
        indir_t_sgl.push_back(w_sge);
    }
    if (vb > 1) {
        pr2serr("%s: indir_t_sgl.size=%d, iter.extend_last=%d\n",
                __func__, (int)indir_t_sgl.size(), !!iter.extend_last);
        if (vb > 2)
            pr_sgl(indir_t_sgl.data(), indir_t_sgl.size(),
                   do_hex > 0, stderr);
    }
    /* Now use ref_index_arr and indir_t_sgl to re-arrange twin_sgl into
     * out_t_sgl */
    for (k = 0; k < ref_sz; ++k) {
        indx = ref_index_arr[k];
        num = ref_sgl[indx].num;
        w_sge = indir_t_sgl[indx];
        i_elem_ind = w_sge.lba;
        i_num = w_sge.num;
        lba = (t_sgep + i_elem_ind)->lba;
        t_num = (t_sgep + i_elem_ind)->num;
        if ((0 == t_num) && ((int)i_elem_ind == (twin_sz - 1)))
            t_num = i_num + num;
        w_sge.lba = lba + i_num;
        if (0 == num) {
            w_sge.num = 0;
            out_t_sgl.push_back(w_sge);
            continue;
        }
        while (num > 0) {
            j = t_num - i_num;
            if (j >= 0) {
                if (j <= (int)num) {
                    if (j > 0) {
                        w_sge.num = j;
                        out_t_sgl.push_back(w_sge);
                    }
                    ++i_elem_ind;
                    lba = (t_sgep + i_elem_ind)->lba;
                    t_num = (t_sgep + i_elem_ind)->num;
                    i_num = 0;
                    w_sge.lba = lba;
                    num -= j;
                } else {    /* j > num, so finishing */
                    w_sge.num = num;
                    out_t_sgl.push_back(w_sge);
                    num = 0;
                }
            } else {
                pr2serr("%s: logic error, t_num=%d, i_num=%d, num=%d\n",
                        __func__, t_num, i_num, num);
                break;
            }
        }   /* while can make out_t_sgl.size() > ref_sgl size */
    }
}

/* When just sort of a-sgl, then twin_sglsize() should be 0 and a dummy can
 * given for out_t_sgl (and nothing should be written to it). For twin sort
 * b-sgl's sum should be >= a-sgl's sum (or b-sgl is "soft": its last
 * segment has a NUM of 0). */
static void
sort2o_sgl(const sgl_vect & sort_sgl, const sgl_vect & twin_sgl,
           sgl_vect & out_sgl, sgl_vect & out_t_sgl, struct sgl_opts_t * op)
{
    int vb = op->verbose;
    vector<int> index_arr(sort_sgl.size());

    sort_based_on(sort_sgl, index_arr, op->sort_cmp_val, vb);
    if (op->out_fn)
        rearrange_sgl(sort_sgl, index_arr, out_sgl, vb > 2);
    if (op->iaf)
        do_write2iaf(index_arr, op);
    if (twin_sgl.size() > 0)
        rearrange_twin_sgl(twin_sgl, sort_sgl, index_arr, out_t_sgl,
                           op->do_hex, vb);
}

static int
cl_sgl_parse(const char * key, const char * buf, int degen_mask,
             sgl_vect & sgl, struct scat_gath_elem * & sge_p,
             int & num_elems, struct sgl_stats & stats,
             int append_xlen, bool flexible, int vb)
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
        sge_p = file2sgl(cp, def_hex, flexible, &num_elems, &err, true);
        if (NULL == sge_p) {
            pr2serr("%s: file2sgl failed, err=%d\n", key, err);
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
            if (strlen(buf) > 0)
                pr2serr("If '%s' is a filename containing a sgl then prefix "
                        "it with a '@'\n", buf);
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

// Parsing contents of FN from command line option --index=@FN
static int
file_index_parse(const char * fnp, bool got_stdin, bool in_hex,
                 struct sgl_opts_t * op)
{
    bool negative, problem;
    bool pre_addr1 = true;
    bool pre_hex_seen = false;
    int in_len, k, j, m, n, err;
    // int off = 0;
    int ret = 0;
    unsigned int u;
    char * lcp;
    FILE * fp;
    char line[1024];

    if (got_stdin)
        fp = stdin;
    else {
        fp = fopen(fnp, "r");
        if (NULL == fp) {
            err = errno;
            pr2serr("%s: opening %s: %s\n", __func__, fnp,
                    safe_strerror(err));
            return sg_convert_errno(err);
        }
    }

    for (j = 0; ; ++j) {
        if (NULL == fgets(line, sizeof(line), fp))
            break;
        // could improve with carry_over logic if sizeof(line) too small
        in_len = strlen(line);
        if (in_len > 0) {
            if ('\n' == line[in_len - 1]) {
                --in_len;
                line[in_len] = '\0';
            } else {
                ret = SG_LIB_SYNTAX_ERROR;
                pr2serr("%s: %s: line too long, max %d bytes\n",
                        __func__, fnp, (int)(sizeof(line) - 1));
                goto err_out;
            }
        }
        if (in_len < 1)
            continue;
        lcp = line;
        m = strspn(lcp, " \t");
        if (m == in_len)
            continue;
        lcp += m;
        in_len -= m;
        if ('#' == *lcp)
            continue;
        if (pre_addr1 || pre_hex_seen) {
            /* Accept lines with leading 'HEX' and ignore as long as there
             * is one _before_ any INDEX lines in the file. This allows
             * HEX marked index arrays to be concaternated together. */
            if (('H' == toupper(lcp[0])) && ('E' == toupper(lcp[1])) &&
                ('X' == toupper(lcp[2]))) {
                pre_hex_seen = true;
                if (in_hex)
                    continue; /* bypass 'HEX' marker line if expecting hex */
                else {
                    if (op->flexible) {
                        in_hex = true; /* okay, switch to hex parse */
                        continue;
                    } else {
                        pr2serr("%s: %s: 'hex' string detected on line %d, "
                                "expecting decimal\n", __func__, fnp, j + 1);
                        ret = SG_LIB_SYNTAX_ERROR;
                        goto err_out;
                    }
                }
            }
        }
        k = strspn(lcp, "0123456789aAbBcCdDeEfFhHxXbBdDiIkKmMgGtTpP, \t");
        if ((k < in_len) && ('#' != lcp[k])) {
            ret = SG_LIB_SYNTAX_ERROR;
            pr2serr("%s: %s: syntax error at line %d, pos %d\n",
                    __func__, fnp, j + 1, m + k + 1);
            goto err_out;
        }
        for (k = 0; k < 256; ++k) {
            /* limit parseable items on one line to 256 */
            for ( ; isspace(*lcp); ++lcp)
                ;       /* bypass leading whitespace */
            negative = ('-' == *lcp);
            problem = false;
            if (negative)
                ++lcp;
            if (in_hex) {      /* don't accept negatives or multipliers */
                if (1 == sscanf(lcp, "%x", &u)) {
                    n = u;
                } else
                    problem = true;
            } else {
                n = sg_get_num(lcp);
                if (-1 == n)
                    problem = true;
            }
            if (problem) {
                if ('#' == *lcp) { /* numbers before #, rest of line comment */
                    --k;
                    break;      /* goes to next line */
                }
                ret = SG_LIB_SYNTAX_ERROR;
                pr2serr("%s: %s: error in line %d, at pos %d\n",
                        __func__, fnp, j + 1, (int)(lcp - line + 1));
                goto err_out;
            }
            if (negative)
                n = -n;
            op->index_arr.push_back(n);
            if (pre_addr1)
                pre_addr1 = false;

            lcp = strpbrk(lcp, " ,\t#");
            if ((NULL == lcp) || ('#' == *lcp))
                break;
            lcp += strspn(lcp, " ,\t");
            if ('\0' == *lcp)
                break;
        }       /* <<< end of for(k < 256) loop */
        // off += (k + 1);
    }   /* <<< end of for( ; ; ) loop, one iteration per line */
    clearerr(fp);    /* even EOF on first pass needs this before rescan */
    if (stdin != fp)
        fclose(fp);
    return 0;
err_out:
    clearerr(fp);
    if (stdin != fp)
        fclose(fp);
    return ret;
}

static int
cl_index_parse(const char * arg, struct sgl_opts_t * op)
{
    bool negative;
    bool def_hex = false;
    bool got_stdin = false;
    int alen = strlen(arg);
    unsigned int u;
    int64_t ll;
    const char * ap = arg;
    const char * fnp = NULL;
    const char * lcp;

    for ( ; ((alen > 0) && isspace(*ap)); ++ap, --alen)
        ;       /* bypass leading whitespace */
    if (alen < 1)
        return SG_LIB_SYNTAX_ERROR;
    if (('-' == *ap) && (1 == alen))
        got_stdin = true;
    else if ('H' == toupper(*ap)) {
        def_hex = true;
        ++ap;
        --alen;
        if (alen < 1)
            return SG_LIB_SYNTAX_ERROR;
    }
    if ((! got_stdin) && ('@' == *ap)) {
        ++ap;
        --alen;
        if (alen < 1)
            return SG_LIB_SYNTAX_ERROR;
        if (isspace(*ap)) {
            pr2serr("%s: expected filename after '@', got whitespace\n",
                    __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        fnp = ap;
    }
    if (got_stdin || fnp)
        return file_index_parse(fnp, got_stdin, def_hex, op);

    while (alen > 0) {
        for ( ; ((alen > 0) && isspace(*ap)); ++ap, --alen)
            ;       /* bypass leading whitespace */
        negative = ('-' == *ap);
        if (negative) {
            ++ap;
            --alen;
        }
        if (def_hex) {
            if (1 != sscanf(ap, "%x", &u)) {
                pr2serr("Unable to decode hex number at: %s\n", ap);
                return SG_LIB_SYNTAX_ERROR;
            }
            ll = u;
        } else {
            ll = sg_get_llnum(ap);
            if (-1LL == ll) {
                pr2serr("Syntax error parsing number at: %s\n", ap);
                return SG_LIB_SYNTAX_ERROR;
            }
        }
        if (negative)
            ll = -ll;
        if (ll >= (INT32_MAX - 1)) {
            pr2serr("index too large at: %s\n", ap);
            return SG_LIB_SYNTAX_ERROR;
        }
        if (ll <= (INT32_MIN + 1)) {
            pr2serr("negative index too small at: %s\n", ap);
            return SG_LIB_SYNTAX_ERROR;
        }
        op->index_arr.push_back((int)ll);
        lcp = strpbrk(ap + 1, " ,\t#");
        if ((NULL == lcp) || ('#' == *lcp))
            break;
        lcp += strspn(lcp, " ,\t");
        if ('\0' == *lcp)
            break;
        alen -= (lcp - ap);
        ap = lcp;
    }
    return 0;
}

/* hold limiting values of CHS or the mapping a flat LBA */
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
        res = iter_add_process(&iter, nblks, output_sge_f, fp, op->do_hex,
                               rblks, vb);
        if (res) {
            pr2serr("%s: error while writing to %s: [0x%x], k=%d, n=%d\n",
                    __func__, fnamep, res, k, n);
            sg_if_can2stderr("", res);
            break;
        }
        if (op->round_blks > 0) {
            if (vb > 5) {
                sgl_iter_print(&iter, "current iter (lhs)", true, false,
                               NULL);
                sgl_iter_print(&hold_iter, "prior iter", true, false, NULL);
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
        res = iter_add_process(&iter, nblks, output_sge_f, fp, op->do_hex,
                               rblks, vb);
        if (res) {
            pr2serr("%s: error while writing to %s: [0x%x], k=%d, n=%d\n",
                    __func__, fnamep, res, k, n);
            sg_if_can2stderr("", res);
            break;
        }
        if (op->round_blks > 0) {
            if (vb > 5) {
                sgl_iter_print(&iter, "current iter (lhs)", true, false,
                               NULL);
                sgl_iter_print(&hold_iter, "prior iter", true, false, NULL);
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
        res = iter_add_process(&b_iter, nblks, output_sge_f, fp, op->do_hex,
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
parse_action(const char * o_arg, struct sgl_opts_t * op)
{
    bool ok;
    int len, n, off;
    const char * cp;
    const char * commap;
    const char * comma2p;
    struct val_name_t * vnp;

    for (vnp = act_array, ok = false; vnp->name; ++vnp) {
        len = strlen(vnp->name);
        commap = strchr(o_arg, ',');
        if ((cp = strchr(vnp->name, '*'))) {  /* action with numeric suffix */
            if (cp == vnp->name)
                break;  /* don't accept '*' in 1st position */
            off = cp - vnp->name;       /* offset corresponding to '*' */
            if (0 == memcmp(o_arg, vnp->name, off)) {
                if ((op->act_val > 0) && (op->act_val == vnp->val))
                    return 0;   /* already processed this action */
                op->act_val = vnp->val;
                if ('-' == o_arg[off]) {
                    if (0 == strcmp("-1", o_arg + off))
                        n = -1;
                    else {      /* decode starting after '-' */
                        n = sg_get_num(o_arg + off + 1);
                        if (-1 == n) {
                            pr2serr("'--action=%s' unable to decode "
                                    "negative number\n", o_arg);
                            return SG_LIB_SYNTAX_ERROR;
                        }
                        n = -n;         /* now turn negative */
                    }
                } else {
                    n = sg_get_num(o_arg + off);
                    if (-1 == n) {
                        pr2serr("'--action=%s' unable to decode number\n",
                                o_arg);
                        return SG_LIB_SYNTAX_ERROR;
                    }
                }
                if (0 == n) {
                    pr2serr("'--action=%s' value of 0 not permitted\n",
                            o_arg);
                    return SG_LIB_SYNTAX_ERROR;
                }
                switch (vnp->val) {
                case ACT_DIVISIBLE_N:
                    if (n < 0) {
                        pr2serr("'--action=%s' negative value not "
                                "permitted\n", o_arg);
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
                                "permitted\n", o_arg);
                        return SG_LIB_SYNTAX_ERROR;
                    }
                    op->split_n = n;
                    break;
                default:
                    pr2serr("'--action=%s' unexpected\n", o_arg);
                    return SG_LIB_LOGIC_ERROR;
                }
            }
        } else if (0 == memcmp(o_arg, vnp->name, len)) {
            ok = true;
            op->act_val = vnp->val;
            break;
        }
    }
    if (! ok) {
        pr2serr("'--action=%s' not found or ill-formed, try "
                "'--action=xxx' to list available\n", o_arg);
        return SG_LIB_SYNTAX_ERROR;
    }
    return 0;
}

static int
parse_command_line(int argc, char * argv[], const char * & a_sgl_arg,
                   const char * & b_sgl_arg, struct sgl_opts_t * op)
{
    bool negative;
    bool verbose_given = false;
    bool version_given = false;
    int n, c, res;
    const char * cp;
    const char * index_arg = NULL;

    while (1) {
        int option_index = 0;

        c = getopt_long(argc, argv, "a:A:B:C:dD:e:E:fhHi:I:No:qr:sS:vVx:",
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
            ++op->document;
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
        case 'E':
            negative = ('-' == *optarg);
            cp = negative ? optarg + 1 : optarg;
            n = sg_get_num(cp);
            if (n < 0) {
                pr2serr("unable to decode --elem=SE\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->start_elem = negative ? -n : n;
            cp = strchr(optarg, ',');
            if (cp) {
                negative = ('-' == *(cp + 1));
                cp = negative ? cp + 2 : cp + 1;
                n = sg_get_num(cp);
                if (n < 0) {
                    pr2serr("unable to decode LE in --elem=SE,LE\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
                op->last_elem = negative ? -n : n;
            } else
                op->last_elem = op->start_elem;
            op->elem_given = true;
            break;
        case 'f':
            op->flexible = true;
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
        case 'I':
            if ('+' == optarg[0]) {
                op->iaf = optarg + 1;
                op->append2iaf = true;
            } else if ('-' == optarg[0]) {
                op->iaf2stdout = true;
                op->iaf = optarg;
            } else
                op->iaf = optarg;
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
        case 'S':
            n = sg_get_num(optarg);
            if (n < 0) {
                pr2serr("for --sort-cmp= expect a value of 0 or greater\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->sort_cmp_val = n;
            break;
        case 'v':
            verbose_given = true;
            ++op->verbose;
            break;
        case 'V':
            version_given = true;
            break;
        case 'x':       /* --index=IA */
            op->index_given = true;
            index_arg = optarg;
            break;
        default:
            pr2serr("unrecognised option code 0x%x ??\n", c);
            usage();
            return SG_LIB_SYNTAX_ERROR;
        }
    }
    if (optind < argc) {
        if (optind < argc) {
            bool first = true;

            for (; optind < argc; ++optind) {
                if (first)
                    pr2serr("ddpt_sgl takes no arguments\n");
                pr2serr("  Unrecognized argument: %s\n", argv[optind]);
                first = false;
            }
        }
    }
    if (op->help > 0) {
        usage();
        return 9999;    /* flag to caller to exit without error */
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
        return 9999;
    }
    if (op->index_given) {
        res = cl_index_parse(index_arg, op);
        if (op->verbose > 3) {
            for (int k = 0; k < (int)op->index_arr.size(); ++k)
                printf("%d\n", op->index_arr[k]);
        }
    }
    return 0;
}


int
main(int argc, char * argv[])
{
    bool lba_side;
    bool ret_bool = true;
    bool use_res_sgl = false;
    bool write2o_sgl = false;
    bool write2o_t_sgl = false;
    int a, k, j, n, res, err, vb;
    int a_num_elems = 0;        /* elements in a_sge_p */
    int b_num_elems = 0;        /* elements in b_sge_p */
    int ret = 0;
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
    struct sgl_stats * a_statsp;
    struct sgl_stats * b_statsp;
    char b[256];
    char s[64];
    static const int blen = sizeof(b);
    struct sgl_opts_t opts;
    struct sgl_info_t sgli;
    sgl_vect a_sgl;     /* depending on degen_mask may have < a_num_elems */
    sgl_vect b_sgl;     /* depending on degen_mask may have < b_num_elems */
    sgl_vect res_sgl;
    sgl_vect res_t_sgl;
    const sgl_vect empty_sgl;
    sgl_vect ignore_sgl;  /* ignore if written to (should not be) */

    op = &opts;
    memset((void *)op, 0, sizeof(opts));  // sizeof(struct sgl_opts_t));
    for (k = 0; k < argc; ++k) {
        if (k)
            op->cmd_line.push_back(' ');
        op->cmd_line += argv[k];
    }
    ssp = &op->ab_sgl_stats;
    a_statsp = &ssp->a_stats;
    b_statsp = &ssp->b_stats;
    a_statsp->lowest_lba = INT64_MAX;
    b_statsp->lowest_lba = INT64_MAX;

    ret = parse_command_line(argc, argv, a_sgl_arg, b_sgl_arg, op);
    if (ret)
        return (ret < 9999) ? ret : 0;

    vb = op->verbose;

    if (ACT_ENUMERATE == op->act_val) {
        action_enum();
        return 0;
    }
    if (a_sgl_arg) {
        res = cl_sgl_parse("a-sgl", a_sgl_arg, op->degen_mask, a_sgl,
                           a_sge_p, a_num_elems, *a_statsp, 0, op->flexible,
                           vb);
        if (res) {
            ret = res;
            goto fini;
        }
        if (op->pr_stats) {
            memset(&sgli, 0, sizeof(sgli));
            sgli.elems = a_num_elems;
            sgli.sglp = a_sge_p;
            pr_statistics(*b_statsp, stdout);
            sgl_print(&sgli, true, "a_sgl", true, true);
        }
    }
    if (b_sgl_arg) {
        res = cl_sgl_parse("b-sgl", b_sgl_arg, op->degen_mask, b_sgl,
                           b_sge_p, b_num_elems, *b_statsp, 0, op->flexible,
                           vb);
        if (res) {
            ret = res;
            goto fini;
        }
        if (op->pr_stats) {
            memset(&sgli, 0, sizeof(sgli));
            sgli.elems = b_num_elems;
            sgli.sglp = b_sge_p;
            pr_statistics(*b_statsp, stdout);
            sgl_print(&sgli, true, "b_sgl", true, true);
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
            if (! op->quiet && (! op->elem_given))
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
                           a_sgl, a_sge_p, a_num_elems, *a_statsp,
                           b_num_elems, op->flexible, vb);
        if (res) {
            ret = res;
            goto fini;
        }
        if (op->pr_stats) {
            memset(&sgli, 0, sizeof(sgli));
            sgli.elems = a_num_elems;
            sgli.sglp = a_sge_p;
            pr_statistics(*a_statsp, stdout);
            sgl_print(&sgli, true, "append_b2a", true, true);
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
            else if (ret_bool)
                cp = "LBAs and NUMs";
            else
                cp = "LBA or NUM";
            pr2serr("--a-sgl=SGL, ");
            if (ret_bool)
                pr2serr("all %s are divisible by %d\n", cp, n);
            else
                pr2serr("at least one %s is indivisible by %d\n", cp, n);
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
        ret_bool = sgl_eq_f(a_sge_p, a_num_elems, 0, b_sge_p, b_num_elems, 0,
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
        ret_bool = sgl_eq_f(a_sge_p, a_num_elems, 0, b_sge_p, b_num_elems, 0,
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
        sort2o_sgl(a_sgl, empty_sgl, res_sgl, ignore_sgl, op);
        sort2o_sgl(b_sgl, empty_sgl, res_t_sgl, ignore_sgl, op);
        ret_bool = sgl_eq_f(res_sgl.data(), res_sgl.size(), 0,
                          res_t_sgl.data(), res_t_sgl.size(), 0, true);
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
        sort2o_sgl(a_sgl, empty_sgl, res_sgl, ignore_sgl, op);
        sort2o_sgl(b_sgl, empty_sgl, res_t_sgl, ignore_sgl, op);
        ret_bool = sgl_eq_f(res_sgl.data(), res_sgl.size(), 0,
                            res_t_sgl.data(), res_t_sgl.size(), 0, false);
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
            if (n >= 0) {
                sgvi->lba *= a;
                sgvi->num *= a;
            } else {
                sgvi->lba /= a;
                sgvi->num /= a;
            }
        }
        use_res_sgl = false;
        write2o_sgl = true;
        break;
    case ACT_SELECT:
        if (! (a_sge_p && op->index_given && op->out_fn)) {
            pr2serr("--action=select needs --a_sgl=, --index= and --out=\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        rearrange_sgl(a_sgl, op->index_arr, res_sgl, vb > 2);
        use_res_sgl = true;
        write2o_sgl = true;
        break;
    case ACT_SORT:
        if (! (a_sge_p && (op->out_fn || op->iaf))) {
            pr2serr("--action=sort needs --a_sgl= and (--out= or -iaf=) "
                    "options\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        sort2o_sgl(a_sgl, empty_sgl, res_sgl, ignore_sgl, op);
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
                time_t t = time(NULL);
                struct tm *tm = localtime(&t);

                strftime(s, sizeof(s), "%c", tm);
                fprintf(fp, "# Scatter gather list generated by ddpt_sgl: "
                        "k=%d    %s\n", k + 1, s);
                if (op->document > 1) {
                    fprintf(fp, "# with this command line:\n");
                    fprintf(fp, "#   %s\n", op->cmd_line.c_str());
                }
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
            pr_statistics(*a_statsp, stderr);
        }

        ret = do_split(a_sgl, a_sge_p, *a_statsp, op);

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
            if (! op->chs_given)
                pr2serr("--action=to-chs needs --chs= option\n");
            else
                pr2serr("--action=to-chs needs --a_sgl= and --out= "
                        "options\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        if (a_statsp->last_degen) {
            pr2serr("a-sgl needs to be a 'hard' sgl but last element is "
                    "degenerate\n");
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
            pr_sgl(res_sgl.data(), res_sgl.size(), op->do_hex > 0, NULL);
        use_res_sgl = true;
        write2o_sgl = true;
        break;
    case ACT_TSELECT:
        if (! (a_sge_p && b_sge_p && op->index_given && op->out_fn)) {
            pr2serr("--action=tselect needs --a_sgl=, --b-sgl=, --index= "
                    "and --out=\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        rearrange_sgl(a_sgl, op->index_arr, res_sgl, vb > 2);
        rearrange_twin_sgl(b_sgl, a_sgl, op->index_arr, res_t_sgl, op->do_hex,
                           vb);
        use_res_sgl = true;
        write2o_sgl = true;
        write2o_t_sgl = true;
        break;
    case ACT_TSORT:
        if (! (a_sge_p && b_sge_p && op->out_fn)) {
            pr2serr("--action=tsort needs --a_sgl=, --b-sgl= and --out= "
                    "options\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        if (vb > 2)
            pr2serr("%s: a-sgl sum=%" PRId64 " last_degen=%d; b-sgl sum=%"
                    PRId64 " last_degen=%d\n", __func__, a_statsp->sum,
                    a_statsp->last_degen, b_statsp->sum,
                    b_statsp->last_degen);
        if ((a_statsp->elems < 1) || (b_statsp->elems < 1)) {
            pr2serr("--action=tsort, either a-sgl or b-sgl has no "
                    "elements??\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        if (a_statsp->last_degen) {
            pr2serr("--action=tsort, need 'hard' a-sgl (i.e. need NUM>0 in "
                    "last element\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        if (a_statsp->sum < b_statsp->sum) {
            if (! op->quiet)
                pr2serr("Only first %" PRId64 " blocks (a-sgl.sum) will be "
                        "used from b-sgl\n", a_statsp->sum);
        } else if ((a_statsp->sum > b_statsp->sum) &&
                   (! b_statsp->last_degen)) {
            pr2serr("--action=tsort, have 'hard' b-sgl but it doesn't have "
                    "enough blocks\n");
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        sort2o_sgl(a_sgl, b_sgl, res_sgl, res_t_sgl, op);
        use_res_sgl = true;
        write2o_sgl = true;
        write2o_t_sgl = true;
        break;
    case ACT_TSPLIT_N:
        if (! (a_sge_p && b_sge_p && op->out_fn)) {
            pr2serr("--action=tsplit_%d needs --a_sgl=, --b_sgl=  and "
                    "--out= options\n", op->split_n);
            ret = SG_LIB_SYNTAX_ERROR;
            goto fini;
        }
        for (k = 0, j = 0; k < op->split_n; ++j, k += ((j % 2) ? 0 : 1)) {
            const char * out_ind = (j % 2) ? "_t" : "";

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
                time_t t = time(NULL);
                struct tm *tm = localtime(&t);

                strftime(s, sizeof(s), "%c", tm);
                fprintf(fp, "# Scatter gather list generated by ddpt_sgl: "
                        "k=%d %s    %s\n", k + 1, out_ind, s);
                        /* index shown origin 1 */
                if (op->document > 1) {
                    fprintf(fp, "# with this command line:\n");
                    fprintf(fp, "#   %s\n", op->cmd_line.c_str());
                }
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
            pr_statistics(*a_statsp, stderr);
            pr2serr("sgl 'B' statistics:\n");
            pr_statistics(*b_statsp, stderr);
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
        ret = do_write2o_sgl(op->out_fn, "", true,
                             (use_res_sgl ? res_sgl : a_sgl), op);
        if (ret)
            goto fini;
    }
    if (write2o_t_sgl && op->out_fn && op->out_fn[0]) {
        ret = do_write2o_sgl(op->out_fn, "_t", false, res_t_sgl, op);
        if (ret)
            goto fini;
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
