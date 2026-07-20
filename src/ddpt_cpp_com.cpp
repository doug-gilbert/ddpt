/*
 * Copyright (c) 2020-2026, Douglas Gilbert
 * All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 *
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



#include "sg_lib.h"
#include "sg_unaligned.h"
#include "sg_pr2serr.h"

#include "ddpt_cpp_com.hpp"
#include "ddpt_sgl.hpp"

using namespace std;
// using namespace std::chrono;

bool
cmp_asc_lba(const sgl_vect & sgl, int lhs, int rhs)
{
    return sgl[lhs].lba < sgl[rhs].lba;
}

bool
cmp_des_lba(const sgl_vect & sgl, int lhs, int rhs)
{
    return sgl[lhs].lba > sgl[rhs].lba;
}

bool
cmp_asc_num(const sgl_vect & sgl, int lhs, int rhs)
{
    return sgl[lhs].num < sgl[rhs].num;
}

bool
cmp_des_num(const sgl_vect & sgl, int lhs, int rhs)
{
    return sgl[lhs].num > sgl[rhs].num;
}

/* Print statistics (to stdout) */
void
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
void
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
            snprintf(num_str, sizeof(num_str), "0x%" PRIx64, sgep->num);
            fprintf(fp, "    %-14s          %-12s\n", lba_str, num_str);
        } else
            fprintf(fp, "    %-14" PRIu64 "          %-12" PRIu64"\n",
                    sgep->lba, sgep->num);
    }
}

/* Check if 'sgl' has any overlapping blocks. First the sgl is sorted
 * by LBA; then each LBA segment is checked with the one following it
 * to see if they overlap (with the number of blocks of the first/lower
 * one taken into account. Returns true if the overlap, false if they
 * don't. 'sgl' is not modified. */
bool
non_overlap_check(const sgl_vect & sgl, const char * id_str, bool quiet)
{
    int k, ind, prev_ind;
    int sz = sgl.size();
    uint64_t prev_lba, ll, c_lba, prev_num;

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

int
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
void
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
void
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
void
rearrange_twin_sgl(const sgl_vect & twin_sgl, const sgl_vect & ref_sgl,
                   const vector<int> & ref_index_arr, sgl_vect & out_t_sgl,
                   int do_hex, int vb)
{
    int k, j, indx;
    const int ref_sz = ref_sgl.size();
    const int twin_sz = twin_sgl.size();
    uint64_t lba, num, i_num, t_num, i_elem_ind;
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
            pr2serr("%s: iterator explodes at k=%d, num=%" PRIu64 "\n",
                     __func__, k, num);
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
                pr2serr("%s: logic error, t_num=%" PRIu64 ", i_num=%" PRIu64
                        ", num=%" PRIu64 "\n", __func__, t_num, i_num, num);
                break;
            }
        }   /* while can make out_t_sgl.size() > ref_sgl size */
    }
}

/* When just sort of a-sgl, then twin_sglsize() should be 0 and a dummy can
 * given for out_t_sgl (and nothing should be written to it). For twin sort
 * b-sgl's sum should be >= a-sgl's sum (or b-sgl is "soft": its last
 * segment has a NUM of 0). */
void
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

