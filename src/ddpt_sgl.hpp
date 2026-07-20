/*
 * Copyright (c) 2026, Douglas Gilbert
 * All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 *
 */

#ifndef DDPT_SGL_HPP
#define DDPT_SGL_HPP

/* This is a C++ header file for the ddpt_sgl and ddpt_sparse utilities.
 * See ddpt_sgl.cpp and ddpt_spare.cpp for more information.
 */

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

/* Note that C++ headers (e.g.  '#include <vector>') placed towards the end
 * of this file */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <signal.h>
#include <sys/time.h>

#include <string>
#include <vector>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "ddpt.h"


struct split_fn_fp {
    // constructor
    split_fn_fp(const char * fn, FILE * a_fp) : out_fn(fn), fp(a_fp) {}
public:
    std::string out_fn;
    FILE * fp;
};

struct sgl_opts_t {
    bool append2iaf;	/* to Index Array filename (overwrite if false) */
    bool append2out_f;  /* to out_fn (overwrite if false) */
    bool chs_given;	/* Cylinder:Head:Sector */
    bool div_lba_only;
    bool div_num_only;
    bool elem_given;
    bool flexible;
    bool iaf2stdout;
    bool index_given;
    bool out2stdout;
    bool non_overlap_chk;
    bool pr_stats;
    bool quiet;
    int act_val;
    int degen_mask;
    int div_scale_n;
    int document;       /* Add comment(s) to O_SGL(s), >1 add cmdline */
    int do_hex;
    int help;
    int interleave;     /* when splitting a sgl, max number of blocks before
                         * moving to next sgl; def=0 --> no interleave */
    int last_elem;      /* init to -1 which makes start_elem a singleton */
    int round_blks;
    int sort_cmp_val;
    int split_n;
    int start_elem;     /* init to -1 which means write out whole O_SGL */
    int verbose;
    const char * b_sgl_arg;
    const char * iaf;           /* index array filename */
    const char * out_fn;	/* output filename (from --out=O_SGL) */
    const char * fne;   /* file name extension (part of filename after '.') */
    struct chs_t chs;
    struct cl_sgl_stats ab_sgl_stats;
    std::string cmd_line;
    std::vector<int> index_arr; /* input from --index=IA */
    std::vector<struct split_fn_fp> split_out_fns;
    std::vector<struct split_fn_fp> b_split_out_fns; /* 'b' side for tsplit */
};

#endif /* end of ifndef DDPT_SGL_HPP */
