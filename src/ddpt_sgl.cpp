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
#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>
#include <sys/stat.h>

/* N.B. config.h must precede anything that depends on HAVE_*  */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


static const char * ddpt_sgl_version_str = "0.96 20180606 [svn: r356]";

#if 0
#ifdef SG_LIB_LINUX
#include <sys/ioctl.h>
#include <sys/file.h>
#include <linux/fs.h>   /* <sys/mount.h> */
#include <linux/mtio.h> /* For tape ioctls */
#ifndef MTWEOFI
#define MTWEOFI 35  /* write an end-of-file record (mark) in immediate mode */
#endif
#include <sys/sysmacros.h>
#ifndef major
#include <sys/types.h>
#endif
#include <linux/major.h>

#ifdef HAVE_FALLOCATE
#include <linux/falloc.h>
#ifndef FALLOC_FL_KEEP_SIZE
#define FALLOC_FL_KEEP_SIZE     0x01    /* from lk 3.1 linux/falloc.h */
#endif
#endif

#endif  /* SG_LIB_LINUX */

#ifdef SG_LIB_FREEBSD
#include <sys/ioctl.h>
#include <libgen.h>
#include <sys/disk.h>
#include <sys/filio.h>
#endif

#ifdef SG_LIB_SOLARIS
#include <sys/ioctl.h>
#include <sys/dkio.h>
#endif

#ifdef SG_LIB_WIN32
#ifndef SG_LIB_MINGW
/* cygwin */
#include <sys/ioctl.h>
#endif
#endif

#endif  /* end '#if 0' */

#include "ddpt.h"
#include "sg_lib.h"
#include "sg_unaligned.h"
#include "sg_pr2serr.h"

using namespace std;
using namespace std::chrono;

typedef vector<struct scat_gath_elem> sgl_vect;

#define DEF_NUM_THREADS 4

struct sgl_stats {
    bool last_degen;
    bool not_mono_asc;  /* negated so zeroed default is mono_asc */
    bool not_mono_desc; /* negated so zeroed default is mono_desc */
    int degen;
    int elems;
    int64_t sum;
    uint64_t lowest_lba;
    uint64_t highest_lba;
};

struct cl_sgl_stats {
    struct sgl_stats a_sgl;
    struct sgl_stats b_sgl;
};

struct my_opts_t {
    bool do_overlap_check;
    int degen;
    int dry_run;
    int help;
    int verbose;
    struct cl_sgl_stats ab_sgl_stats;
    vector<const char *> dev_names;
};


static struct option long_options[] = {
        {"a-sgl", required_argument, 0, 'A'},
        {"a_sgl", required_argument, 0, 'A'},
        {"b-sgl", required_argument, 0, 'B'},
        {"b_sgl", required_argument, 0, 'B'},
        {"degen", required_argument, 0, 'D'},
        {"dry-run", no_argument, 0, 'd'},
        {"dry_run", no_argument, 0, 'd'},
        {"help", no_argument, 0, 'h'},
        {"hex", no_argument, 0, 'H'},
        {"overlap-sgl", no_argument, 0, 'o'},
        {"overlap_sgl", no_argument, 0, 'o'},
        {"verbose", no_argument, 0, 'v'},
        {"version", no_argument, 0, 'V'},
        {0, 0, 0, 0},
};

static bool
operator == (const struct scat_gath_elem & lhs,
             const struct scat_gath_elem & rhs)
{
    if (lhs.lba == rhs.lba) {
        if (lhs.num == rhs.num)
            true;
    }
    return false;
}


static int
cl_sgl_parse(const char * key, const char * buf, int degen_treat,
             sgl_vect & sgl, int & num_elems, struct sgl_stats & stats,
             int vb)
{
    bool def_hex = false;
    bool prev_lba_valid = false;
    int len, err, orig_sz, k;
    uint32_t num;
    int64_t ll;
    uint64_t lba;
    uint64_t prev_lba = 0;
    const char * cp;
    const char * method = "";
    struct scat_gath_elem * sgl_p = NULL;
    struct scat_gath_elem s_sgl;

    orig_sz = sgl.size();
    if (orig_sz > 0) {
        prev_lba = sgl[orig_sz - 1].lba;
        prev_lba_valid = true;
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
        sgl_p = file2sgl(cp, def_hex, &num_elems, &err, true);
        if (NULL == sgl_p) {
            pr2serr("%s: bad argument, err=%d\n", key, err);
            return err ? err : SG_LIB_SYNTAX_ERROR;
        }
        method = "in file";
        goto process;
    } else if (num_either_ch_in_str(buf, len, ',', ' ') > 0) {
        sgl_p = cli2sgl(buf, &num_elems, vb > 0);
        if (NULL == sgl_p) {
            pr2serr("%s: bad cli argument\n", key);
            return SG_LIB_SYNTAX_ERROR;
        }
        method = "on cl";
        goto process;
    } else {    /* single number on command line (e.g. skip=1234) */
        sgl_p = &s_sgl;
        ll = sg_get_llnum(buf);
        if (-1LL == ll) {
            pr2serr("%s: unable to decode number\n", key);
            return SG_LIB_SYNTAX_ERROR;
        }
        sgl_p->lba = (uint64_t)ll;
        sgl_p->num = 0;
        stats.last_degen = true;
        ++stats.degen;
        if (0x1 & degen_treat) {
            if (lba < stats.lowest_lba)
                stats.lowest_lba = lba;
            else if (lba > stats.highest_lba)
                stats.highest_lba = lba;
        }
        if (prev_lba_valid && (0x2 & degen_treat)) {
            if ((k > 0) || prev_lba_valid) {
                if (! stats.not_mono_asc) {
                    if (lba < prev_lba)
                        stats.not_mono_asc = true;
                }
                if (! stats.not_mono_desc) {
                    if (lba > prev_lba)
                        stats.not_mono_desc = true;
                }
            }
        }
        sgl.reserve(orig_sz + 1);
        sgl.push_back(*sgl_p);
        ++stats.elems;
        if (vb > 1)
            pr2serr("%s: singleton, half a degenerate sgl element\n", key);
    }
    return 0;

process:
    sgl.reserve(orig_sz + num_elems);
    if (0 == stats.elems)
        stats.lowest_lba = UINT64_MAX;

    for (k = 0; k < num_elems; ++k, ++sgl_p) {
        num = sgl_p->num;
        lba = sgl_p->lba;
        if (0 == num) {
            ++stats.degen;
            if (k == (num_elems - 1))
                stats.last_degen = true;        /* always append */
            else if (0 == degen_treat)
                continue;

            if (0x1 & degen_treat) {
                if (lba < stats.lowest_lba)
                    stats.lowest_lba = lba;
                else if (lba > stats.highest_lba)
                    stats.highest_lba = lba;
            }
            if (0x2 & degen_treat) {
                if ((k > 0) || prev_lba_valid) {
                    if (! stats.not_mono_asc) {
                        if (lba < prev_lba)
                            stats.not_mono_asc = true;
                    }
                    if (! stats.not_mono_desc) {
                        if (lba > prev_lba)
                            stats.not_mono_desc = true;
                    }
                }
                prev_lba = lba;
            }
        } else {        /* normal (i.e. not degenerate) element */
            stats.sum += num;
            if (k > 0) {
                if (! stats.not_mono_asc) {
                    if (lba < prev_lba)
                        stats.not_mono_asc = true;
                }
                if (! stats.not_mono_desc) {
                    if (lba > prev_lba)
                        stats.not_mono_desc = true;
                }
            }
            prev_lba = lba;
            if (lba < stats.lowest_lba)
                stats.lowest_lba = lba;
            else if (lba > stats.highest_lba)
                stats.highest_lba = lba;
        }
        sgl.push_back(*sgl_p);
        ++stats.elems;
    }           /* end of for loop over sgl elements (lba,num pairs) */
    if (vb > 1)
        pr2serr("%s: %s, %d sgl elements\n", key, method, num_elems);
    free(sgl_p);
    return 0;
}


int
main(int argc, char * argv[])
{
    int k, n, c, res;
    int a_num_elems = 0;
    int b_num_elems = 0;
    int force = 0;
    int ret = 0;
    int64_t ll;
    int num_threads = DEF_NUM_THREADS;
    char b[128];
    struct timespec start_tm, end_tm;
    struct my_opts_t opts;
    struct my_opts_t * op;
    const char * a_sgl_arg = NULL;
    const char * b_sgl_arg = NULL;
    const char * cp;
    const char * dev_name;
    sgl_vect a_sgl;
    sgl_vect b_sgl;

    op = &opts;
    memset(op, 0, sizeof(opts));
#if 0
    op->direct = DEF_DIRECT;
    op->lba = DEF_LBA;
    op->hi_lba = 0;
    op->lb_sz = DEF_LB_SZ;
    op->maxq_per_thread = MAX_Q_PER_FD;
    op->num_per_thread = DEF_NUM_PER_THREAD;
    op->no_xfer = !! DEF_NO_XFER;
    op->verbose = 0;
    op->wait_ms = DEF_WAIT_MS;
    op->c2e = SCSI_TUR;
    op->blqd = BLQ_DEFAULT;
    op->block = !! DEF_BLOCKING;
    op->myqd = MYQD_HIGH;
    page_size = sysconf(_SC_PAGESIZE);
#endif

    while (1) {
        int option_index = 0;

        c = getopt_long(argc, argv, "A:B:dD:hHovV",
                        long_options, &option_index);
        if (c == -1)
            break;

        switch (c) {
        case 'A':
            a_sgl_arg = optarg;
            break;
        case 'B':
            b_sgl_arg = optarg;
            break;
        case 'd':
            ++op->dry_run;
            break;
        case 'D':
            n = sg_get_num(optarg);
            if ((n < 0) || (n > 3)) {
                pr2serr("for --degen= expect a vlaue from 0 to 2 inclusive\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->degen = n;
            break;
        case 'h':
        case '?':
            ++op->help;
            break;
        case 'o':
            op->do_overlap_check = true;
            break;
        case 'v':
            ++op->verbose;
            break;
        case 'V':
            pr2serr("version: %s\n", ddpt_sgl_version_str);
            return 0;
        default:
            pr2serr("unrecognised option code 0x%x ??\n", c);
            pr2serr("Code usage message\n");
            // usage();
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
        pr2serr("Code usage message\n");
        // usage();
        return 0;
    }
    if (a_sgl_arg) {
        res = cl_sgl_parse("a-sgl", a_sgl_arg, op->degen, a_sgl, a_num_elems,
                           op->ab_sgl_stats.a_sgl, op->verbose);
        if (res) {
            ret = res;
            goto fini;
        }
    }
    if (b_sgl_arg) {
        res = cl_sgl_parse("b-sgl", b_sgl_arg, op->degen, b_sgl, b_num_elems,
                           op->ab_sgl_stats.b_sgl, op->verbose);
        if (res) {
            ret = res;
            goto fini;
        }
    }

#if 0
    if (0 == op->dev_names.size()) {
        fprintf(stderr, "No sg_disk_device-s given\n\n");
        usage();
        return 1;
    }
    if (op->hi_lba && (op->lba > op->hi_lba)) {
        cerr << "lba,hi_lba range is illegal" << endl;
        return 1;
    }

    try {
        struct stat a_stat;

        for (k = 0; k < (int)op->dev_names.size(); ++k) {
            dev_name = op->dev_names[k];
            if (stat(dev_name, &a_stat) < 0) {
                snprintf(b, sizeof(b), "could not stat() %s", dev_name);
                perror(b);
                return 1;
            }
            if (! S_ISCHR(a_stat.st_mode)) {
                pr2serr_lk("%s should be a sg device which is a char "
                           "device. %s\n", dev_name, dev_name);
                pr2serr_lk("is not a char device and damage could be done "
                           "if it is a BLOCK\ndevice, exiting ...\n");
                return 1;
            }
            if (! force) {
                res = do_inquiry_prod_id(dev_name, op->block, b, sizeof(b));
                if (res) {
                    pr2serr_lk("INQUIRY failed on %s\n", dev_name);
                    return 1;
                }
                // For safety, since <lba> written to, only permit scsi_debug
                // devices. Bypass this with '-f' option.
                if (0 != memcmp("scsi_debug", b, 10)) {
                    pr2serr_lk("Since this utility may write to LBAs, "
                               "only devices with the\n"
                               "product ID 'scsi_debug' accepted. Use '-f' "
                               "to override.\n");
                    return 2;
                }
            }
            if (UINT_MAX == op->hi_lba) {
                unsigned int last_lba;
                unsigned int blk_sz;

                res = do_read_capacity(dev_name, op->block, &last_lba,
                                       &blk_sz);
                if (2 == res)
                    res = do_read_capacity(dev_name, op->block, &last_lba,
                                           &blk_sz);
                if (res) {
                    pr2serr_lk("READ CAPACITY(10) failed on %s\n", dev_name);
                    return 1;
                }
                op->hi_lbas.push_back(last_lba);
                if (blk_sz != (unsigned int)op->lb_sz)
                    pr2serr_lk(">>> warning: Logical block size (%d) of %s\n"
                               "    differs from command line option (or "
                               "default)\n", blk_sz, dev_name);
            }
        }

        start_tm.tv_sec = 0;
        start_tm.tv_nsec = 0;
        if (clock_gettime(CLOCK_MONOTONIC, &start_tm) < 0)
            perror("clock_gettime failed");

        vector<thread *> vt;

        /* start multi-threaded section */
        for (k = 0; k < num_threads; ++k) {
            thread * tp = new thread {work_thread, k, op};
            vt.push_back(tp);
        }

        // g++ 4.7.3 didn't like range-for loop here
        for (k = 0; k < (int)vt.size(); ++k)
            vt[k]->join();
        /* end multi-threaded section, just this main thread left */

        for (k = 0; k < (int)vt.size(); ++k)
            delete vt[k];

        n = uniq_pack_id.load() - 1;
        if (((n > 0) || op->generic_pt) &&
            (0 == clock_gettime(CLOCK_MONOTONIC, &end_tm))) {
            struct timespec res_tm;
            double a, b;

            if (op->generic_pt)
                n = op->num_per_thread * num_threads;
            res_tm.tv_sec = end_tm.tv_sec - start_tm.tv_sec;
            res_tm.tv_nsec = end_tm.tv_nsec - start_tm.tv_nsec;
            if (res_tm.tv_nsec < 0) {
                --res_tm.tv_sec;
                res_tm.tv_nsec += 1000000000;
            }
            a = res_tm.tv_sec;
            a += (0.000001 * (res_tm.tv_nsec / 1000));
            b = (double)n;
            if (a > 0.000001) {
                printf("Time to complete %d commands was %d.%06d seconds\n",
                       n, (int)res_tm.tv_sec, (int)(res_tm.tv_nsec / 1000));
                printf("Implies %.0f IOPS\n", (b / a));
            }
        }

        if (op->verbose || op->stats) {
            cout << "Number of sync_starts: " << sync_starts.load() << endl;
            cout << "Number of async_starts: " << async_starts.load() << endl;
            cout << "Number of async_finishes: " << async_finishes.load() <<
                    endl;
            cout << "Last pack_id: " << n << endl;
            cout << "Number of EBUSYs: " << ebusy_count.load() << endl;
            cout << "Number of start EAGAINs: " << start_eagain_count.load()
                 << endl;
            cout << "Number of finish EAGAINs: " << fin_eagain_count.load()
                 << endl;
        }
    }
    catch(system_error& e)  {
        cerr << "got a system_error exception: " << e.what() << '\n';
        auto ec = e.code();
        cerr << "category: " << ec.category().name() << '\n';
        cerr << "value: " << ec.value() << '\n';
        cerr << "message: " << ec.message() << '\n';
        cerr << "\nNote: if g++ may need '-pthread' or similar in "
                "compile/link line" << '\n';
    }
    catch(...) {
        cerr << "got another exception: " << '\n';
    }

#endif

    ret = 0;

fini:
    return ret;
}


