PROPS-END
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


static const char * ddpt_sgl_version_str = "0.96 20180601 [svn: r356]";

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
#include "sg_pr2serr.h"


#define DEF_NUM_THREADS 4

struct my_opts_t {
    int verbose;
};


int
main(int argc, char * argv[])
{
    int k, n, c, res;
    int force = 0;
    int64_t ll;
    int num_threads = DEF_NUM_THREADS;
    char b[128];
    struct timespec start_tm, end_tm;
    struct opts_t opts;
    struct opts_t * op;
    const char * cp;
    const char * dev_name;

#if 0
    op = &opts;
    memset(op, 0, sizeof(opts));
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

    while (1) {
        int option_index = 0;

        c = getopt_long(argc, argv, "dfghl:M:n:Nq:Q:Rs:St:TvVw:W",
                        long_options, &option_index);
        if (c == -1)
            break;

        switch (c) {
        case 'd':
            op->direct = 1;
            break;
        case 'f':
            force = true;
            break;
        case 'g':
            op->generic_pt = true;
            break;
        case 'h':
        case '?':
            usage();
            return 0;
        case 'l':
            if (isdigit(*optarg)) {
                ll = sg_get_llnum(optarg);
                if (-1 == ll) {
                    pr2serr_lk("could not decode lba\n");
                    return 1;
                } else
                    op->lba = (uint64_t)ll;
                cp = strchr(optarg, ',');
                if (cp) {
                    if (0 == strcmp("-1", cp + 1))
                        op->hi_lba = UINT_MAX;
                    else {
                        ll = sg_get_llnum(cp + 1);
                        if ((-1 == ll) || (ll > UINT_MAX)) {
                            pr2serr_lk("could not decode hi_lba, or > "
                                       "UINT_MAX\n");
                            return 1;
                        } else
                            op->hi_lba = (unsigned int)ll;
                    }
                }
            } else {
                pr2serr_lk("--lba= expects a number\n");
                return 1;
            }
            break;
        case 'M':
            if (isdigit(*optarg)) {
                n = atoi(optarg);
                if ((n < 1) || (n > MAX_Q_PER_FD)) {
                    pr2serr_lk("-M expects a value from 1 to %d\n",
                               MAX_Q_PER_FD);
                    return 1;
                }
                op->maxq_per_thread = n;
            } else {
                pr2serr_lk("--maxqpt= expects a number\n");
                return 1;
            }
            break;
        case 'n':
            if (isdigit(*optarg))
                op->num_per_thread = sg_get_num(optarg);
            else {
                pr2serr_lk("--numpt= expects a number\n");
                return 1;
            }
            break;
        case 'N':
            op->no_xfer = true;
            break;
        case 'q':
            if (isdigit(*optarg)) {
                n = atoi(optarg);
                if (0 == n)
                    op->blqd = BLQ_AT_HEAD;
                else if (1 == n)
                    op->blqd = BLQ_AT_TAIL;
            } else {
                pr2serr_lk("--qat= expects a number: 0 or 1\n");
                return 1;
            }
            break;
        case 'Q':
            if (isdigit(*optarg)) {
                n = atoi(optarg);
                if (0 == n)
                    op->myqd = MYQD_LOW;
                else if (1 == n)
                    op->myqd = MYQD_MEDIUM;
                else if (2 == n)
                    op->myqd = MYQD_HIGH;
            } else {
                pr2serr_lk("--qfav= expects a number: 0, 1 or 2\n");
                return 1;
            }
            break;
        case 'R':
            op->c2e = SCSI_READ16;
            break;
        case 's':
            if (isdigit(*optarg)) {
                op->lb_sz = atoi(optarg);
                if (op->lb_sz < 256) {
                    cerr << "Strange lb_sz, using 256" << endl;
                    op->lb_sz = 256;
                }
            } else {
                pr2serr_lk("--szlb= expects a number\n");
                return 1;
            }
            break;
        case 'S':
            ++op->stats;
            break;
        case 't':
            if (isdigit(*optarg))
                num_threads = atoi(optarg);
            else {
                pr2serr_lk("--tnum= expects a number\n");
                return 1;
            }
            break;
        case 'T':
            op->c2e = SCSI_TUR;
            break;
        case 'v':
            ++op->verbose;
            break;
        case 'V':
            pr2serr_lk("version: %s\n", version_str);
            return 0;
        case 'w':
            if ((isdigit(*optarg) || ('-' == *optarg))) {
                if ('-' == *optarg)
                    op->wait_ms = - atoi(optarg + 1);
                else
                    op->wait_ms = atoi(optarg);
            } else {
                pr2serr_lk("--wait= expects a number\n");
                return 1;
            }
            break;
        case 'W':
            op->c2e = SCSI_WRITE16;
            break;
        default:
            pr2serr_lk("unrecognised option code 0x%x ??\n", c);
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
    return 0;
}


