/*
 * Copyright (c) 2008-2010 Douglas Gilbert.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

/* ddpt is a utility program for copying files. It broadly follows the syntax
 * and semantics of the "dd" program found in Unix. ddpt is specialised for
 * "files" that represent storage devices, especially those that understand
 * the SCSI command set accessed via a pass-through.
 */

/*
 * The ddpt utility is a rewritten and extended version of the sg_dd utility
 * found in the sg3_utils package. sg_dd has a GPL (version 2) which has been
 * changed to a somewhat freer FreeBSD style license in ddpt.
 * Both licenses are considered "open source".
 *
 * Windows "block" devices, when _not_ accessed via the pass-through, don't
 * seem to work when POSIX/Unix like IO calls are used (e.g. write()).
 * So may need CreateFile, ReadFile, WriteFile, SetFilePointer and friends.
 */

static char * version_str = "0.91 20100715";

/* Was needed for posix_fadvise() */
/* #define _XOPEN_SOURCE 600 */

/* Need _GNU_SOURCE for O_DIRECT */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
#include <time.h>
#elif defined(HAVE_GETTIMEOFDAY)
#include <sys/time.h>
#endif

#include "ddpt.h"

#ifdef SG_LIB_LINUX
#include <sys/ioctl.h>
#include <sys/sysmacros.h>
#include <sys/file.h>
#include <linux/major.h>
#include <linux/fs.h>   /* <sys/mount.h> */
#endif

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

#include "sg_lib.h"
#include "sg_cmds_basic.h"
#include "sg_cmds_extra.h"
#include "sg_pt.h"


static int64_t dd_count = -1;   /* of input blocks */
static int64_t req_count = 0;
static int64_t in_full = 0;
static int in_partial = 0;
static int64_t out_full = 0;
static int out_partial = 0;
static int out_sparse_active = 0;
static int out_sparing_active = 0;
static int out_trim_active = 0;
static int64_t out_sparse = 0;  /* used for sparse, sparing + trim */
static int recovered_errs = 0;
static int unrecovered_errs = 0;
static int trim_errs = 0;
static int64_t lowest_unrecovered = 0;
static int64_t highest_unrecovered = -1;
static int num_retries = 0;
static int sum_of_resids = 0;
static int interrupted_retries = 0;

static struct sg_pt_base * if_ptvp = NULL;
static struct sg_pt_base * of_ptvp = NULL;

static int do_time = 1;         /* default was 0 in sg_dd */
static int verbose = 0;
static int quiet = 0;
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
static int start_tm_valid = 0;
static struct timespec start_tm;
#elif defined(HAVE_GETTIMEOFDAY)
static int start_tm_valid = 0;
static struct timeval start_tm;
#endif
static int ibs_hold = 0;
static int max_uas = MAX_UNIT_ATTENTIONS;
static int max_aborted = MAX_ABORTED_CMDS;
static int coe_limit = 0;
static int coe_count = 0;
static int some_coe_set = 0;

static unsigned char * zeros_buff = NULL;


static void calc_duration_throughput(int contin);


static void
usage()
{
    fprintf(stderr, "Usage: "
           "ddpt  [bs=BS] [conv=CONVS] [count=COUNT] [ibs=IBS] if=IFILE\n"
           "             [iflag=FLAGS] [obs=OBS] [of=OFILE] [oflag=FLAGS] "
           "[seek=SEEK]\n"
           "             [skip=SKIP] [status=STAT] [--help] [--version]\n\n"
           "             [bpt=BPT[,OBPC]] [cdbsz=6|10|12|16] [coe=0|1] "
           "[coe_limit=CL]\n"
           "             [of2=OFILE2] [retries=RETR] [verbose=VERB] "
           "[--verbose]\n"
           "  where:\n"
           "    bpt         input Blocks Per Transfer (BPT) (def: 128 if "
           "IBS<2048, else 32)\n"
           "                Output Blocks Per Check (OBPC) (def: 0 implies "
           "BPT*IBS/OBS)\n"
           "    bs          block size for input and output (overrides "
           "ibs and obs)\n");
    fprintf(stderr,
           "    cdbsz       size of SCSI READ or WRITE cdb (default is "
           "10)\n"
           "    coe         0->exit on error (def), 1->continue on pt "
           "error (zero fill)\n"
           "    coe_limit   limit consecutive 'bad' blocks on reads to CL "
           "times\n"
           "                when coe=1 (default: 0 which is no limit)\n"
           "    conv        conversions, comma separated list of CONVS "
           "(see below)\n"
           "    count       number of input blocks to copy (def: "
           "(remaining)\n"
           "                device size)\n"
           "    ibs         input block size (default 512 bytes)\n"
           "    if          file or device to read from (for stdin use "
           "'-')\n"
           "    iflag       input flags, comma separated list from FLAGS "
           "(see below)\n"
           "    obs         output block size (def: 512), when IBS is "
           "not equal OBS\n"
           "                [ (((IBS * BPT) %% OBS) == 0) is required\n"
           "    of          file or device to write to (def: /dev/null)\n");
    fprintf(stderr,
           "    of2         additional output file (def: /dev/null), "
           "OFILE2 should be\n"
           "                normal file or pipe\n"
           "    oflag       output flags, comma separated list from FLAGS "
           "(see below)\n"
           "    retries     retry pass-through errors RETR times "
           "(def: 0)\n"
           "    seek        block position to start writing to OFILE\n"
           "    skip        block position to start reading from IFILE\n"
           "    status      value: 'noxfer' suppresses throughput "
           "calculation\n"
           "    verbose     0->normal(def), 1->some noise, 2->more noise, "
           "etc\n"
           "                -1->quiet (stderr->/dev/null)\n"
           "    --help      print out this usage message then exit\n"
           "    --verbose   equivalent to verbose=1\n"
           "    --version   print version information then exit\n\n"
           "Copy all or part of IFILE to OFILE, IBS*BPT bytes at a time. "
           "Similar to\n"
           "dd command. Support for block devices, especially those "
           "accessed via\na SCSI pass-through.\n"
           "FLAGS: append(o),coe,direct,dpo,excl,flock,fua,fua_nv,nocache,"
           "nowrite(o),\nnull,pt,resume(o),sparing(o),sparse(o),ssync(o),"
           "sync,trim(o),trunc(o),unmap(o)\n"
           "CONVS: noerror,null,resume,sparing,sparse,sync,trunc\n");
}


static void
print_stats(const char * str)
{
    if (0 != dd_count)
        fprintf(stderr, "  remaining block count=%"PRId64"\n", dd_count);
    fprintf(stderr, "%s%"PRId64"+%d records in\n", str, in_full - in_partial,
            in_partial);
    fprintf(stderr, "%s%"PRId64"+%d records out\n", str,
            out_full - out_partial, out_partial);
    if (out_sparse_active || out_sparing_active) {
        if (out_trim_active)
            fprintf(stderr, "%s%"PRId64" %s records out\n", str, out_sparse,
                    (trim_errs ? "attempted trim" : "trimmed"));
        else
            fprintf(stderr, "%s%"PRId64" bypassed records out\n", str,
                    out_sparse);
    }
    if (recovered_errs > 0)
        fprintf(stderr, "%s%d recovered errors\n", str, recovered_errs);
    if (num_retries > 0)
        fprintf(stderr, "%s%d retries attempted\n", str, num_retries);
    if (some_coe_set || unrecovered_errs)
        fprintf(stderr, "%s%d unrecovered error%s\n", str, unrecovered_errs,
                ((1 == unrecovered_errs) ? "" : "s"));
    if (unrecovered_errs && (highest_unrecovered >= 0))
        fprintf(stderr, "lowest unrecovered lba=%"PRId64", highest "
                "unrecovered lba=%"PRId64"\n", lowest_unrecovered,
                highest_unrecovered);
    if (trim_errs)
        fprintf(stderr, "%s%d trim errors\n", str, trim_errs);
    if (interrupted_retries > 0)
        fprintf(stderr, "%s%d retries after interrupted system call(s)\n",
                str, interrupted_retries);
}

static void
register_handler(int sig_num, void (*sig_handler) (int sig))
{
#ifdef SG_LIB_MINGW
    if ((signal(sig_num, sig_handler) == SIG_ERR) && verbose)
        fprintf(stderr, "register_handler: failed in sig_num=%d\n", sig_num);

#else
    struct sigaction sigact;

    sigaction(sig_num, NULL, &sigact);
    if (((SIGINT != sig_num) && (SIGQUIT != sig_num)) ||
        (sigact.sa_handler != SIG_IGN)) {
        sigact.sa_handler = sig_handler;
        sigemptyset(&sigact.sa_mask);
        sigact.sa_flags = 0;
        sigaction(sig_num, &sigact, NULL);
    }
#endif
}

static void
interrupt_handler(int sig)
{
#ifdef SG_LIB_MINGW
    fprintf(stderr, "Interrupted by signal,");
    print_stats("");
    if (do_time)
        calc_duration_throughput(0);
    /* kill(getpid(), sig); */
    sig = sig;
    exit(127);
#else
    struct sigaction sigact;

    sigact.sa_handler = SIG_DFL;
    sigemptyset(&sigact.sa_mask);
    sigact.sa_flags = 0;
    sigaction(sig, &sigact, NULL);
    fprintf(stderr, "Interrupted by signal,");
    print_stats("");
    if (do_time)
        calc_duration_throughput(0);
    fprintf(stderr, "Interrupted by signal %d\n", sig);
    fprintf(stderr, "To resume, invoke with same arguments plus "
            "oflag=resume\n");
    kill(getpid(), sig);
#endif
}

static void
siginfo_handler(int sig)
{
    sig = sig;  /* dummy to stop -W warning messages */
    fprintf(stderr, "Progress report, continuing ...\n");
    print_stats("  ");
    if (do_time)
        calc_duration_throughput(1);
}

/* Process arguments given to 'conv=" option. */
static int
process_conv(const char * arg, struct flags_t * ifp, struct flags_t * ofp)
{
    char buff[256];
    char * cp;
    char * np;

    strncpy(buff, arg, sizeof(buff));
    buff[sizeof(buff) - 1] = '\0';
    if ('\0' == buff[0]) {
        fprintf(stderr, "no conversions found\n");
        return 1;
    }
    cp = buff;
    do {
        np = strchr(cp, ',');
        if (np)
            *np++ = '\0';
        if (0 == strcmp(cp, "noerror"))
            ++ifp->coe;         /* will still fail on write error */
        else if (0 == strcmp(cp, "null"))
            ;
        else if (0 == strcmp(cp, "resume"))
            ++ofp->resume;
        else if (0 == strcmp(cp, "sparing"))
            ++ofp->sparing;
        else if (0 == strcmp(cp, "sparse"))
            ++ofp->sparse;
        else if (0 == strcmp(cp, "sync"))
            ;   /* dd(susv4): pad errored block(s) with zeros but ddpt does
                 * that by default. Typical dd use: 'dd conv=noerror,sync' */
        else if (0 == strcmp(cp, "trunc"))
            ++ofp->trunc;
        else {
            fprintf(stderr, "unrecognised flag: %s\n", cp);
            return 1;
        }
        cp = np;
    } while (cp);
    return 0;
}

/* Process arguments given to 'iflag=" and 'oflag=" options. */
static int
process_flags(const char * arg, struct flags_t * fp)
{
    char buff[256];
    char * cp;
    char * np;

    strncpy(buff, arg, sizeof(buff));
    buff[sizeof(buff) - 1] = '\0';
    if ('\0' == buff[0]) {
        fprintf(stderr, "no flag found\n");
        return 1;
    }
    cp = buff;
    do {
        np = strchr(cp, ',');
        if (np)
            *np++ = '\0';
        if (0 == strcmp(cp, "append"))
            ++fp->append;
        else if (0 == strcmp(cp, "coe"))
            ++fp->coe;
        else if (0 == strcmp(cp, "direct"))
            ++fp->direct;
        else if (0 == strcmp(cp, "dpo"))
            ++fp->dpo;
        else if (0 == strcmp(cp, "excl"))
            ++fp->excl;
        else if (0 == strcmp(cp, "flock"))
            ++fp->flock;
        else if (0 == strcmp(cp, "fua_nv"))     /* check fua_nv before fua */
            ++fp->fua_nv;
        else if (0 == strcmp(cp, "fua"))
            ++fp->fua;
        else if (0 == strcmp(cp, "nocache"))
            ++fp->nocache;
        else if (0 == strcmp(cp, "nowrite"))
            ++fp->nowrite;
        else if (0 == strcmp(cp, "null"))
            ;
        else if (0 == strcmp(cp, "pt"))
            ++fp->pt;
        else if (0 == strcmp(cp, "resume"))
            ++fp->resume;
        else if (0 == strcmp(cp, "sparing"))
            ++fp->sparing;
        else if (0 == strcmp(cp, "sparse"))
            ++fp->sparse;
        else if (0 == strcmp(cp, "ssync"))
            ++fp->ssync;
        else if (0 == strcmp(cp, "sync"))
            ++fp->sync;
        else if ((0 == strcmp(cp, "trim")) || (0 == strcmp(cp, "unmap"))) {
            /* treat trim (ATA term) and unmap (SCSI term) as synonyms */
            ++fp->wsame16;
            fp->sparse += 2;
        } else if (0 == strcmp(cp, "trunc"))
            ++fp->trunc;
        else {
            fprintf(stderr, "unrecognised flag: %s\n", cp);
            return 1;
        }
        cp = np;
    } while (cp);
    return 0;
}

/* Process options on the command line. */
static int
process_cl(struct opts_t * optsp, int argc, char * argv[])
{
    char str[STR_SZ];
    char * key;
    char * buf;
    char * cp;
    int k, n;

    for (k = 1; k < argc; ++k) {
        if (argv[k]) {
            strncpy(str, argv[k], STR_SZ);
            str[STR_SZ - 1] = '\0';
        } else
            continue;
        for (key = str, buf = key; *buf && *buf != '=';)
            ++buf;
        if (*buf)
            *buf++ = '\0';
        if (0 == strncmp(key, "app", 3)) {
            optsp->iflagp->append = sg_get_num(buf);
            optsp->oflagp->append = optsp->iflagp->append;
        } else if (0 == strcmp(key, "bpt")) {
            cp = strchr(buf, ',');
            if (cp)
                *cp = '\0';
            if ((n = sg_get_num(buf)) < 0) {
                fprintf(stderr, ME "bad BPT argument to 'bpt='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (n > 0) {
                optsp->bpt_i = n;
                optsp->bpt_given = 1;
            }
            if (cp) {
                n = sg_get_num(cp + 1);
                if (n < 0) {
                    fprintf(stderr, ME "bad OBPC argument to 'bpt='\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
                optsp->obpc = n;
            }
        } else if (0 == strcmp(key, "bs")) {
            n = sg_get_num(buf);
            if (n < 0) {
                fprintf(stderr, ME "bad argument to 'bs='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (optsp->bs_given) {
                fprintf(stderr, ME "second 'bs=' option given, dangerous\n");
                return SG_LIB_SYNTAX_ERROR;
            } else
                optsp->bs_given = 1;
            if ((optsp->ibs_given) || (optsp->obs_given)) {
                fprintf(stderr, ME "'bs=' option cannot be combined with "
                        "'ibs=' or 'obs='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            optsp->ibs = n;
            optsp->obs = n;
        } else if (0 == strcmp(key, "cdbsz")) {
            optsp->iflagp->cdbsz = sg_get_num(buf);
            optsp->oflagp->cdbsz = optsp->iflagp->cdbsz;
            optsp->cdbsz_given = 1;
        } else if (0 == strcmp(key, "coe")) {
            optsp->iflagp->coe = sg_get_num(buf);
            optsp->oflagp->coe = optsp->iflagp->coe;
        } else if (0 == strcmp(key, "coe_limit")) {
            coe_limit = sg_get_num(buf);
            if (-1 == coe_limit) {
                fprintf(stderr, ME "bad argument to 'coe_limit='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "conv")) {
            if (process_conv(buf, optsp->iflagp, optsp->oflagp)) {
                fprintf(stderr, ME "bad argument to 'conv='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "count")) {
            if (0 != strcmp("-1", buf)) {
                dd_count = sg_get_llnum(buf);
                if (-1LL == dd_count) {
                    fprintf(stderr, ME "bad argument to 'count='\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
            }   /* 'count=-1' is accepted, means calculate count */
        } else if (0 == strcmp(key, "ibs")) {
            n = sg_get_num(buf);
            if (n < 0) {
                fprintf(stderr, ME "bad argument to 'ibs='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (optsp->bs_given) {
                fprintf(stderr, ME "'ibs=' option cannot be combined with "
                        "'bs='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            ++optsp->ibs_given;
            optsp->ibs = n;
        } else if (strcmp(key, "if") == 0) {
            if ('\0' != optsp->inf[0]) {
                fprintf(stderr, "Second IFILE argument??\n");
                return SG_LIB_SYNTAX_ERROR;
            } else
                strncpy(optsp->inf, buf, INOUTF_SZ);
        } else if (0 == strcmp(key, "iflag")) {
            if (process_flags(buf, optsp->iflagp)) {
                fprintf(stderr, ME "bad argument to 'iflag='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "obs")) {
            n = sg_get_num(buf);
            if (n < 0) {
                fprintf(stderr, ME "bad argument to 'obs='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (optsp->bs_given) {
                fprintf(stderr, ME "'obs=' option cannot be combined with "
                        "'bs='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            ++optsp->obs_given;
            optsp->obs = n;
        } else if (strcmp(key, "of") == 0) {
            if ('\0' != optsp->outf[0]) {
                fprintf(stderr, "Second OFILE argument??\n");
                return SG_LIB_SYNTAX_ERROR;
            } else
                strncpy(optsp->outf, buf, INOUTF_SZ);
        } else if (strcmp(key, "of2") == 0) {
            if ('\0' != optsp->out2f[0]) {
                fprintf(stderr, "Second OFILE2 argument??\n");
                return SG_LIB_SYNTAX_ERROR;
            } else
                strncpy(optsp->out2f, buf, INOUTF_SZ);
        } else if (0 == strcmp(key, "oflag")) {
            if (process_flags(buf, optsp->oflagp)) {
                fprintf(stderr, ME "bad argument to 'oflag='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "retries")) {
            optsp->iflagp->retries = sg_get_num(buf);
            optsp->oflagp->retries = optsp->iflagp->retries;
            if (-1 == optsp->iflagp->retries) {
                fprintf(stderr, ME "bad argument to 'retries='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "seek")) {
            optsp->seek = sg_get_llnum(buf);
            if (-1LL == optsp->seek) {
                fprintf(stderr, ME "bad argument to 'seek='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "skip")) {
            optsp->skip = sg_get_llnum(buf);
            if (-1LL == optsp->skip) {
                fprintf(stderr, ME "bad argument to 'skip='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "status")) {
            if (strncmp(buf, "null", 4))
                ;
            else if (strncmp(buf, "noxfer", 6))
                do_time = 0;
            else {
                fprintf(stderr, ME "status=' expects 'noxfer' or 'null'\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strncmp(key, "verb", 4)) {
            verbose = sg_get_num(buf);
            if (verbose < 0) {
                ++quiet;
                verbose = 0;
            }
        } else if (0 == strncmp(key, "--verb", 6))
            ++verbose;
        else if (0 == strncmp(key, "-vvvv", 5))
            verbose += 4;
        else if (0 == strncmp(key, "-vvv", 4))
            verbose += 3;
        else if (0 == strncmp(key, "-vv", 3))
            verbose += 2;
        else if (0 == strncmp(key, "-v", 2))
            ++verbose;
        else if ((0 == strncmp(key, "--help", 7)) ||
                 (0 == strncmp(key, "-h", 2)) ||
                 (0 == strcmp(key, "-?"))) {
            usage();
            return -1;
        } else if ((0 == strncmp(key, "--vers", 6)) ||
                   (0 == strncmp(key, "-V", 2))) {
            fprintf(stderr, ME "%s\n", version_str);
            return -1;
        } else {
            fprintf(stderr, "Unrecognized option '%s'\n", key);
            fprintf(stderr, "For more information use '--help'\n");
            return SG_LIB_SYNTAX_ERROR;
        }
    }

    if ((0 == optsp->ibs) && (0 == optsp->obs)) {
        optsp->ibs = DEF_BLOCK_SIZE;
        optsp->obs = DEF_BLOCK_SIZE;
        if (optsp->inf[0])
            fprintf(stderr, "Assume block size of %d bytes for both "
                    "input and output\n", DEF_BLOCK_SIZE);
    } else if (0 == optsp->obs) {
        optsp->obs = optsp->ibs;
        if (verbose && (optsp->ibs != DEF_BLOCK_SIZE) && optsp->outf[0])
            fprintf(stderr, "warning: obs not given and ibs=%d so set "
                    "obs=%d\n", optsp->ibs, optsp->obs); 
    } else if (0 == optsp->ibs) {
        optsp->ibs = optsp->obs;
        if (verbose && (optsp->obs != DEF_BLOCK_SIZE))
            fprintf(stderr, "warning: ibs not given and obs=%d so set "
                    "ibs=%d\n", optsp->obs, optsp->ibs); 
    }
    ibs_hold = optsp->ibs;
    /* defaulting transfer size to 128*2048 for CD/DVDs is too large
       for the block layer in lk 2.6 and results in an EIO on the
       SG_IO ioctl. So reduce it in that case. */
    if ((optsp->ibs >= 2048) && (0 == optsp->bpt_given))
        optsp->bpt_i = DEF_BLOCKS_PER_2048TRANSFER;

    if ((optsp->ibs != optsp->obs) &&
        (0 != ((optsp->ibs * optsp->bpt_i) % optsp->obs))) {
        fprintf(stderr, "'ibs' and 'obs' can only differ when "
                "((ibs*bpt)/obs) has no remainder\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if ((optsp->skip < 0) || (optsp->seek < 0)) {
        fprintf(stderr, "skip and seek cannot be negative\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if ((optsp->oflagp->append > 0) && (optsp->seek > 0)) {
        fprintf(stderr, "Can't use both append and seek switches\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if (optsp->bpt_i < 1) {
        fprintf(stderr, "bpt must be greater than 0\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if (optsp->iflagp->append)
        fprintf(stderr, "append flag ignored on input\n");
    if (optsp->iflagp->sparing)
        fprintf(stderr, "sparing flag ignored on input\n");
    if (optsp->iflagp->sparse)
        fprintf(stderr, "sparse flag ignored on input\n");
    if (optsp->iflagp->ssync)
        fprintf(stderr, "ssync flag ignored on input\n");
    if (optsp->oflagp->trunc) {
        if (optsp->oflagp->resume) {
            optsp->oflagp->trunc = 0;
            fprintf(stderr, "trunc ignored due to resume flag\n");
        } else if (optsp->oflagp->append) {
            optsp->oflagp->trunc = 0;
            fprintf(stderr, "trunc ignored due to append flag\n");
        } else if (optsp->oflagp->sparing) {
            fprintf(stderr, "trunc flag conflicts with sparing\n");
            return SG_LIB_SYNTAX_ERROR;
        }
    }

    if (verbose) {      /* report flags used but not supported */
#ifndef SG_LIB_LINUX
        if (optsp->iflagp->flock || optsp->oflagp->flock)
            fprintf(stderr, "warning: 'flock' flag not supported on this "
                    "platform\n");
#endif

#ifndef HAVE_POSIX_FADVISE
        if (optsp->iflagp->nocache || optsp->oflagp->nocache)
            fprintf(stderr, "warning: 'nocache' flag not supported on this "
                    "platform\n");
#endif

#if O_SYNC == 0
        if (optsp->iflagp->sync || optsp->oflagp->sync)
            fprintf(stderr, "warning: 'sync' flag (O_SYNC) not supported on "
                    "this platform\n");
#endif
#if O_DIRECT == 0
        if (optsp->iflagp->direct || optsp->oflagp->direct)
            fprintf(stderr, "warning: 'direct' flag (O_DIRECT) not supported "
                    "on this platform\n");
#endif
    }
    if ((optsp->iflagp->coe > 0) || (optsp->oflagp->coe > 0))
        some_coe_set = 1;
    return 0;
}

/* Attempt to categorize the file type from the given filename.
 * Separate version for Windows and Unix. Windows version does some
 * file name processing. */
#ifndef SG_LIB_WIN32

static int
dd_filetype(const char * filename)
{
    struct stat st;
    size_t len = strlen(filename);

    if ((1 == len) && ('.' == filename[0]))
        return FT_DEV_NULL;
    if (stat(filename, &st) < 0)
        return FT_ERROR;
    if (S_ISREG(st.st_mode)) {
        // fprintf(stderr, "dd_filetype: regular file, st_size=%"PRId64"\n",
        //         st.st_size);
        return FT_REG;
    } else if (S_ISCHR(st.st_mode)) {
#ifdef SG_LIB_LINUX
        /* major() and minor() defined in sys/sysmacros.h */
        if ((MEM_MAJOR == major(st.st_rdev)) &&
            (DEV_NULL_MINOR_NUM == minor(st.st_rdev)))
            return FT_DEV_NULL;
        if (SCSI_GENERIC_MAJOR == major(st.st_rdev))
            return FT_PT;
        if (SCSI_TAPE_MAJOR == major(st.st_rdev))
            return FT_TAPE;
        return FT_OTHER;
#elif SG_LIB_FREEBSD
        {
            /* int d_flags;  for FIOFTYPE ioctl see sys/filio.h */
            char s[STR_SZ];
            char * bname;

            strcpy(s, filename);
            bname = basename(s);
            if (0 == strcmp("null", bname))
                return FT_DEV_NULL;
            else if (0 == memcmp("pass", bname, 4))
                return FT_PT;
            else
                return FT_BLOCK;  /* freebsd doesn't have block devices! */
        }
#elif SG_LIB_SOLARIS
        /* might be /dev/rdsk or /dev/scsi , require pt override */
        return FT_BLOCK;  
#else
        return FT_PT;
#endif
    } else if (S_ISBLK(st.st_mode))
        return FT_BLOCK;
    else if (S_ISFIFO(st.st_mode))
        return FT_FIFO;
    return FT_OTHER;
}
#endif

static char *
dd_filetype_str(int ft, char * buff)
{
    int off = 0;

    if (FT_DEV_NULL & ft)
        off += snprintf(buff + off, 32, "null device ");
    if (FT_PT & ft)
        off += snprintf(buff + off, 32, "pass-through [pt] device ");
    if (FT_BLOCK & ft)
        off += snprintf(buff + off, 32, "block device ");
    if (FT_FIFO & ft)
        off += snprintf(buff + off, 32, "fifo [named pipe] ");
    if (FT_TAPE & ft)
        off += snprintf(buff + off, 32, "SCSI tape device ");
    if (FT_REG & ft)
        off += snprintf(buff + off, 32, "regular file ");
    if (FT_OTHER & ft)
        off += snprintf(buff + off, 32, "other file type ");
    if (FT_ERROR & ft)
        off += snprintf(buff + off, 32, "unable to 'stat' file ");
    return buff;
}

/* Return of 0 -> success, see sg_ll_read_capacity*() otherwise */
static int
scsi_read_capacity(int sg_fd, int64_t * num_sect, int * sect_sz)
{
    int k, res;
    unsigned int ui;
    unsigned char rcBuff[RCAP16_REPLY_LEN];
    int verb;

    verb = (verbose ? verbose - 1: 0);
    res = sg_ll_readcap_10(sg_fd, 0, 0, rcBuff, READ_CAP_REPLY_LEN, 0, verb);
    if (0 != res)
        return res;

    if ((0xff == rcBuff[0]) && (0xff == rcBuff[1]) && (0xff == rcBuff[2]) &&
        (0xff == rcBuff[3])) {
        int64_t ls;

        res = sg_ll_readcap_16(sg_fd, 0, 0, rcBuff, RCAP16_REPLY_LEN, 0,
                               verb);
        if (0 != res)
            return res;
        for (k = 0, ls = 0; k < 8; ++k) {
            ls <<= 8;
            ls |= rcBuff[k];
        }
        *num_sect = ls + 1;
        *sect_sz = (rcBuff[8] << 24) | (rcBuff[9] << 16) |
                   (rcBuff[10] << 8) | rcBuff[11];
    } else {
        ui = ((rcBuff[0] << 24) | (rcBuff[1] << 16) | (rcBuff[2] << 8) |
              rcBuff[3]);
        /* take care not to sign extend values > 0x7fffffff */
        *num_sect = (int64_t)ui + 1;
        *sect_sz = (rcBuff[4] << 24) | (rcBuff[5] << 16) |
                   (rcBuff[6] << 8) | rcBuff[7];
    }
    return 0;
}

/* get_blkdev_capacity() returns 0 -> success or -1 -> failure. If
 * successful writes back sector size (logical block size) using the sect_sz
 * pointer. Also writes back the number of sectors (logical blocks) on the
 * block device using num_sect pointer. */

#ifdef SG_LIB_LINUX
static int
get_blkdev_capacity(struct opts_t * optsp, int which_arg, int64_t * num_sect,
                    int * sect_sz, int verb)
{
    int blk_fd;
    const char * fname;

    blk_fd = (DDPT_ARG_IN == which_arg) ? optsp->infd : optsp->outfd;
    fname = (DDPT_ARG_IN == which_arg) ? optsp->inf : optsp->outf;
    if (verb > 2)
        fprintf(stderr, "get_blkdev_capacity: for %s\n", fname);
    /* BLKGETSIZE64, BLKGETSIZE and BLKSSZGET macros problematic (from
     *  <linux/fs.h> or <sys/mount.h>). */
#ifdef BLKSSZGET
    if ((ioctl(blk_fd, BLKSSZGET, sect_sz) < 0) && (*sect_sz > 0)) {
        perror("BLKSSZGET ioctl error");
        return -1;
    } else {
 #ifdef BLKGETSIZE64
        uint64_t ull;

        if (ioctl(blk_fd, BLKGETSIZE64, &ull) < 0) {

            perror("BLKGETSIZE64 ioctl error");
            return -1;
        }
        *num_sect = ((int64_t)ull / (int64_t)*sect_sz);
        if (verb > 5)
            fprintf(stderr, "Used Linux BLKGETSIZE64 ioctl\n");
 #else
        unsigned long ul;

        if (ioctl(blk_fd, BLKGETSIZE, &ul) < 0) {
            perror("BLKGETSIZE ioctl error");
            return -1;
        }
        *num_sect = (int64_t)ul;
        if (verb > 5)
            fprintf(stderr, "Used Linux BLKGETSIZE ioctl\n");
 #endif
    }
    return 0;
#else
    blk_fd = blk_fd;
    if (verb)
        fprintf(stderr, "      BLKSSZGET+BLKGETSIZE ioctl not available\n");
    *num_sect = 0;
    *sect_sz = 0;
    return -1;
#endif
}
#endif

#ifdef SG_LIB_FREEBSD
static int
get_blkdev_capacity(struct opts_t * optsp, int which_arg, int64_t * num_sect,
                    int * sect_sz, int verb)
{
// Why do kernels invent their own typedefs and not use C standards?
#define u_int unsigned int
    off_t mediasize;
    unsigned int sectorsize;
    int blk_fd;
    const char * fname;

    blk_fd = (DDPT_ARG_IN == which_arg) ? optsp->infd : optsp->outfd;
    fname = (DDPT_ARG_IN == which_arg) ? optsp->inf : optsp->outf;
    if (verb > 2)
        fprintf(stderr, "get_blkdev_capacity: for %s\n", fname);

    /* For FreeBSD post suggests that /usr/sbin/diskinfo uses
     * ioctl(fd, DIOCGMEDIASIZE, &mediasize), where mediasize is an off_t.
     * also: ioctl(fd, DIOCGSECTORSIZE, &sectorsize) */
    if (ioctl(blk_fd, DIOCGSECTORSIZE, &sectorsize) < 0) {
        perror("DIOCGSECTORSIZE ioctl error");
        return -1;
    }
    *sect_sz = sectorsize;
    if (ioctl(blk_fd, DIOCGMEDIASIZE, &mediasize) < 0) {
        perror("DIOCGMEDIASIZE ioctl error");
        return -1;
    }
    if (sectorsize)
        *num_sect = mediasize / sectorsize;
    else
        *num_sect = 0;
    return 0;
}
#endif

#ifdef SG_LIB_SOLARIS
static int
get_blkdev_capacity(struct opts_t * optsp, int which_arg, int64_t * num_sect,
                    int * sect_sz, int verb)
{
    struct dk_minfo info;
    int blk_fd;
    const char * fname;

    blk_fd = (DDPT_ARG_IN == which_arg) ? optsp->infd : optsp->outfd;
    fname = (DDPT_ARG_IN == which_arg) ? optsp->inf : optsp->outf;
    if (verb > 2)
        fprintf(stderr, "get_blkdev_capacity: for %s\n", fname);

    /* this works on "char" block devs (e.g. in /dev/rdsk) but not /dev/dsk */
    if (ioctl(blk_fd, DKIOCGMEDIAINFO , &info) < 0) {
        perror("DKIOCGMEDIAINFO ioctl error");
        *num_sect = 0;
        *sect_sz = 0;
        return -1;
    }
    *num_sect = info.dki_capacity;
    *sect_sz = info.dki_lbsize;
    return 0;
}
#endif


/* Build a SCSI READ or WRITE CDB. */
static int
pt_build_scsi_cdb(unsigned char * cdbp, int cdb_sz, unsigned int blocks,
                  int64_t start_block, int write_true, int fua, int fua_nv,
                  int dpo)
{
    int rd_opcode[] = {0x8, 0x28, 0xa8, 0x88};
    int wr_opcode[] = {0xa, 0x2a, 0xaa, 0x8a};
    int sz_ind;

    memset(cdbp, 0, cdb_sz);
    if (dpo)
        cdbp[1] |= 0x10;
    if (fua)
        cdbp[1] |= 0x8;
    if (fua_nv)
        cdbp[1] |= 0x2;

    switch (cdb_sz) {
    case 6:
        sz_ind = 0;
        cdbp[0] = (unsigned char)(write_true ? wr_opcode[sz_ind] :
                                               rd_opcode[sz_ind]);
        /* Overwrite fua, fua_nv and dpo settings, n/a for 6 byte variants */
        cdbp[1] = (unsigned char)((start_block >> 16) & 0x1f);
        cdbp[2] = (unsigned char)((start_block >> 8) & 0xff);
        cdbp[3] = (unsigned char)(start_block & 0xff);
        cdbp[4] = (256 == blocks) ? 0 : (unsigned char)blocks;
        if (blocks > 256) {
            fprintf(stderr, ME "for 6 byte commands, maximum number of "
                            "blocks is 256\n");
            return 1;
        }
        if ((start_block + blocks - 1) & (~0x1fffff)) {
            fprintf(stderr, ME "for 6 byte commands, can't address blocks"
                            " beyond %d\n", 0x1fffff);
            return 1;
        }
        if (dpo || fua) {
            fprintf(stderr, ME "for 6 byte commands, neither dpo nor fua"
                            " bits supported\n");
            return 1;
        }
        break;
    case 10:
        sz_ind = 1;
        cdbp[0] = (unsigned char)(write_true ? wr_opcode[sz_ind] :
                                               rd_opcode[sz_ind]);
        cdbp[2] = (unsigned char)((start_block >> 24) & 0xff);
        cdbp[3] = (unsigned char)((start_block >> 16) & 0xff);
        cdbp[4] = (unsigned char)((start_block >> 8) & 0xff);
        cdbp[5] = (unsigned char)(start_block & 0xff);
        cdbp[7] = (unsigned char)((blocks >> 8) & 0xff);
        cdbp[8] = (unsigned char)(blocks & 0xff);
        if (blocks & (~0xffff)) {
            fprintf(stderr, ME "for 10 byte commands, maximum number of "
                            "blocks is %d\n", 0xffff);
            return 1;
        }
        break;
    case 12:
        sz_ind = 2;
        cdbp[0] = (unsigned char)(write_true ? wr_opcode[sz_ind] :
                                               rd_opcode[sz_ind]);
        cdbp[2] = (unsigned char)((start_block >> 24) & 0xff);
        cdbp[3] = (unsigned char)((start_block >> 16) & 0xff);
        cdbp[4] = (unsigned char)((start_block >> 8) & 0xff);
        cdbp[5] = (unsigned char)(start_block & 0xff);
        cdbp[6] = (unsigned char)((blocks >> 24) & 0xff);
        cdbp[7] = (unsigned char)((blocks >> 16) & 0xff);
        cdbp[8] = (unsigned char)((blocks >> 8) & 0xff);
        cdbp[9] = (unsigned char)(blocks & 0xff);
        break;
    case 16:
        sz_ind = 3;
        cdbp[0] = (unsigned char)(write_true ? wr_opcode[sz_ind] :
                                               rd_opcode[sz_ind]);
        cdbp[2] = (unsigned char)((start_block >> 56) & 0xff);
        cdbp[3] = (unsigned char)((start_block >> 48) & 0xff);
        cdbp[4] = (unsigned char)((start_block >> 40) & 0xff);
        cdbp[5] = (unsigned char)((start_block >> 32) & 0xff);
        cdbp[6] = (unsigned char)((start_block >> 24) & 0xff);
        cdbp[7] = (unsigned char)((start_block >> 16) & 0xff);
        cdbp[8] = (unsigned char)((start_block >> 8) & 0xff);
        cdbp[9] = (unsigned char)(start_block & 0xff);
        cdbp[10] = (unsigned char)((blocks >> 24) & 0xff);
        cdbp[11] = (unsigned char)((blocks >> 16) & 0xff);
        cdbp[12] = (unsigned char)((blocks >> 8) & 0xff);
        cdbp[13] = (unsigned char)(blocks & 0xff);
        break;
    default:
        fprintf(stderr, ME "expected cdb size of 6, 10, 12, or 16 but got"
                        " %d\n", cdb_sz);
        return 1;
    }
    return 0;
}

/* Read using the pass-through. No retries or remedial work here.
 * 0 -> successful, SG_LIB_SYNTAX_ERROR -> unable to build cdb,
 * SG_LIB_CAT_UNIT_ATTENTION -> try again,
 * SG_LIB_CAT_MEDIUM_HARD_WITH_INFO -> 'io_addrp' written to,
 * SG_LIB_CAT_MEDIUM_HARD -> no info field,
 * SG_LIB_CAT_NOT_READY, SG_LIB_CAT_ABORTED_COMMAND,
 * -2 -> ENOMEM
 * -1 other errors */
static int
pt_low_read(int sg_fd, int in0_out1, unsigned char * buff, int blocks,
            int64_t from_block, int bs, const struct flags_t * ifp,
            uint64_t * io_addrp)
{
    unsigned char rdCmd[MAX_SCSI_CDBSZ];
    unsigned char sense_b[SENSE_BUFF_LEN];
    int res, k, info_valid, slen, sense_cat, ret, vt;
    struct sg_pt_base * ptvp = (in0_out1 ? of_ptvp : if_ptvp);

    if (pt_build_scsi_cdb(rdCmd, ifp->cdbsz, blocks, from_block, 0,
                          ifp->fua, ifp->fua_nv, ifp->dpo)) {
        fprintf(stderr, ME "bad rd cdb build, from_block=%"PRId64", "
                "blocks=%d\n", from_block, blocks);
        return SG_LIB_SYNTAX_ERROR;
    }
    if (verbose > 2) {
        fprintf(stderr, "    READ cdb: ");
        for (k = 0; k < ifp->cdbsz; ++k)
            fprintf(stderr, "%02x ", rdCmd[k]);
        fprintf(stderr, "\n");
    }

    if (NULL == sg_warnings_strm)
        sg_warnings_strm = stderr;

    if (NULL == ptvp) {
        fprintf(stderr, "pt_low_read: if_ptvp NULL?\n");
        return -1;
    }
    clear_scsi_pt_obj(ptvp);
    set_scsi_pt_cdb(ptvp, rdCmd, ifp->cdbsz);
    set_scsi_pt_sense(ptvp, sense_b, sizeof(sense_b));
    set_scsi_pt_data_in(ptvp, buff, bs * blocks);
#ifdef SCSI_PT_FLAGS_FUNCTION
    set_scsi_pt_flags(ptvp, SCSI_PT_FLAGS_QUEUE_AT_TAIL);
#endif
    vt = (verbose ? (verbose - 1) : 0);
    while (((res = do_scsi_pt(ptvp, sg_fd, DEF_TIMEOUT, vt)) < 0) &&
           (-EINTR == res))
        ++interrupted_retries;     /* resubmit if interrupted system call */

    vt = ((verbose > 1) ? (verbose - 1) : verbose);
    ret = sg_cmds_process_resp(ptvp, "READ", res, bs * blocks, sense_b,
                               0 /* noisy */, vt, &sense_cat);
    if (-1 == ret)
        ;
    else if (-2 == ret) {
        slen = get_scsi_pt_sense_len(ptvp);
        ret = sense_cat;

        switch (sense_cat) {
        case SG_LIB_CAT_NOT_READY:
        case SG_LIB_CAT_INVALID_OP:
            ++unrecovered_errs;
            break;
        case SG_LIB_CAT_UNIT_ATTENTION:
        case SG_LIB_CAT_ABORTED_COMMAND:
            break;
        case SG_LIB_CAT_RECOVERED:
            ++recovered_errs;
            info_valid = sg_get_sense_info_fld(sense_b, slen, io_addrp);
            if (info_valid)
                fprintf(stderr, "    lba of last recovered error in this "
                        "READ=0x%"PRIx64"\n", *io_addrp);
            else
                fprintf(stderr, "Recovered error: [no info] reading from "
                        "block=0x%"PRIx64", num=%d\n", from_block, blocks);
            break;
        case SG_LIB_CAT_MEDIUM_HARD:
            ++unrecovered_errs;
            info_valid = sg_get_sense_info_fld(sense_b, slen, io_addrp);
            /* MMC devices don't necessarily set VALID bit */
            if ((info_valid) || ((5 == ifp->pdt) && (*io_addrp > 0)))
                ret = SG_LIB_CAT_MEDIUM_HARD_WITH_INFO;
            else
                fprintf(stderr, "Medium, hardware or blank check error but "
                        "no lba of failure in sense\n");
            break;
        case SG_LIB_CAT_NO_SENSE:
            ret = 0;
            break;
        case SG_LIB_CAT_ILLEGAL_REQ:
            if (5 == ifp->pdt) {    /* MMC READs can go down this path */
                struct sg_scsi_sense_hdr ssh;
                int ili;

                if (sg_scsi_normalize_sense(sense_b, slen, &ssh) &&
                    (0x64 == ssh.asc) && (0x0 == ssh.ascq)) {
                    if (sg_get_sense_filemark_eom_ili(sense_b, slen, NULL,
                                                      NULL, &ili) && ili) {
                        info_valid = sg_get_sense_info_fld(sense_b, slen,
                                                           io_addrp);
                        if (*io_addrp > 0) {
                            ++unrecovered_errs;
                            ret = SG_LIB_CAT_MEDIUM_HARD_WITH_INFO;
                        } else
                            fprintf(stderr, "MMC READ gave 'illegal mode for "
                                    "this track' and ILI but no LBA of failure\n");
                    }
                    ++unrecovered_errs;
                    ret = SG_LIB_CAT_MEDIUM_HARD;
                }
            }
        default:
            break;
        }
    } else
        ret = 0;

    sum_of_resids += get_scsi_pt_resid(ptvp);
    return ret;
}

/* Control pass-through read retries and coe (continue on error).
 * 0 -> successful, SG_LIB_SYNTAX_ERROR -> unable to build cdb,
 * SG_LIB_CAT_UNIT_ATTENTION -> try again, SG_LIB_CAT_NOT_READY,
 * SG_LIB_CAT_MEDIUM_HARD, SG_LIB_CAT_ABORTED_COMMAND,
 * -2 -> ENOMEM, -1 other errors */
static int
pt_read(int sg_fd, int in0_out1, unsigned char * buff, int blocks,
        int64_t from_block, int bs, struct flags_t * ifp, int * blks_readp)
{
    uint64_t io_addr;
    int64_t lba;
    int res, blks, use_io_addr, xferred;
    unsigned char * bp;
    int retries_tmp;
    int ret = 0;
    int may_coe = 0;

    retries_tmp = ifp->retries;
    for (xferred = 0, blks = blocks, lba = from_block, bp = buff;
         blks > 0; blks = blocks - xferred) {
        io_addr = 0;
        use_io_addr = 0;
        may_coe = 0;
        res = pt_low_read(sg_fd, in0_out1, bp, blks, lba, bs, ifp, &io_addr);
        switch (res) {
        case 0:         /* this is the fast path after good pt_low_read() */
            if (blks_readp)
                *blks_readp = xferred + blks;
            if (coe_limit > 0)
                coe_count = 0;  /* good read clears coe_count */
            return 0;
        case -2:        /* ENOMEM */
            return res;
        case SG_LIB_CAT_NOT_READY:
            fprintf(stderr, "Device (r) not ready\n");
            return res;
        case SG_LIB_CAT_ABORTED_COMMAND:
            if (--max_aborted > 0)
                fprintf(stderr, "Aborted command, continuing (r)\n");
            else {
                fprintf(stderr, "Aborted command, too many (r)\n");
                return res;
            }
            break;
        case SG_LIB_CAT_UNIT_ATTENTION:
            if (--max_uas > 0)
                fprintf(stderr, "Unit attention, continuing (r)\n");
            else {
                fprintf(stderr, "Unit attention, too many (r)\n");
                return res;
            }
            break;
        case SG_LIB_CAT_MEDIUM_HARD_WITH_INFO:
            if (retries_tmp > 0) {
                fprintf(stderr, ">>> retrying pt read: starting "
                        "lba=%"PRId64" [0x%"PRIx64"] blocks=%d\n", lba,
                        (uint64_t)lba, blks);
                --retries_tmp;
                ++num_retries;
                if (unrecovered_errs > 0)
                    --unrecovered_errs;
            } else
                use_io_addr = 1;
            ret = SG_LIB_CAT_MEDIUM_HARD;
            break; /* unrecovered read error at lba=io_addr */
        case SG_LIB_SYNTAX_ERROR:
            ifp->coe = 0;
            ret = res;
            goto err_out;
        case -1:
            ret = res;
            goto err_out;
        case SG_LIB_CAT_MEDIUM_HARD:
            may_coe = 1;
            /* fall through */
        default:
            if (retries_tmp > 0) {
                fprintf(stderr, ">>> retrying pt read: starting "
                        "lba=%"PRId64" [0x%"PRIx64"] blocks=%d\n", lba,
                        (uint64_t)lba, blks);
                --retries_tmp;
                ++num_retries;
                if (unrecovered_errs > 0)
                    --unrecovered_errs;
                break;
            }
            ret = res;
            goto err_out;
        }
        if (! use_io_addr)
            continue;
        if ((io_addr < (uint64_t)lba) ||
            (io_addr >= (uint64_t)(lba + blks))) {
                fprintf(stderr, "  Unrecovered error lba 0x%"PRIx64" not in "
                    "correct range:\n\t[0x%"PRIx64",0x%"PRIx64"]\n", io_addr,
                    (uint64_t)lba,
                    (uint64_t)(lba + blks - 1));
            may_coe = 1;
            goto err_out;
        }
        if (highest_unrecovered < 0) {
            highest_unrecovered = io_addr;
            lowest_unrecovered = io_addr;
        } else {
            if ((int64_t)io_addr < lowest_unrecovered)
                lowest_unrecovered = io_addr;
            if ((int64_t)io_addr > highest_unrecovered)
                highest_unrecovered = io_addr;
        }
        blks = (int)(io_addr - (uint64_t)lba);
        if (blks > 0) {
            if (verbose)
                fprintf(stderr, "  partial read of %d blocks prior to "
                        "medium error\n", blks);
            res = pt_low_read(sg_fd, in0_out1, bp, blks, lba, bs, ifp,
                              &io_addr);
            switch (res) {
            case 0:
                break;
            case -1:
                ifp->coe = 0;
                ret = res;
                goto err_out;
            case -2:
                fprintf(stderr, "ENOMEM again, unexpected (r)\n");
                return -1;
            case SG_LIB_CAT_NOT_READY:
                fprintf(stderr, "device (r) not ready\n");
                return res;
            case SG_LIB_CAT_UNIT_ATTENTION:
                fprintf(stderr, "Unit attention, unexpected (r)\n");
                return res;
            case SG_LIB_CAT_ABORTED_COMMAND:
                fprintf(stderr, "Aborted command, unexpected (r)\n");
                return res;
            case SG_LIB_CAT_MEDIUM_HARD_WITH_INFO:
            case SG_LIB_CAT_MEDIUM_HARD:
                ret = SG_LIB_CAT_MEDIUM_HARD;
                goto err_out;
            case SG_LIB_SYNTAX_ERROR:
            default:
                fprintf(stderr, ">> unexpected result=%d from "
                        "pt_low_read() 2\n", res);
                ret = res;
                goto err_out;
            }
        }
        xferred += blks;
        if (0 == ifp->coe) {
            /* give up at block before problem unless 'coe' */
            if (blks_readp)
                *blks_readp = xferred;
            return ret;
        }
        bp += (blks * bs);
        lba += blks;
        fprintf(stderr, ">> unrecovered read error at blk=%"PRId64", "
                "substitute zeros\n", lba);
        memset(bp, 0, bs);
        ++xferred;
        bp += bs;
        ++lba;
        if ((coe_limit > 0) && (++coe_count > coe_limit)) {
            if (blks_readp)
                *blks_readp = xferred + blks;
            fprintf(stderr, ">> coe_limit on consecutive reads exceeded\n");
            return SG_LIB_CAT_MEDIUM_HARD;
        }
        retries_tmp = ifp->retries;
    }
    if (blks_readp)
        *blks_readp = xferred;
    return 0;

err_out:
    if (ifp->coe) {
        memset(bp, 0, bs * blks);
        fprintf(stderr, ">> unable to read at blk=%"PRId64" for "
                "%d bytes, use zeros\n", lba, bs * blks);
        if (blks > 1)
            fprintf(stderr, ">>   try reducing bpt to limit number "
                    "of zeros written near bad block(s)\n");
        /* fudge success */
        if (blks_readp)
            *blks_readp = xferred + blks;
        if ((coe_limit > 0) && (++coe_count > coe_limit)) {
            fprintf(stderr, ">> coe_limit on consecutive reads exceeded\n");
            return ret;
        }
        return may_coe ? 0 : ret;
    } else
        return ret ? ret : -1;
}


/* Write block(s) via the pass-through.
 * 0 -> successful, SG_LIB_SYNTAX_ERROR -> unable to build cdb,
 * SG_LIB_CAT_NOT_READY, SG_LIB_CAT_UNIT_ATTENTION, SG_LIB_CAT_MEDIUM_HARD,
 * SG_LIB_CAT_ABORTED_COMMAND, -2 -> recoverable (ENOMEM),
 * -1 -> unrecoverable error + others */
static int
pt_low_write(int sg_fd, unsigned char * buff, int blocks, int64_t to_block,
             int bs, const struct flags_t * ofp)
{
    unsigned char wrCmd[MAX_SCSI_CDBSZ];
    unsigned char sense_b[SENSE_BUFF_LEN];
    int res, k, info_valid, ret, sense_cat, slen, vt;
    uint64_t io_addr = 0;
    struct sg_pt_base * ptvp = of_ptvp;

    if (pt_build_scsi_cdb(wrCmd, ofp->cdbsz, blocks, to_block, 1, ofp->fua,
                          ofp->fua_nv, ofp->dpo)) {
        fprintf(stderr, ME "bad wr cdb build, to_block=%"PRId64", blocks=%d\n",
                to_block, blocks);
        return SG_LIB_SYNTAX_ERROR;
    }
    if (verbose > 2) {
        fprintf(stderr, "    WRITE cdb: ");
        for (k = 0; k < ofp->cdbsz; ++k)
            fprintf(stderr, "%02x ", wrCmd[k]);
        fprintf(stderr, "\n");
    }

    if (NULL == sg_warnings_strm)
        sg_warnings_strm = stderr;

    if (NULL == ptvp) {
        fprintf(stderr, "pt_low_write: of_ptvp NULL?\n");
        return -1;
    }
    clear_scsi_pt_obj(ptvp);
    set_scsi_pt_cdb(ptvp, wrCmd, ofp->cdbsz);
    set_scsi_pt_sense(ptvp, sense_b, sizeof(sense_b));
    set_scsi_pt_data_out(ptvp, buff, bs * blocks);
    vt = (verbose ? (verbose - 1) : 0);
    while (((res = do_scsi_pt(ptvp, sg_fd, DEF_TIMEOUT, vt)) < 0) &&
           (-EINTR == res))
        ++interrupted_retries;     /* resubmit if interrupted system call */

    vt = ((verbose > 1) ? (verbose - 1) : verbose);
    ret = sg_cmds_process_resp(ptvp, "WRITE", res, bs * blocks, sense_b,
                               0 /* noisy */, vt, &sense_cat);
    if (-1 == ret)
        ;
    else if (-2 == ret) {
        slen = get_scsi_pt_sense_len(ptvp);
        ret = sense_cat;

        switch (sense_cat) {
        case SG_LIB_CAT_RECOVERED:
            ++recovered_errs;
            info_valid = sg_get_sense_info_fld(sense_b, slen, &io_addr);
            if (info_valid)
                fprintf(stderr, "    lba of last recovered error in this "
                        "WRITE=0x%"PRIx64"\n", io_addr);
            else
                fprintf(stderr, "Recovered error: [no info] writing to "
                        "block=0x%"PRIx64", num=%d\n", to_block, blocks);
            break;
        case SG_LIB_CAT_ABORTED_COMMAND:
        case SG_LIB_CAT_UNIT_ATTENTION:
            break;
        case SG_LIB_CAT_NOT_READY:
            ++unrecovered_errs;
            break;
        case SG_LIB_CAT_MEDIUM_HARD:
        default:
            ++unrecovered_errs;
            if (ofp->coe) {
                fprintf(stderr, ">> ignored errors for out blk=%"PRId64" for "
                        "%d bytes\n", to_block, bs * blocks);
                ret = 0; /* fudge success */
            }
            break;
        }
    } else
        ret = 0;

    return ret;
}


/* Control pass-through write retries.
 * 0 -> successful, SG_LIB_SYNTAX_ERROR -> unable to build cdb,
 * SG_LIB_CAT_UNIT_ATTENTION -> try again, SG_LIB_CAT_NOT_READY,
 * SG_LIB_CAT_MEDIUM_HARD, SG_LIB_CAT_ABORTED_COMMAND,
 * -2 -> ENOMEM, -1 other errors */
static int
pt_write(int sg_fd, unsigned char * buff, int blocks, int64_t to_block,
         int bs, struct flags_t * ofp)
{
    int retries_tmp;
    int first = 1;
    int ret = 0;

    retries_tmp = ofp->retries;
    while (1) {
        ret = pt_low_write(sg_fd, buff, blocks, to_block, bs, ofp);
        if (0 == ret)
            break;
        if ((SG_LIB_CAT_NOT_READY == ret) ||
            (SG_LIB_SYNTAX_ERROR == ret))
            break;
        else if ((SG_LIB_CAT_UNIT_ATTENTION == ret) && first) {
            if (--max_uas > 0)
                fprintf(stderr, "Unit attention, continuing (w)\n");
            else {
                fprintf(stderr, "Unit attention, too many (w)\n");
                break;
            }
        } else if ((SG_LIB_CAT_ABORTED_COMMAND == ret) && first) {
            if (--max_aborted > 0)
                fprintf(stderr, "Aborted command, continuing (w)\n");
            else {
                fprintf(stderr, "Aborted command, too many (w)\n");
                break;
            }
        } else if (ret < 0)
            break;
        else if (retries_tmp > 0) {
            fprintf(stderr, ">>> retrying pt write: starting lba=%"PRId64" "
                    "[0x%"PRIx64"] blocks=%d\n", to_block,
                    (uint64_t)to_block, blocks);
            --retries_tmp;
            ++num_retries;
            if (unrecovered_errs > 0)
                --unrecovered_errs;
        } else
            break;
        first = 0;
    }
    return ret;
}

static int
pt_write_same16(int sg_fd, int in0_out1, unsigned char * buff, int bs,
                int blocks, int64_t start_block)
{
    int k, ret, res, sense_cat, vt;
    uint64_t llba;
    uint32_t unum;
    unsigned char wsCmdBlk[16];
    unsigned char sense_b[SENSE_BUFF_LEN];
    struct sg_pt_base * ptvp = in0_out1 ? of_ptvp : if_ptvp;

    memset(wsCmdBlk, 0, sizeof(wsCmdBlk));
    wsCmdBlk[0] = 0x93;         /* WRITE SAME(16) opcode */
    /* set UNMAP; clear wrprotect, anchor, pbdata, lbdata */
    wsCmdBlk[1] = 0x8;
    llba = start_block;
    for (k = 7; k >= 0; --k) {
        wsCmdBlk[2 + k] = (llba & 0xff);
        llba >>= 8;
    }
    unum = blocks;
    for (k = 3; k >= 0; --k) {
        wsCmdBlk[10 + k] = (unum & 0xff);
        unum >>= 8;
    }
    if (verbose > 2) {
        fprintf(stderr, "    WRITE SAME(16) cdb: ");
        for (k = 0; k < (int)sizeof(wsCmdBlk); ++k)
            fprintf(stderr, "%02x ", wsCmdBlk[k]);
        fprintf(stderr, "\n");
        if (verbose > 4)
            fprintf(stderr, "    Data-out buffer length=%d\n",
                    bs);
    }
    if (NULL == sg_warnings_strm)
        sg_warnings_strm = stderr;

    if (NULL == ptvp) {
        fprintf(stderr, "pt_write_same16: ptvp NULL?\n");
        return -1;
    }
    clear_scsi_pt_obj(ptvp);
    set_scsi_pt_cdb(ptvp, wsCmdBlk, sizeof(wsCmdBlk));
    set_scsi_pt_sense(ptvp, sense_b, sizeof(sense_b));
    set_scsi_pt_data_out(ptvp, buff, bs);
    vt = ((verbose > 1) ? (verbose - 1) : 0);
    while (((res = do_scsi_pt(ptvp, sg_fd, WRITE_SAME16_TIMEOUT, vt)) < 0) &&
           (-EINTR == res))
        ++interrupted_retries;     /* resubmit if interrupted system call */
    ret = sg_cmds_process_resp(ptvp, "Write same(16)", res, 0, sense_b,
                               1 /*noisy */, vt, &sense_cat);
    if (-1 == ret)
        ;
    else if (-2 == ret) {
        switch (sense_cat) {
        case SG_LIB_CAT_NOT_READY:
        case SG_LIB_CAT_UNIT_ATTENTION:
        case SG_LIB_CAT_INVALID_OP:
        case SG_LIB_CAT_ILLEGAL_REQ:
        case SG_LIB_CAT_ABORTED_COMMAND:
            ret = sense_cat;
            break;
        case SG_LIB_CAT_RECOVERED:
        case SG_LIB_CAT_NO_SENSE:
            ret = 0;
            break;
        case SG_LIB_CAT_MEDIUM_HARD:
            {
                int valid, slen;
                uint64_t ull = 0;

                slen = get_scsi_pt_sense_len(ptvp);
                valid = sg_get_sense_info_fld(sense_b, slen, &ull);
                if (valid)
                    fprintf(stderr, "Medium or hardware error starting at "
                            "lba=%"PRIu64" [0x%"PRIx64"]\n", ull, ull);
            }
            ret = sense_cat;
            break;
        default:
            ret = -1;
            break;
        }
    } else
        ret = 0;

    return ret;
}

/* Print number of blocks, block size and size in GB (10**9 bytes) (if large
 * enough) to stderr. */
static void
print_blk_sizes(const char * fname, const char * access_typ, int64_t num_sect,
                int sect_sz)
{
    int sect_per_gb, gb, mib;
    int64_t n;

    gb = 0;
    if ((num_sect <= 0) || (sect_sz <= 0))
        goto cont;
    sect_per_gb = 1073741824 / sect_sz;
    if ((1073741824 != (sect_per_gb * sect_sz)) || (sect_per_gb <= 0))
        goto cont;
    n = num_sect / sect_per_gb;
    n = (n * 43) / 40;  /* rough correction for decimal gigabyte */
    gb = ((n > INT_MAX) || (n < 0)) ? INT_MAX : (int)n;

cont:
    if (gb)
        fprintf(stderr, "%s [%s]: blocks=%"PRId64" [0x%"PRIx64"], "
                "block_size=%d, %d GB approx\n", fname, access_typ, num_sect,
                num_sect, sect_sz, gb);
    else if ((num_sect > 0) && (sect_sz > 0)) {
        mib = (int)num_sect * sect_sz / 1048576;
        fprintf(stderr, "%s [%s]: blocks=%"PRId64" [0x%"PRIx64"], "
                "block_size=%d, %d MiB approx\n", fname, access_typ, num_sect,
                num_sect, sect_sz, mib);
    } else
        fprintf(stderr, "%s [%s]: blocks=%"PRId64" [0x%"PRIx64"], "
                "block_size=%d\n", fname, access_typ, num_sect, num_sect,
                sect_sz);
}

static void
calc_duration_throughput(int contin)
{
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    struct timespec end_tm, res_tm;
    double a, b;
    int64_t blks;

    if (start_tm_valid && (start_tm.tv_sec || start_tm.tv_nsec)) {
        // blks = (in_full > out_full) ? in_full : out_full;
        blks = in_full;
        clock_gettimeofday(CLOCK_MONOTONIC, &end_tm);
        res_tm.tv_sec = end_tm.tv_sec - start_tm.tv_sec;
        res_tm.tv_nsec = end_tm.tv_nsec - start_tm.tv_nsec;
        if (res_tm.tv_nsec < 0) {
            --res_tm.tv_sec;
            res_tm.tv_nsec += 1000000000;
        }
        a = res_tm.tv_sec;
        a += (0.000001 * (res_tm.tv_nsec / 1000));
        b = (double)ibs_hold * blks;
        fprintf(stderr, "time to transfer data%s: %d.%06d secs",
                (contin ? " so far" : ""), (int)res_tm.tv_sec,
                (int)(res_tm.tv_nsec / 1000));
        if ((a > 0.00001) && (b > 511))
            fprintf(stderr, " at %.2f MB/sec\n", b / (a * 1000000.0));
        else
            fprintf(stderr, "\n");
    }
#elif defined(HAVE_GETTIMEOFDAY)
    struct timeval end_tm, res_tm;
    double a, b;
    int64_t blks;

    if (start_tm_valid && (start_tm.tv_sec || start_tm.tv_usec)) {
        // blks = (in_full > out_full) ? in_full : out_full;
        blks = in_full;
        gettimeofday(&end_tm, NULL);
        res_tm.tv_sec = end_tm.tv_sec - start_tm.tv_sec;
        res_tm.tv_usec = end_tm.tv_usec - start_tm.tv_usec;
        if (res_tm.tv_usec < 0) {
            --res_tm.tv_sec;
            res_tm.tv_usec += 1000000;
        }
        a = res_tm.tv_sec;
        a += (0.000001 * res_tm.tv_usec);
        b = (double)ibs_hold * blks;
        fprintf(stderr, "time to transfer data%s: %d.%06d secs",
                (contin ? " so far" : ""), (int)res_tm.tv_sec,
                (int)res_tm.tv_usec);
        if ((a > 0.00001) && (b > 511))
            fprintf(stderr, " at %.2f MB/sec\n", b / (a * 1000000.0));
        else
            fprintf(stderr, "\n");
    }
#else
    contin = contin;    // suppress warning
#endif
}

/* Returns open input file descriptor (>= 0) or a negative value
 * (-SG_LIB_FILE_ERROR or -SG_LIB_CAT_OTHER) if error.
 */
static int
open_if(struct opts_t * optsp, int verbose)
{
    int flags, fl, verb;
    int fd = -SG_LIB_FILE_ERROR;
    char ebuff[EBUFF_SZ];
    struct sg_simple_inquiry_resp sir;
    struct flags_t * ifp = optsp->iflagp;
    const char * inf = optsp->inf;

    verb = (verbose ? verbose - 1: 0);
    optsp->in_type = dd_filetype(inf);
    if (verbose)
        fprintf(stderr, " >> Input file type: %s\n",
                dd_filetype_str(optsp->in_type, ebuff));
    if (FT_ERROR & optsp->in_type) {
        fprintf(stderr, ME "unable to access %s\n", inf);
        goto file_err;
    } else if (((FT_BLOCK | FT_OTHER) & optsp->in_type) && ifp->pt)
        optsp->in_type |= FT_PT;

    if (FT_TAPE & optsp->in_type) {
        fprintf(stderr, ME "unable to use scsi tape device %s\n", inf);
        goto file_err;
    } else if (FT_PT & optsp->in_type) {
        flags = O_NONBLOCK;
        if (ifp->direct)
            flags |= O_DIRECT;
        if (ifp->excl)
            flags |= O_EXCL;
        if (ifp->sync)
            flags |= O_SYNC;
        fl = O_RDWR;
        if ((fd = scsi_pt_open_flags(inf, (fl | flags), verbose)) < 0) {
            fl = O_RDONLY;
            if ((fd = scsi_pt_open_flags(inf, (fl | flags), verbose)) < 0) {
                fprintf(stderr, "could not open %s for pt reading: %s\n", inf,
                        safe_strerror(-fd));
                goto file_err;
            }
        }
        if (sg_simple_inquiry(fd, &sir, 0, verb)) {
            fprintf(stderr, "INQUIRY failed on %s\n", inf);
            goto other_err;
        }
        ifp->pdt = sir.peripheral_type;
        if (verbose)
            fprintf(stderr, "    %s: %.8s  %.16s  %.4s  [pdt=%d]\n",
                    inf, sir.vendor, sir.product, sir.revision, ifp->pdt);
    }
#ifdef SG_LIB_WIN32
    else if (FT_BLOCK & optsp->in_type) {
        if (win32_open_if(optsp, verbose))
            goto file_err;
        fd = 0;
    }
#endif
    else {
        flags = O_RDONLY;
        if (ifp->direct)
            flags |= O_DIRECT;
        if (ifp->excl)
            flags |= O_EXCL;
        if (ifp->sync)
            flags |= O_SYNC;
        fd = open(inf, flags);
        if (fd < 0) {
            snprintf(ebuff, EBUFF_SZ,
                     ME "could not open %s for reading", inf);
            perror(ebuff);
            goto file_err;
        } else {
            if (sg_set_binary_mode(fd) < 0)
                perror("sg_set_binary_mode");
            if (verbose)
                fprintf(stderr, "        open input, flags=0x%x\n",
                        flags);
#ifdef HAVE_POSIX_FADVISE
            if (ifp->nocache) {
                int rt;

                rt = posix_fadvise(fd, 0, 0, POSIX_FADV_SEQUENTIAL);
                if (rt)
                    fprintf(stderr, "open_if: posix_fadvise(SEQUENTIAL), "
                            "err=%d\n", rt);
            }
#endif
        }
    }
#ifdef SG_LIB_LINUX
    if (ifp->flock) {
        int res;

        res = flock(fd, LOCK_EX | LOCK_NB);
        if (res < 0) {
            close(fd);
            snprintf(ebuff, EBUFF_SZ, ME "flock(LOCK_EX | LOCK_NB) on %s "
                     "failed", inf);
            perror(ebuff);
            return -SG_LIB_FLOCK_ERR;
        }
    }
#endif
    return fd;

file_err:
    return -SG_LIB_FILE_ERROR;
other_err:
    return -SG_LIB_CAT_OTHER;
}

/* Returns open output file descriptor (>= 0), -1 for don't
 * bother opening (e.g. /dev/null), or a more negative value
 * (-SG_LIB_FILE_ERROR or -SG_LIB_CAT_OTHER) if error.
 */
static int
open_of(struct opts_t * optsp, int verbose)
{
    int flags, verb;
    int fd = -SG_LIB_FILE_ERROR;
    int outf_exists = 0;
    char ebuff[EBUFF_SZ];
    struct sg_simple_inquiry_resp sir;
    struct stat st;
    struct flags_t * ofp = optsp->oflagp;
    const char * outf = optsp->outf;

    verb = (verbose ? verbose - 1: 0);
    optsp->out_type = dd_filetype(outf);
    if (verbose)
        fprintf(stderr, " >> Output file type: %s\n",
                dd_filetype_str(optsp->out_type, ebuff));

    if (((FT_BLOCK | FT_OTHER) & optsp->out_type) && ofp->pt)
        optsp->out_type |= FT_PT;

    if (FT_TAPE & optsp->out_type) {
        fprintf(stderr, ME "unable to use scsi tape device %s\n", outf);
        goto file_err;
    } else if (FT_PT & optsp->out_type) {
        flags = O_RDWR | O_NONBLOCK;
        if (ofp->direct)
            flags |= O_DIRECT;
        if (ofp->excl)
            flags |= O_EXCL;
        if (ofp->sync)
            flags |= O_SYNC;
        if ((fd = scsi_pt_open_flags(outf, flags, verbose)) < 0) {
            fprintf(stderr, "could not open %s for pt writing: %s\n",
                    outf, safe_strerror(-fd));
            goto file_err;
        }
        if (sg_simple_inquiry(fd, &sir, 0, verb)) {
            fprintf(stderr, "INQUIRY failed on %s\n", outf);
            goto other_err;
        }
        ofp->pdt = sir.peripheral_type;
        if (verbose)
            fprintf(stderr, "    %s: %.8s  %.16s  %.4s  [pdt=%d]\n",
                    outf, sir.vendor, sir.product, sir.revision, ofp->pdt);
    } else if (FT_DEV_NULL & optsp->out_type) {
        fd = -1; /* don't bother opening */
    }
#ifdef SG_LIB_WIN32
    else if (FT_BLOCK & optsp->out_type) {
        if (win32_open_of(optsp, verbose))
            goto file_err;
        fd = 0;
    }
#endif
    else {      /* typically regular file or block device node */
        if (0 == stat(outf, &st))
            outf_exists = 1;
        flags = ofp->sparing ? O_RDWR : O_WRONLY;
        if (0 == outf_exists)
            flags |= O_CREAT;
        if (ofp->direct)
            flags |= O_DIRECT;
        if (ofp->excl)
            flags |= O_EXCL;
        if (ofp->sync)
            flags |= O_SYNC;
        if (ofp->append)
            flags |= O_APPEND;
        if ((FT_REG & optsp->out_type) && outf_exists && ofp->trunc &&
            (! ofp->nowrite))
            flags |= O_TRUNC;
        if ((fd = open(outf, flags, 0666)) < 0) {
            snprintf(ebuff, EBUFF_SZ,
                    ME "could not open %s for writing", outf);
            perror(ebuff);
            goto file_err;
        }
        if (sg_set_binary_mode(fd) < 0)
            perror("sg_set_binary_mode");
        if (verbose)
            fprintf(stderr, "        %s output, flags=0x%x\n",
                    (outf_exists ? "open" : "create"), flags);
    }
#ifdef SG_LIB_LINUX
    if (ofp->flock) {
        int res;

        res = flock(fd, LOCK_EX | LOCK_NB);
        if (res < 0) {
            close(fd);
            snprintf(ebuff, EBUFF_SZ, ME "flock(LOCK_EX | LOCK_NB) on %s "
                     "failed", outf);
            perror(ebuff);
            return -SG_LIB_FLOCK_ERR;
        }
    }
#endif
    return fd;

file_err:
    return -SG_LIB_FILE_ERROR;
other_err:
    return -SG_LIB_CAT_OTHER;
}

/* Calculates the number of blocks associated with the in and out files.
 * May also yield the block size in bytes of devices. For regular files
 * uses ibs or obs as the block (sector) size. */
static void
calc_count(struct opts_t * optsp, int64_t * in_num_sectp, int * in_sect_szp,
           int64_t * out_num_sectp, int * out_sect_szp)
{
    int res;
    struct stat st;

    if (verbose > 4)
        fprintf(stderr, "calc_count: enter, in: %s, out: %s\n", optsp->inf,
                optsp->outf);
    *in_num_sectp = -1;
    *in_sect_szp = -1;
    if (FT_PT & optsp->in_type) {
        res = scsi_read_capacity(optsp->infd, in_num_sectp, in_sect_szp);
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            fprintf(stderr, "Unit attention (readcap in), continuing\n");
            res = scsi_read_capacity(optsp->infd, in_num_sectp, in_sect_szp);
        } else if (SG_LIB_CAT_ABORTED_COMMAND == res) {
            fprintf(stderr, "Aborted command (readcap in), continuing\n");
            res = scsi_read_capacity(optsp->infd, in_num_sectp, in_sect_szp);
        }
        if (0 != res) {
            if (res == SG_LIB_CAT_INVALID_OP)
                fprintf(stderr, "read capacity not supported on %s\n",
                        optsp->inf);
            else if (res == SG_LIB_CAT_NOT_READY)
                fprintf(stderr, "read capacity failed on %s - not ready\n",
                        optsp->inf);
            else
                fprintf(stderr, "Unable to read capacity on %s\n", optsp->inf);
            *in_num_sectp = -1;
        } else {
            if (verbose)
                print_blk_sizes(optsp->inf, "pt", *in_num_sectp, *in_sect_szp);
            if (*in_sect_szp != optsp->ibs)
                fprintf(stderr, ">> warning: %s block size confusion: bs=%d, "
                        "device claims=%d\n", optsp->inf, optsp->ibs,
                        *in_sect_szp);
        }
    } else if (FT_BLOCK & optsp->in_type) {
        if (0 != get_blkdev_capacity(optsp, DDPT_ARG_IN, in_num_sectp,
                                    in_sect_szp, verbose)) {
            fprintf(stderr, "Unable to read block capacity on %s\n",
                    optsp->inf);
            *in_num_sectp = -1;
        }
        if (verbose)
            print_blk_sizes(optsp->inf, "blk", *in_num_sectp, *in_sect_szp);
        if (optsp->ibs != *in_sect_szp) {
            fprintf(stderr, ">> warning: %s block size confusion: bs=%d, "
                    "device claims=%d\n", optsp->inf, optsp->ibs,
                     *in_sect_szp);
            *in_num_sectp = -1;
        }
    } else if (FT_REG & optsp->in_type) {
        if (fstat(optsp->infd, &st) < 0) {
            perror("fstat(infd) error");
            *in_num_sectp = -1;
        } else {
            *in_num_sectp = st.st_size / optsp->ibs;
            if (0 != (st.st_size % optsp->ibs))
                ++*in_num_sectp;
        }
    }

    *out_num_sectp = -1;
    *out_sect_szp = -1;
    if (FT_PT & optsp->out_type) {
        res = scsi_read_capacity(optsp->outfd, out_num_sectp, out_sect_szp);
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            fprintf(stderr, "Unit attention (readcap out), continuing\n");
            res = scsi_read_capacity(optsp->outfd, out_num_sectp,
                                     out_sect_szp);
        } else if (SG_LIB_CAT_ABORTED_COMMAND == res) {
            fprintf(stderr, "Aborted command (readcap out), continuing\n");
            res = scsi_read_capacity(optsp->outfd, out_num_sectp,
                                     out_sect_szp);
        }
        if (0 != res) {
            if (res == SG_LIB_CAT_INVALID_OP)
                fprintf(stderr, "read capacity not supported on %s\n",
                        optsp->outf);
            else
                fprintf(stderr, "Unable to read capacity on %s\n",
                        optsp->outf);
            *out_num_sectp = -1;
        } else {
            if (verbose)
                print_blk_sizes(optsp->outf, "pt", *out_num_sectp,
                                *out_sect_szp);
            if (optsp->obs != *out_sect_szp)
                fprintf(stderr, ">> warning: %s block size confusion: "
                        "obs=%d, device claims=%d\n", optsp->outf,
                        optsp->obs, *out_sect_szp);
        }
    } else if (FT_BLOCK & optsp->out_type) {
        if (0 != get_blkdev_capacity(optsp, DDPT_ARG_OUT, out_num_sectp,
                                     out_sect_szp, verbose)) {
            fprintf(stderr, "Unable to read block capacity on %s\n",
                    optsp->outf);
            *out_num_sectp = -1;
        } else {
            if (verbose)
                print_blk_sizes(optsp->outf, "blk", *out_num_sectp,
                                *out_sect_szp);
            if (optsp->obs != *out_sect_szp) {
                fprintf(stderr, ">> warning: %s block size confusion: "
                        "obs=%d, device claims=%d\n", optsp->outf,
                        optsp->obs, *out_sect_szp);
                *out_num_sectp = -1;
            }
        }
    } else if (FT_REG & optsp->out_type) {
        if (fstat(optsp->outfd, &st) < 0) {
            perror("fstat(outfd) error");
            *out_num_sectp = -1;
        } else {
            *out_num_sectp = st.st_size / optsp->obs;
            if (0 != (st.st_size % optsp->obs))
                ++*out_num_sectp;
        }
    }
}
        
#ifdef HAVE_POSIX_FADVISE
static void
do_fadvise(struct opts_t * op, int bytes_if, int bytes_of, int bytes_of2)
{
    int rt, in_valid, out2_valid, out_valid;

    in_valid = ((FT_REG == op->in_type) || (FT_BLOCK == op->in_type));
    out2_valid = ((FT_REG == op->out2_type) || (FT_BLOCK == op->out2_type));
    out_valid = ((FT_REG == op->out_type) || (FT_BLOCK == op->out_type));
    if (op->iflagp->nocache && (bytes_if > 0) && in_valid) {
        rt = posix_fadvise(op->infd, 0, (op->skip * op->ibs) +
                                    bytes_if, POSIX_FADV_DONTNEED);
        // rt = posix_fadvise(op->infd, (op->skip * op->ibs),
                           // bytes_if, POSIX_FADV_DONTNEED);
        // rt = posix_fadvise(op->infd, 0, 0, POSIX_FADV_DONTNEED);
        if (rt)         /* returns error as result */
            fprintf(stderr, "posix_fadvise on read, skip=%"PRId64" ,err=%d\n",
                    op->skip, rt);
    }
    if ((op->oflagp->nocache & 2) && (bytes_of2 > 0) && out2_valid) {
        rt = posix_fadvise(op->out2fd, 0, 0, POSIX_FADV_DONTNEED);
        if (rt)
            fprintf(stderr, "posix_fadvise on of2, seek="
                    "%"PRId64" ,err=%d\n", op->seek, rt);
    }
    if ((op->oflagp->nocache & 1) && (bytes_of > 0) && out_valid) {
        rt = posix_fadvise(op->outfd, 0, 0, POSIX_FADV_DONTNEED);
        if (rt)
            fprintf(stderr, "posix_fadvise on output, seek=%"PRId64" , "
                    "err=%d\n", op->seek, rt);
    }
}
#endif

static int
cp_read_pt(struct opts_t * optsp, struct cp_state_t * csp,
           unsigned char * wrkPos)
{
    int res, blks_read;

    res = pt_read(optsp->infd, 0, wrkPos, csp->iblocks, optsp->skip,
                  optsp->ibs, optsp->iflagp, &blks_read);
    if (res) {
        fprintf(stderr, "pt_read failed,%s at or after lba=%"PRId64" "
                "[0x%"PRIx64"]\n", ((-2 == res) ?  " try reducing bpt," : ""),
                 optsp->skip, optsp->skip);
        return res;
    } else {
        if (blks_read < csp->iblocks) {
            dd_count = 0;   /* force exit after write */
            csp->iblocks = blks_read;
        }
        in_full += csp->iblocks;
    }
    return 0;
}

static int
cp_read_block_reg(struct opts_t * optsp, struct cp_state_t * csp,
                  unsigned char * wrkPos)
{
    int res;
    off_t offset = optsp->skip * optsp->ibs;
    int numbytes = csp->iblocks * optsp->ibs;

#ifdef SG_LIB_WIN32
    if (FT_BLOCK & optsp->in_type) {
        if (offset != csp->if_filepos) {
            if (verbose > 2)
                fprintf(stderr, "moving if filepos: new_pos="
                        "%"PRId64"\n", (int64_t)offset);
            if (win32_set_file_pos(optsp, DDPT_ARG_IN, offset, verbose))
                return SG_LIB_FILE_ERROR;
            csp->if_filepos = offset;
        }
        res = win32_block_read(optsp, wrkPos, numbytes, verbose);
        if (res < 0) {
            fprintf(stderr, ME "read(win32_block), skip=%"PRId64" ",
                    optsp->skip);
            return -1;
        } else {
            if (res < numbytes) {
                dd_count = 0;   /* force exit after write */
                csp->iblocks = res / optsp->ibs;
            }
            csp->if_filepos += res;
            in_full += csp->iblocks;
        }
    } else
#endif
    {
        char ebuff[EBUFF_SZ];

        if (offset != csp->if_filepos) {
            off_t off_res;

            if (verbose > 2)
                fprintf(stderr, "moving if filepos: new_pos="
                        "%"PRId64"\n", (int64_t)offset);
            off_res = lseek(optsp->infd, offset, SEEK_SET);
            if (off_res < 0) {
                fprintf(stderr, "failed moving if filepos: new_pos="
                        "%"PRId64"\n", (int64_t)offset);
                perror("lseek on input");
                return SG_LIB_FILE_ERROR;
            }
            csp->if_filepos = offset;
        }
        while (((res = read(optsp->infd, wrkPos, numbytes)) < 0) &&
               (EINTR == errno))
            ++interrupted_retries;
        if (verbose > 2)
            fprintf(stderr, "read(unix): requested bytes=%d, res=%d\n",
                    numbytes, res);
        if (res < 0) {
            snprintf(ebuff, EBUFF_SZ, ME "reading, skip=%"PRId64" ",
                     optsp->skip);
            perror(ebuff);
            return -1;
        } else if (res < numbytes) {
            dd_count = 0;
            csp->iblocks = res / optsp->ibs;
            if ((res % optsp->ibs) > 0) {
                ++csp->iblocks;
                ++in_partial;
            }
            csp->oblocks = res / optsp->obs;
            if ((res % optsp->obs) > 0) {
                ++csp->oblocks;
                // ++out_full;
                // ++out_partial;
            }
        }
        csp->if_filepos += res;
        csp->bytes_read = res;
        in_full += csp->iblocks;
    }
    return 0;
}

static int
cp_write_of2(struct opts_t * optsp, struct cp_state_t * csp,
             unsigned char * wrkPos)
{
    int res;
    char ebuff[EBUFF_SZ];
    int numbytes = csp->oblocks * optsp->obs;

    while (((res = write(optsp->out2fd, wrkPos, numbytes)) < 0) &&
           (EINTR == errno))
        ++interrupted_retries;
    if (verbose > 2)
        fprintf(stderr, "write to of2: count=%d, res=%d\n", numbytes, res);
    if (res < 0) {
        snprintf(ebuff, EBUFF_SZ, ME "writing to of2, seek=%"PRId64" ",
                 optsp->seek);
        perror(ebuff);
        return -1;
    }
    csp->bytes_of2 = res;
    return 0;
}

static int
cp_read_of_pt(struct opts_t * optsp, struct cp_state_t * csp,
              unsigned char * wrkPos2)
{
    int res, blks_read;

    res = pt_read(optsp->outfd, 1, wrkPos2, csp->oblocks, optsp->seek,
                  optsp->obs, optsp->oflagp, &blks_read);
    if (res) {
        fprintf(stderr, "pt_read(sparing) failed, at or after "
                "lba=%"PRId64" [0x%"PRIx64"]\n", optsp->seek,
                optsp->seek);
        return res;
    } else if (blks_read != csp->oblocks)
        return 1;
    return 0;
}

static int
cp_read_of_block_reg(struct opts_t * optsp, struct cp_state_t * csp,
                     unsigned char * wrkPos2)
{
    int res;
    off_t offset = optsp->seek * optsp->obs;
    int numbytes = csp->oblocks * optsp->obs;

#ifdef SG_LIB_WIN32
    if (FT_BLOCK & optsp->out_type) {
        if (offset != csp->of_filepos) {
            if (verbose > 2)
                fprintf(stderr, "moving of filepos: new_pos="
                        "%"PRId64"\n", (int64_t)offset);
            if (win32_set_file_pos(optsp, DDPT_ARG_OUT, offset, verbose))
                return SG_LIB_FILE_ERROR;
            csp->of_filepos = offset;
        }
        res = win32_block_read_from_of(optsp, wrkPos2, numbytes, verbose);
        if (verbose > 2)
            fprintf(stderr, "read(sparing): requested bytes=%d, res=%d\n",
                    numbytes, res);
        if (res < 0) {
            fprintf(stderr, ME "read(sparing), seek=%"PRId64"\n",
                    optsp->seek);
            return -1;
        } else if (res == numbytes) {
            csp->of_filepos += numbytes;
            return 0;
        } else {
            if (verbose > 2)
                fprintf(stderr, "short read\n");
            return 1;
        }
    } else
#endif
    {
        char ebuff[EBUFF_SZ];

        if (offset != csp->of_filepos) {
            off_t off_res;

            if (verbose > 2)
                fprintf(stderr, "moving of filepos: new_pos="
                        "%"PRId64"\n", (int64_t)offset);
            off_res = lseek(optsp->outfd, offset, SEEK_SET);
            if (off_res < 0) {
                fprintf(stderr, "failed moving of filepos: new_pos="
                        "%"PRId64"\n", (int64_t)offset);
                perror("lseek on output");
                return SG_LIB_FILE_ERROR;
            }
            csp->of_filepos = offset;
        }
        while (((res = read(optsp->outfd, wrkPos2, numbytes)) < 0) &&
               (EINTR == errno))
            ++interrupted_retries;
        if (verbose > 2)
            fprintf(stderr, "read(sparing): requested bytes=%d, res=%d\n",
                    numbytes, res);
        if (res < 0) {
            snprintf(ebuff, EBUFF_SZ, ME "read(sparing), seek=%"PRId64" ",
                     optsp->seek);
            perror(ebuff);
            return -1;
        } else if (res == numbytes) {
            csp->of_filepos += numbytes;
            return 0;
        } else {
            if (verbose > 2)
                fprintf(stderr, "short read\n");
            return 1;
        }
    }
}

static int
cp_write_pt(struct opts_t * optsp, int seek_delta, int blks,
            unsigned char * wrkPos)
{
    int res;
    int64_t aseek = optsp->seek + seek_delta;

    if (optsp->oflagp->nowrite)
        return 0;
    res = pt_write(optsp->outfd, wrkPos, blks, aseek, optsp->obs,
                   optsp->oflagp);
    if (0 != res) {
        fprintf(stderr, "pt_write failed,%s seek=%"PRId64"\n",
                ((-2 == res) ? " try reducing bpt," : ""), aseek);
        return res;
    } else
        out_full += blks;
    return 0;
}

static int
cp_write_block_reg(struct opts_t * optsp, struct cp_state_t * csp,
                   int seek_delta, int blks, unsigned char * wrkPos)
{
    int res;
    int numbytes = blks * optsp->obs;
    off_t offset;
    int64_t aseek = optsp->seek + seek_delta;

    if (optsp->oflagp->nowrite)
        return 0;
    offset = aseek * optsp->obs;
#ifdef SG_LIB_WIN32
    if (FT_BLOCK & optsp->out_type) {
        if (offset != csp->of_filepos) {
            if (verbose > 2)
                fprintf(stderr, "moving of filepos: new_pos="
                        "%"PRId64"\n", (int64_t)offset);
            if (win32_set_file_pos(optsp, DDPT_ARG_OUT, offset, verbose))
                return SG_LIB_FILE_ERROR;
            csp->of_filepos = offset;
        }
        res = win32_block_write(optsp, wrkPos, numbytes, verbose);
        if (res < 0) {
            fprintf(stderr, ME "write(win32_block, seek=%"PRId64" ", aseek);
            return -1;
        } else if (res < numbytes) {
            fprintf(stderr, "output file probably full, seek=%"PRId64" ",
                    aseek);
            out_full += res / optsp->obs;
            if ((res % optsp->obs) > 0)
                ++out_partial;
            return -1;
        } else {
            csp->of_filepos += numbytes;
            csp->bytes_of = numbytes;
            out_full += blks;
        }
        return 0;
    } else
#endif
    {
        char ebuff[EBUFF_SZ];

        if (offset != csp->of_filepos) {
            off_t off_res;

            if (verbose > 2)
                fprintf(stderr, "moving of filepos: new_pos="
                        "%"PRId64"\n", (int64_t)offset);
            off_res = lseek(optsp->outfd, offset, SEEK_SET);
            if (off_res < 0) {
                fprintf(stderr, "failed moving of filepos: new_pos="
                        "%"PRId64"\n", (int64_t)offset);
                perror("lseek on output");
                return SG_LIB_FILE_ERROR;
            }
            csp->of_filepos = offset;
        }
        while (((res = write(optsp->outfd, wrkPos, numbytes))
                < 0) && (EINTR == errno))
            ++interrupted_retries;
        if (verbose > 2)
            fprintf(stderr, "write(unix): requested bytes=%d, res=%d\n",
                    numbytes, res);
        if (res < 0) {
            snprintf(ebuff, EBUFF_SZ, ME "writing, seek=%"PRId64" ", aseek);
            perror(ebuff);
            return -1;
        } else if (res < numbytes) {
            fprintf(stderr, "output file probably full, seek=%"PRId64" ",
                    aseek);
            out_full += res / optsp->obs;
            if ((res % optsp->obs) > 0)
                ++out_partial;
            return -1;
        } else {
            csp->of_filepos += numbytes;
            csp->bytes_of = numbytes;
            out_full += blks;
        }
        return 0;
    }
}

static int
cp_finer_comp_wr(struct opts_t * optsp, struct cp_state_t * csp,
                 unsigned char * b1p, unsigned char * b2p)
{
    int res, k, n, oblks, numbytes, chunk, need_wr, wr_len, wr_k, obs;
    int trim_check, need_tr, tr_len, tr_k;

    oblks = csp->oblocks;
    obs = optsp->obs;
    if (optsp->obpc >= oblks) {
        if (FT_PT & optsp->out_type) {
            if ((res = cp_write_pt(optsp, 0, oblks, b1p)))
                return res;
        } else if ((res = cp_write_block_reg(optsp, csp, 0, oblks, b1p)))
            return res;
        return 0;
    }
    numbytes = oblks * obs;
    chunk = optsp->obpc * obs;
    trim_check = (optsp->oflagp->sparse && optsp->oflagp->wsame16 &&
                  (FT_PT & optsp->out_type));
    need_tr = 0;
    tr_len = 0;
    tr_k = 0;
    for (k = 0, need_wr = 0, wr_len = 0, wr_k = 0; k < numbytes; k += chunk) {
        n = ((k + chunk) < numbytes) ? chunk : (numbytes - k);
        if (0 == memcmp(b1p + k, b2p + k, n)) {
            if (need_wr) {
                if (FT_PT & optsp->out_type) {
                    if ((res = cp_write_pt(optsp, wr_k / obs, wr_len / obs,
                                           b1p + wr_k)))
                        return res;
                } else if ((res = cp_write_block_reg(optsp, csp,
                                wr_k / obs, wr_len / obs, b1p + wr_k)))
                    return res;
                need_wr = 0;
            }
            if (need_tr)
                tr_len += n;
            else if (trim_check) {
                need_tr = 1;
                tr_len = n;
                tr_k = k;
            }
            out_sparse += (n / obs);
        } else {   /* look for a sequence of unequals */
            if (need_wr)
                wr_len += n;
            else {
                need_wr = 1;
                wr_len = n;
                wr_k = k;
            }
            if (need_tr) {
                res = pt_write_same16(optsp->outfd, 1, b2p, obs, tr_len / obs,
                                      optsp->seek + (tr_k / obs));
                if (res)
                    ++trim_errs;
                /* continue past trim errors */
                need_tr = 0;
            }
        }
    }
    if (need_wr) {
        if (FT_PT & optsp->out_type) {
            if ((res = cp_write_pt(optsp, wr_k / obs, wr_len / obs,
                                   b1p + wr_k)))
                return res;
        } else if ((res = cp_write_block_reg(optsp, csp, wr_k / obs,
                                             wr_len / obs, b1p + wr_k)))
            return res;
    }
    if (need_tr) {
        res = pt_write_same16(optsp->outfd, 1, b2p, obs, tr_len / obs,
                              optsp->seek + (tr_k / obs));
        if (res)
            ++trim_errs;
        /* continue past trim errors */
    }
    return 0;
}

static int
cp_construct_pt_zero(struct opts_t * optsp, int obpt)
{
    if ((FT_PT & optsp->in_type) && (NULL == if_ptvp)) {
        if_ptvp = construct_scsi_pt_obj();
        if (NULL == if_ptvp) {
            fprintf(stderr, "if construct_scsi_pt_obj: out of memory\n");
            return -1;
        }
    }
    if ((FT_PT & optsp->out_type) && (NULL == of_ptvp)) {
        of_ptvp = construct_scsi_pt_obj();
        if (NULL == of_ptvp) {
            fprintf(stderr, "of construct_scsi_pt_obj: out of memory\n");
            return -1;
        }
    }
    if ((optsp->oflagp->sparse) && (NULL == zeros_buff)) {
        zeros_buff = (unsigned char *)calloc(obpt * optsp->obs, 1);
        if (NULL == zeros_buff) {
            fprintf(stderr, "zeros_buff calloc failed\n");
            return -1;
        }
    }
    return 0;
}

static void
cp_destruct_pt(void)
{
    if (if_ptvp) {
        destruct_scsi_pt_obj(if_ptvp);
        if_ptvp = NULL;
    }
    if (of_ptvp) {
        destruct_scsi_pt_obj(of_ptvp);
        of_ptvp = NULL;
    }
}

/* Look at IFILE and OFILE lengths and blocks sizes. If dd_count
 * not given, try to deduce a value for it. If oflag=resume do skip,
 * seek, dd_count adjustments. Returns 0 to start copy, returns -1
 * to bypass copy and exit */
static int
resume_calc_count(struct opts_t * op)
{
    int64_t in_num_sect = -1;
    int64_t out_num_sect = -1;
    int64_t ibytes, obytes, ibk;
    int valid_resume = 0;
    int in_sect_sz, out_sect_sz;

    calc_count(op, &in_num_sect, &in_sect_sz, &out_num_sect,
               &out_sect_sz);
    if (verbose > 2)
        fprintf(stderr, "Start of loop, count=%"PRId64", in_num_sect"
                "=%"PRId64", out_num_sect=%"PRId64"\n", dd_count,
                in_num_sect, out_num_sect);
    if (op->skip && (FT_REG == op->in_type) &&
        (op->skip > in_num_sect)) {
        fprintf(stderr, "cannot skip to specified offset on %s\n",
                op->inf);
        dd_count = 0;
        return -1;
    }
    if (op->oflagp->resume) {
        if (FT_REG == op->out_type) {
            if (out_num_sect < 0)
                fprintf(stderr, "resume cannot determine size of OFILE, "
                        "ignore\n");
            else
                valid_resume = 1;
        } else
            fprintf(stderr, "resume expects OFILE to be regular, ignore\n");
    }
    if ((dd_count < 0) && (! valid_resume)) {
        /* Scale back in_num_sect by value of skip */
        if (op->skip && (in_num_sect > op->skip))
            in_num_sect -= op->skip;
        /* Scale back out_num_sect by value of seek */
        if (op->seek && (out_num_sect > op->seek))
            out_num_sect -= op->seek;
        if ((out_num_sect < 0) && (in_num_sect > 0))
            dd_count = in_num_sect;
        else if ((out_num_sect < 0) && (in_num_sect <= 0))
            ;
        else {
            ibytes = (in_num_sect > 0) ? (op->ibs * in_num_sect) : 0;
            obytes = op->obs * out_num_sect;
            if (0 == ibytes)
                dd_count = obytes / op->ibs;
            else if ((ibytes > obytes) && (FT_REG != op->out_type)) {
                dd_count = obytes / op->ibs;
            } else
                dd_count = in_num_sect;
        }
    }
    if (valid_resume) {
        if (dd_count < 0)
            dd_count = in_num_sect - op->skip;
        if (out_num_sect <= op->seek)
            fprintf(stderr, "resume finds no previous copy, restarting\n");
        else {
            obytes = op->obs * (out_num_sect - op->seek);
            ibk = obytes / op->ibs;
            if (ibk >= dd_count) {
                fprintf(stderr, "resume finds copy complete, exiting\n");
                dd_count = 0;
                return -1;
            }
            /* align to bpt multiple */
            ibk = (ibk / op->bpt_i) * op->bpt_i;
            op->skip += ibk;
            op->seek += (ibk * op->ibs) / op->obs;
            dd_count -= ibk;
            fprintf(stderr, "resume adjusting skip=%"PRId64", seek=%"
                    PRId64", and count=%"PRId64"\n", op->skip, op->seek,
                    dd_count);
        }
    }
    return 0;
}

/* This is the main copy loop. Attempts to copy 'dd_count' (a static)
 * blocks (size given by bs or ibs) in chunks of optsp->bpt_i blocks.
 * Returns 0 if successful.  */
static int
do_copy(struct opts_t * optsp, unsigned char * wrkPos,
        unsigned char * wrkPos2)
{
    int ibpt, obpt, res, n, iblocks_hold;
    int penult_sparse_skip = 0;
    int penult_blocks = 0;
    int ret = 0;
    struct cp_state_t cp_st;
    struct cp_state_t * csp;

    csp = &cp_st;
    memset(csp, 0, sizeof(struct cp_state_t));
    ibpt = optsp->bpt_i;
    obpt = (optsp->ibs * optsp->bpt_i) / optsp->obs;
    if ((ret = cp_construct_pt_zero(optsp, obpt)))
        goto loop_end;
    /* Both csp->if_filepos and csp->of_filepos are 0 */

    /* <<< main loop that does the copy >>> */
    while (dd_count > 0) {
        csp->bytes_read = 0;
        csp->bytes_of = 0;
        csp->bytes_of2 = 0;
        penult_sparse_skip = csp->sparse_skip;
        penult_blocks = penult_sparse_skip ? csp->oblocks : 0;
        csp->sparing_skip = 0;
        csp->sparse_skip = 0;
        if (dd_count >= ibpt) {
            csp->iblocks = ibpt;
            csp->oblocks = obpt;
        } else {
            csp->iblocks = dd_count;
            res = dd_count;
            n = res * optsp->ibs;
            csp->oblocks = n / optsp->obs;
            if (n % optsp->obs) {
                ++csp->oblocks;
                memset(wrkPos, 0, optsp->ibs * ibpt);
            }
        }
        iblocks_hold = csp->iblocks;

        /* Start of reading section */
        if (FT_PT & optsp->in_type) {
            if ((ret = cp_read_pt(optsp, csp, wrkPos)))
                break;
        } else {
             if ((ret = cp_read_block_reg(optsp, csp, wrkPos)))
                break;
        }
        if (0 == csp->iblocks)
            break;      /* nothing read so leave loop */

        if ((optsp->out2f[0]) &&
            ((ret = cp_write_of2(optsp, csp, wrkPos))))
            break;

        if ((optsp->oflagp->sparse > 1) ||
            (optsp->oflagp->sparse && 
             (dd_count > iblocks_hold) &&
             (! (FT_DEV_NULL & optsp->out_type)))) {
            if (0 == memcmp(wrkPos, zeros_buff, csp->oblocks * optsp->obs)) {
                csp->sparse_skip = 1;
                if (optsp->oflagp->wsame16 && (FT_PT & optsp->out_type)) {
                    res = pt_write_same16(optsp->outfd, 1, zeros_buff,
                                  optsp->obs, csp->oblocks, optsp->seek);
                    if (res)
                        ++trim_errs;
                }
            } else if (optsp->obpc) {
                ret = cp_finer_comp_wr(optsp, csp, wrkPos, zeros_buff);
                if (ret)
                    break;
                goto bypass_write;
            }
        }
        if (optsp->oflagp->sparing && (! csp->sparse_skip)) {
            /* In write sparing, we read from the output */
            if (FT_PT & optsp->out_type)
                res = cp_read_of_pt(optsp, csp, wrkPos2);
            else
                res = cp_read_of_block_reg(optsp, csp, wrkPos2);
            if (0 == res) {
                if (0 == memcmp(wrkPos, wrkPos2, csp->oblocks * optsp->obs))
                    csp->sparing_skip = 1;
                else if (optsp->obpc) {
                    ret = cp_finer_comp_wr(optsp, csp, wrkPos, wrkPos2);
                    if (ret)
                        break;
                    goto bypass_write;
                }
            }
        }
        /* Start of writing section */
        if (csp->sparing_skip || csp->sparse_skip) {
#if 0
            /* need to track cp_finer_comp_wr() and see trim */
            if (verbose > 2)
                fprintf(stderr, "%s bypassing write: start blk=%" PRId64
                        ", number of blks=%d\n", (csp->sparing_skip ?
                         "sparing" : "sparse"), optsp->seek, csp->oblocks);
#endif
            out_sparse += csp->oblocks;
        } else {
            if (FT_PT & optsp->out_type) {
                if ((ret = cp_write_pt(optsp, 0, csp->oblocks, wrkPos)))
                    break;
            } else if (FT_DEV_NULL & optsp->out_type)
                out_full += csp->oblocks; /* as if written out */
            else if ((ret = cp_write_block_reg(optsp, csp, 0, csp->oblocks,
                                               wrkPos)))
                break;
        }
bypass_write:
#ifdef HAVE_POSIX_FADVISE
        do_fadvise(optsp, csp->bytes_read, csp->bytes_of, csp->bytes_of2);
#endif
        if (dd_count > 0)
            dd_count -= csp->iblocks;
        optsp->skip += csp->iblocks;
        optsp->seek += csp->oblocks;
    } /* end of main loop that does the copy ... */

loop_end:
    if (ret && penult_sparse_skip && (penult_blocks > 0)) {
        /* if error and skipped last output due to sparse ... */
        if ((FT_PT & optsp->out_type) || (FT_DEV_NULL & optsp->out_type) ||
            (FT_BLOCK & optsp->out_type))
            ;
        else { /* write zeros to extend ofile to length prior to error */
            csp->of_filepos -= penult_blocks * optsp->obs;
            res = cp_write_block_reg(optsp, csp, 0, penult_blocks, zeros_buff);
            if (res < 0)
                fprintf(stderr, "writing(sparse after error) error, "
                        "seek=%"PRId64"\n", optsp->seek);
        }
    }
    cp_destruct_pt();
    return ret;
}


int
main(int argc, char * argv[])
{
    int res, fd;
    unsigned char * wrkBuff = NULL;
    unsigned char * wrkPos;
    unsigned char * wrkBuff2 = NULL;
    unsigned char * wrkPos2 = NULL;
    char ebuff[EBUFF_SZ];
    int ret = 0;
    struct opts_t opts;
    struct flags_t iflag;
    struct flags_t oflag;

    memset(&opts, 0, sizeof(opts));
    opts.bpt_i = DEF_BLOCKS_PER_TRANSFER;
    opts.out_type = FT_OTHER;
    opts.in_type = FT_OTHER;
    memset(&iflag, 0, sizeof(iflag));
    memset(&oflag, 0, sizeof(oflag));
    opts.iflagp = &iflag;
    opts.oflagp = &oflag;
    opts.infd = -1;
    opts.outfd = -1;
    opts.out2fd = -1;
    iflag.cdbsz = DEF_SCSI_CDBSZ;
    oflag.cdbsz = DEF_SCSI_CDBSZ;
    res = process_cl(&opts, argc, argv);
    if (res < 0)
        return 0;
    else if (res > 0)
        return res;

    // Will this work in Windows
    if (quiet) {
        if (NULL == freopen("/dev/null", "w", stderr))
            fprintf(stderr, "freopen: failed to redirect stderr to "
                    "/dev/null : %s\n", safe_strerror(errno));
    }

    register_handler(SIGINT, interrupt_handler);
    register_handler(SIGQUIT, interrupt_handler);
    register_handler(SIGPIPE, interrupt_handler);
    register_handler(SIGUSR1, siginfo_handler);
#ifdef SIGINFO
    if (SIGUSR1 != SIGINFO)
        register_handler(SIGINFO, siginfo_handler);
#endif

#ifdef SG_LIB_WIN32
    win32_adjust_fns(&opts);
#endif
    opts.iflagp->pdt = -1;
    opts.oflagp->pdt = -1;
    if (opts.inf[0]) {
        if ('-' == opts.inf[0])
            fd = STDIN_FILENO;
        else {
            fd = open_if(&opts, verbose);
            if (fd < 0)
                return -fd;
        }
        opts.infd = fd;
    } else {
        fprintf(stderr, "'if=IFILE' option must be given. For stdin as "
                "input use 'if=-'\n");
        fprintf(stderr, "For more information use '--help'\n");
        return SG_LIB_SYNTAX_ERROR;
    }

    if ('\0' == opts.outf[0])
        strcpy(opts.outf, "."); /* treat no 'of=OFILE' option as /dev/null */
    if ('-' == opts.outf[0])
        fd = STDOUT_FILENO;
    else {
        fd = open_of(&opts, verbose);
        if (fd < -1)
            return -fd;
    }
    opts.outfd = fd;

    if (opts.out2f[0]) {
        opts.out2_type = dd_filetype(opts.out2f);
        if ((fd = open(opts.out2f, O_WRONLY | O_CREAT, 0666)) < 0) {
            res = errno;
            snprintf(ebuff, EBUFF_SZ,
                     ME "could not open %s for writing", opts.out2f);
            perror(ebuff);
            return res;
        }
        if (sg_set_binary_mode(fd) < 0)
            perror("sg_set_binary_mode");
    } else
        fd = -1;
    opts.out2fd = fd;

    if ((STDIN_FILENO == opts.infd) && (STDOUT_FILENO == opts.outfd)) {
        fprintf(stderr,
                "Can't have both 'if' as stdin _and_ 'of' as stdout\n");
        fprintf(stderr, "For more information use '--help'\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if (oflag.sparse) {
        if (STDOUT_FILENO == opts.outfd) {
            fprintf(stderr, "oflag=sparse needs seekable output file\n");
            return SG_LIB_SYNTAX_ERROR;
        }
        out_sparse_active = 1;
        if (oflag.wsame16)
            out_trim_active = 1;
    }
    if (oflag.sparing) {
        if (STDOUT_FILENO == opts.outfd) {
            fprintf(stderr, "oflag=sparing needs seekable output file\n");
            return SG_LIB_SYNTAX_ERROR;
        }
        out_sparing_active = 1;
    }

    if ((dd_count < 0) || opts.oflagp->resume ||
        ((verbose > 0) && (0 == dd_count))) {
        if (resume_calc_count(&opts))
            goto cleanup;
    }
    if (dd_count < 0) {
        fprintf(stderr, "Couldn't calculate count, please give one\n");
        return SG_LIB_CAT_OTHER;
    }
    if (! opts.cdbsz_given) {
        if ((FT_PT & opts.in_type) && (MAX_SCSI_CDBSZ != opts.iflagp->cdbsz) &&
            (((dd_count + opts.skip) > UINT_MAX) || (opts.bpt_i > USHRT_MAX))) {
            fprintf(stderr, "Note: SCSI command size increased to 16 bytes "
                    "(for 'if')\n");
            opts.iflagp->cdbsz = MAX_SCSI_CDBSZ;
        }
        if ((FT_PT & opts.out_type) &&
            (MAX_SCSI_CDBSZ != opts.oflagp->cdbsz) &&
            (((dd_count + opts.seek) > UINT_MAX) || (opts.bpt_i > USHRT_MAX))) {
            fprintf(stderr, "Note: SCSI command size increased to 16 bytes "
                    "(for 'of')\n");
            opts.oflagp->cdbsz = MAX_SCSI_CDBSZ;
        }
    }

    if (opts.iflagp->direct || opts.oflagp->direct) {
        size_t psz;

#ifdef SG_LIB_MINGW
        psz = getpagesize();    // implicit definition but links okay
#else
        psz = sysconf(_SC_PAGESIZE); /* was getpagesize() */
#endif

        wrkBuff = (unsigned char*)calloc(opts.ibs * opts.bpt_i + psz, 1);
        if (0 == wrkBuff) {
            fprintf(stderr, "Not enough user memory for aligned usage\n");
            return SG_LIB_CAT_OTHER;
        }
        // posix_memalign() could be a better way to do this
        wrkPos = (unsigned char *)(((unsigned long)wrkBuff + psz - 1) &
                                   (~(psz - 1)));
        if (opts.oflagp->sparing) {
            wrkBuff2 = (unsigned char*)calloc(opts.ibs * opts.bpt_i + psz, 1);
            if (0 == wrkBuff2) {
                fprintf(stderr, "Not enough user memory for aligned "
                        "usage(2)\n");
                return SG_LIB_CAT_OTHER;
            }
            wrkPos2 = (unsigned char *)(((unsigned long)wrkBuff2 + psz - 1) &
                                   (~(psz - 1)));
        }
    } else {
        wrkBuff = (unsigned char*)calloc(opts.ibs * opts.bpt_i, 1);
        if (0 == wrkBuff) {
            fprintf(stderr, "Not enough user memory\n");
            return SG_LIB_CAT_OTHER;
        }
        wrkPos = wrkBuff;
        if (opts.oflagp->sparing) {
            wrkBuff2 = (unsigned char*)calloc(opts.ibs * opts.bpt_i, 1);
            if (0 == wrkBuff2) {
                fprintf(stderr, "Not enough user memory(2)\n");
                return SG_LIB_CAT_OTHER;
            }
            wrkPos2 = wrkBuff2;
        }
    }

    if (verbose) {
        fprintf(stderr, "skip=%"PRId64" (blocks on input), seek=%"PRId64" "
                "(blocks on output)\n", opts.skip, opts.seek);
        fprintf(stderr, "  initial count=%"PRId64" (blocks of input), "
                "blocks_per_transfer=%d\n", dd_count, opts.bpt_i);
    }
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    if (do_time) {
        start_tm.tv_sec = 0;
        start_tm.tv_nsec = 0;
        if (0 == clock_gettime(CLOCK_MONOTONIC, &start_tm))
            start_tm_valid = 1;
    }
#elif defined(HAVE_GETTIMEOFDAY)
    if (do_time) {
        start_tm.tv_sec = 0;
        start_tm.tv_usec = 0;
        gettimeofday(&start_tm, NULL);
        start_tm_valid = 1;
    }
#endif
    req_count = dd_count;

    ret = do_copy(&opts, wrkPos, wrkPos2);

    print_stats("");
    if (sum_of_resids)
        fprintf(stderr, ">> Non-zero sum of residual counts=%d\n",
                sum_of_resids);
    if (do_time)
        calc_duration_throughput(0);

    if ((opts.oflagp->ssync) && (FT_PT & opts.out_type)) {
        fprintf(stderr, ">> SCSI synchronizing cache on %s\n", opts.outf);
        res = sg_ll_sync_cache_10(opts.outfd, 0, 0, 0, 0, 0, 0, 0);
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            fprintf(stderr, "Unit attention (out, sync cache), "
                    "continuing\n");
            res = sg_ll_sync_cache_10(opts.outfd, 0, 0, 0, 0, 0, 0, 0);
        }
        if (0 != res)
            fprintf(stderr, "Unable to do SCSI synchronize cache\n");
    }

cleanup:
    if (wrkBuff)
        free(wrkBuff);
    if (wrkBuff2)
        free(wrkBuff2);
    if (zeros_buff)
        free(zeros_buff);
    if (FT_PT & opts.in_type)
        scsi_pt_close_device(opts.infd);
    else if ((opts.infd >= 0) && (STDIN_FILENO != opts.infd))
        close(opts.infd);
    if (FT_PT & opts.out_type)
        scsi_pt_close_device(opts.outfd);
    if ((opts.outfd >= 0) && (STDOUT_FILENO != opts.outfd) &&
        !(FT_DEV_NULL & opts.out_type))
        close(opts.outfd);
    if ((opts.out2fd >= 0) && (STDOUT_FILENO != opts.out2fd))
        close(opts.out2fd);
    if (0 != dd_count) {
        fprintf(stderr, "Some error occurred\n");
        if (0 == ret)
            ret = SG_LIB_CAT_OTHER;
    }
    return (ret >= 0) ? ret : SG_LIB_CAT_OTHER;
}
