/*
 * Copyright (c) 2013-2020, Douglas Gilbert
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

/*
 * This file contains some common functions for ddpt.
 */

/* Was needed for posix_fadvise() */
/* #define _XOPEN_SOURCE 600 */

/* Need _GNU_SOURCE for O_DIRECT */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#define __STDC_LIMIT_MACROS 1   /* for UINT64_MAX, UINT32_MAX, etc */
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>
#include <sys/types.h>
#include <sys/stat.h>

/* N.B. config.h must precede anything that depends on HAVE_*  */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_NANOSLEEP
#include <time.h>
#endif

#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
#include <time.h>
#elif defined(HAVE_GETTIMEOFDAY)
#include <time.h>
#include <sys/time.h>
#endif

#include "ddpt.h"       /* includes <signal.h> */

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

#endif  /* end SG_LIB_WIN32 */

#include "sg_lib.h"
#include "sg_pr2serr.h"

const char * ddpt_arg_strs[] = {"if", "of", "of2"};

static const char * errblk_file = "errblk.txt";


void
sleep_ms(int millisecs)
{
#ifdef SG_LIB_WIN32
    win32_sleep_ms(millisecs);
#elif defined(HAVE_NANOSLEEP)
    struct timespec request;

    if (millisecs > 0) {
        request.tv_sec = millisecs / 1000;
        request.tv_nsec = (millisecs % 1000) * 1000000;
        if ((nanosleep(&request, NULL) < 0) && (EINTR != errno))
            perror("nanosleep");
    }
#endif
}

const char *
ddpt_arg_str(int ddpt_arg)
{
    if ((ddpt_arg < 0) || (ddpt_arg >= (int)SG_ARRAY_SIZE(ddpt_arg_strs)))
        return "bad ddpt_arg";
    else
        return ddpt_arg_strs[ddpt_arg];
}

void
state_init(struct opts_t * op, struct flags_t * ifp, struct flags_t * ofp,
           struct dev_info_t * idip, struct dev_info_t * odip,
           struct dev_info_t * o2dip)
{
    memset(op, 0, sizeof(struct opts_t));       /* sets bools to false */
    op->bs_same = true;                         /* most likely case */
    op->dd_count = DDPT_COUNT_INDEFINITE;       /* -1 */
    op->highest_unrecovered = -1;
    op->do_time = true;    /* default was false (0) in sg_dd */
    op->id_usage = -1;
    op->list_id = 1;
    op->prio = 1;
    op->max_uas = MAX_UNIT_ATTENTIONS;
    op->max_aborted = MAX_ABORTED_CMDS;
    memset(ifp, 0, sizeof(struct flags_t));
    memset(ofp, 0, sizeof(struct flags_t));
    op->iflagp = ifp;
    op->oflagp = ofp;
    memset(idip, 0, sizeof(struct dev_info_t));
    memset(odip, 0, sizeof(struct dev_info_t));
    memset(o2dip, 0, sizeof(struct dev_info_t));
    idip->d_type = FT_OTHER;
    idip->fd = -1;
    idip->reg_sz = -1;
    idip->ddpt_arg = DDPT_ARG_IN;
    idip->dir_n = "in";
    op->idip = idip;
    odip->d_type = FT_OTHER;
    odip->fd = -1;
    odip->reg_sz = -1;
    odip->ddpt_arg = DDPT_ARG_OUT;
    odip->dir_n = "out";
    op->odip = odip;
    o2dip->d_type = FT_OTHER;
    o2dip->fd = -1;
    o2dip->ddpt_arg = DDPT_ARG_OUT2;
    o2dip->dir_n = "out2";
    op->o2dip = o2dip;
    ifp->cdbsz = DEF_SCSI_CDBSZ;
    ofp->cdbsz = DEF_SCSI_CDBSZ;
#ifdef HAVE_POSIX_FADVISE
    op->lowest_skip = -1;
    op->lowest_seek = -1;
#endif
    op->idip->pdt = -1;
    op->odip->pdt = -1;
    op->rtf_fd = -1;
}

/* When who<=0 print both in+out, when who==1 print in, else print out */
void
print_stats(const char * str, struct opts_t * op, int who, bool estimate)
{
    const char * cp;
    const char * out_verify = op->verify_given ? "verified" : "out";
    struct cp_statistics_t * sp = op->stp ? op->stp : &op->stats;

#ifdef SG_LIB_LINUX
    /* Print tape read summary if necessary . */
    print_tape_summary(op, 0, str);
#endif

    if ((op->dd_count > 0) && (! op->reading_fifo)) {
        pr2serr("  remaining block count=%" PRId64, op->dd_count);
        // if ((who < 2) && (op->in_full <= sp->dd_count_start) &&
        if (estimate && (who < 2) && (sp->in_full <= sp->dd_count_start) &&
            (sp->dd_count_start >= 4196)) {
            /* with ints will overflow around 4 TB for bs=1 */
            // int num = (int)((op->in_full * 100) / 4196);
            int num = (int)((sp->in_full * 100) / 4196);
            int den = (int)(sp->dd_count_start / 4196);  /* will be >= 1 */
            int percent;

            percent = num / den;
            // if ((100 == percent) && (op->in_full < op->dd_count_start))
            if ((100 == percent) &&
                (sp->in_full < sp->dd_count_start))
                --percent;      /* don't want rounding to 100 % */
            pr2serr("   %d%% completed\n", percent);
        } else
            pr2serr("\n");
    }
    if (who < 2)
        pr2serr("%s%" PRId64 "+%d records in\n", str, sp->in_full,
                sp->in_partial);
    if (1 != who) {
        cp = (op->oflagp->nowrite && (sp->out_full || sp->out_partial)) ?
                        " [not done due to nowrite flag]" : "";
        pr2serr("%s%" PRId64 "+%d records %s%s\n", str, sp->out_full,
                sp->out_partial, out_verify, cp);
    }
    if (op->out_sparse_active || op->out_sparing_active) {
        if (op->out_trim_active) {
            cp = sp->trim_errs ? "attempted trim" : "trimmed";
            if (sp->out_sparse_partial > 0)
                pr2serr("%s%" PRId64 "+%d %s records out\n", str,
                        sp->out_sparse, sp->out_sparse_partial, cp);
            else
                pr2serr("%s%" PRId64 " %s records %s\n", str,
                        sp->out_sparse, cp, out_verify);
        } else if (sp->out_sparse_partial > 0)
            pr2serr("%s%" PRId64 "+%d bypassed records out\n", str,
                    sp->out_sparse, sp->out_sparse_partial);
        else
            pr2serr("%s%" PRId64 " bypassed records out\n", str,
                    sp->out_sparse);
    }
    if (sp->recovered_errs > 0)
        pr2serr("%s%d recovered read errors\n", str, sp->recovered_errs);
    if (sp->num_retries > 0)
        pr2serr("%s%d retries attempted\n", str, sp->num_retries);
    if (sp->unrecovered_errs > 0)
        pr2serr("%s%d unrecovered read error%s\n", str, sp->unrecovered_errs,
                ((1 == sp->unrecovered_errs) ? "" : "s"));
    if (sp->unrecovered_errs && (op->highest_unrecovered >= 0))
        pr2serr("lowest unrecovered read lba=%" PRId64 ", highest "
                "unrecovered lba=%" PRId64 "\n", op->lowest_unrecovered,
                op->highest_unrecovered);
    if (sp->wr_recovered_errs > 0)
        pr2serr("%s%d recovered write errors\n", str, sp->wr_recovered_errs);
    if (sp->wr_unrecovered_errs > 0)
        pr2serr("%s%d unrecovered write error%s\n", str,
                sp->wr_unrecovered_errs,
                ((1 == sp->wr_unrecovered_errs) ? "" : "s"));
    if (sp->trim_errs)
        pr2serr("%s%d trim errors\n", str, sp->trim_errs);
    if (sp->interrupted_retries > 0)
        pr2serr("%s%d %s after interrupted system call(s)\n",
                str, sp->interrupted_retries,
                ((1 == sp->interrupted_retries) ? "retry" : "retries"));
    if (sp->io_eagains > 0)
        pr2serr("%s%d %s after EAGAIN error(s) during IO\n",
                str, sp->io_eagains,
                ((1 == sp->io_eagains) ? "retry" : "retries"));
    if (op->has_xcopy)
        pr2serr("%s%" PRId64 " xcopy command%s done\n", str, op->num_xcopy,
                ((1 == op->num_xcopy) ? "" : "s"));
}

/* Attempt to categorize the file type from the given filename.
 * Separate version for Windows and Unix. Windows version does some
 * file name processing. */
#ifndef SG_LIB_WIN32

#ifdef SG_LIB_LINUX
static bool bsg_major_checked = false;
static int bsg_major = 0;

/* In Linux search /proc/devices for bsg character driver in order to
 * find its major device number since it is allocated dynamically.  */
static void
find_bsg_major(int verbose)
{
    int n;
    const char * proc_devices = "/proc/devices";
    FILE *fp;
    char a[128];
    char b[128];
    char * cp;

    if (NULL == (fp = fopen(proc_devices, "r"))) {
        if (verbose)
            pr2serr("fopen %s failed: %s\n", proc_devices,
                    safe_strerror(errno));
        return;
    }
    while ((cp = fgets(b, sizeof(b), fp))) {
        if ((1 == sscanf(b, "%126s", a)) &&
            (0 == memcmp(a, "Character", 9)))
            break;
    }
    while (cp && (cp = fgets(b, sizeof(b), fp))) {
        if (2 == sscanf(b, "%d %126s", &n, a)) {
            if (0 == strcmp("bsg", a)) {
                bsg_major = n;
                break;
            }
        } else
            break;
    }
    if (verbose > 5) {
        if (cp)
            pr2serr("found bsg_major=%d\n", bsg_major);
        else
            pr2serr("found no bsg char device in %s\n", proc_devices);
    }
    fclose(fp);
}
#endif

/* Categorize file by using the stat() system call on its filename.
 * If not found FT_ERROR returned. The FT_* constants are a bit mask
 * and later logic can combine them (e.g. FT_BLOCK | FT_PT).
 */
static int
unix_dd_filetype(const char * filename, int verbose)
{
    size_t len = strlen(filename);
    struct stat st;

    if (verbose) { ; }    /* suppress warning */
    if ((1 == len) && ('.' == filename[0]))
        return FT_DEV_NULL;
    if (stat(filename, &st) < 0)
        return FT_ERROR;
    if (S_ISREG(st.st_mode)) {
        // pr2serr("dd_filetype: regular file, st_size=%" PRId64 "\n",
        //         st.st_size);
        return FT_REG;
    } else if (S_ISCHR(st.st_mode)) {
#ifdef SG_LIB_LINUX
        /* major() and minor() defined in sys/sysmacros.h */
        if ((MEM_MAJOR == major(st.st_rdev)) &&
            ((DEV_NULL_MINOR_NUM == minor(st.st_rdev)) ||
             (DEV_ZERO_MINOR_NUM == minor(st.st_rdev))))
            return FT_DEV_NULL; /* treat /dev/null + /dev/zero the same */
        if (SCSI_GENERIC_MAJOR == major(st.st_rdev))
            return FT_PT;
        if (SCSI_TAPE_MAJOR == major(st.st_rdev))
            return FT_TAPE;
        if (! bsg_major_checked) {
            bsg_major_checked = true;
            find_bsg_major(verbose);
        }
        if (bsg_major == (int)major(st.st_rdev))
            return FT_PT;
        return FT_CHAR; /* assume something like /dev/zero */
#elif SG_LIB_FREEBSD
        {
            /* int d_flags;  for FIOFTYPE ioctl see sys/filio.h */
            char * bname;
            char s[STR_SZ];

            strcpy(s, filename);
            bname = basename(s);
            if (0 == strcmp("null", bname))
                return FT_DEV_NULL;
            else if (0 == memcmp("pass", bname, 4))
                return FT_PT;
            else if (0 == memcmp("sa", bname, 2))
                return FT_TAPE;
            else if (0 == strcmp("zero", bname))
                return FT_DEV_NULL;  /* treat /dev/null+/dev/zero the same */
            else
                return FT_BLOCK;  /* freebsd doesn't have block devices! */
        }
#elif SG_LIB_SOLARIS
        /* might be /dev/rdsk or /dev/scsi , require pt override */
        return FT_BLOCK;
#else
        return FT_PT;
#endif
    } else if (S_ISBLK(st.st_mode)) {
#ifdef SG_LIB_LINUX
	if (BLOCK_EXT_MAJOR == major(st.st_rdev))
	    return FT_NVME;
#endif 
        return FT_BLOCK;
    } else if (S_ISFIFO(st.st_mode))
        return FT_FIFO;
    return FT_OTHER;
}
#endif          /* if not SG_LIB_WIN32 */

/* Categorize file by using the stat() system call on its filename.
 * If not found FT_ERROR returned. The FT_* constants are a bit mask
 * and later logic can combine them (e.g. FT_BLOCK | FT_PT). */
int
dd_filetype(const char * filename, int vb)
{
    int ret;
    char b[160];

#ifdef SG_LIB_WIN32
    ret = win32_dd_filetype(filename, vb);
#else
    ret = unix_dd_filetype(filename, vb);
#endif

    if (vb > 2) {
        const char * cp = filename ? filename : "<null>";

        if (0 == memcmp(cp, ".", 2))
            cp = "<none>";
        dd_filetype_str(ret, b, sizeof(b), filename);
        pr2serr("%s: guess %s filetype is: %s\n", __func__, cp, b);
    }
    return ret;
}

char *
dd_filetype_str(int ft, char * buff, int max_bufflen, const char * fname)
{
    int off = 0;

    if (FT_DEV_NULL & ft)
        off += sg_scnpr(buff + off, max_bufflen - off, "null device ");
    if (FT_PT & ft)
        off += sg_scnpr(buff + off, max_bufflen - off,
                        "pass-through [pt] device ");
    if (FT_TAPE & ft)
        off += sg_scnpr(buff + off, max_bufflen - off, "SCSI tape device ");
    if (FT_BLOCK & ft)
        off += sg_scnpr(buff + off, max_bufflen - off, "block device ");
    if (FT_FIFO & ft)
        off += sg_scnpr(buff + off, max_bufflen - off,
                        "fifo [stdin, stdout, named pipe] ");
    if (FT_REG & ft)
        off += sg_scnpr(buff + off, max_bufflen - off, "regular file ");
    if (FT_CHAR & ft)
        off += sg_scnpr(buff + off, max_bufflen - off, "char device ");
    if (FT_NVME & ft)
        off += sg_scnpr(buff + off, max_bufflen - off, "NVMe device ");
    if (FT_ALL_FF & ft)
        off += sg_scnpr(buff + off, max_bufflen - off,
		        "null device full of 0xff bytes ");
    if (FT_OTHER & ft)
        off += sg_scnpr(buff + off, max_bufflen - off, "other file type ");
    if (FT_ERROR & ft)
        /* off += */ sg_scnpr(buff + off, max_bufflen - off, "unable to "
                              "'stat' %s ", (fname ? fname : "file"));
    return buff;
}

#ifdef SG_LIB_LINUX
static int
lin_get_blkdev_capacity(struct opts_t * op, int which_arg, int64_t * num_blks,
                        int * blk_sz)
{
    int blk_fd;
    const char * fname;

    blk_fd = (DDPT_ARG_IN == which_arg) ? op->idip->fd : op->odip->fd;
    fname = (DDPT_ARG_IN == which_arg) ? op->idip->fn : op->odip->fn;
    if (op->verbose > 2)
        pr2serr("lin_get_blkdev_capacity: for %s\n", fname);
    /* BLKGETSIZE64, BLKGETSIZE and BLKSSZGET macros problematic (from
     *  <linux/fs.h> or <sys/mount.h>). */
#ifdef BLKSSZGET
    if ((ioctl(blk_fd, BLKSSZGET, blk_sz) < 0) && (*blk_sz > 0)) {
        perror("BLKSSZGET ioctl error");
        return -1;
    } else {
 #ifdef BLKGETSIZE64
        uint64_t ull;

        if (ioctl(blk_fd, BLKGETSIZE64, &ull) < 0) {

            perror("BLKGETSIZE64 ioctl error");
            return -1;
        }
        *num_blks = ((int64_t)ull / (int64_t)*blk_sz);
        if (op->verbose > 5)
            pr2serr("Used Linux BLKGETSIZE64 ioctl\n");
 #else
        unsigned long ul;

        if (ioctl(blk_fd, BLKGETSIZE, &ul) < 0) {
            perror("BLKGETSIZE ioctl error");
            return -1;
        }
        *num_blks = (int64_t)ul;
        if (op->verbose > 5)
            pr2serr("Used Linux BLKGETSIZE ioctl\n");
 #endif
    }
    return 0;
#else   /* not BLKSSZGET */
    blk_fd = blk_fd;
    if (op->verbose)
        pr2serr("      BLKSSZGET+BLKGETSIZE ioctl not available\n");
    *num_blks = 0;
    *blk_sz = 0;
    return -1;
#endif
}
#endif  /* BLKSSZGET */

#ifdef SG_LIB_FREEBSD
static int
fbsd_get_blkdev_capacity(struct opts_t * op, int which_arg,
                         int64_t * num_blks, int * blk_sz)
{
// Why do kernels invent their own typedefs and not use C standards?
#define u_int unsigned int
    off_t mediasize;
    unsigned int sectorsize;
    int blk_fd;
    const char * fname;

    blk_fd = (DDPT_ARG_IN == which_arg) ? op->idip->fd : op->odip->fd;
    fname = (DDPT_ARG_IN == which_arg) ? op->idip->fn : op->odip->fn;
    if (op->verbose > 2)
        pr2serr("%s: for %s\n", __func__, fname);

    /* For FreeBSD post suggests that /usr/sbin/diskinfo uses
     * ioctl(fd, DIOCGMEDIASIZE, &mediasize), where mediasize is an off_t.
     * also: ioctl(fd, DIOCGSECTORSIZE, &sectorsize) */
    if (ioctl(blk_fd, DIOCGSECTORSIZE, &sectorsize) < 0) {
        perror("DIOCGSECTORSIZE ioctl error");
        return -1;
    }
    *blk_sz = sectorsize;
    if (ioctl(blk_fd, DIOCGMEDIASIZE, &mediasize) < 0) {
        perror("DIOCGMEDIASIZE ioctl error");
        return -1;
    }
    if (sectorsize)
        *num_blks = mediasize / sectorsize;
    else
        *num_blks = 0;
    return 0;
}
#endif

#ifdef SG_LIB_SOLARIS
static int
sol_get_blkdev_capacity(struct opts_t * op, int which_arg,
                        int64_t * num_blks, int * blk_sz)
{
    struct dk_minfo info;
    int blk_fd;
    const char * fname;

    blk_fd = (DDPT_ARG_IN == which_arg) ? op->idip->fd : op->odip->fd;
    fname = (DDPT_ARG_IN == which_arg) ? op->idip->fn : op->odip->fn;
    if (op->verbose > 2)
        pr2serr("%s: for %s\n", __func__, fname);

    /* this works on "char" block devs (e.g. in /dev/rdsk) but not /dev/dsk */
    if (ioctl(blk_fd, DKIOCGMEDIAINFO , &info) < 0) {
        perror("DKIOCGMEDIAINFO ioctl error");
        *num_blks = 0;
        *blk_sz = 0;
        return -1;
    }
    *num_blks = info.dki_capacity;
    *blk_sz = info.dki_lbsize;
    return 0;
}
#endif

/* get_blkdev_capacity() returns 0 -> success or -1 -> failure.
 * which_arg should either be DDPT_ARG_IN, DDPT_ARG_OUT or DDPT_ARG_OUT2.
 * If successful writes back logical block size using the blk_sz pointer.
 * Also writes back the number of logical blocks) on the block device using
 * num_blks pointer. */
int
get_blkdev_capacity(struct opts_t * op, int which_arg, int64_t * num_blks,
                    int * blk_sz)
{
#ifdef SG_LIB_LINUX
    return lin_get_blkdev_capacity(op, which_arg, num_blks, blk_sz);
#elif defined(SG_LIB_FREEBSD)
    return fbsd_get_blkdev_capacity(op, which_arg, num_blks, blk_sz);
#elif defined(SG_LIB_SOLARIS)
    return sol_get_blkdev_capacity(op, which_arg, num_blks, blk_sz);
#elif defined(SG_LIB_WIN32)
    return win32_get_blkdev_capacity(op, which_arg, num_blks, blk_sz);
#else
    return -1;
#endif
}

void
zero_coe_limit_count(struct opts_t * op)
{
    if (op->coe_limit > 0)
        op->coe_count = 0;
}

/* Print number of blocks, block size. If over 1 MB print size in MB
 * (10**6 bytes), GB (10**9 bytes) or TB (10**12 bytes) to stderr. */
void
print_blk_sizes(const char * fname, const char * access_typ, int64_t num_blks,
                int blk_sz, bool to_stderr)
{
    int mb, gb, tb;
    size_t len;
    int64_t n = 0;
    char b[32];
    char dec[4];
    int (*print_p)(const char *, ...);

    print_p = to_stderr ? pr2serr : printf;
    if (num_blks <= 0) {
        print_p("  %s [%s]: num_blocks=%" PRId64 ", block_size=%d\n", fname,
                access_typ, num_blks, blk_sz);
        return;
    }
    gb = 0;
    if ((num_blks > 0) && (blk_sz > 0)) {
        n = num_blks * blk_sz;
        gb = n / 1000000000;
    }
    if (gb > 999999) {
        tb = gb / 1000;
        snprintf(b, sizeof(b), "%d", tb);
        len = strlen(b); // len must be >= 4
        dec[0] = b[len - 3];
        dec[1] = b[len - 2];
        dec[2] = '\0';
        b[len - 3] = '\0';
        print_p("  %s [%s]: num_blocks=%" PRId64 " [0x%" PRIx64 "], "
                "block_size=%d, %s.%s PB\n", fname, access_typ, num_blks,
                num_blks, blk_sz, b, dec);
    } else if (gb > 99999) {
        tb = gb / 1000;
        print_p("  %s [%s]: num_blocks=%" PRId64 " [0x%" PRIx64 "], "
                "block_size=%d, %d TB\n", fname, access_typ, num_blks,
                num_blks, blk_sz, tb);
    } else {
        mb = n / 1000000;
        if (mb > 999999) {
            gb = mb / 1000;
            snprintf(b, sizeof(b), "%d", gb);
            len = strlen(b); // len must be >= 4
            dec[0] = b[len - 3];
            dec[1] = b[len - 2];
            dec[2] = '\0';
            b[len - 3] = '\0';
            print_p("  %s [%s]: num_blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "block_size=%d, %s.%s TB\n", fname, access_typ, num_blks,
                    num_blks, blk_sz, b, dec);
        } else if (mb > 99999) {
            gb = mb / 1000;
            print_p("  %s [%s]: num_blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "block_size=%d, %d GB\n", fname, access_typ, num_blks,
                    num_blks, blk_sz, gb);
        } else if (mb > 999) {
            snprintf(b, sizeof(b), "%d", mb);
            len = strlen(b); // len must be >= 4
            dec[0] = b[len - 3];
            dec[1] = b[len - 2];
            dec[2] = '\0';
            b[len - 3] = '\0';
            print_p("  %s [%s]: num_blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "block_size=%d, %s.%s GB\n", fname, access_typ, num_blks,
                    num_blks, blk_sz, b, dec);
        } else if (mb > 0) {
            print_p("  %s [%s]: num_blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "block_size=%d, %d MB%s\n", fname, access_typ, num_blks,
                    num_blks, blk_sz, mb, ((mb < 10) ? " approx" : ""));
        } else
            print_p("  %s [%s]: num_blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "block_size=%d\n", fname, access_typ, num_blks, num_blks,
                    blk_sz);
    }
}

void
calc_duration_init(struct opts_t * op)
{
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    if (op->do_time) {
        op->start_tm.tv_sec = 0;
        op->start_tm.tv_nsec = 0;
        if (0 == clock_gettime(CLOCK_MONOTONIC, &op->start_tm))
            op->start_tm_valid = true;
    }
#elif defined(HAVE_GETTIMEOFDAY)
    if (op->do_time) {
        op->start_tm.tv_sec = 0;
        op->start_tm.tv_usec = 0;
        gettimeofday(&op->start_tm, NULL);
        op->start_tm_valid = true;
    }
#else
    if (op) { ; }
#endif
}

/* Calculates transfer throughput, typically in Megabytes per second.
 * A megabyte in this context is 1000000 bytes (gives bigger numbers so
 * is preferred by industry). The clock_gettime() interface is preferred
 * since time is guaranteed to advance; gettimeofday() is impacted if the
 * user (or something like ntpd) changes the time.
 * Also if the transfer is large enough and isn't about to finish, it
 * makes an estimate of the time remaining (when contin=true). */
void
calc_duration_throughput(const char * leadin, bool contin, struct opts_t * op)
{
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    bool use_out_full;
    struct timespec end_tm, res_tm, delta_tm;
    double a, b, da, db, r, dr;
    int secs, h, m, elapsed_secs;
    struct cp_statistics_t * sp = op->stp ? op->stp : &op->stats;

    if (op->start_tm_valid && (op->start_tm.tv_sec || op->start_tm.tv_nsec)) {
        use_out_full = ((0 == sp->in_full) && (op->obs_lb > 0));
        clock_gettime(CLOCK_MONOTONIC, &end_tm);
        res_tm.tv_sec = end_tm.tv_sec - op->start_tm.tv_sec;
        res_tm.tv_nsec = end_tm.tv_nsec - op->start_tm.tv_nsec;
        if (res_tm.tv_nsec < 0) {
            --res_tm.tv_sec;
            res_tm.tv_nsec += 1000000000;
        }
        elapsed_secs = res_tm.tv_sec;
        a = res_tm.tv_sec;
        a += (0.000001 * (res_tm.tv_nsec / 1000));
        if (sp->prev_valid) {
            delta_tm.tv_sec = end_tm.tv_sec - sp->prev_tm.tv_sec;
            delta_tm.tv_nsec = end_tm.tv_nsec - sp->prev_tm.tv_nsec;
            if (delta_tm.tv_nsec < 0) {
                --delta_tm.tv_sec;
                delta_tm.tv_nsec += 1000000000;
            }
            da = delta_tm.tv_sec;
            da += (0.000001 * (delta_tm.tv_nsec / 1000));
        } else
            da = 0.0000001;
        db = 1.0;

        sp->prev_tm = end_tm;
        if (use_out_full) {
            b = (double)op->obs_lb * sp->out_full;
            if (sp->prev_valid)
                db = (double)op->obs_lb * (sp->out_full - sp->prev_count);
            sp->prev_count = sp->out_full;
        } else {
            b = (double)op->ibs_hold * sp->in_full;
            if (sp->prev_valid)
                db = (double)op->obs_lb * (sp->in_full - sp->prev_count);
            sp->prev_count = sp->in_full;
        }
        pr2serr("%stime to %s data%s: %d.%06d secs", leadin,
                (op->read1_or_transfer ? "read" : "transfer"),
                (contin ? " so far" : ""), (int)res_tm.tv_sec,
                (int)(res_tm.tv_nsec / 1000));
        r = 0.0;
        if ((a > 0.00001) && (b > 511)) {
            r = b / (a * 1000000.0);
            if (r < 1.0)
                pr2serr(" at %.1f KB/sec", r * 1000);
            else
                pr2serr(" at %.2f MB/sec", r);
        }
        if (sp->prev_valid) {
            if ((da > 0.00001) && (db > 511)) {
                dr = db / (da * 1000000.0);
                if (dr < 1.0)
                    pr2serr(" (delta %.1f KB/sec)", dr * 1000);
                else
                    pr2serr(" (delta %.2f MB/sec)", dr);
            }
        }
        pr2serr("\n");
        sp->prev_valid = true;

        if (contin && (! op->reading_fifo) && (r > 0.01) &&
            (op->dd_count > 100)) {
            secs = (int)(((double)op->ibs_hold * op->dd_count) /
                         (r * 1000000));
            if (secs > 10) {
                pr2serr("%s%d%% complete, ", leadin,
                        (100 * elapsed_secs) / (secs + elapsed_secs));
                h = secs / 3600;
                secs = secs - (h * 3600);
                m = secs / 60;
                secs = secs - (m * 60);
                if (h > 0)
                    pr2serr("estimated time remaining: %d:%02d:%02d\n",
                            h, m, secs);
                else
                    pr2serr("estimated time remaining: %d:%02d\n", m, secs);
            }
        }
    }
#elif defined(HAVE_GETTIMEOFDAY)
    bool use_out_full;
    struct timeval end_tm, res_tm, delta_tm;
    double a, b, da, db, dr, r;
    int secs, h, m, elapsed_secs;
    struct cp_statistics_t * sp = op->stp ? op->stp : &op->stats;

    if (op->start_tm_valid && (op->start_tm.tv_sec || op->start_tm.tv_usec)) {
        use_out_full = ((0 == sp->in_full) && (op->obs_lb > 0));
        gettimeofday(&end_tm, NULL);
        res_tm.tv_sec = end_tm.tv_sec - op->start_tm.tv_sec;
        res_tm.tv_usec = end_tm.tv_usec - op->start_tm.tv_usec;
        if (res_tm.tv_usec < 0) {
            --res_tm.tv_sec;
            res_tm.tv_usec += 1000000;
        }
        elapsed_secs = res_tm.tv_sec;
        a = res_tm.tv_sec;
        a += (0.000001 * res_tm.tv_usec);
        if (sp->prev_valid) {
            delta_tm.tv_sec = end_tm.tv_sec - sp->prev_tm.tv_sec;
            delta_tm.tv_usec = end_tm.tv_usec - sp->prev_tm.tv_usec;
            if (delta_tm.tv_usec < 0) {
                --delta_tm.tv_sec;
                delta_tm.tv_usec += 1000000;
            }
            da = delta_tm.tv_sec;
            da += (0.000001 * delta_tm.tv_usec);
        } else
            da = 0.0000001;
        db = 1.0;

        sp->prev_tm = end_tm;
        if (use_out_full) {
            b = (double)op->obs_lb * sp->out_full;
            if (sp->prev_valid)
                db = (double)op->obs_lb * (sp->out_full - sp->prev_count);
            sp->prev_count = sp->out_full;
        } else {
            b = (double)op->ibs_hold * sp->in_full;
            if (sp->prev_valid)
                db = (double)op->obs_lb * (sp->in_full - sp->prev_count);
            sp->prev_count = sp->in_full;
        }
        pr2serr("%stime to %s data%s: %d.%06d secs", leadin,
                (op->read1_or_transfer ? "read" : "transfer"),
                (contin ? " so far" : ""), (int)res_tm.tv_sec,
                (int)res_tm.tv_usec);
        r = 0.0;
        if ((a > 0.00001) && (b > 511)) {
            r = b / (a * 1000000.0);
            if (r < 1.0)
                pr2serr(" at %.1f KB/sec", r * 1000);
            else
                pr2serr(" at %.2f MB/sec", r);
        }
        if (sp->prev_valid) {
            if ((da > 0.00001) && (db > 511)) {
                dr = db / (da * 1000000.0);
                if (dr < 1.0)
                    pr2serr(" (delta %.1f KB/sec)", dr * 1000);
                else
                    pr2serr(" (delta %.2f MB/sec)", dr);
            }
        }
        pr2serr("\n");
        sp->prev_valid = true;

        if (contin && (! op->reading_fifo) && (r > 0.01) &&
            (op->dd_count > 100)) {
            secs = (int)(((double)op->ibs_hold * op->dd_count) /
                         (r * 1000000));
            if (secs > 10) {
                pr2serr("%s%d%% complete, ", leadin,
                        (100 * elapsed_secs) / (secs + elapsed_secs));
                h = secs / 3600;
                secs = secs - (h * 3600);
                m = secs / 60;
                secs = secs - (m * 60);
                if (h > 0)
                    pr2serr("estimated time remaining: %d:%02d:%02d\n",
                            h, m, secs);
                else
                    pr2serr("estimated time remaining: %d:%02d\n", m, secs);
            }
        }
    }
#else   /* no clock reading functions available */
    if (op) { ; }        // suppress warning
    if (leadin) { ; }    // suppress warning
    if (contin) { ; }    // suppress warning
#endif
}

/* Returns true when it time to output a progress report; else false. */
static bool
check_progress(const struct opts_t * op)
{
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    static bool have_prev, measure;
    static struct timespec prev_true_tm;
    static int count, threshold;
    bool res = false;
    uint32_t elapsed_ms, ms;
    struct timespec now_tm, res_tm;

    if (op->progress) {
        if (! have_prev) {
            have_prev = true;
            measure = true;
            clock_gettime(CLOCK_MONOTONIC, &prev_true_tm);
            return false;       /* starting reference */
        }
        if (! measure) {
            if (++count >= threshold)
                count = 0;
            else
                return false;
        }
        clock_gettime(CLOCK_MONOTONIC, &now_tm);
        res_tm.tv_sec = now_tm.tv_sec - prev_true_tm.tv_sec;
        res_tm.tv_nsec = now_tm.tv_nsec - prev_true_tm.tv_nsec;
        if (res_tm.tv_nsec < 0) {
            --res_tm.tv_sec;
            res_tm.tv_nsec += 1000000000;
        }
        elapsed_ms = (1000 * res_tm.tv_sec) + (res_tm.tv_nsec / 1000000);
        if (measure) {
            ++threshold;
            if (elapsed_ms > 80)        /* 80 milliseconds */
                measure = false;
        }
        if (elapsed_ms >= PROGRESS3_TRIGGER_MS) {
            if (elapsed_ms >= PROGRESS2_TRIGGER_MS) {
                if (elapsed_ms >= PROGRESS_TRIGGER_MS) {
                    ms = PROGRESS_TRIGGER_MS;
                    res = true;
                } else if (op->progress > 1) {
                    ms = PROGRESS2_TRIGGER_MS;
                    res = true;
                }
            } else if (op->progress > 2) {
                ms = PROGRESS3_TRIGGER_MS;
                res = true;
            }
        }
        if (res) {
            prev_true_tm.tv_sec += (ms / 1000);
            prev_true_tm.tv_nsec += (ms % 1000) * 1000000;
            if (prev_true_tm.tv_nsec >= 1000000000) {
                ++prev_true_tm.tv_sec;
                prev_true_tm.tv_nsec -= 1000000000;
            }
        }
    }
    return res;

#elif defined(HAVE_GETTIMEOFDAY)
    static bool have_prev, measure;
    static struct timeval prev_true_tm;
    static int count, threshold;
    bool res = false;
    uint32_t elapsed_ms, ms;
    struct timeval now_tm, res_tm;

    if (op->progress) {
        if (! have_prev) {
            have_prev = true;
            gettimeofday(&prev_true_tm, NULL);
            return false;       /* starting reference */
        }
        if (! measure) {
            if (++count >= threshold)
                count = 0;
            else
                return false;
        }
        gettimeofday(&now_tm, NULL);
        res_tm.tv_sec = now_tm.tv_sec - prev_true_tm.tv_sec;
        res_tm.tv_usec = now_tm.tv_usec - prev_true_tm.tv_usec;
        if (res_tm.tv_usec < 0) {
            --res_tm.tv_sec;
            res_tm.tv_usec += 1000000;
        }
        elapsed_ms = (1000 * res_tm.tv_sec) + (res_tm.tv_usec / 1000);
        if (measure) {
            ++threshold;
            if (elapsed_ms > 80)        /* 80 milliseconds */
                measure = false;
        }
        if (elapsed_ms >= PROGRESS3_TRIGGER_MS) {
            if (elapsed_ms >= PROGRESS2_TRIGGER_MS) {
                if (elapsed_ms >= PROGRESS_TRIGGER_MS) {
                    ms = PROGRESS_TRIGGER_MS;
                    res = true;
                } else if (op->progress > 1) {
                    ms = PROGRESS2_TRIGGER_MS;
                    res = true;
                }
            } else if (op->progress > 2) {
                ms = PROGRESS3_TRIGGER_MS;
                res = true;
            }
        }
        if (res) {
            prev_true_tm.tv_sec += (ms / 1000);
            prev_true_tm.tv_usec += (ms % 1000) * 1000;
            if (prev_true_tm.tv_usec >= 1000000) {
                ++prev_true_tm.tv_sec;
                prev_true_tm.tv_usec -= 1000000;
            }
        }
    }
    return res;

#else   /* no clock reading functions available */
    if (op) { ; }    // suppress warning
    return false;
#endif
}

/* Create errblk file (see iflag=errblk) and if we have gettimeofday
 * puts are start timestampl on the first line. */
void
errblk_open(struct opts_t * op)
{
    op->errblk_fp = fopen(errblk_file, "a");        /* append */
    if (NULL == op->errblk_fp)
        pr2serr("unable to open or create %s\n", errblk_file);
    else {
#ifdef HAVE_GETTIMEOFDAY
        {
            time_t t;
            char b[64];

            t = time(NULL);
            strftime(b, sizeof(b), "# start: %Y-%m-%d %H:%M:%S\n",
                     localtime(&t));
            fputs(b, op->errblk_fp);
        }
#else
        fputs("# start\n", op->errblk_fp);
#endif
    }
}

void    /* Global function, used by ddpt_pt.c */
errblk_put(uint64_t lba, struct opts_t * op)
{
    if (op->errblk_fp)
        fprintf(op->errblk_fp, "0x%" PRIx64 "\n", lba);
}

void    /* Global function, used by ddpt_pt.c */
errblk_put_range(uint64_t lba, int num, struct opts_t * op)
{
    if (op->errblk_fp) {
        if (1 == num)
            errblk_put(lba, op);
        else if (num > 1)
            fprintf(op->errblk_fp, "0x%" PRIx64 "-0x%" PRIx64 "\n", lba,
                    lba + (num - 1));
    }
}

void
errblk_close(struct opts_t * op)
{
    if (op->errblk_fp) {
#ifdef HAVE_GETTIMEOFDAY
        {
            time_t t;
            char b[64];

            t = time(NULL);
            strftime(b, sizeof(b), "# stop: %Y-%m-%d %H:%M:%S\n",
                     localtime(&t));
            fputs(b, op->errblk_fp);
        }
#else
        fputs("# stop\n", op->errblk_fp);
#endif
        fclose(op->errblk_fp);
        op->errblk_fp = NULL;
    }
}


#ifdef SG_LIB_LINUX

/* Summarise previous consecutive same-length reads. Do that when:
 * - read length (res) differs from the previous read length, and
 * - there were more than one consecutive reads of the same length
 * The str argument is a prefix string, typically one or two spaces, used
 * to e.g. make output line up when printing on kill -USR1. */
void
print_tape_summary(struct opts_t * op, int res, const char * str)
{
    int len = op->last_tape_read_len;
    int num = op->read_tape_numbytes;

    if ((op->verbose > 1) && (res != len) && (op->consec_same_len_reads >= 1))
        pr2serr("%s(%d%s read%s of %d byte%s)\n", str,
                op->consec_same_len_reads, (len < num) ? " short" : "",
                (op->consec_same_len_reads != 1) ? "s" : "", len,
                (len != 1) ? "s" : "");
}

static void
show_tape_pos_error(const char * postfix)
{
    pr2serr("Could not get tape position%s: %s\n", postfix,
            safe_strerror(errno));
}

/* Print tape position(s) if verbose > 1. If both reading from and writing to
 * tape, make clear in output which is which. Also only print the position if
 * necessary, i.e. not already printed.
 * Prefix argument is e.g. "Initial " or "Final ". */
void
print_tape_pos(const char * prefix, const char * postfix, struct opts_t * op)
{
    static bool lastreadposvalid = false;       /* <<< static autos */
    static bool lastwriteposvalid = false;
    static int lastreadpos, lastwritepos;

    int res;
    struct mtpos pos;

    if (op->verbose > 1) {
        if (FT_TAPE & op->idip->d_type) {
            res = ioctl(op->idip->fd, MTIOCPOS, &pos);
            if (0 == res) {
                if ((pos.mt_blkno != lastreadpos) ||
                    (! lastreadposvalid)) {
                    lastreadpos = pos.mt_blkno;
                    lastreadposvalid = true;
                    pr2serr("%stape position%s: %u%s\n", prefix,
                            (FT_TAPE & op->odip->d_type) ? " (reading)" : "",
                            lastreadpos, postfix);
                }
            } else {
                lastreadposvalid = false;
                show_tape_pos_error((FT_TAPE & op->odip->d_type) ?
                                    " (reading)" : "");
            }
        }

        if (FT_TAPE & op->odip->d_type) {
            res = ioctl(op->odip->fd, MTIOCPOS, &pos);
            if (0 == res) {
                if ((pos.mt_blkno != lastwritepos) ||
                    (! lastwriteposvalid)) {
                    lastwritepos = pos.mt_blkno;
                    lastwriteposvalid = true;
                    pr2serr("%stape position%s: %u%s\n", prefix,
                            (FT_TAPE & op->idip->d_type) ? " (writing)" : "",
                            lastwritepos, postfix);
                }
            } else {
                lastwriteposvalid = false;
                show_tape_pos_error((FT_TAPE & op->idip->d_type) ?
                                    " (writing)" : "");
            }
        }

    }
}

#endif  /* SG_LIB_LINUX */


/* The use of signals is borrowed from GNU's dd source code which is
 * found in their coreutils package. If SA_NOCLDSTOP is non-zero then
 * a modern Posix compliant version of signals is assumed. Still
 * thinking about SIGHUP which will be delivered if the controlling
 * process/terminal is terminated or receives SIGHUP. */

/* If nonzero, the value of the pending fatal signal.  */
static sig_atomic_t volatile interrupt_signal;

/* A count of pending info(usr1) signals, decremented as processed */
static sig_atomic_t volatile info_signals_pending;

static struct val_str_t signum_name_arr[] = {
    {SIGINT, "SIGINT"},
    {SIGQUIT, "SIGQUIT"},
    {SIGPIPE, "SIGPIPE"},
#if SIGINFO == SIGUSR1
    {SIGUSR1, "SIGUSR1"},
#else
    {SIGINFO, "SIGINFO"},
#endif
#ifndef SG_LIB_WIN32
    {SIGHUP, "SIGHUP"},
#endif
    {0, NULL},
};



/* Return signal name for signum if known, else return signum as a string. */
static const char *
get_signal_name(int signum, char * b, int blen)
{
    const struct val_str_t * sp;

    for (sp = signum_name_arr; sp->num; ++sp) {
        if (signum == sp->num)
            break;
    }
    if (blen < 1)
        return b;
    b[blen - 1] = '\0';
    if (sp->num)
        strncpy(b, sp->name, blen - 1);
    else
        snprintf(b, blen, "%d", signum);
    return b;
}

/* An ordinary signal was received; arrange for the program to exit.  */
static void
interrupt_handler(int sig)
{
    if (! SA_RESETHAND)
        signal(sig, SIG_DFL);
    interrupt_signal = sig;
}

/* An info signal was received; arrange for the program to print status.  */
static void
siginfo_handler(int sig)
{
    if (! SA_NOCLDSTOP)
        signal(sig, siginfo_handler);
    ++info_signals_pending;
}

/* Install the signal handlers. We try to cope gracefully with signals whose
 * disposition is 'ignored'. SUSv3 recommends that a process should start
 * with no blocked signals; if needed unblock SIGINFO, SIGINT or SIGPIPE.  */
void
install_signal_handlers(struct opts_t * op)
{
#if SIGINFO == SIGUSR1
    const char * sname = "SIGUSR1";
#else
    const char * sname = "SIGINFO";
#endif

    if (op->verbose > 2) {
        pr2serr(" >> %s signal implementation assumed "
                "[SA_NOCLDSTOP=%d],\n >>   will %smask out signals during "
                "IO\n", (SA_NOCLDSTOP ? "modern" : "old"), SA_NOCLDSTOP,
                (op->interrupt_io ? "not " : ""));
        if (! op->interrupt_io)
            pr2serr(" >>   then check for signals between IOs\n");
    }
#if SA_NOCLDSTOP
    bool unblock_starting_mask = false;
    int num_members = 0;
    struct sigaction act;
    sigset_t starting_mask;

    sigemptyset(&op->caught_signals);
    sigemptyset(&op->orig_mask);
    sigaction(SIGINFO, NULL, &act);
    if (act.sa_handler != SIG_IGN)
        sigaddset(&op->caught_signals, SIGINFO);
    else if (op->verbose)
        pr2serr("%s ignored, progress reports not available\n", sname);
    sigaction(SIGINT, NULL, &act);
    if (act.sa_handler != SIG_IGN)
        sigaddset(&op->caught_signals, SIGINT);
    else if (op->verbose)
        pr2serr("SIGINT ignored\n");
    sigaction(SIGPIPE, NULL, &act);
    if (act.sa_handler != SIG_IGN)
        sigaddset(&op->caught_signals, SIGPIPE);
    else if (op->verbose)
        pr2serr("SIGPIPE ignored\n");

    sigprocmask(SIG_UNBLOCK /* ignored */, NULL, &starting_mask);
    if (sigismember(&starting_mask, SIGINFO)) {
        if (op->verbose)
            pr2serr("%s blocked on entry, unblock\n", sname);
        unblock_starting_mask = true;
    }
    if (sigismember(&starting_mask, SIGINT)) {
        if (op->verbose)
            pr2serr("SIGINT blocked on entry, unblock\n");
        unblock_starting_mask = true;
    }
    if (sigismember(&starting_mask, SIGPIPE)) {
        if (op->verbose)
            pr2serr("SIGPIPE blocked on entry, unblock\n");
        unblock_starting_mask = true;
    }
    act.sa_mask = op->caught_signals;

    if (sigismember(&op->caught_signals, SIGINFO)) {
        act.sa_handler = siginfo_handler;
        act.sa_flags = 0;
        sigaction(SIGINFO, &act, NULL);
        ++num_members;
    }

    if (sigismember(&op->caught_signals, SIGINT)) {
        act.sa_handler = interrupt_handler;
        act.sa_flags = SA_NODEFER | SA_RESETHAND;
        sigaction(SIGINT, &act, NULL);
        ++num_members;
    }

    if (sigismember(&op->caught_signals, SIGPIPE)) {
        act.sa_handler = interrupt_handler;
        act.sa_flags = SA_NODEFER | SA_RESETHAND;
        sigaction(SIGPIPE, &act, NULL);
        ++num_members;
    }
    if (unblock_starting_mask)
        sigprocmask(SIG_UNBLOCK, &op->caught_signals, NULL);

    if ((0 == op->interrupt_io) && (num_members > 0))
        sigprocmask(SIG_BLOCK, &op->caught_signals, &op->orig_mask);
#else   /* not SA_NOCLDSTOP */
    if (op) { ; }    /* suppress warning */
    if (signal(SIGINFO, SIG_IGN) != SIG_IGN) {
        signal(SIGINFO, siginfo_handler);
        siginterrupt(SIGINFO, 1);
    } else if (op->verbose)
        pr2serr("old %s ignored, progress report not available\n", sname);
    if (signal(SIGINT, SIG_IGN) != SIG_IGN) {
        signal(SIGINT, interrupt_handler);
        siginterrupt(SIGINT, 1);
    } else if (op->verbose)
        pr2serr("old SIGINT ignored\n");
#endif  /* SA_NOCLDSTOP */
}

/* Process any pending signals and perhaps do delay. If signals are caught,
 * this function should be called periodically.  Ideally there should never
 * be an unbounded amount of time when signals are not being processed. */
void    /* Global function, used by ddpt.c and ddpt_xcopy.c */
signals_process_delay(struct opts_t * op, int delay_type)
{
    bool got_something = false;
    char b[32];
    int delay = 0;

#if SA_NOCLDSTOP
    bool found_pending = false;

    if ((0 == op->interrupt_io) &&
        (sigismember(&op->caught_signals, SIGINT) ||
         sigismember(&op->caught_signals, SIGPIPE) ||
         sigismember(&op->caught_signals, SIGINFO))) {
        sigset_t pending_set;

        sigpending(&pending_set);
        if (sigismember(&pending_set, SIGINT) ||
            sigismember(&pending_set, SIGPIPE) ||
            sigismember(&pending_set, SIGINFO)) {
            /* Signal handler for a pending signal run during suspend */
            sigsuspend(&op->orig_mask);
            found_pending = true;
        } else {        /* nothing pending so perhaps delay */
            if ((op->delay > 0) && (DELAY_COPY_SEGMENT == delay_type))
                delay = op->delay;
            else if ((op->wdelay > 0) && (DELAY_WRITE == delay_type)) {
                if (op->subsequent_wdelay)
                    delay = op->wdelay;
                else
                    op->subsequent_wdelay = true;
            }
            if (op->progress && (DELAY_COPY_SEGMENT == delay_type)) {
                if (check_progress(op)) {
                    calc_duration_throughput("", true, op);
                    if (op->verbose)
                        print_stats("", op, 0 /* both in and out */, false);
                }
            }
            if (delay) {
                sigprocmask(SIG_SETMASK, &op->orig_mask, NULL);
                if (op->verbose > 3)
                    pr2serr("delay=%d milliseconds [%s]\n", delay,
                            ((DELAY_WRITE == delay_type) ? "write" : "copy"));
                sleep_ms(delay);
                sigprocmask(SIG_BLOCK, &op->caught_signals, NULL);
            }
            if (! (interrupt_signal || info_signals_pending))
                return;
        }
    }
#endif

    while (interrupt_signal || info_signals_pending) {
        int interrupt;
        int infos;

        got_something = true;
#if SA_NOCLDSTOP
        if (! found_pending)
            sigprocmask(SIG_BLOCK, &op->caught_signals, NULL);
#endif

        /* Reload interrupt_signal and info_signals_pending, in case a new
           signal was handled before sigprocmask took effect.  */
        interrupt = interrupt_signal;
        infos = info_signals_pending;

        if (infos)
            info_signals_pending = infos - 1;

#if SA_NOCLDSTOP
        if (! found_pending)
            sigprocmask(SIG_SETMASK, &op->orig_mask, NULL);
#endif

        if (interrupt) {
            pr2serr("Interrupted by signal %s\n",
                    get_signal_name(interrupt, b, sizeof(b)));
            print_stats("", op, 0 /* both in and out */, true);
            /* Don't show next message if using oflag=pre-alloc and we didn't
             * use FALLOC_FL_KEEP_SIZE */
            if ((! op->reading_fifo) && (FT_REG & op->odip->d_type_hold)
                 && (0 == op->oflagp->prealloc)) {
                if (op->oflagp->strunc) {
                    int64_t osize = get_low_lba_from_linear(&op->o_sgli);

                    if (osize > 0) {
                        osize *= op->obs_pi;
                        pr2serr(">> Did not perform: "
                                "'truncate --size=%" PRId64 " %s'\n",
                                osize, op->odip->fn);
                        pr2serr(">> User should check and perform by hand "
                                "if necessary\n");
                    }
                }
                pr2serr("To resume, invoke with same arguments plus "
                        "oflag=resume\n");
            }
            // Could more cleanup or suggestions be made here?
        } else {
            pr2serr("Progress report:\n");
            print_stats("  ", op, 0, true);
            if (op->do_time)
                calc_duration_throughput("  ", true /* contin */, op);
            if (op->progress && (op->verbose < 2) && (! op->quiet)) {
                pr2serr("Toggling verbose %s for %s progress information\n",
                        (op->verbose ? "off" : "on"),
                        (op->verbose ? "less" : "more"));
                op->verbose ^= 1;
                pr2serr("Send this signal again if this is unwanted and it "
                        "will toggle back\n");
            }
            pr2serr("  continuing ...\n");
        }
        if (interrupt) {
#if SA_NOCLDSTOP
            if (found_pending) {
                sigset_t int_set;

                sigemptyset(&int_set);
                sigaddset(&int_set, interrupt);
                sigprocmask(SIG_UNBLOCK, &int_set, NULL);
            }
#endif
            raise(interrupt);
        }
    }

    if (! got_something) {
        delay = 0;
        if ((op->delay > 0) && (DELAY_COPY_SEGMENT == delay_type))
            delay = op->delay;
        else if ((op->wdelay > 0) && (DELAY_WRITE == delay_type)) {
            if (op->subsequent_wdelay)
                delay = op->wdelay;
            else
                op->subsequent_wdelay = true;
        }
        if (op->progress && (DELAY_COPY_SEGMENT == delay_type)) {
            if (check_progress(op)) {
                calc_duration_throughput("", true, op);
                if (op->verbose)
                    print_stats("", op, 0 /* both in and out */, false);
            }
        }
        if (delay)
            sleep_ms(op->delay);
    }
}

void
decode_designation_descriptor(const uint8_t * bp, int len_less_4,
                              bool to_stderr, int verb)
{
    int (*print_p)(const char *, ...);
    char b[2048];

    print_p = to_stderr ? pr2serr : printf;
    sg_get_designation_descriptor_str(NULL, bp, len_less_4 + 4, verb,
                                      verb, sizeof(b), b);
    print_p("%s", b);
}

/* Helper for case when EIO or EREMOTE errno suggests the equivalent
 * of a medium error. Returns 0 unless coe_limit exceeded. */
int             /* Global function, used by ddpt_win32.c */
coe_process_eio(struct opts_t * op, int64_t skip)
{
    struct cp_statistics_t * sp = op->stp ? op->stp : &op->stats;

    if ((op->coe_limit > 0) && (++op->coe_count > op->coe_limit)) {
        pr2serr(">> coe_limit on consecutive reads "
                "exceeded\n");
        return SG_LIB_CAT_MEDIUM_HARD;
    }
    if (op->highest_unrecovered < 0) {
        op->highest_unrecovered = skip;
        op->lowest_unrecovered = skip;
    } else {
        if (skip < op->lowest_unrecovered)
            op->lowest_unrecovered = skip;
        if (skip > op->highest_unrecovered)
            op->highest_unrecovered = skip;
    }
    ++sp->unrecovered_errs;
    ++sp->in_partial;
    --sp->in_full;
    pr2serr(">> unrecovered read error at blk=%" PRId64 ", "
            "substitute zeros\n", skip);
    return 0;
}

char *
rod_type_str(uint32_t rt, char * b, int b_mlen)
{
    bool got_pit = false;       /* Point In Time copy */
    const char * cp = NULL;
    const char * pitp = "point in time copy - ";

    switch (rt) {
    case RODT_CM_INTERNAL:
        cp = "copy manager internal";
        break;
    case RODT_ACCESS_ON_REF:
        cp = "access upon reference";
        break;
    case RODT_PIT_DEF:
        cp = "default";
        got_pit = true;
        break;
    case RODT_PIT_VULN:
        cp = "change vulnerable";
        got_pit = true;
        break;
    case RODT_PIT_PERS:
        cp = "persistent";
        got_pit = true;
        break;
    case RODT_PIT_COW:
        cp = "copy on write";
        got_pit = true;
        break;
    case RODT_PIT_ANY:
        cp = "any";
        got_pit = true;
        break;
    case RODT_BLK_ZERO:
        cp = "block device zero";
        break;
    default:
        if (rt >= 0xfffffff0)
            cp = "vendor specific";
        else if (rt >= 0xff000000)
            cp = "device type specific";
        else
            cp = "reserved";
        break;
    }
    if (cp)
        snprintf(b, b_mlen, "%s%s [0x%" PRIx32 "]", (got_pit ? pitp : ""),
                 cp, rt);
    else
        snprintf(b, b_mlen, "0x%" PRIx32 "", rt);
    return b;
}

char *
rt_cm_id_str(const uint8_t * rtp, int rt_len, char * b, int b_mlen)
{
    int m, num;

    if (rt_len < 16)
        snprintf(b, b_mlen, "ROD Token too short (%d < 16)\n", rt_len);
    num = 0;
    num += sg_scnpr(b, b_mlen, "0x");
    for (m = 0; ((m < 8) && (num < b_mlen)); ++m)
        num += sg_scnpr(b + num, b_mlen - num, "%02x",
                        (unsigned int)rtp[8 + m]);
    return b;
}

void
print_exit_status_msg(const char * prefix, int exit_stat, bool to_stderr)
{
    int (*print_p)(const char *, ...);
    int n;
    char b[256];
    const int b_len = sizeof(b);

    if (0 == exit_stat)
        return;
    print_p = to_stderr ? pr2serr : printf;
    if (prefix)
        snprintf(b, b_len, "%s: ", prefix);
    else
        b[0] = '\0';
    n = (int)strlen(b);
    if (n >= (b_len - 1))
        return;         /* prefix too big for internal buffer */

    if (exit_stat < 0) {
        print_p("%sunexpected negative exit status value: %d\n", b,
                 exit_stat);
        return;
    }
    if (exit_stat > 0) {
        if (exit_stat < 126) {
            if (sg_exit2str(exit_stat, false, b_len - n, b + n))
                print_p("%s\n", b);
            else
                print_p("%sunexpected exit status value: %d\n", b, exit_stat);
        } else
            print_p("%sunexpectedly high exit status value: %d\n", b,
                    exit_stat);
    }
}

/* Return minimum(num_blks, <blocks_from_sgl-post-skip_blks>). First it
 * starts skipping skip_blks blocks and if elems is exceeded then it
 * returns 0. Then it sums up the number of blocks from each subsequent
 * sg element checking that elems and max_descriptors are not exceeded. It
 * also stops counting if that sum exceeds num_blks. If max_descriptors is
 * 0 then it is not constraining. Note that elems and skip_blks are relative
 * to the start of the sgl; while num_blks and max_descriptors are relative
 * to the sgl+skip_blks . */
uint64_t
count_sgl_blocks_from(const struct scat_gath_elem * sglp, int elems,
                      uint64_t skip_blks, uint32_t num_blks,
                      uint32_t max_descriptors /* from skip_blks */)
{
    int k, j, md;
    uint64_t res;

    if ((0 == max_descriptors) || (max_descriptors > INT_MAX))
        md = INT_MAX;
    else
        md = (int)max_descriptors;

    for (k = 0; k < elems; ++k, ++sglp) {
        if ((uint64_t)sglp->num < skip_blks)
            skip_blks -= sglp->num;
        else
            break;
    }
    if (k >= elems)
        return 0;

    for (j = 0, res = 0;
         (k < elems) && (j < md) && (res < (uint64_t)num_blks);
         ++k, ++j, ++sglp) {
        if (0 == j)
            res = (uint64_t)sglp->num - skip_blks;  /* will be positive */
        else
            res += (uint64_t)sglp->num;
    }
    return (res < (uint64_t)num_blks) ? res : (uint64_t)num_blks;
}

/* Points to start of sgl after init, sets extend_last bit */
void
sgl_iter_init(struct sgl_iter_t * iter_p, struct scat_gath_elem * sglp,
              int elems)
{
    if ((NULL == iter_p) || (elems < 0))
        return;
    memset(iter_p, 0, sizeof(struct sgl_iter_t));
    iter_p->sglp = sglp;
    iter_p->elems = elems;
    if (sglp && (elems > 0))
        iter_p->extend_last = (0 == (sglp + (elems - 1))->num);
}

/* Given a blk_count, the iterator (*iter_p) is moved toward the EOL. If
 * relative is true the move is from the current position of the iterator. If
 * relative is false then the move is from the start of the sgl. The
 * sgl_iter_add(itp, 0, false) call sets the iterator to the start of the sgl.
 * Returns true unless blk_count takes iterator two or more past the last
 * element. So if blk_count takes the iterator to the EOL, this function
 * returns true. Takes into account iterator's extend_last flag. */
bool
sgl_iter_add(struct sgl_iter_t * iter_p, uint64_t blk_count, bool relative)
{
    bool first, extend_last;
    int k = relative ? iter_p->it_e_ind : 0;
    int elems = iter_p->elems;
    uint32_t num;
    uint64_t bc = 0;
    const struct scat_gath_elem * sglp = iter_p->sglp + k;

    if (0 == blk_count)
        return true;
    extend_last = iter_p->extend_last;
    for (first = true; k < elems; ++k, ++sglp, first = false) {
        num = (extend_last && (k == (elems - 1))) ? (INT32_MAX - 1) :
                                                    sglp->num;
        if (first && relative) {
            bc = blk_count + iter_p->it_bk_off;
            if ((uint64_t)num < bc)
                blk_count -= (num - iter_p->it_bk_off);
            else
                break;
        } else {
            bc = blk_count;
            if ((uint64_t)num < bc)
                blk_count -= num;
            else
                break;
        }
    }
    iter_p->it_e_ind = k;
    iter_p->it_bk_off = (uint32_t)bc;
    if (k < elems)
        return true;
    else if ((k == elems) && (0 == bc))
        return true;    /* EOL */
    else
        return false;
}

/* Move the iterator from its current position (which may be to EOL) towards
 * the start of the sgl (i.e. backwards) for blk_count blocks. Returns true
 * if iterator is valid after the move, else returns false. N.B. if false is
 * returned, then the iterator is invalid and may need to set it to a valid
 * value. */
bool
sgl_iter_sub(struct sgl_iter_t * iter_p, uint64_t blk_count)
{
    bool first;
    int k = iter_p->it_e_ind;
    uint64_t bc = 0;
    const struct scat_gath_elem * sglp = iter_p->sglp + k;

    if (0 == blk_count)
        return true;
    for (first = true; k >= 0; --k, --sglp) {
        if (first) {
            if (blk_count > (uint64_t)iter_p->it_bk_off)
                blk_count -= iter_p->it_bk_off;
            else {
                iter_p->it_bk_off -= blk_count;
                break;
            }
            first = false;
        } else {
            bc = blk_count;
            if (bc > (uint64_t)sglp->num)
                blk_count -= sglp->num;
            else {
                bc = sglp->num - bc;
                break;
            }
        }
    }
    if (k < 0)
        return false;
    iter_p->it_e_ind = k;
    if (! first)
        iter_p->it_bk_off = (uint32_t)bc;
    return true;
}

/* Read numbers (up to 64 bits in size) from command line (comma (or
 * (single) space **) separated list). Assumed decimal unless prefixed
 * by '0x', '0X' or contains trailing 'h' or 'H' (which indicate hex).
 * Returns 0 if ok, or 1 if error. Assumed to be LBA (64 bit) and
 * number_of_block (32 bit) pairs. ** Space on command line needs to
 * be escaped, otherwise it is an operand/option separator. */
struct scat_gath_elem *
cl2sgl(const char * inp, int * arr_elemsp, bool b_vb)
{
    bool split, full_pair;
    int in_len, k, j, n;
    const int max_nbs = INT32_MAX - 1;  /* 2**31 - 2; leaving headroom */
    int64_t ll, large_num;
    uint64_t prev_lba;
    char * cp;
    char * c2p;
    const char * lcp;
    struct scat_gath_elem * res_p = NULL;
    struct scat_gath_elem * sge_p;

    if ((NULL == inp) || (NULL == arr_elemsp)) {
        if (b_vb)
            pr2serr("%s: bad arguments\n", __func__);
        return NULL;
    }
    n = MAX_FIXED_SGL_ELEMS;
    res_p = (struct scat_gath_elem *)calloc(n, sizeof(struct scat_gath_elem));
    lcp = inp;
    in_len = strlen(inp);
    if (0 == in_len)
        *arr_elemsp = 0;
    if ('-' == inp[0]) {        /* read from stdin */
        pr2serr("%s: logic error: no stdin here\n", __func__);
        goto err_out;
    } else {        /* list of numbers (default decimal) on command line */
        k = strspn(inp, "0123456789aAbBcCdDeEfFhHxXiIkKmMgGtTpP, ");
        if (in_len != k) {
            if (b_vb)
                pr2serr("%s: error at pos %d\n", __func__, k + 1);
            goto err_out;
        }
        j = 0;
        full_pair = true;
        for (k = 0, sge_p = res_p, split = false; k < n; ++k, ++sge_p) {
            if (split) {
                /* splitting given elem with large number_of_blocks into
                 * multiple elems within array being built */
                ++j;
                sge_p->lba = prev_lba + (uint64_t)max_nbs;
                if (large_num > max_nbs) {
                    sge_p->num = (uint32_t)max_nbs;
                    prev_lba = sge_p->lba;
                    large_num -= max_nbs;
                } else {
                    sge_p->num = (uint32_t)large_num;
                    split = false;
                    if (b_vb)
                        pr2serr("%s: split large sg elem into %d elements\n",
                                __func__, j);
                    goto check_for_next;
                }
                continue;
            }
            full_pair = false;
            ll = sg_get_llnum(lcp);
            if (-1 != ll) {
                sge_p->lba = (uint64_t)ll;
                cp = (char *)strchr(lcp, ',');
                c2p = (char *)strchr(lcp, ' ');
                if (NULL == cp) {
                    cp = c2p;
                    if (NULL == cp)
                        break;
                }
                if (c2p && (c2p < cp))
                    cp = c2p;
                lcp = cp + 1;
            } else {
                if (b_vb)
                    pr2serr("%s: error at pos %d\n", __func__,
                            (int)(lcp - inp + 1));
                goto err_out;
            }
            ll = sg_get_llnum(lcp);
            if (ll >= 0) {
                full_pair = true;
                if (ll > max_nbs) {
                    sge_p->num = (uint32_t)max_nbs;
                    prev_lba = sge_p->lba;
                    large_num = ll - max_nbs;
                    split = true;
                    j = 1;
                    continue;
                }
                sge_p->num = (uint32_t)ll;
            } else {    /* bad or negative number as number_of_blocks */
                if (b_vb)
                    pr2serr("%s: bad number at pos %d\n", __func__,
                            (int)(lcp - inp + 1));
                goto err_out;
            }
check_for_next:
            cp = (char *)strchr(lcp, ',');
            c2p = (char *)strchr(lcp, ' ');
            if (NULL == cp) {
                cp = c2p;
                if (NULL == cp)
                    break;
            }
            if (c2p && (c2p < cp))
                cp = c2p;
            lcp = cp + 1;
        }       /* end of for loop over items in operand */
        /* other than first pair, expect even number of items */
        if ((k > 0) && (! full_pair)) {
            if (b_vb)
                pr2serr("%s:  expected even number of items: "
                        "LBA0,NUM0,LBA1,NUM1...\n", __func__);
            goto err_out;
        }
        *arr_elemsp = k + 1;
        if (k == n) {
            if (b_vb)
                pr2serr("%s: array length exceeded\n", __func__);
            goto err_out;
        }
    }
    return res_p;
err_out:
    if (res_p)
        free(res_p);
    return NULL;
}

static int
file2sgl_helper(FILE * fp, const char * fnp, bool def_hex, bool flexible,
                bool real_scan, int max_elems, struct scat_gath_elem * res_p,
                int * errp, bool b_vb)
{
    bool bit0;
    bool pre_addr1 = true;
    bool pre_hex_seen = false;
    int in_len, k, j, m, ind;
    const int max_nbs = INT32_MAX - 1;  /* 2**31 - 2; leaving headroom */
    int off = 0;
    int64_t ll;
    uint64_t ull, prev_lba;
    char * lcp;
    struct scat_gath_elem * sge_p;
    char line[1024];

    for (j = 0, sge_p = res_p; ; ++j) {
        if (NULL == fgets(line, sizeof(line), fp))
            break;
        // could improve with carry_over logic if sizeof(line) too small
        in_len = strlen(line);
        if (in_len > 0) {
            if ('\n' == line[in_len - 1]) {
                --in_len;
                line[in_len] = '\0';
            } else {
                *errp = SG_LIB_SYNTAX_ERROR;
                if (b_vb)
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
             * is one _before_ any LBA,NUM lines in the file. This allows
             * HEX marked sgls to be concaternated together. */
            if (('H' == toupper(lcp[0])) && ('E' == toupper(lcp[1])) &&
                ('X' == toupper(lcp[2]))) {
                pre_hex_seen = true;
                if (def_hex)
                    continue; /* bypass 'HEX' marker line if expecting hex */
                else {
                    if (flexible) {
                        def_hex = true; /* okay, switch to hex parse */
                        continue;
                    } else {
                        pr2serr("%s: %s: 'hex' string detected on line %d, "
                                "expecting decimal\n", __func__, fnp, j + 1);
                        *errp = SG_LIB_SYNTAX_ERROR;
                        goto err_out;
                    }
                }
            }
        }
        k = strspn(lcp, "0123456789aAbBcCdDeEfFhHxXbBdDiIkKmMgGtTpP, \t");
        if ((k < in_len) && ('#' != lcp[k])) {
            *errp = SG_LIB_SYNTAX_ERROR;
            if (b_vb)
                pr2serr("%s: %s: syntax error at line %d, pos %d\n",
                        __func__, fnp, j + 1, m + k + 1);
            goto err_out;
        }
        for (k = 0; k < 256; ++k) {
            /* limit parseable items on one line to 256 */
            if (def_hex) {      /* don't accept negatives or multipliers */
                if (1 == sscanf(lcp, "%" SCNx64, &ull))
                    ll = (int64_t)ull;
                else
                    ll = -1;    /* use (2**64 - 1) as error flag */
            } else
                ll = sg_get_llnum(lcp);
            if (-1 != ll) {
                ind = ((off + k) >> 1);
                bit0 = !! (0x1 & (off + k));
                if ((max_elems > 0) && (ind >= max_elems)) {
                    *errp = SG_LIB_SYNTAX_ERROR;
                    if (b_vb)
                        pr2serr("%s: %s: array length exceeded\n", __func__,
                                fnp);
                    goto err_out;
                }
                if (bit0) {     /* bit0 set when decoding a NUM */
                    if (ll < 0) {
                        *errp = SG_LIB_SYNTAX_ERROR;
                        if (b_vb)
                            pr2serr("%s: %s: bad number in line %d, at pos "
                                    "%d\n", __func__, fnp, j + 1,
                                    (int)(lcp - line + 1));
                        goto err_out;
                    }
                    if (ll > max_nbs) {
                        int h = 1;

                        /* split up this elem into multiple, smaller elems */
                        do {
                            if (real_scan) {
                                sge_p->num = (uint32_t)max_nbs;
                                prev_lba = sge_p->lba;
                                ++sge_p;
                                sge_p->lba = prev_lba + (uint64_t)max_nbs;
                                ++h;
                            }
                            off += 2;
                            ll -= max_nbs;
                        } while (ll > max_nbs);
                        if (b_vb && real_scan)
                            pr2serr("%s: split large sg elem into %d "
                                    "elements\n", __func__, h);
                    }
                    if (real_scan) {
                        sge_p->num = (uint32_t)ll;
                        ++sge_p;
                    }
                } else {        /* bit0 clear when decoding a LBA */
                    if (pre_addr1)
                        pre_addr1 = false;
                    if (real_scan)
                        sge_p->lba = (uint64_t)ll;
                }
            } else {    /* failed to decode number on line */
                if ('#' == *lcp) { /* numbers before #, rest of line comment */
                    --k;
                    break;      /* goes to next line */
                }
                *errp = SG_LIB_SYNTAX_ERROR;
                if (b_vb)
                    pr2serr("%s: %s: error in line %d, at pos %d\n",
                            __func__, fnp, j + 1, (int)(lcp - line + 1));
                goto err_out;
            }
            lcp = strpbrk(lcp, " ,\t#");
            if ((NULL == lcp) || ('#' == *lcp))
                break;
            lcp += strspn(lcp, " ,\t");
            if ('\0' == *lcp)
                break;
        }       /* <<< end of for(k < 256) loop */
        off += (k + 1);
    }   /* <<< end of for(sge_p = res_p) loop, one iteration per line */
    /* allow one items, but not higher odd number of items */
    if ((off > 1) && (0x1 & off)) {
        *errp = SG_LIB_SYNTAX_ERROR;
        if (b_vb)
            pr2serr("%s: %s: expect even number of items: "
                    "LBA0,NUM0,LBA1,NUM1...\n", __func__, fnp);
        goto err_out;
    }
    clearerr(fp);    /* even EOF on first pass needs this before rescan */
    if (! real_scan)
        rewind(fp);
    return (1 == off) ? 1 : (off >> 1);
err_out:
    clearerr(fp);
    if (! real_scan)
        rewind(fp);
    return -1;
}

/* Read numbers from filename (or stdin), line by line (comma (or (single)
 * space) separated list); places starting_LBA,number_of_block pairs in an
 * array of scat_gath_elem elements pointed to by the returned value. If
 * this fails NULL is returned and an error number is written to errp (if it
 * is non-NULL). Assumed decimal (and may have suffix multipliers) when
 * def_hex==false; if a number is prefixed by '0x', '0X' or contains trailing
 * 'h' or 'H' that denotes a hex number. When def_hex==true all numbers are
 * assumed to be hex (ignored '0x' prefixes and 'h' suffixes) and multiplers
 * are not permitted. Heap allocates an array just big enough to hold all
 * elements if the file is countable. Pipes and stdin are not considered
 * countable. In the non-countable case an array of MAX_FIXED_SGL_ELEMS
 * elements is pre-allocated; if it is exceeded sg_convert_errno(EDOM) is
 * placed in *errp (if it is non-NULL). One of the first actions is to write
 * 0 to *errp (if it is non-NULL) so the caller does not need to zero it
 * before calling. */
struct scat_gath_elem *
file2sgl(const char * file_name, bool def_hex, bool flexible,
         int * arr_elemsp, int * errp, bool b_vb)
{
    bool have_stdin;
    bool countable = true;
    int m, n, err, err_dummy;
    FILE * fp;
    const char * fnp;
    struct stat a_stat;
    struct scat_gath_elem * res_p = NULL;
    struct scat_gath_elem sge_dummy;

    if (errp)
        *errp = 0;
    else
        errp = &err_dummy;
    if (arr_elemsp)
        *arr_elemsp = 0;
    have_stdin = ((1 == strlen(file_name)) && ('-' == file_name[0]));
    if (have_stdin) {
        fp = stdin;
        fnp = "<stdin>";
    } else {
        fnp = file_name;
        if (stat(fnp, &a_stat) < 0) {
            err = errno;
            *errp = sg_convert_errno(err);
            if (b_vb)
                pr2serr("%s: %s: %s\n", __func__, fnp, safe_strerror(err));
            return NULL;
        }
#ifdef SG_LIB_MINGW
        /* MinGW doesn't have S_ISSOCK */
        if (S_ISDIR(a_stat.st_mode) || S_ISCHR(a_stat.st_mode)) {
            if (b_vb)
                pr2serr("%s: %s unsuitable (directory ?)\n", __func__, fnp);
            *errp = sg_convert_errno(EBADF);
            return NULL;
        } else if (S_ISFIFO(a_stat.st_mode))
            countable = false;
#else
        if (S_ISDIR(a_stat.st_mode) || S_ISCHR(a_stat.st_mode) ||
            S_ISSOCK(a_stat.st_mode)) {
            if (b_vb)
                pr2serr("%s: %s unsuitable (directory ?)\n", __func__, fnp);
            *errp = sg_convert_errno(EBADF);
            return NULL;
        } else if (S_ISFIFO(a_stat.st_mode) || S_ISSOCK(a_stat.st_mode))
            countable = false;
#endif
        fp = fopen(fnp, "r");
        if (NULL == fp) {
            err = errno;
            *errp = sg_convert_errno(err);
            if (b_vb)
                pr2serr("%s: opening %s: %s\n", __func__, fnp,
                        safe_strerror(err));
            return NULL;
        }
    }
    if (countable) {
        n = file2sgl_helper(fp, fnp, def_hex, flexible, false, 0, &sge_dummy,
                            errp, b_vb);
        if (n < 0)
            goto err_out;
    } else
        n = MAX_FIXED_SGL_ELEMS;
    m = n;

    if (n <= 0) {
        if (b_vb)
            pr2serr("%s: unable to decode any sgl elements from %s\n",
                    __func__, fnp);
        *errp = SG_LIB_SYNTAX_ERROR;
        goto err_out;
    }
    res_p = (struct scat_gath_elem *)calloc(n, sizeof(struct scat_gath_elem));
    if (NULL == res_p) {
        *errp = sg_convert_errno(ENOMEM);
        if (b_vb)
            pr2serr("%s: calloc: %s\n", __func__, safe_strerror(ENOMEM));
        return NULL;
    }
    n = file2sgl_helper(fp, fnp, def_hex, flexible, true, m, res_p, errp,
                        b_vb);
    if (countable) {
        if (m != n) {
            pr2serr("%s: first pass found %d pairs, second one found %d "
                    "pairs?\n", __func__, m, n);
            goto err_out;
        }
    }
    if (arr_elemsp)
        *arr_elemsp = n;
    if (! have_stdin)
        fclose(fp);
    return res_p;

err_out:
    free(res_p);
    if (! have_stdin)
        fclose(fp);
    return NULL;
}

/* Returns the number of times 'ch' is found in string 's' given the
 * string's length. */
int
num_chs_in_str(const char * s, int slen, int ch)
{
    int res = 0;

    while (--slen >= 0) {
        if (ch == s[slen])
            ++res;
    }
    return res;
}

/* Returns the number of times either 'ch1' or 'ch2' is found in
 * string 's' given the string's length. */
int
num_either_ch_in_str(const char * s, int slen, int ch1, int ch2)
{
    int k;
    int res = 0;

    while (--slen >= 0) {
        k = s[slen];
        if ((ch1 == k) || (ch2 == k))
            ++res;
    }
    return res;
}

static int
sgl_iter_forward_blks(struct dev_info_t * dip, struct sgl_iter_t * itp,
                      uint32_t n_blks, struct cp_state_t * csp, ddpt_rw_f fp,
                      struct opts_t * op)
{
    bool more, extend_last, on_last;
    bool in_side = (DDPT_ARG_IN == dip->ddpt_arg);
    int b_off = 0;
    int elems = itp->elems;
    int res = 0;
    int vb = op->verbose;
    uint32_t rem_blks, num;
    uint64_t off, lba;
    int64_t ablocks = n_blks;
    const struct scat_gath_elem * sgl_p = itp->sglp + itp->it_e_ind;

    extend_last = itp->extend_last;
    on_last = (extend_last && (itp->it_e_ind == (elems - 1)));
    while (ablocks > 0) {
        off = itp->it_bk_off + ablocks;
        lba = sgl_p->lba;
        num = on_last ? INT32_MAX - 1 : sgl_p->num;
        more = (off >= num);
        if (vb > 2)
            pr2serr("%s: [%s] off=%" PRId64 ", ablocks=%" PRId64 "  <<%s>>\n",
                    __func__, dip->dir_n, off, ablocks,
                    (more ? "more" : "last"));
        if (itp->it_e_ind >= elems) {
            if ((itp->it_e_ind > elems) || (off > 0)) {
                if (vb)
                    pr2serr("%s: [%s] incrementing iterator (%d,%u) past "
                            "end\n", __func__, dip->dir_n, itp->it_e_ind,
                            itp->it_bk_off);
                return -9999;
            }
        }
        if (more) {             /* more elements after this */
            rem_blks = num - itp->it_bk_off;    /* rhs must be >= 0 */
            if ((rem_blks > 0) && fp) {
                if (vb > 2)
                    pr2serr("  ... %s%s LBA=0x%" PRIx64 ", num_blks=%u, "
                            "b_off=%d\n", (csp->reading ? "<<<" : ">>>"),
                            ((csp->reading && (! in_side)) ? "[out]" : ""),
                            lba + itp->it_bk_off, rem_blks, b_off);
                if (in_side) {
                    csp->cur_in_lba = lba + itp->it_bk_off;
                    csp->cur_in_num = rem_blks;
                } else {
                    csp->cur_out_lba = lba + itp->it_bk_off;
                    csp->cur_out_num = rem_blks;
                }
                csp->cur_bp = csp->subseg_bp + b_off;
                /*   vvvvvvvvvvvvvvvvvvvvvv  indirect call to workers */
                if ((res = fp(dip, csp, op)))
                    return res;
                if (csp->blks_xfer < (int)rem_blks) {
                    if (vb) {
                        pr2serr("%s(more): [%s] shorted, got %d blocks",
                                __func__, dip->dir_n, csp->blks_xfer);
                        if (csp->bytes_xfer > 0)
                            pr2serr(" (%d bytes)", csp->bytes_xfer);
                        if (csp->last_segment)
                            pr2serr(" last_seg");
                        if (csp->leave_reason >= 0)
                            pr2serr(", leave_reason=%d\n", csp->leave_reason);
                        else
                            pr2serr("\n");
                    }
                    b_off += ((csp->bytes_xfer >= 0) ? csp->bytes_xfer :
                                     (csp->blks_xfer * dip->bs_pi));
                    csp->rem_seg_bytes = b_off;
                    if (csp->cur_countp)
                        *csp->cur_countp += csp->blks_xfer;
                    /* forget about iterator, won't use again ... */
                    return DDPT_CAT_SEE_LEAVE_REASON;
                }
            }
            if (rem_blks > 0) {
                /* itp->filepos = lba + itp->it_bk_off + rem_blks; */
                b_off += rem_blks * dip->bs_pi;
                if (csp->cur_countp)
                    *csp->cur_countp += rem_blks;
            }
            ablocks -= (int64_t)rem_blks;
            ++itp->it_e_ind;    /* step to next element */
            if (extend_last && (! on_last))
                on_last = (itp->it_e_ind = (elems - 1));
            itp->it_bk_off = 0;
            ++sgl_p;
        } else {  /* last: move iter (and call *fp) within current sgl elem */
            rem_blks = (uint32_t)ablocks;
            if (fp) {
                if (vb > 2)
                    pr2serr("  ... %s%s LBA=0x%" PRIx64 ", num_blks=%u, "
                            "b_off=%d\n", (csp->reading ? "<<<" : ">>>"),
                            ((csp->reading && (! in_side)) ? "[out]" : ""),
                            lba + itp->it_bk_off, rem_blks, b_off);
                if (in_side) {
                    csp->cur_in_lba = lba + itp->it_bk_off;
                    csp->cur_in_num = rem_blks;
                } else {
                    csp->cur_out_lba = lba + itp->it_bk_off;
                    csp->cur_out_num = rem_blks;
                }
                csp->cur_bp = csp->subseg_bp + b_off;
                /*   vvvvvvvvvvvvvvvvvvvvvv  indirect call to workers */
                if ((res = fp(dip, csp, op)))
                    return res;
                if (csp->blks_xfer < (int)rem_blks) {
                    if (vb) {
                        pr2serr("%s(more): [%s] shorted, got %d blocks",
                                __func__, dip->dir_n, csp->blks_xfer);
                        if (csp->bytes_xfer > 0)
                            pr2serr(" (%d bytes)", csp->bytes_xfer);
                        if (csp->leave_reason >= 0)
                            pr2serr(", leave_reason=%d\n", csp->leave_reason);
                        else
                            pr2serr("\n");
                    }
                    b_off += ((csp->bytes_xfer >= 0) ? csp->bytes_xfer :
                                     (csp->blks_xfer * dip->bs_pi));
                    csp->rem_seg_bytes = b_off;
                    if (csp->cur_countp)
                        *csp->cur_countp += csp->blks_xfer;
                    /* forget about iterator, won't use again ... */
                    return DDPT_CAT_SEE_LEAVE_REASON;
                }
                /* itp->filepos = lba + itp->it_bk_off + rem_blks; */
                b_off += rem_blks * dip->bs_pi;
            }
            if (csp->cur_countp)
                *csp->cur_countp += rem_blks;
            ablocks -= (int64_t)rem_blks;
            itp->it_bk_off = (uint32_t)off;
            break;
        }
    }
    return 0;
}

/* Copies |add_blks| blocks from current position of file/device (referred
 * to by dip) to or from a segment of the computer's ram. The copy is into
 * ram when ddpt_arg is 0 (i.e. a "read"), otherwise it is from ram (i.e. a
 * "write"). IO is performed by the fp (callback function) and the iterator
 * (or file pointer) is moved forward (i.e. toward the end) when add_blks > 0.
 * When add_blks < 0, the iterator is moved backward (i.e. toward the
 * beginning). Returns 0 for okay else an error number. -9999 is returned
 * for an unexpected error with the iterator. */
int
cp_via_sgl_iter(struct dev_info_t * dip, struct cp_state_t * csp,
                int add_blks, ddpt_rw_f fp, struct opts_t * op)
{
    int e_ind, elems;
    int vb = op->verbose;
    uint32_t n_blks;
    struct sgl_iter_t * itp;
    const char * arg_str = dip->dir_n;
    const struct scat_gath_elem * sgep;

    if (add_blks < 0) {
        pr2serr("%s: [%s] don't support backward iteration\n", __func__,
                arg_str);
        return SG_LIB_SYNTAX_ERROR;
    }
    n_blks = (uint32_t)add_blks;
    if (0 == n_blks)
        return 0;       /* valid, nothing to do */

    switch (dip->ddpt_arg) {
    case DDPT_ARG_IN:
        itp = &csp->in_iter;
        break;
    case DDPT_ARG_OUT:
        itp = &csp->out_iter;
        break;
    case DDPT_ARG_OUT2: /* don't use iterator on OFILE2 */
        if (0 == add_blks)
            return 0;
        if (fp) {               /* call worker directly */
            csp->cur_out_num = n_blks;
            return (*fp)(dip, csp, op);
        }
        return 0;
    default:
        pr2serr("%s: bad ddpt_arg: %d\n", __func__, dip->ddpt_arg);
        return SG_LIB_SYNTAX_ERROR;
    }
    e_ind = itp->it_e_ind;
    elems = itp->elems;
    if (elems < 1)
        return 0;       /* nothing to do */
    if (e_ind == (elems - 1))
        sgep = itp->sglp + e_ind;
    else
        sgep = NULL;
    if ((e_ind >= elems) ||
        (sgep && (n_blks + itp->it_bk_off > sgep->num))) {
        if ((e_ind == elems) && (0 == itp->it_bk_off)) {
            if (vb > 2)
                pr2serr("%s: [%s] iterator at end\n", __func__, arg_str);
        }
        if (vb)
            pr2serr("%s: [%s] iterator: %d,%u + %d blocks exceeds range\n",
                    __func__, arg_str, e_ind, itp->it_bk_off, n_blks);
        return -9999;
    }
    if (vb > 3) {
        int64_t lba = sgl_iter_lba(itp);

        pr2serr("%s: [%s] add_blks=%d, ", __func__, arg_str, add_blks);
        if (lba < 0)
            pr2serr("prior lba is invalid or at end\n");
        else
            pr2serr("prior lba=0x%" PRIx64 "\n", lba);
    }
    return sgl_iter_forward_blks(dip, itp, n_blks, csp, fp, op);
}

/* id_str may be NULL (if so replace by "unknown"), present to enhance verbose
 * output. */
void
sgl_print(struct sgl_info_t * sgli_p, bool skip_meta, const char * id_str,
          bool to_stdout, bool show_sgl)
{
    int k;
    int elems = sgli_p->elems;
    const char * caller = id_str ? id_str : "unknown";
    const struct scat_gath_elem * sgep = sgli_p->sglp;
    FILE * fp = to_stdout ? stdout : stderr;

    if (! skip_meta) {
        fprintf(fp, "%s: from: %s: elems=%d, sgl %spresent, monotonic=%s\n",
                __func__, caller, elems, (sgli_p->sglp ? "" : "not "),
                (sgli_p->monotonic ? "true" : "false"));
        fprintf(fp, "  sum=%" PRId64 ", lowest=0x%" PRIx64 ", high_lba_p1=",
                sgli_p->sum, sgli_p->lowest_lba);
        fprintf(fp, "0x%" PRIx64 "\n", sgli_p->high_lba_p1);
        fprintf(fp, "  overlapping=%s, sum_hard=%s, fragmented=%s\n",
                (sgli_p->overlapping ? "true" : "false"),
                (sgli_p->sum_hard ? "true" : "false"),
                (sgli_p->fragmented ? "true" : "false"));
    }
    fprintf(fp, "  >> %s scatter gather list (%d elements):\n", caller,
            elems);
    if (sgli_p->sglp && show_sgl) {
        for (k = 0, sgep = sgli_p->sglp; k < elems; ++k, ++sgep) {
            fprintf(fp, "    lba: 0x%" PRIx64 ", number: 0x%" PRIx32,
                    sgep->lba, sgep->num);
            if (sgep->lba > 0)
                fprintf(fp, " [next lba: 0x%" PRIx64 "]",
                        sgep->lba + sgep->num);
            fprintf(fp, "\n");
        }
    }
}

/* Prints a single sge (scatter gather list element) to stderr or stdout. */
void
sge_print(const struct scat_gath_elem * sgep, const char * id_str,
          bool to_stdout)
{
    const char * caller = id_str ? id_str : "unknown";
    FILE * fp = to_stdout ? stdout : stderr;

    fprintf(fp, "%s    lba: 0x%" PRIx64 ", number: 0x%" PRIx32 "\n", caller,
            sgep->lba, sgep->num);
}

/* Assumes sgli_p->elems and sgli_p->slp are setup and the other fields
 * in struct sgl_info_t are zeroed. This function will populate the other
 * fields in that structure. Does one pass through the scatter gather list
 * (array). Sets these fields in struct sgl_info_t: fragmented, lowest_lba,
 * high_lba_p1, monotonic, overlapping, sum and sum_hard. Degenerate
 * elements (i.e. those with 0 blocks) are ignored apart from when one is
 * last which makes sum_hard false and its LBA becomes high_lba_p1 if it
 * is the highest in the list. An empty sgl is equivalent to a 1 element
 * list with [0, 0], so sum_hard==false, monit==true, fragmented==false
 * overlapping ==false . id_str may be NULL, present to enhance verbose
 * output. */
void
sgl_sum_scan(struct sgl_info_t * sgli_p, const char * id_str, bool show_sgl,
             bool b_vb)
{
    bool degen = false;
    bool first = true;
    bool monot = true;          /* monotonic increasing, skips degenerates */
    bool regular = true;        /* no overlapping segments detected */
    bool fragmented = false;
    int k;
    int elems = sgli_p->elems;
    uint32_t prev_num, t_num;
    uint64_t prev_lba, t_lba, sum, low, high, end;
    const struct scat_gath_elem * sgep = sgli_p->sglp;

    for (k = 0, sum = 0, low = 0, high = 0; k < elems; ++k, ++sgep) {
        degen = false;
        t_num = sgep->num;
        if (0 == t_num) {
            degen = true;
            if (! first)
                continue;
        }
        if (first) {
            prev_lba = sgep->lba;
            low = prev_lba;
            prev_num = t_num;
            sum = t_num;
            high = prev_lba + prev_num;
            first = false;
        } else {
            t_lba = sgep->lba;
            if (! fragmented) {
                if ((prev_lba + prev_num) != t_lba)
                    fragmented = true;
            }
            sum += t_num;
            end = t_lba + t_num;
            if (end > high)
                high = end;     /* high is one plus highest LBA */
            if (prev_lba < t_lba)
                ;
            else if (prev_lba == t_lba) {
                if (prev_num > 0) {
                    monot = false;
                    break;
                }
            } else {
                low = t_lba;
                monot = false;
                break;
            }
            if (regular) {
                if ((prev_lba + prev_num) > t_lba)
                    regular = false;
            }
            prev_lba = t_lba;
            prev_num = t_num;
        }
    }
    if (k < elems) {    /* monot is now false */
        sgli_p->monotonic = false;
        sgli_p->overlapping = false;    /* no longer valid */
        sgli_p->fragmented = true;      /* must be fragmented */
        prev_lba = t_lba;
        ++k;
        ++sgep;
        for ( ; k < elems; ++k, ++sgep) {
            degen = false;
            t_lba = sgep->lba;
            t_num = sgep->num;
            if (0 == t_num) {
                degen = true;
                continue;
            }
            sum += t_num;
            end = t_lba + t_num;
            if (end > high)
                high = end;
            if (prev_lba > t_lba) {
                if (t_lba < low)
                    low = t_lba;
            }
            prev_lba = t_lba;
        }
    } else {
        sgli_p->monotonic = monot;
        sgli_p->overlapping = ! regular;
        sgli_p->fragmented = fragmented;
    }
    sgli_p->lowest_lba = low;
    if (degen && (elems > 0)) { /* last element always impacts high_lba_p1 */
        t_lba = (sgep - 1)->lba;
        sgli_p->high_lba_p1 = (t_lba > high) ? t_lba : high;
    } else
        sgli_p->high_lba_p1 = high;
    sgli_p->sum = sum;
    sgli_p->sum_hard = (elems > 0) ? ! degen : false;
    if (b_vb)
        sgl_print(sgli_p, false, id_str, false, show_sgl);
}

/* Returns number of elements in scatter gather list (array) whose pointer is
 * written to *sge_pp. The sgl is linear starting at start_lba, for blks
 * blocks with all elements smaller than max_nbs.  On error returns negated
 * error number and NULL is written to *sge_pp . The caller is responsible for
 * freeing memory associated with *sge_pp . If blks or start_lba are negative,
 * NULL is written to *sge_pp and returns 0. */
int
build_sgl(struct scat_gath_elem ** sge_pp, int64_t blks, int64_t start_lba)
{
    int k, n;
    const int max_nbs = (INT_MAX - 1);
    const int64_t cnt = blks;
    struct scat_gath_elem * sgep;

    if ((blks < 0) || (start_lba < 0)) {
        *sge_pp = NULL;
        return 0;
    }
    for (k = 0; blks > 0; ++k) {
        if (blks <= max_nbs) {
            ++k;
            break;
        }
        blks -= max_nbs;
    }
    n = k;
    *sge_pp = (struct scat_gath_elem *)
                        calloc(n, sizeof(struct scat_gath_elem));
    if (NULL == *sge_pp) {
        pr2serr("%s: no memory available for sgl\n", __func__);
        return -sg_convert_errno(ENOMEM);
    }
    sgep = *sge_pp;
    for (k = 0, blks = 0; k < n; ++k, ++sgep, blks += max_nbs) {
        sgep->lba = start_lba + blks;
        if ((cnt - blks) <= max_nbs)
            sgep->num = cnt - blks;
        else
            sgep->num = max_nbs;
    }
    return n;
}

/* Builds single element sgl with (*sge_pp)->num==0 . */
int
build_degen_sgl(struct scat_gath_elem ** sge_pp, int64_t start_lba)
{
    if (start_lba < 0) {
        *sge_pp = NULL;
        return 0;
    }
    *sge_pp = (struct scat_gath_elem *)
                        calloc(1, sizeof(struct scat_gath_elem));
    if (NULL == *sge_pp) {
        pr2serr("%s: no memory available for sgl\n", __func__);
        return -sg_convert_errno(ENOMEM);
    }
    (*sge_pp)->lba = start_lba;         /* ->num will be 0 due to calloc */
    return 1;
}

/* Similar to build_sgl but appends to existing sgl whose length is cur_elems.
 * Note that *sge_pp will probably change (in which case the previous *sge_pp
 * is freed). Returns number of elements in sgl after append or negated error
 * number. */
int
append2sgl(struct scat_gath_elem ** sge_pp, int cur_elems, int64_t extra_blks,
           int64_t start_lba)
{
    int k, n;
    const int max_nbs = (INT_MAX - 1);
    const int64_t cnt = extra_blks;
    struct scat_gath_elem * prev_sgep;
    struct scat_gath_elem * hold_sgep;
    struct scat_gath_elem * sgep;

    if (extra_blks <= 0)
        return cur_elems;       /* nothing to do */
    if (cur_elems < 1) {
        if (*sge_pp) {
            free(*sge_pp);
            *sge_pp = NULL;
        }
        return build_sgl(sge_pp, extra_blks, start_lba);
    }
    prev_sgep = *sge_pp;
    hold_sgep = prev_sgep;
    if ((extra_blks < 0) || (start_lba < 0)) {
        *sge_pp = NULL;
        return 0;
    }
    for (k = 0; extra_blks > 0; ++k) {
        if (extra_blks <= max_nbs) {
            ++k;
            break;
        }
        extra_blks -= max_nbs;
    }
    n = k + cur_elems;
    *sge_pp = (struct scat_gath_elem *)
                        calloc(n, sizeof(struct scat_gath_elem));
    if (NULL == *sge_pp) {
        pr2serr("%s: no memory available for sgl\n", __func__);
        return -sg_convert_errno(ENOMEM);
    }
    sgep = *sge_pp;
    for (k = 0; k < cur_elems; ++k, ++sgep, ++prev_sgep) {
        sgep->lba = prev_sgep->lba;
        sgep->num = prev_sgep->num;
    }
    free(hold_sgep);    /* previous sgl no longer needed */
    for (extra_blks = 0; k < n; ++k, ++sgep, extra_blks += max_nbs) {
        sgep->lba = start_lba + extra_blks;
        if ((cnt - extra_blks) <= max_nbs)
            sgep->num = cnt - extra_blks;
        else
            sgep->num = max_nbs;
    }
    return n;
}


/* Returns LBA referred to by iterator if valid or returns DDPT_LBA_INVALID
 * (-1) if at end or invalid. */
int64_t
sgl_iter_lba(const struct sgl_iter_t * itp)
{
    int64_t res = DDPT_LBA_INVALID; /* for at end or invalid (-1) */

    if (itp->sglp && (itp->it_e_ind < itp->elems)) {
        struct scat_gath_elem * sgep = itp->sglp + itp->it_e_ind;

        if ((uint32_t)itp->it_bk_off < sgep->num)
            return sgep->lba + itp->it_bk_off;
        else if (((uint32_t)itp->it_bk_off == sgep->num) &&
                 ((itp->it_e_ind + 1) < itp->elems)) {
            struct sgl_iter_t iter = *itp;

            ++iter.it_e_ind;
            iter.it_bk_off = 0;
            /* worst case recursion will stop at end of sgl */
            return sgl_iter_lba(&iter);
        }
    }
    return res;
}

/* Returns true of no sgl or sgl is at the end [elems, 0], otherwise it
 * returns false. */
bool
sgl_iter_at_end(const struct sgl_iter_t * itp)
{
    if ((NULL == itp->sglp) ||
        ((itp->it_e_ind == itp->elems) && (0 == itp->it_bk_off)))
        return true;
    else
        return false;
}

/* Returns true if associated iterator is monotonic (increasing) and not
 * fragmented. Empty sgl and single element degenerate considered linear.
 * Assumes sgl_sum_scan() has been called on sgl. */
bool
is_iter_linear(const struct sgl_info_t * sglip)
{
    if (sglip)
        return (sglip->monotonic && (! sglip->fragmented));
    else
        return false;
}

/* Returns true if either argument is 0/NULL or a 1 element list with both
 * lba and num 0; otherwise returns false. */
bool
sgl_empty(struct scat_gath_elem * sglp, int elems)
{
    if ((0 == elems) || (NULL == sglp))
        return true;
    if ((1 == elems) && (0 == sglp->lba) && (0 == sglp->num))
        return true;
    return false;
}

/* print data held in struct sgl_iter_t to fp. If fp is NULL then to stderr */
void
sgl_iter_print(const struct sgl_iter_t * itp, const char * leadin,
               bool index_only, bool in_hex, FILE * fp)
{
    if (NULL == fp)
        fp = stderr;
    if (NULL == leadin)
        leadin = "";
    if (in_hex)
        fprintf(fp, "%s e_index=0x%x, blk_off=0x%x", leadin, itp->it_e_ind,
                itp->it_bk_off);
    else
        fprintf(fp, "%s e_index=%d, blk_off=%d", leadin, itp->it_e_ind,
                itp->it_bk_off);
    if (index_only) {
        fprintf(fp, "\n");
        return;
    }
    if (in_hex)
        fprintf(fp, "  elems=0x%x extend_last=%d %sfilepos%c0\n", itp->elems,
                !! itp->extend_last, (itp->sglp ? "" : "sglp=NULL"),
                (itp->filepos ? '>' : '='));
    else
        fprintf(fp, "  elems=%d extend_last=%d %sfilepos%c0\n", itp->elems,
                !! itp->extend_last, (itp->sglp ? "" : "sglp=NULL "),
                (itp->filepos ? '>' : '='));
}

/* Returns >= 0 if sgl can be simplified to a single LBA. So an empty sgl
 * will return 0; a one element sgl will return its LBA. A multiple element
 * sgl only returns the first element's LBA (that is not degenerate) if the
 * sgl is monotonic and not fragmented. In the extreme case takes last
 * element's LBA if all prior elements are degenerate. Else returns -1 .
 * Assumes sgl_sum_scan() has been called. */
int64_t
get_low_lba_from_linear(const struct sgl_info_t * sglip)
{
    int k;
    struct scat_gath_elem * sgep = sglip->sglp;

    if ((NULL == sglip) || (sglip->elems < 1) || (NULL == sglip->sglp))
        return 0;
    else if (1 == sglip->elems)
        return sglip->sglp->lba;        /* even if degenerate */
    else {
        if (sglip->monotonic && (! sglip->fragmented)) {
            for (k = 0; k < (sglip->elems - 1); ++k, ++sgep) {
                if (sgep->num > 0)
                    return sgep->lba;
            }
            /* take last element's LBA if all early are degenerate */
            return sgep->lba;
        } else
            return -1;
    }
}

/* If bad arguments returns -1, otherwise returns the lowest LBA in *sglp .
 * If no elements considered returns 0. If ignore_degen is true than
 * ignores all elements with num_blks zero unless always_last is also
 * true in which case the last element is always considered. */
int64_t
get_lowest_lba(const struct scat_gath_elem * sglp, int num_elems,
               bool ignore_degen, bool always_last)
{
    bool some = (num_elems > 0);
    int64_t res = INT64_MAX;

    if ((NULL == sglp) || (num_elems < 0))
        return -1;
    for ( ; num_elems > 0; --num_elems, ++sglp) {
        if ((0 == sglp->num) && ignore_degen)
            continue;
        if ((int64_t)sglp->lba < res)
            res = sglp->lba;
    }
    if (always_last && some) {
        if ((int64_t)((sglp - 1)->lba) < res)
            res = (sglp - 1)->lba;
    }
    return (INT64_MAX == res) ? 0 : res;
}

void
cp_state_init(struct cp_state_t * csp, struct opts_t * op)
{
    memset(csp, 0, sizeof(struct cp_state_t));
    if (op->i_sgli.sglp) {
        csp->in_iter.sglp = op->i_sgli.sglp;
        csp->in_iter.elems = op->i_sgli.elems;
    }
    if (op->o_sgli.sglp) {
        csp->out_iter.sglp = op->o_sgli.sglp;
        csp->out_iter.elems = op->o_sgli.elems;
    }
    csp->stats = op->stats;
    op->stp = &csp->stats;
    csp->buf_name = "unknown";
    csp->leave_reason = DDPT_REASON_UNKNOWN;         /* -1 */
}

/* For each segment from and including iter_p, for length blk_count call
 * *a_fp with fp, hex and vb as arguments. Move iter_p forward by blk_count
 * (if valid). Returns 0 for good, else error value. */
int
iter_add_process(struct sgl_iter_t * ip, uint64_t blk_count,
                 process_sge_f a_fp, FILE * fp, int hex, int rblks, int vb)
{
    bool extend_last, on_last;
    int k, it_off, elems;
    int res = 0;
    uint32_t num;
    uint64_t lba;
    struct scat_gath_elem * sgep;
    struct scat_gath_elem a_sge;

    a_sge.lba = 0;
    a_sge.num = 0;
    if ((NULL == ip) || (NULL == a_fp) || (NULL == fp)) {
        pr2serr("%s: bad %s argument\n", __func__,
                (ip ? (a_fp ? "FILE *" : "a_funcp") : "iter_p" ));
        return sg_convert_errno(EINVAL);
    }

    if (vb > 4)
        pr2serr("%s: enter, blk_count=%" PRIu64 ", rblks=%d\n", __func__,
                blk_count, rblks);
    extend_last = ip->extend_last;
    elems = ip->elems;
    on_last = (extend_last & (elems < 2));
    for (k = 0; blk_count > (uint32_t)rblks; ++k, blk_count -= num) {
        it_off = ip->it_bk_off;
        if (ip->it_e_ind >= elems) {
            if ((ip->it_e_ind == elems) && (0 == it_off)) {
                if (vb > 3)
                    pr2serr("%s: at end, segment(k)=%d\n", __func__, k);
                return 0;
            }
            pr2serr("%s: element index (%d) >= sgl (%d), segment=%d\n",
                    __func__, ip->it_e_ind, elems, k);
            return SG_LIB_LOGIC_ERROR;
        }
        sgep = ip->sglp + ip->it_e_ind;
        num = on_last ? (INT32_MAX - 1) : sgep->num;
        if ((uint32_t)it_off >= num) {
            ++ip->it_e_ind;
            if (extend_last)
                on_last = (ip->it_e_ind >= (elems - 1));
            ip->it_bk_off = 0;
            continue;
        }
        lba = sgep->lba + it_off;
        num -= it_off;  /* number remaining in current element */
        a_sge.lba = lba;
        if (blk_count <= num) {
            if ((rblks + blk_count) >= num) {
                if ((vb > 3) && (blk_count != num))
                    pr2serr("%s: ((rblks+blk_count)>= num), blk_count=%"
                            PRIu64 ", num=%u\n", __func__, blk_count, num);
                blk_count = num;
            }
            a_sge.num = (uint32_t)blk_count;
            if (vb > 3)
                sge_print(&a_sge, "final  ", false);
            res = (*a_fp)(fp, &a_sge, hex, vb);
            if (blk_count == num) {
                ++ip->it_e_ind;
                ip->it_bk_off = 0;
            } else
                ip->it_bk_off += (int)blk_count;
            blk_count = 0;      /* for 'on exit' verbose reporting below */
            break;
        } else {        /* blk_count > num */
            a_sge.num = num;
            if (vb > 3)
                sge_print(&a_sge, "ongoing", false);
            res = (*a_fp)(fp, &a_sge, hex, vb);
            if (res)
                break;
            ++ip->it_e_ind;
            if (extend_last)
                on_last = (ip->it_e_ind >= (elems - 1));
            ip->it_bk_off = 0;
        }
    }   /* end of decrementing blk_count _for_ loop */
    if (vb > 4)
        pr2serr("%s: on exit blk_count=%" PRIu64 ", res=%d\n", __func__,
                blk_count, res);
    return res;
}

/* Writes the LBA and number_of_blocks in sge_r to a line at the current
 * file position (typically the end) of fp, with a trailing \n character.
 * Returns 0 on success, else SG_LIB_FILE_ERROR . */
int
output_sge_f(FILE * fp, const struct scat_gath_elem * sgep, int hex, int vb)
{
    if (0 == hex)
        fprintf(fp, "%" PRIu64 ",%u\n", sgep->lba, sgep->num);
    else if (1 == hex)
        fprintf(fp, "0x%" PRIx64 ",0x%x\n", sgep->lba, sgep->num);
    else if (hex > 1)
        fprintf(fp, "%" PRIx64 ",%x\n", sgep->lba, sgep->num);
    if (ferror(fp)) {
        if (vb)
            pr2serr("%s: failed during formatted write to output sgl\n",
                    __func__);
        clearerr(fp);   /* error flag stays set unless .... */
        return SG_LIB_FILE_ERROR;
    } else
        return 0;
}

/* Calculates difference between iterators, logically: res <-- lhs - rhs
 * Checks that lhsp and rhsp have same underlying sgl, if not returns
 * INT_MIN. Assumes iterators close enough for result to lie in range
 * from (-INT_MAX) to INT_MAX (inclusive). */
int
sgl_iter_diff(const struct sgl_iter_t * lhsp, const struct sgl_iter_t * rhsp)
{
    int res, k, r_e_ind, l_e_ind;
    const struct scat_gath_elem * sgep;

    if (! (lhsp && rhsp && (lhsp->sglp == rhsp->sglp) &&
           (lhsp->elems == rhsp->elems))) {
        pr2serr("%s: bad args\n", __func__);
        return INT_MIN;
    }
    r_e_ind = rhsp->it_e_ind;
    l_e_ind = lhsp->it_e_ind;
    if (l_e_ind < r_e_ind) { /* so difference will be negative */
        res = sgl_iter_diff(rhsp, lhsp);        /* cheat */
        if (INT_MIN == res)
            return res;
        return -res;
    } else if (l_e_ind == r_e_ind)
        return (int)lhsp->it_bk_off - (int)rhsp->it_bk_off;
    /* (l_e_ind > r_e_ind) so (lhs > rhs) */
    sgep = rhsp->sglp + r_e_ind;
    res = (int)sgep->num - rhsp->it_bk_off;
    for (k = 1, ++sgep; (r_e_ind + k) < l_e_ind; ++k, ++sgep) {
        // pr2serr("iter_diff: k=%d, res=%d, num=%d\n", k, res,
        //         (int)sgep->num);
        res += (int)sgep->num;
    }
    res += lhsp->it_bk_off;
    // pr2serr("iter_diff: at exit res=%d\n", res);
    return res;
}

/* Compares from lsgep, offset by l_bk_off against rsgep, offset by r_bk_off.
 * While lbas compare equal lsgep is advanced by up to lelems, while rsgep
 * is advanced by up to relems. Returns false on the first inequality;
 * otherwise if both list are exhausted at the same point, then returns true.
 * If no inequality and one list is exhausted before the other, then returns
 * allow_partial. */
bool
sgl_eq_f(const struct scat_gath_elem * lsgep, int lelems, int l_bk_off,
         const struct scat_gath_elem * rsgep, int relems, int r_bk_off,
         bool allow_partial)
{
    int l_e_ind = 0;
    int r_e_ind = 0;
    int lrem, rrem;

    while ((l_e_ind < lelems) && (r_e_ind < relems)) {
        if ((lsgep->lba + l_bk_off) != (rsgep->lba + r_bk_off))
            return false;
        lrem = lsgep->num - l_bk_off;
        rrem = rsgep->num - r_bk_off;
        if (lrem == rrem) {
            ++lsgep;
            ++l_e_ind;
            l_bk_off = 0;
            ++rsgep;
            ++r_e_ind;
            r_bk_off = 0;
        } else if (lrem < rrem) {
            ++lsgep;
            ++l_e_ind;
            l_bk_off = 0;
            r_bk_off += lrem;
        } else {
            ++rsgep;
            ++r_e_ind;
            r_bk_off = 0;
            l_bk_off += rrem;
        }
    }
    if ((l_e_ind >= lelems) && (r_e_ind >= relems))
        return true;
    return allow_partial;
}

/* Compares from the current iterator positions of lhsp and rhsp until
 * the shorter list is exhausted. Returns false on the first inequality.
 * If no inequality and both remaining lists are same length then returns
 * true. If no inequality but remaining lists differ in length then returns
 * allow_partial. */
bool
sgl_iter_eq(const struct sgl_iter_t * lhsp, const struct sgl_iter_t * rhsp,
            bool allow_partial)
{
    const struct scat_gath_elem * lsgep;
    const struct scat_gath_elem * rsgep;

    if (! (lhsp && rhsp)) {
        pr2serr("%s: bad args\n", __func__);
        return false;
    }
    lsgep = lhsp->sglp + lhsp->it_e_ind;
    rsgep = rhsp->sglp + rhsp->it_e_ind;
    return sgl_eq_f(lsgep, lhsp->elems - lhsp->it_e_ind, lhsp->it_bk_off,
                    rsgep, rhsp->elems - rhsp->it_e_ind, rhsp->it_bk_off,
                    allow_partial);
}


