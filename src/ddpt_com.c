/*
 * Copyright (c) 2013-2018, Douglas Gilbert
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


static const char * errblk_file = "errblk.txt";


#ifdef __GNUC__
static int my_snprintf(char * cp, int cp_max_len, const char * fmt, ...)
        __attribute__ ((format (printf, 3, 4)));
#else
static int my_snprintf(char * cp, int cp_max_len, const char * fmt, ...);
#endif

/* Want safe, 'n += snprintf(b + n, blen - n, ...)' style sequence of
 * functions. Returns number number of chars placed in cp excluding the
 * trailing null char. So for cp_max_len > 0 the return value is always
 * < cp_max_len; for cp_max_len <= 1 the return value is 0 and no chars
 * are written to cp. Note this means that when cp_max_len = 1, this
 * function assumes that cp[0] is the null character and does nothing
 * (and returns 0).  */
static int
my_snprintf(char * cp, int cp_max_len, const char * fmt, ...)
{
    va_list args;
    int n;

    if (cp_max_len < 2)
        return 0;
    va_start(args, fmt);
    n = vsnprintf(cp, cp_max_len, fmt, args);
    va_end(args);
    return (n < cp_max_len) ? n : (cp_max_len - 1);
}

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

void
state_init(struct opts_t * op, struct flags_t * ifp, struct flags_t * ofp,
           struct dev_info_t * idip, struct dev_info_t * odip,
           struct dev_info_t * o2dip)
{
    memset(op, 0, sizeof(struct opts_t));       /* sets bools to false */
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
    odip->d_type = FT_OTHER;
    odip->fd = -1;
    o2dip->d_type = FT_OTHER;
    o2dip->fd = -1;
    op->idip = idip;
    op->odip = odip;
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
print_stats(const char * str, struct opts_t * op, int who)
{
#ifdef SG_LIB_LINUX
    /* Print tape read summary if necessary . */
    print_tape_summary(op, 0, str);
#endif

    if ((op->dd_count > 0) && (! op->reading_fifo)) {
        pr2serr("  remaining block count=%" PRId64, op->dd_count);
        if ((who < 2) && (op->in_full <= op->dd_count_start) &&
            (op->dd_count_start >= 4196)) {
            /* with ints will overflow around 8 TB for bs=1 */
            int num = (int)((op->in_full * 100) / 4196);
            int den = (int)(op->dd_count_start / 4196);  /* will be >= 1 */
            int percent;

            percent = num / den;
            if ((100 == percent) && (op->in_full < op->dd_count_start))
                --percent;      /* don't want rounding to 100 % */
            pr2serr("   %d%% completed\n", percent);
        } else
            pr2serr("\n");
    }
    if (who < 2)
        pr2serr("%s%" PRId64 "+%d records in\n", str, op->in_full,
                op->in_partial);
    if (1 != who)
        pr2serr("%s%" PRId64 "+%d records out\n", str, op->out_full,
                op->out_partial);
    if (op->out_sparse_active || op->out_sparing_active) {
        if (op->out_trim_active) {
            const char * cp;

            cp = op->trim_errs ? "attempted trim" : "trimmed";
            if (op->out_sparse_partial > 0)
                pr2serr("%s%" PRId64 "+%d %s records out\n", str,
                        op->out_sparse, op->out_sparse_partial, cp);
            else
                pr2serr("%s%" PRId64 " %s records out\n", str,
                        op->out_sparse, cp);
        } else if (op->out_sparse_partial > 0)
            pr2serr("%s%" PRId64 "+%d bypassed records out\n", str,
                    op->out_sparse, op->out_sparse_partial);
        else
            pr2serr("%s%" PRId64 " bypassed records out\n", str,
                    op->out_sparse);
    }
    if (op->recovered_errs > 0)
        pr2serr("%s%d recovered read errors\n", str, op->recovered_errs);
    if (op->num_retries > 0)
        pr2serr("%s%d retries attempted\n", str, op->num_retries);
    if (op->unrecovered_errs > 0)
        pr2serr("%s%d unrecovered read error%s\n", str, op->unrecovered_errs,
                ((1 == op->unrecovered_errs) ? "" : "s"));
    if (op->unrecovered_errs && (op->highest_unrecovered >= 0))
        pr2serr("lowest unrecovered read lba=%" PRId64 ", highest "
                "unrecovered lba=%" PRId64 "\n", op->lowest_unrecovered,
                op->highest_unrecovered);
    if (op->wr_recovered_errs > 0)
        pr2serr("%s%d recovered write errors\n", str, op->wr_recovered_errs);
    if (op->wr_unrecovered_errs > 0)
        pr2serr("%s%d unrecovered write error%s\n", str,
                op->wr_unrecovered_errs,
                ((1 == op->wr_unrecovered_errs) ? "" : "s"));
    if (op->trim_errs)
        pr2serr("%s%d trim errors\n", str, op->trim_errs);
    if (op->interrupted_retries > 0)
        pr2serr("%s%d %s after interrupted system call(s)\n",
                str, op->interrupted_retries,
                ((1 == op->interrupted_retries) ? "retry" : "retries"));
    if (op->io_eagains > 0)
        pr2serr("%s%d %s after EAGAIN error(s) during IO\n",
                str, op->io_eagains,
                ((1 == op->io_eagains) ? "retry" : "retries"));
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
            (DEV_NULL_MINOR_NUM == minor(st.st_rdev)))
            return FT_DEV_NULL;
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
#endif          /* if not SG_LIB_WIN32 */

/* Categorize file by using the stat() system call on its filename.
 * If not found FT_ERROR returned. The FT_* constants are a bit mask
 * and later logic can combine them (e.g. FT_BLOCK | FT_PT).
 */
int
dd_filetype(const char * filename, int verbose)
{
#ifdef SG_LIB_WIN32
    return win32_dd_filetype(filename, verbose);
#else
    return unix_dd_filetype(filename, verbose);
#endif
}

char *
dd_filetype_str(int ft, char * buff, int max_bufflen, const char * fname)
{
    int off = 0;

    if (FT_DEV_NULL & ft)
        off += my_snprintf(buff + off, max_bufflen - off, "null device ");
    if (FT_PT & ft)
        off += my_snprintf(buff + off, max_bufflen - off,
                           "pass-through [pt] device ");
    if (FT_TAPE & ft)
        off += my_snprintf(buff + off, max_bufflen - off, "SCSI tape device ");
    if (FT_BLOCK & ft)
        off += my_snprintf(buff + off, max_bufflen - off, "block device ");
    if (FT_FIFO & ft)
        off += my_snprintf(buff + off, max_bufflen - off,
                           "fifo [stdin, stdout, named pipe] ");
    if (FT_REG & ft)
        off += my_snprintf(buff + off, max_bufflen - off, "regular file ");
    if (FT_CHAR & ft)
        off += my_snprintf(buff + off, max_bufflen - off, "char device ");
    if (FT_OTHER & ft)
        off += my_snprintf(buff + off, max_bufflen - off, "other file type ");
    if (FT_ERROR & ft)
        /* off += */ my_snprintf(buff + off, max_bufflen - off, "unable to "
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
    struct timespec end_tm, res_tm;
    double a, b, r;
    int secs, h, m, elapsed_secs;

    if (op->start_tm_valid && (op->start_tm.tv_sec || op->start_tm.tv_nsec)) {
        use_out_full = ((0 == op->in_full) && (op->obs > 0));
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
        if (use_out_full)
            b = (double)op->obs * op->out_full;
        else
            b = (double)op->ibs_hold * op->in_full;
        pr2serr("%stime to %s data%s: %d.%06d secs", leadin,
                (op->read1_or_transfer ? "read" : "transfer"),
                (contin ? " so far" : ""), (int)res_tm.tv_sec,
                (int)(res_tm.tv_nsec / 1000));
        r = 0.0;
        if ((a > 0.00001) && (b > 511)) {
            r = b / (a * 1000000.0);
            if (r < 1.0)
                pr2serr(" at %.1f KB/sec\n", r * 1000);
            else
                pr2serr(" at %.2f MB/sec\n", r);
        } else
            pr2serr("\n");
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
    struct timeval end_tm, res_tm;
    double a, b, r;
    int secs, h, m, elapsed_secs;

    if (op->start_tm_valid && (op->start_tm.tv_sec || op->start_tm.tv_usec)) {
        use_out_full = ((0 == op->in_full) && (op->obs > 0));
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
        if (use_out_full)
            b = (double)op->obs * op->out_full;
        else
            b = (double)op->ibs_hold * op->in_full;
        pr2serr("%stime to %s data%s: %d.%06d secs", leadin,
                (op->read1_or_transfer ? "read" : "transfer"),
                (contin ? " so far" : ""), (int)res_tm.tv_sec,
                (int)res_tm.tv_usec);
        r = 0.0;
        if ((a > 0.00001) && (b > 511)) {
            r = b / (a * 1000000.0);
            if (r < 1.0)
                pr2serr(" at %.1f KB/sec\n", r * 1000);
            else
                pr2serr(" at %.2f MB/sec\n", r);
        } else
            pr2serr("\n");
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
        if (elapsed_ms >= PROGRESS2_TRIGGER_MS) {
            if (elapsed_ms >= PROGRESS_TRIGGER_MS) {
                ms = PROGRESS_TRIGGER_MS;
                res = true;
            } else if (op->progress > 1) {
                ms = PROGRESS2_TRIGGER_MS;
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
        if (elapsed_ms >= PROGRESS2_TRIGGER_MS) {
            if (elapsed_ms >= PROGRESS_TRIGGER_MS) {
                ms = PROGRESS_TRIGGER_MS;
                res = true;
            } else if (op->progress > 1) {
                ms = PROGRESS2_TRIGGER_MS;
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

    if (op->verbose > 2)
        pr2serr(" >> %s signal implementation assumed "
                "[SA_NOCLDSTOP=%d], %smasking during IO\n",
                (SA_NOCLDSTOP ? "modern" : "old"), SA_NOCLDSTOP,
                (op->interrupt_io ? "not " : ""));
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
void    /* Global function, used by ddpt_xcopy.c */
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
                if (check_progress(op))
                    calc_duration_throughput("", true, op);
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
            print_stats("", op, 0 /* both in and out */);
            /* Don't show next message if using oflag=pre-alloc and we didn't
             * use FALLOC_FL_KEEP_SIZE */
            if ((! op->reading_fifo) && (FT_REG & op->odip->d_type_hold)
                 && (0 == op->oflagp->prealloc)) {
                if (op->oflagp->strunc) {
                    int64_t osize = (op->seek * op->obs);

                    pr2serr(">> Did not perform: "
                            "'truncate --size=%" PRId64 " %s'\n",
                            osize, op->odip->fn);
                    pr2serr(">> User should check and perform by hand if "
                            "necessary\n");
                }
                pr2serr("To resume, invoke with same arguments plus "
                        "oflag=resume\n");
            }
            // Could more cleanup or suggestions be made here?
        } else {
            pr2serr("Progress report:\n");
            print_stats("  ", op, 0);
            if (op->do_time)
                calc_duration_throughput("  ", true /* contin */, op);
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
            if (check_progress(op))
                calc_duration_throughput("", true, op);
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
    ++op->unrecovered_errs;
    ++op->in_partial;
    --op->in_full;
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
    num += snprintf(b, b_mlen, "0x");
    for (m = 0; ((m < 8) && (num < b_mlen)); ++m)
        num += snprintf(b + num, b_mlen - num, "%02x",
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
    if ((exit_stat > 0) && (exit_stat <= 99)) { /* SG_LIB_UNUSED_ABOVE */
        if (sg_exit2str(exit_stat, false, b_len - n, b + n))
            print_p("%s\n", b);
        else
            print_p("%sunexpected exit status value: %d\n", b, exit_stat);
        return;
    }

    /* here only if exit_stat > 99 (SG_LIB_UNUSED_ABOVE) */
    switch (exit_stat) {
    case DDPT_CAT_PARAM_LST_LEN_ERR:   /* 100 */
        print_p("%sparameter list length error\n", b);
        break;
    case DDPT_CAT_INVALID_FLD_IN_PARAM:   /* 101 */
        print_p("%sinvalid field in parameter list\n", b);
        break;
    case DDPT_CAT_TOO_MANY_SEGS_IN_PARAM:   /* 102 */
        print_p("%stoo many segments in parameter list\n", b);
        break;
    case DDPT_CAT_TARGET_UNDERRUN:   /* 103 */
        print_p("%starget underrun\n", b);
        break;
    case DDPT_CAT_TARGET_OVERRUN:   /* 104 */
        print_p("%starget overrun\n", b);
        break;
    case DDPT_CAT_OP_IN_PROGRESS:   /* 105 */
        print_p("%soperation in progress [list_id in use]\n", b);
        break;
    case DDPT_CAT_INSUFF_RES_CREATE_ROD:   /* 106 */
        print_p("%sinsufficient resources to create ROD\n", b);
        break;
    case DDPT_CAT_INSUFF_RES_CREATE_RODTOK:   /* 107 */
        print_p("%sinsufficient resources to create ROD Token\n", b);
        break;
    case DDPT_CAT_CMDS_CLEARED_BY_DEV_SVR:   /* 108 */
        print_p("%scommands cleared by device servers\n", b);
        break;
    default:
        if ((exit_stat >= DDPT_CAT_TOKOP_BASE) &&
            (exit_stat < 126)) {        /* 126+127 used by Unix OSes */
            print_p("%sinvalid token operation, ", b);
            switch (exit_stat - DDPT_CAT_TOKOP_BASE) {  /* asc=0x23 */
            case 0:     /* asc=0x23, asq=0x0 */
                print_p("cause not reportable\n");
                break;
            case 1:     /* asc=0x23, asq=0x1 */
                print_p("unsupported token type\n");
                break;
            case 2:
                print_p("remote token usage not supported\n");
                break;
            case 3:
                print_p("remote ROD token creation not supported\n");
                break;
            case 4:
                print_p("token unknown\n");
                break;
            case 5:
                print_p("token corrupt\n");
                break;
            case 6:
                print_p("token revoked\n");
                break;
            case 7:
                print_p("token expired\n");
                break;
            case 8:
                print_p("token cancelled\n");
                break;
            case 9:
                print_p("token deleted\n");
                break;
            case 0xa:
                print_p("invalid token length\n");
                break;
            default:
                print_p("asc=0x23, asq=0x%x\n",
                        exit_stat - DDPT_CAT_TOKOP_BASE);
                break;
            }
        } else
            print_p("%sunexpected exit status value: %d [0x%x]\n", b,
                    exit_stat, exit_stat);
        break;
    }
}

/* Return minimum(num_blks, <blocks_from_sgl-post-blk_off>). First it
 * starts skipping blk_off blocks and if elems is exceeded then it
 * returns 0. Then it sums up the number of blocks from each subsequent
 * sg element checking that elems and max_descriptors are not exceeded. It
 * also stops counting if that sum exceeds num_blks. If max_descriptors is
 * 0 then it is not constraining. Note that elems and blk_off are relative
 * to the start of the sgl; while num_blks and max_descriptors are relative
 * to the sgl+blk_off . */
uint64_t
count_sgl_blocks_from(const struct scat_gath_elem * sglp, int elems,
                      uint64_t blk_off, uint32_t num_blks,
                      uint32_t max_descriptors)
{
    int k, j, md;
    uint64_t res;

    if ((0 == max_descriptors) || (max_descriptors > INT_MAX))
        md = INT_MAX;
    else
        md = (int)max_descriptors;
    for (k = 0; k < elems; ++k, ++sglp) {
        if ((uint64_t)sglp->num > blk_off)
            break;
        blk_off -= sglp->num;
    }
    if (k >= elems)
        return 0;
    for (j = 0, res = 0;
         (k < elems) && (j < md) && (res < (uint64_t)num_blks);
         ++k, ++j, ++sglp) {
        if (0 == j)
            res = (uint64_t)sglp->num - blk_off;  /* will be positive */
        else
            res += (uint64_t)sglp->num;
    }
    return (res < (uint64_t)num_blks) ? res : (uint64_t)num_blks;
}

uint32_t
last_sgl_elem_num(const struct scat_gath_elem * sglp, int elems)
{
    return (sglp && (elems > 0)) ? (sglp + elems - 1)->num : 0;
}

/* Read numbers (up to 64 bits in size) from command line (comma (or
 * (single) space **) separated list). Assumed decimal unless prefixed
 * by '0x', '0X' or contains trailing 'h' or 'H' (which indicate hex).
 * Returns 0 if ok, or 1 if error. Assumed to be LBA (64 bit) and
 * number_of_block (32 bit) pairs. ** Space on command line needs to
 * be escaped, otherwise it is an option separator. */
struct scat_gath_elem *
cli2sgl(const char * inp, int * arr_elemsp, bool b_vb)
{
    bool split;
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
file2sgl_helper(FILE * fp, const char * fnp, bool def_hex, bool real_scan,
                int max_elems, struct scat_gath_elem * res_p, int * errp,
                bool b_vb)
{
    bool bit0;
    bool pre_addr1 = true;
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
            }
            else {
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
        if (pre_addr1) {
            if (('H' == toupper(lcp[0])) && ('E' == toupper(lcp[1])) &&
                ('X' == toupper(lcp[2]))) {
                if (def_hex)
                    continue; /* bypass 'HEX' marker line if expecting hex */
                else {
                    pr2serr("%s: %s: 'hex' string detected on line %d, "
                            "expecting decimal\n", __func__, fnp, j + 1);
                    *errp = SG_LIB_SYNTAX_ERROR;
                    goto err_out;
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
            if (def_hex) {
                if (1 == sscanf(lcp, "%" SCNx64, &ull))
                    ll = (int64_t)ull;
                else
                    ll = -1;
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
                if (bit0) {
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
                } else {
                    if (pre_addr1)
                        pre_addr1 = false;
                    if (real_scan)
                        sge_p->lba = (uint64_t)ll;
                }
            } else {
                if ('#' == *lcp) {
                    --k;
                    break;
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
        }       /* for loop, multiple numbers on 1 line */
        off += (k + 1);
    }   /* end of for loop, one iteration per line */
    if (0x1 & off) {
        *errp = SG_LIB_SYNTAX_ERROR;
        if (b_vb)
            pr2serr("%s: %s: expect LBA,NUM pairs but decoded odd number\n",
                    __func__, fnp);
        goto err_out;
    }
    clearerr(fp);    /* even EOF on first pass needs this before rescan */
    if (! real_scan)
        rewind(fp);
    return off >> 1;
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
file2sgl(const char * file_name, bool def_hex, int * arr_elemsp, int * errp,
         bool b_vb)
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
        if (S_ISDIR(a_stat.st_mode) || S_ISCHR(a_stat.st_mode) ||
            S_ISSOCK(a_stat.st_mode)) {
            if (b_vb)
                pr2serr("%s: %s unsuitable (directory ?)\n", __func__, fnp);
            *errp = sg_convert_errno(EBADF);
            return NULL;
        } else if (S_ISFIFO(a_stat.st_mode) || S_ISSOCK(a_stat.st_mode))
            countable = false;
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
        n = file2sgl_helper(fp, fnp, def_hex, false, 0, &sge_dummy, errp,
                            b_vb);
        if (n <= 0)
            goto err_out;
    } else
        n = MAX_FIXED_SGL_ELEMS;
    m = n;

    res_p = (struct scat_gath_elem *)calloc(n, sizeof(struct scat_gath_elem));
    if (NULL == res_p) {
        *errp = sg_convert_errno(ENOMEM);
        if (b_vb)
            pr2serr("%s: calloc: %s\n", __func__, safe_strerror(ENOMEM));
        return NULL;
    }
    n = file2sgl_helper(fp, fnp, def_hex, true, m, res_p, errp, b_vb);
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
sgl_iter_forward_blks(const struct scat_gath_elem * start_p, int sgl_elems,
                      struct sgl_iter_t * ip, uint32_t add_blks,
                      struct dev_info_t * dip, uint8_t * bp,
                      ddpt_rw_f fp, struct opts_t * op)
{
    bool more;
    int b_off = 0;
    int res = 0;
    int vb = op->verbose;
    uint32_t rem_blks, num;
    uint64_t off, lba;
    int64_t ablocks = add_blks;
    const struct scat_gath_elem * sgl_p = start_p + ip->elem_ind;

    while (ablocks > 0) {
        off = ip->blk_off + ablocks;
        lba = sgl_p->lba;
        num = sgl_p->num;
        more = (off >= num);
        if (vb > 2)
            pr2serr("%s: %s, off=%" PRId64 ", ablocks=%" PRId64 "\n",
                    __func__,  (more ? "more" : "last"), off, ablocks);
        if (ip->elem_ind >= sgl_elems) {
            if ((ip->elem_ind > 0) || (off > 0)) {
                if (vb)
                    pr2serr("%s: incrementing iterator (%d,%u) past end\n",
                            __func__, ip->elem_ind, ip->blk_off);
                return -9999;
            }
        }
        if (more) {             /* more elements after this */
            rem_blks = num - ip->blk_off;    /* rhs must be >= 0 */
            if (rem_blks > 0) {
                if (fp) {
                    pr2serr("  ... LBA=0x%" PRIx64 ", num_blks=%u, b_off=%d"
                            "\n", lba + ip->blk_off, rem_blks, b_off);
                    if ((res = fp(dip, lba + ip->blk_off, rem_blks,
                                  bp + b_off, op)))
                        return res;
                    b_off += rem_blks * dip->bs_pi;
                }
            }
            ablocks -= (int64_t)rem_blks;
            ++ip->elem_ind;
            ip->blk_off = 0;
            ++sgl_p;
        } else {        /* move iter (and call *fp) within current sgl elem */
            rem_blks = (uint32_t)ablocks;
            if (fp) {
                if (vb > 2)
                    pr2serr("  ... LBA=0x%" PRIx64 ", num_blks=%u, b_off=%d"
                            "\n", lba + ip->blk_off, rem_blks, b_off);
                if ((res = fp(dip, lba + ip->blk_off, rem_blks,
                              bp + b_off, op)))
                    return res;
                b_off += rem_blks * dip->bs_pi;
            }
            ablocks -= (int64_t)(rem_blks);
            ip->blk_off = (uint32_t)off;
            break;
        }
    }
    return 0;
}

static int
sgl_iter_backward_blks(const struct scat_gath_elem * start_p,
                       struct sgl_iter_t * ip, uint32_t sub_blks,
                       struct dev_info_t * dip, uint8_t * bp, int b_len,
                       ddpt_rw_f fp, struct opts_t * op)
{
    bool more;
    int b_off = b_len;
    int res = 0;
    int vb = op->verbose;
    uint32_t rem_blks;
    uint64_t lba;
    int64_t off;
    int64_t sblocks = sub_blks;
    const struct scat_gath_elem * sgl_p;

    sgl_p = start_p + ip->elem_ind;
    while (sblocks > 0) {
        off = (int64_t)ip->blk_off - sblocks;
        lba = sgl_p->lba;
        more = (off < 0);
        if (vb > 2)
            pr2serr("%s: %s, off=%" PRId64 ", sblocks=%" PRId64 "\n",
                    __func__,  (more ? "more" : "last"), off, sblocks);
        if (more) {                  /* backward: more than one element */
            if (vb > 2)
                pr2serr("%s: more than 1 element, off=%" PRId64 ", sblocks=%"
                        PRId64 "\n", __func__, off, sblocks);
            if (fp) {
                b_off -= ip->blk_off * dip->bs_pi;
                if (vb > 2)
                    pr2serr("  ... LBA=0x%" PRIx64 ", num_blks=%u, "
                            "b_off=%d\n", lba, ip->blk_off, b_off);
                if ((res = fp(dip, lba, ip->blk_off, bp + b_off, op)))
                    return res;
            }
            sblocks -= (int64_t)ip->blk_off;
            if (0 == ip->elem_ind) {
                if (vb)
                    pr2serr("%s: decrementing iterator (%d,%u) negative\n",
                            __func__, ip->elem_ind, ip->blk_off);
                return -9999;
            }
            --ip->elem_ind;
            --sgl_p;
            ip->blk_off = sgl_p->num;
        } else {        /* move iter (and call *fp) within current sgl elem */
            if (vb > 2)
                pr2serr("%s: last, off=%" PRId64 ", sblocks=%" PRId64 "\n",
                        __func__, off, sblocks);
            rem_blks = (uint32_t)sblocks;
            if (fp) {
                b_off -= rem_blks * dip->bs_pi;
                if (vb > 2)
                    pr2serr("  ... LBA=0x%" PRIx64 ", num_blks=%u, "
                            "b_off=%d\n", lba + off, rem_blks, b_off);
                if ((res = fp(dip, lba + off , rem_blks, bp + b_off, op)))
                    return res;
            }
            sblocks -= (int64_t)rem_blks;
            ip->blk_off = (uint32_t)off;
            break;
        }
    }
    return 0;
}

/* Takes an iterator (iter_p) to a scatter gather list (sgl) array starting
 * at start_p. The iterator is then moved forward (toward the end of the sgl)
 * when add_blks is positive or moved backward when add_blks is negative. If
 * fp is non-NULL then *fp (a function) is called for each sg element
 * traversed by the iter_p. Returns 0 for okay else an error number. -9999
 * is returned for an unexpected error with the iterator. */
int
sgl_iter_add_blks(const struct scat_gath_elem * start_p, int sgl_elems,
                  struct sgl_iter_t * iter_p, int add_blks,
                  struct dev_info_t * dip, uint8_t * bp, int b_len,
                  ddpt_rw_f fp, struct opts_t * op)
{
    bool backwards = (add_blks < 0);
    int e_ind = iter_p->elem_ind;
    uint32_t n_blks = (uint32_t)(backwards ? -add_blks : add_blks);

    if (op->verbose > 3)
        pr2serr("%s: start, sgl_elems=%d, add_blks=%d, b_len=%d\n", __func__,
                sgl_elems, add_blks, b_len);
    if ((0 == add_blks) || (sgl_elems < 1) || (fp && (b_len < 1)))
        return 0;       /* nothing to do */
    if (backwards) {
        if ((e_ind < 0) || ((e_ind == 0) && (n_blks > iter_p->blk_off))) {
            if (op->verbose)
                pr2serr("%s: iterator: %d,%u goes negative\n", __func__,
                        e_ind, iter_p->blk_off);
            return -9999;
        }
        return sgl_iter_backward_blks(start_p, iter_p, n_blks, dip, bp,
                                      b_len, fp, op);
    } else {    /* forward movement */
        if ((e_ind >= sgl_elems) || ((e_ind == (sgl_elems - 1)) &&
              (n_blks + iter_p->blk_off > (start_p + sgl_elems - 1)->num))) {
            if (op->verbose)
                pr2serr("%s: iterator: %d,%u exceeds range\n", __func__,
                        e_ind, iter_p->blk_off);
            return -9999;
        }
        return sgl_iter_forward_blks(start_p, sgl_elems, iter_p, n_blks,
                                     dip, bp, fp, op);
    }
}

/* Assumes sgli_p->elems and sgli_p->slp are setup and the other fields
 * in struct sgl_info_t are zeroed. This function will populate the other
 * fields in that structure. Does one pass through the scatter gather list
 * (array). Sets these fields in struct sgl_info_t: lowest_lba, monotonic,
 * overlapping, sum and sum_hard.  */
void
sgl_sum_scan(struct sgl_info_t * sgli_p, bool b_vb)
{
    bool monot = true;
    bool regular = true;
    int k;
    int elems = sgli_p->elems;
    uint32_t prev_num, t_num;
    uint64_t prev_lba, t_lba, sum, low, high, end;
    const struct scat_gath_elem * start_p = sgli_p->sgl;

    for (k = 0, sum = 0, low = 0, high = 0; k < elems; ++k) {
        if (0 == k) {
            prev_lba = start_p->lba;
            low = prev_lba;
            prev_num = start_p->num;
            sum = start_p->num;
            high = prev_lba + prev_num;
        } else {
            t_lba = (start_p + k)->lba;
            t_num = (start_p + k)->num;
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
    if (k < sgli_p->elems) {
        sgli_p->monotonic = false;
        sgli_p->overlapping = false;
        prev_lba = t_lba;
        ++k;
        for ( ; k < elems; ++k) {
            t_lba = (start_p + k)->lba;
            t_num = (start_p + k)->num;
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
    }
    sgli_p->lowest_lba = low;
    sgli_p->highest_lba = high - 1;   /* adjust so pointing to highest LBA */
    sgli_p->sum = sum;
    sgli_p->sum_hard = (elems > 0) ? ((start_p + elems - 1)->num > 0) : false;
    if (b_vb) {
        pr2serr("%s: elems=%d, arr %spresent, monotonic=%s\n", __func__,
                sgli_p->elems, (sgli_p->sgl ? "" : "not "),
                (sgli_p->monotonic ? "true" : "false"));
        pr2serr("  sum=%" PRId64 ", lowest=0x%" PRIx64 ", highest=",
                sgli_p->sum, sgli_p->lowest_lba);
        if (DDPT_COUNT_INDEFINITE == sgli_p->highest_lba)
            pr2serr("-1\n");
        else
            pr2serr("0x%" PRIX64 "\n", sgli_p->highest_lba);
        pr2serr("  overlapping=%s, sum_hard=%s\n", (sgli_p->overlapping ?
                "true" : "false"), (sgli_p->sum_hard ? "true" : "false"));
    }
}

/* Returns number elements in scatter gather list (array) whose pointer
 * is written to *sge_pp. On error returns negated error number and
 * NULL is written to *sge_pp . The caller is responsible for freeing
 * memory associated with *sge_pp . */
int
build_sgl(struct scat_gath_elem ** sge_pp, int64_t count, int64_t offs)
{
    int k, n;
    const int max_nbs = (INT_MAX - 1);
    const int64_t cnt = count;
    struct scat_gath_elem * sge;

    for (k = 0; count > 0; ++k) {
        if (count <= max_nbs)
            break;
        count -= max_nbs;
    }
    n = k + 1;
    *sge_pp = (struct scat_gath_elem *)
                        calloc(n, sizeof(struct scat_gath_elem));
    if (NULL == *sge_pp) {
        pr2serr("%s: no memory available for sgl\n", __func__);
        return -sg_convert_errno(ENOMEM);
    }
    sge = *sge_pp;
    for (k = 0, count = 0; k < n; ++k, ++sge, count += max_nbs) {
        sge->lba = offs + count;
        if ((cnt - count) <= max_nbs)
            sge->num = cnt - count;
        else
            sge->num = max_nbs;
    }
    return n;
}


