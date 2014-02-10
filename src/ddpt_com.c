/*
 * Copyright (c) 2013-2014 Douglas Gilbert.
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

/*
 * This file contains some common functions for ddpt.
 */

/* Was needed for posix_fadvise() */
/* #define _XOPEN_SOURCE 600 */

/* Need _GNU_SOURCE for O_DIRECT */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
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

#include "ddpt.h"

#ifdef SG_LIB_LINUX
#include <sys/ioctl.h>
#include <sys/sysmacros.h>
#include <sys/file.h>
#include <linux/major.h>
#include <linux/fs.h>   /* <sys/mount.h> */
#include <linux/mtio.h> /* For tape ioctls */
#ifndef MTWEOFI
#define MTWEOFI 35  /* write an end-of-file record (mark) in immediate mode */
#endif

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


static const char * errblk_file = "errblk.txt";


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

/* Abbreviation of fprintf(stderr, ...) */
int     /* Global function */
pr2serr(const char * fmt, ...)
{
    va_list args;
    int n;

    va_start(args, fmt);
    n = vfprintf(stderr, fmt, args);
    va_end(args);
    return n;
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
print_stats(const char * str, struct opts_t * op)
{
#ifdef SG_LIB_LINUX
    /* Print tape read summary if necessary . */
    print_tape_summary(op, 0, str);
#endif

    if ((0 != op->dd_count) && (! op->reading_fifo))
        pr2serr("  remaining block count=%" PRId64 "\n", op->dd_count);
    pr2serr("%s%" PRId64 "+%d records in\n", str, op->in_full,
            op->in_partial);
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
    if (op->has_xcopy)
        pr2serr("%s%" PRId64 " xcopy command%s done\n", str, op->num_xcopy,
                ((1 == op->num_xcopy) ? "" : "s"));
}

/* Attempt to categorize the file type from the given filename.
 * Separate version for Windows and Unix. Windows version does some
 * file name processing. */
#ifndef SG_LIB_WIN32

#ifdef SG_LIB_LINUX
static int bsg_major_checked = 0;
static int bsg_major = 0;

/* In Linux search /proc/devices for bsg character driver in order to
 * find its major device number since it is allocated dynamically.  */
static void
find_bsg_major(int verbose)
{
    const char * proc_devices = "/proc/devices";
    FILE *fp;
    char a[128];
    char b[128];
    char * cp;
    int n;

    if (NULL == (fp = fopen(proc_devices, "r"))) {
        if (verbose)
            pr2serr("fopen %s failed: %s\n", proc_devices,
                    strerror(errno));
        return;
    }
    while ((cp = fgets(b, sizeof(b), fp))) {
        if ((1 == sscanf(b, "%s", a)) &&
            (0 == memcmp(a, "Character", 9)))
            break;
    }
    while (cp && (cp = fgets(b, sizeof(b), fp))) {
        if (2 == sscanf(b, "%d %s", &n, a)) {
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
int
dd_filetype(const char * filename, int verbose)
{
    struct stat st;
    size_t len = strlen(filename);

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
            bsg_major_checked = 1;
            find_bsg_major(verbose);
        }
        if (bsg_major == (int)major(st.st_rdev))
            return FT_PT;
        return FT_CHAR; /* assume something like /dev/zero */
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
        off += my_snprintf(buff + off, max_bufflen - off,
                           "unable to 'stat' %s ", (fname ? fname : "file"));
    return buff;
}

/* get_blkdev_capacity() returns 0 -> success or -1 -> failure.
 * which_arg should either be DDPT_ARG_IN, DDPT_ARG_OUT or DDPT_ARG_OUT2.
 * If successful writes back sector size (logical block
 * size) using the sect_sz * pointer. Also writes back the number of
 * sectors (logical blocks) on the block device using num_sect pointer. */

#ifdef SG_LIB_LINUX
int
get_blkdev_capacity(struct opts_t * op, int which_arg, int64_t * num_sect,
                    int * sect_sz)
{
    int blk_fd;
    const char * fname;

    blk_fd = (DDPT_ARG_IN == which_arg) ? op->idip->fd : op->odip->fd;
    fname = (DDPT_ARG_IN == which_arg) ? op->idip->fn : op->odip->fn;
    if (op->verbose > 2)
        pr2serr("get_blkdev_capacity: for %s\n", fname);
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
        if (op->verbose > 5)
            pr2serr("Used Linux BLKGETSIZE64 ioctl\n");
 #else
        unsigned long ul;

        if (ioctl(blk_fd, BLKGETSIZE, &ul) < 0) {
            perror("BLKGETSIZE ioctl error");
            return -1;
        }
        *num_sect = (int64_t)ul;
        if (op->verbose > 5)
            pr2serr("Used Linux BLKGETSIZE ioctl\n");
 #endif
    }
    return 0;
#else   /* not BLKSSZGET */
    blk_fd = blk_fd;
    if (op->verbose)
        pr2serr("      BLKSSZGET+BLKGETSIZE ioctl not available\n");
    *num_sect = 0;
    *sect_sz = 0;
    return -1;
#endif
}
#endif  /* BLKSSZGET */

#ifdef SG_LIB_FREEBSD
int
get_blkdev_capacity(struct opts_t * op, int which_arg, int64_t * num_sect,
                    int * sect_sz)
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
        pr2serr("get_blkdev_capacity: for %s\n", fname);

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
int
get_blkdev_capacity(struct opts_t * op, int which_arg, int64_t * num_sect,
                    int * sect_sz)
{
    struct dk_minfo info;
    int blk_fd;
    const char * fname;

    blk_fd = (DDPT_ARG_IN == which_arg) ? op->idip->fd : op->odip->fd;
    fname = (DDPT_ARG_IN == which_arg) ? op->idip->fn : op->odip->fn;
    if (op->verbose > 2)
        pr2serr("get_blkdev_capacity: for %s\n", fname);

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

void
zero_coe_limit_count(struct opts_t * op)
{
    if (op->coe_limit > 0)
        op->coe_count = 0;
}

/* Print number of blocks, block size. If over 1 MB print size in MB
 * (10**6 bytes), GB (10**9 bytes) or TB (10**12 bytes) to stderr. */
void
print_blk_sizes(const char * fname, const char * access_typ, int64_t num_sect,
                int sect_sz)
{
    int mb, gb, tb;
    size_t len;
    int64_t n = 0;
    char b[32];
    char dec[4];

    if (num_sect <= 0) {
        pr2serr("  %s [%s]: blocks=%" PRId64 ", _bs=%d\n", fname, access_typ,
                num_sect, sect_sz);
        return;
    }
    gb = 0;
    if ((num_sect > 0) && (sect_sz > 0)) {
        n = num_sect * sect_sz;
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
        pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                "_bs=%d, %s.%s PB\n", fname, access_typ, num_sect,
                num_sect, sect_sz, b, dec);
    } else if (gb > 99999) {
        tb = gb / 1000;
        pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                "_bs=%d, %d TB\n", fname, access_typ, num_sect,
                num_sect, sect_sz, tb);
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
            pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "_bs=%d, %s.%s TB\n", fname, access_typ, num_sect,
                    num_sect, sect_sz, b, dec);
        } else if (mb > 99999) {
            gb = mb / 1000;
            pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "_bs=%d, %d GB\n", fname, access_typ, num_sect,
                    num_sect, sect_sz, gb);
        } else if (mb > 999) {
            snprintf(b, sizeof(b), "%d", mb);
            len = strlen(b); // len must be >= 4
            dec[0] = b[len - 3];
            dec[1] = b[len - 2];
            dec[2] = '\0';
            b[len - 3] = '\0';
            pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "_bs=%d, %s.%s GB\n", fname, access_typ, num_sect,
                    num_sect, sect_sz, b, dec);
        } else if (mb > 0) {
            pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "_bs=%d, %d MB%s\n", fname, access_typ, num_sect,
                    num_sect, sect_sz, mb, ((mb < 10) ? " approx" : ""));
        } else
            pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "_bs=%d\n", fname, access_typ, num_sect, num_sect,
                    sect_sz);
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
            op->start_tm_valid = 1;
    }
#elif defined(HAVE_GETTIMEOFDAY)
    if (op->do_time) {
        op->start_tm.tv_sec = 0;
        op->start_tm.tv_usec = 0;
        gettimeofday(&op->start_tm, NULL);
        op->start_tm_valid = 1;
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
 * makes an estimate of the time remaining. */
void
calc_duration_throughput(const char * leadin, int contin, struct opts_t * op)
{
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    struct timespec end_tm, res_tm;
    double a, b, r;
    int secs, h, m;
    int64_t blks;

    if (op->start_tm_valid && (op->start_tm.tv_sec || op->start_tm.tv_nsec)) {
        blks = op->in_full;
        clock_gettime(CLOCK_MONOTONIC, &end_tm);
        res_tm.tv_sec = end_tm.tv_sec - op->start_tm.tv_sec;
        res_tm.tv_nsec = end_tm.tv_nsec - op->start_tm.tv_nsec;
        if (res_tm.tv_nsec < 0) {
            --res_tm.tv_sec;
            res_tm.tv_nsec += 1000000000;
        }
        a = res_tm.tv_sec;
        a += (0.000001 * (res_tm.tv_nsec / 1000));
        b = (double)op->ibs_hold * blks;
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
                h = secs / 3600;
                secs = secs - (h * 3600);
                m = secs / 60;
                secs = secs - (m * 60);
                if (h > 0)
                    pr2serr("%sestimated time remaining: %d:%02d:%02d\n",
                            leadin, h, m, secs);
                else
                    pr2serr("%sestimated time remaining: %d:%02d\n",
                            leadin, m, secs);
            }
        }
    }
#elif defined(HAVE_GETTIMEOFDAY)
    struct timeval end_tm, res_tm;
    double a, b, r;
    int secs, h, m;
    int64_t blks;

    if (op->start_tm_valid && (op->start_tm.tv_sec || op->start_tm.tv_usec)) {
        blks = op->in_full;
        gettimeofday(&end_tm, NULL);
        res_tm.tv_sec = end_tm.tv_sec - op->start_tm.tv_sec;
        res_tm.tv_usec = end_tm.tv_usec - op->start_tm.tv_usec;
        if (res_tm.tv_usec < 0) {
            --res_tm.tv_sec;
            res_tm.tv_usec += 1000000;
        }
        a = res_tm.tv_sec;
        a += (0.000001 * res_tm.tv_usec);
        b = (double)op->ibs_hold * blks;
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
                h = secs / 3600;
                secs = secs - (h * 3600);
                m = secs / 60;
                secs = secs - (m * 60);
                if (h > 0)
                    pr2serr("%sestimated time remaining: "
                            "%d:%02d:%02d\n", leadin, h, m, secs);
                else
                    pr2serr("%sestimated time remaining: "
                            "%d:%02d\n", leadin, m, secs);
            }
        }
    }
#else   /* no clock reading functions available */
    if (leadin) { ; }    // suppress warning
    if (contin) { ; }    // suppress warning
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
    static int lastreadpos, lastwritepos;
    static char lastreadposvalid = 0;
    static char lastwriteposvalid = 0;
    int res;
    struct mtpos pos;

    if (op->verbose > 1) {
        if (FT_TAPE & op->idip->d_type) {
            res = ioctl(op->idip->fd, MTIOCPOS, &pos);
            if (0 == res) {
                if ((pos.mt_blkno != lastreadpos) ||
                    (0 == lastreadposvalid)) {
                    lastreadpos = pos.mt_blkno;
                    lastreadposvalid = 1;
                    pr2serr("%stape position%s: %u%s\n", prefix,
                            (FT_TAPE & op->odip->d_type) ? " (reading)" : "",
                            lastreadpos, postfix);
                }
            } else {
                lastreadposvalid = 0;
                show_tape_pos_error((FT_TAPE & op->odip->d_type) ?
                                    " (reading)" : "");
            }
        }

        if (FT_TAPE & op->odip->d_type) {
            res = ioctl(op->odip->fd, MTIOCPOS, &pos);
            if (0 == res) {
                if ((pos.mt_blkno != lastwritepos) ||
                    (0 == lastwriteposvalid)) {
                    lastwritepos = pos.mt_blkno;
                    lastwriteposvalid = 1;
                    pr2serr("%stape position%s: %u%s\n", prefix,
                            (FT_TAPE & op->idip->d_type) ? " (writing)" : "",
                            lastwritepos, postfix);
                }
            } else {
                lastwriteposvalid = 0;
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
    struct sigaction act;
    sigset_t starting_mask;
    int num_members = 0;
    int unblock_starting_mask = 0;

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
        ++unblock_starting_mask;
    }
    if (sigismember(&starting_mask, SIGINT)) {
        if (op->verbose)
            pr2serr("SIGINT blocked on entry, unblock\n");
        ++unblock_starting_mask;
    }
    if (sigismember(&starting_mask, SIGPIPE)) {
        if (op->verbose)
            pr2serr("SIGPIPE blocked on entry, unblock\n");
        ++unblock_starting_mask;
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
    char b[32];
    int got_something = 0;
    int delay = 0;

#if SA_NOCLDSTOP
    int found_pending = 0;

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
            found_pending = 1;
        } else {        /* nothing pending so perhaps delay */
            if ((op->delay > 0) && (DELAY_COPY_SEGMENT == delay_type))
                delay = op->delay;
            else if ((op->wdelay > 0) && (DELAY_WRITE == delay_type)) {
                if (op->subsequent_wdelay)
                    delay = op->wdelay;
                else
                    op->subsequent_wdelay = 1;
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

        got_something = 1;
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
            print_stats("", op);
            /* Don't show next message if using oflag=pre-alloc and we didn't
             * use FALLOC_FL_KEEP_SIZE */
            if ((0 == op->reading_fifo) && (FT_REG & op->odip->d_type_hold)
                 && (0 == op->oflagp->prealloc))
                pr2serr("To resume, invoke with same arguments plus "
                        "oflag=resume\n");
            ; // >>>>>>>>>>>>> cleanup ();
        } else {
            pr2serr("Progress report:\n");
            print_stats("  ", op);
            if (op->do_time)
                calc_duration_throughput("  ", 1, op);
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
                op->subsequent_wdelay = 1;
        }
        if (delay)
            sleep_ms(op->delay);
    }
}
