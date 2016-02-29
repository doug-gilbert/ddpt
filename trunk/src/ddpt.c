/*
 * Copyright (c) 2008-2016 Douglas Gilbert.
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
 * So need CreateFile, ReadFile, WriteFile, SetFilePointer and friends.
 */

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
#include <sys/types.h>
#include <sys/stat.h>

/* N.B. config.h must precede anything that depends on HAVE_*  */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


static const char * ddpt_version_str = "0.96 20160229 [svn: r325]";

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
#endif

#include "ddpt.h"
#include "sg_lib.h"
#include "sg_pr2serr.h"

#ifndef EREMOTEIO
#define EREMOTEIO EIO
#endif

/* Used for outputting diagnostic messages for oflag=pre-alloc */
#define PREALLOC_DEBUG 1



/* Returns open input file descriptor (>= 0) or a negative value
 * (-SG_LIB_FILE_ERROR or -SG_LIB_CAT_OTHER) if error.
 */
static int
open_if(struct opts_t * op)
{
    int flags;
    int fd = -SG_LIB_FILE_ERROR;
    char ebuff[EBUFF_SZ];
    struct flags_t * ifp = op->iflagp;
    struct dev_info_t * idip = op->idip;
    const char * ifn = idip->fn;

    idip->d_type = dd_filetype(ifn, op->verbose);
    if (FT_ERROR & idip->d_type) {
        pr2serr("unable to access %s\n", ifn);
        goto file_err;
    } else if (((FT_BLOCK | FT_TAPE | FT_OTHER) & idip->d_type) && ifp->pt)
        idip->d_type |= FT_PT;
    if (op->verbose)
        pr2serr(" >> Input file type: %s\n",
                dd_filetype_str(idip->d_type, ebuff, EBUFF_SZ, ifn));
    if (!(FT_PT & idip->d_type) && op->rdprotect)
        pr2serr("rdprotect ignored on non-pt device\n");
    if ((FT_FIFO | FT_CHAR | FT_TAPE) & idip->d_type)
        ++op->reading_fifo;

    if ((FT_TAPE & idip->d_type) && (FT_PT & idip->d_type)) {
        pr2serr("SCSI tape device %s not supported via pt\n", ifn);
        goto file_err;
    }
    if (FT_PT & idip->d_type) {
        fd = pt_open_if(op, NULL);
        if (-1 == fd)
            goto file_err;
        else if (fd < -1)
            goto other_err;
    }
#ifdef SG_LIB_WIN32
    else if (FT_BLOCK & idip->d_type) {
        if (win32_open_if(op, (ifp->excl ? O_EXCL : 0), op->verbose))
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
        fd = open(ifn, flags);
        if (fd < 0) {
            pr2serr("could not open %s for reading: %s\n", ifn,
                    safe_strerror(errno));
            goto file_err;
        } else {
            if (sg_set_binary_mode(fd) < 0)
                perror("sg_set_binary_mode");
            if (op->verbose)
                pr2serr("        open %s, flags=0x%x\n", ifn, flags);
#ifdef HAVE_POSIX_FADVISE
            if (ifp->nocache) {
                int rt;

                rt = posix_fadvise(fd, 0, 0, POSIX_FADV_SEQUENTIAL);
                if (rt)
                    pr2serr("%s: posix_fadvise(SEQUENTIAL), err=%d\n",
                            __func__, rt);
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
            pr2serr("flock(LOCK_EX | LOCK_NB) on %s failed: %s\n",
                    ifn, safe_strerror(errno));
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
open_of(struct opts_t * op)
{
    int flags;
    int fd = -SG_LIB_FILE_ERROR;
    int outf_exists = 0;
    char ebuff[EBUFF_SZ];
    struct stat st;
    struct flags_t * ofp = op->oflagp;
    struct dev_info_t * odip = op->odip;
    const char * ofn = odip->fn;

    odip->d_type = dd_filetype(ofn, op->verbose);
    if (((FT_BLOCK | FT_TAPE | FT_OTHER) & odip->d_type) && ofp->pt)
        odip->d_type |= FT_PT;
    odip->d_type_hold = odip->d_type;
    if (op->verbose)
        pr2serr(" >> Output file type: %s\n",
                dd_filetype_str(odip->d_type, ebuff, EBUFF_SZ, ofn));
    if (!(FT_PT & odip->d_type) && op->wrprotect)
        pr2serr("wrprotect ignored on non-pt device\n");

    if ((FT_TAPE & odip->d_type) && (FT_PT & odip->d_type)) {
        pr2serr("SCSI tape device %s not supported via pt\n", ofn);
        goto file_err;
    }
    if (FT_PT & odip->d_type) {
        fd = pt_open_of(op, NULL);
        if (-1 == fd)
            goto file_err;
        else if (fd < -1)
            goto other_err;
    } else if (FT_DEV_NULL & odip->d_type)
        fd = -1; /* don't bother opening */
#ifdef SG_LIB_WIN32
    else if (FT_BLOCK & odip->d_type) {
        if (win32_open_of(op, (ofp->excl ? O_EXCL : 0), op->verbose))
            goto file_err;
        fd = 0;
    }
#endif
    else {      /* typically regular file or block device node */
        int needs_ftruncate = 0;
        int64_t offset = 0;

        memset(&st, 0, sizeof(st));
        if (0 == stat(ofn, &st))
            outf_exists = 1;
        else if (ofp->pt) {
            /* if oflag=pt, then creating a regular file is unhelpful */
            pr2serr("Cannot create a regular file called %s as a pt\n", ofn);
            goto other_err;
        }
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
        if ((FT_REG & odip->d_type) && outf_exists && ofp->trunc &&
            (! ofp->nowrite)) {
            if (op->seek > 0) {
                offset = op->seek * op->obs;
                if (st.st_size > offset)
                    ++needs_ftruncate;  // only truncate to shorten
            } else
                flags |= O_TRUNC;
        }
        if ((fd = open(ofn, flags, 0666)) < 0) {
            pr2serr("could not open %s for writing: %s\n", ofn,
                    safe_strerror(errno));
            goto file_err;
        }
        if (needs_ftruncate && (offset > 0)) {
            if (ftruncate(fd, offset) < 0) {
                pr2serr("could not ftruncate %s after open (seek): %s\n",
                        ofn, safe_strerror(errno));
                goto file_err;
            }
            /* N.B. file offset (pointer) not changed by ftruncate */
        }
        if ((! outf_exists) && (FT_ERROR & odip->d_type)) {
            odip->d_type = FT_REG;   /* exists now */
            odip->d_type_hold = odip->d_type;
        }
        if (sg_set_binary_mode(fd) < 0)
            perror("sg_set_binary_mode");
        if (op->verbose) {
            pr2serr("        %s %s, flags=0x%x\n",
                    (outf_exists ? "open" : "create"), ofn, flags);
            if (needs_ftruncate && (offset > 0))
                pr2serr("        truncated file at byte offset "
                        "%" PRId64 " \n", offset);
        }
    }
#ifdef SG_LIB_LINUX
    if (ofp->flock) {
        int res;

        res = flock(fd, LOCK_EX | LOCK_NB);
        if (res < 0) {
            close(fd);
            pr2serr("flock(LOCK_EX | LOCK_NB) on %s failed: %s\n",
                    ofn, safe_strerror(errno));
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

/* Helper for calc_count(). Attempts to size IFILE. Returns 0 if no error
 * detected. */
static int
calc_count_in(struct opts_t * op, int64_t * in_num_blksp)
{
    int res;
    struct stat st;
    int in_blk_sz, in_type;
#ifndef SG_LIB_WIN32
    int64_t num_blks, t;
    int blk_sz;
#endif
    const char * ifn = op->idip->fn;

    *in_num_blksp = DDPT_COUNT_INDEFINITE;
    in_type = op->idip->d_type;
    if (FT_PT & in_type) {
        if (op->iflagp->norcap) {
            if ((FT_BLOCK & in_type) && (0 == op->iflagp->force)) {
                pr2serr(">> warning: norcap on input block device "
                        "accessed via pt is risky.\n");
                pr2serr(">> Abort copy, use iflag=force to override.\n");
                return -1;
            }
            return 0;
        }
        res = pt_read_capacity(op, DDPT_ARG_IN, in_num_blksp, &in_blk_sz);
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            pr2serr("Unit attention (readcap in), continuing\n");
            res = pt_read_capacity(op, DDPT_ARG_IN, in_num_blksp,
                                   &in_blk_sz);
        } else if (SG_LIB_CAT_ABORTED_COMMAND == res) {
            pr2serr("Aborted command (readcap in), continuing\n");
            res = pt_read_capacity(op, DDPT_ARG_IN, in_num_blksp,
                                   &in_blk_sz);
        }
        if (0 != res) {
            if (res == SG_LIB_CAT_INVALID_OP)
                pr2serr("read capacity not supported on %s\n", ifn);
            else if (res == SG_LIB_CAT_NOT_READY)
                pr2serr("read capacity failed on %s - not ready\n", ifn);
            else
                pr2serr("Unable to read capacity on %s\n", ifn);
            *in_num_blksp = DDPT_COUNT_INDEFINITE;
            return res;
        } else {
            if (op->verbose) {
                print_blk_sizes(ifn, "readcap", *in_num_blksp, in_blk_sz, 1);
                if (op->idip->prot_type > 0)
                    pr2serr("    reports Protection_type=%d, p_i_exp=%d\n",
                            op->idip->prot_type, op->idip->p_i_exp);
            }
            if ((*in_num_blksp > 0) && (in_blk_sz != op->ibs)) {
                pr2serr(">> warning: %s block size confusion: ibs=%d, "
                        "device claims=%d\n", ifn, op->ibs, in_blk_sz);
                if (0 == op->iflagp->force) {
                    pr2serr(">> abort copy, use iflag=force to override\n");
                    return -1;
                }
            }
        }
#ifndef SG_LIB_WIN32
        if ((FT_BLOCK & in_type) && (0 == op->iflagp->force) &&
            (0 == get_blkdev_capacity(op, DDPT_ARG_IN, &num_blks,
                                      &blk_sz))) {
            t = (*in_num_blksp) * in_blk_sz;
            if (t != (num_blks * blk_sz)) {
                pr2serr(">> warning: Size of input block device is "
                        "different from pt size.\n>> Pass-through on block "
                        "partition can give unexpected offsets.\n");
                pr2serr(">> Abort copy, use iflag=force to override.\n");
                return -1;
            }
        }
#endif
    } else if ((op->dd_count > 0) && (0 == op->oflagp->resume))
        return 0;
    else if (FT_BLOCK & in_type) {
        if (0 != get_blkdev_capacity(op, DDPT_ARG_IN, in_num_blksp,
                                     &in_blk_sz)) {
            pr2serr("Unable to read block capacity on %s\n", ifn);
            *in_num_blksp = DDPT_COUNT_INDEFINITE;
        }
        if (op->verbose)
            print_blk_sizes(ifn, "blk", *in_num_blksp, in_blk_sz, 1);
        if ((*in_num_blksp > 0) && (op->ibs != in_blk_sz)) {
            pr2serr(">> warning: %s block size confusion: bs=%d, "
                    "device claims=%d\n", ifn, op->ibs, in_blk_sz);
            *in_num_blksp = DDPT_COUNT_INDEFINITE;
        }
    } else if (FT_REG & in_type) {
        if (fstat(op->idip->fd, &st) < 0) {
            perror("fstat(idip->fd) error");
            *in_num_blksp = DDPT_COUNT_INDEFINITE;
        } else {
            *in_num_blksp = st.st_size / op->ibs;
            res = st.st_size % op->ibs;
            if (op->verbose) {
                print_blk_sizes(ifn, "reg", *in_num_blksp, op->ibs, 1);
                if (res)
                    pr2serr("    residual_bytes=%d\n", res);
            }
            if (res)
                ++*in_num_blksp;
        }
    }
    return 0;
}

/* Helper for calc_count(). Attempts to size OFILE. Returns 0 if no error
 * detected. */
static int
calc_count_out(struct opts_t * op, int64_t * out_num_blksp)
{
    int res;
    struct stat st;
    int out_blk_sz, out_type;
#ifndef SG_LIB_WIN32
    int64_t num_blks, t;
    int blk_sz;
#endif
    const char * ofn = op->odip->fn;

    *out_num_blksp = DDPT_COUNT_INDEFINITE;
    out_type = op->odip->d_type;
    if (FT_PT & out_type) {
        if (op->oflagp->norcap) {
            if ((FT_BLOCK & out_type) && (0 == op->oflagp->force)) {
                pr2serr(">> warning: norcap on output block device "
                        "accessed via pt is risky.\n");
                pr2serr(">> Abort copy, use oflag=force to override.\n");
                return -1;
            }
            return 0;
        }
        res = pt_read_capacity(op, DDPT_ARG_OUT, out_num_blksp, &out_blk_sz);
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            pr2serr("Unit attention (readcap out), continuing\n");
            res = pt_read_capacity(op, DDPT_ARG_OUT, out_num_blksp,
                                   &out_blk_sz);
        } else if (SG_LIB_CAT_ABORTED_COMMAND == res) {
            pr2serr("Aborted command (readcap out), continuing\n");
            res = pt_read_capacity(op, DDPT_ARG_OUT, out_num_blksp,
                                   &out_blk_sz);
        }
        if (0 != res) {
            if (res == SG_LIB_CAT_INVALID_OP)
                pr2serr("read capacity not supported on %s\n", ofn);
            else
                pr2serr("Unable to read capacity on %s\n", ofn);
            *out_num_blksp = DDPT_COUNT_INDEFINITE;
            return res;
        } else {
            if (op->verbose) {
                print_blk_sizes(ofn, "readcap", *out_num_blksp, out_blk_sz,
                                1);
                if (op->odip->prot_type > 0)
                    pr2serr("    reports Protection_type=%d, p_i_exp=%d\n",
                            op->odip->prot_type, op->odip->p_i_exp);
            }
            if ((*out_num_blksp > 0) && (op->obs != out_blk_sz)) {
                pr2serr(">> warning: %s block size confusion: "
                        "obs=%d, device claims=%d\n", ofn, op->obs,
                        out_blk_sz);
                if (0 == op->oflagp->force) {
                    pr2serr(">> abort copy, use oflag=force to override\n");
                    return -1;
                }
            }
        }
#ifndef SG_LIB_WIN32
        if ((FT_BLOCK & out_type) && (0 == op->oflagp->force) &&
             (0 == get_blkdev_capacity(op, DDPT_ARG_OUT, &num_blks,
                                       &blk_sz))) {
            t = (*out_num_blksp) * out_blk_sz;
            if (t != (num_blks * blk_sz)) {
                pr2serr(">> warning: size of output block device is "
                        "different from pt size.\n>> Pass-through on block "
                        "partition can give unexpected results.\n");
                pr2serr(">> abort copy, use oflag=force to override\n");
                return -1;
            }
        }
#endif
    } else if ((op->dd_count > 0) && (0 == op->oflagp->resume))
        return 0;
    if (FT_BLOCK & out_type) {
        if (0 != get_blkdev_capacity(op, DDPT_ARG_OUT, out_num_blksp,
                                     &out_blk_sz)) {
            pr2serr("Unable to read block capacity on %s\n", ofn);
            *out_num_blksp = DDPT_COUNT_INDEFINITE;
        } else {
            if (op->verbose)
                print_blk_sizes(ofn, "blk", *out_num_blksp, out_blk_sz, 1);
            if ((*out_num_blksp > 0) && (op->obs != out_blk_sz)) {
                pr2serr(">> warning: %s block size confusion: obs=%d, "
                        "device claims=%d\n", ofn, op->obs, out_blk_sz);
                *out_num_blksp = DDPT_COUNT_INDEFINITE;
            }
        }
    } else if (FT_REG & out_type) {
        if (fstat(op->odip->fd, &st) < 0) {
            perror("fstat(odip->fd) error");
            *out_num_blksp = DDPT_COUNT_INDEFINITE;
        } else {
            *out_num_blksp = st.st_size / op->obs;
            res = st.st_size % op->obs;
            if (op->verbose) {
                print_blk_sizes(ofn, "reg", *out_num_blksp, op->obs, 1);
                if (res)
                    pr2serr("    residual_bytes=%d\n", res);
            }
            if (res)
                ++*out_num_blksp;
        }
    }
    return 0;
}


/* Calculates the number of blocks associated with the in and out files.
 * May also yield the block size in bytes of devices. For regular files
 * uses ibs or obs as the logical block size. Returns 0 for continue,
 * otherwise bypass copy and exit. */
static int
calc_count(struct opts_t * op, int64_t * in_num_blksp,
           int64_t * out_num_blksp)
{
    int res;

    res = calc_count_in(op, in_num_blksp);
    if (res) {
        *out_num_blksp = DDPT_COUNT_INDEFINITE;
        return res;
    }
    return calc_count_out(op, out_num_blksp);
}

#ifdef HAVE_POSIX_FADVISE
/* Used by iflag=nocache and oflag=nocache to suggest (via posix_fadvise()
 * system call) that the OS doesn't cache data it has just read or written
 * since it is unlikely to be used again in the short term. iflag=nocache
 * additionally increases the read-ahead. Errors ignored. */
static void
do_fadvise(struct opts_t * op, int bytes_if, int bytes_of, int bytes_of2)
{
    int rt, in_valid, out2_valid, out_valid, id_type, od_type, o2d_type;

    id_type = op->idip->d_type;
    od_type = op->odip->d_type;
    o2d_type = op->o2dip->d_type;
    in_valid = ((FT_REG == id_type) || (FT_BLOCK == id_type));
    out2_valid = ((FT_REG == o2d_type) || (FT_BLOCK == o2d_type));
    out_valid = ((FT_REG == od_type) || (FT_BLOCK == od_type));
    if (op->iflagp->nocache && (bytes_if > 0) && in_valid) {
        if ((op->lowest_skip < 0) || (op->skip > op->lowest_skip))
            op->lowest_skip = op->skip;
        rt = posix_fadvise(op->idip->fd, (op->lowest_skip * op->ibs),
                           ((op->skip - op->lowest_skip) * op->ibs) + bytes_if,
                           POSIX_FADV_DONTNEED);
        if (rt)         /* returns error as result */
            pr2serr("posix_fadvise on read, skip=%" PRId64 " ,err=%d\n",
                    op->skip, rt);
    }
    if ((op->oflagp->nocache & 2) && (bytes_of2 > 0) && out2_valid) {
        rt = posix_fadvise(op->o2dip->fd, 0, 0, POSIX_FADV_DONTNEED);
        if (rt)
            pr2serr("posix_fadvise on of2, seek=%" PRId64 " ,err=%d\n",
                    op->seek, rt);
    }
    if ((op->oflagp->nocache & 1) && (bytes_of > 0) && out_valid) {
        if ((op->lowest_seek < 0) || (op->seek > op->lowest_seek))
            op->lowest_seek = op->seek;
        rt = posix_fadvise(op->odip->fd, (op->lowest_seek * op->obs),
                   ((op->seek - op->lowest_seek) * op->obs) + bytes_of,
                           POSIX_FADV_DONTNEED);
        if (rt)
            pr2serr("posix_fadvise on output, seek=%" PRId64 " , err=%d\n",
                    op->seek, rt);
    }
}
#endif

/* Main copy loop's read (input) via pt. Returns 0 on success, else see
 * pt_read()'s return values. */
static int
cp_read_pt(struct opts_t * op, struct cp_state_t * csp, unsigned char * bp)
{
    int res;
    int blks_read = 0;

    res = pt_read(op, 0, bp, csp->icbpt, &blks_read);
    if (res) {
        if (0 == blks_read) {
            pr2serr("pt_read failed,%s at or after lba=%" PRId64 " "
                    "[0x%" PRIx64 "]\n",
                    ((-2 == res) ?  " try reducing bpt," : ""),
                    op->skip, op->skip);
            return res;
        }
        /* limp on if data, should stop after write; hold err number */
        op->err_to_report = res;
    }
    if (blks_read < csp->icbpt) {
        /* assume close to end, or some data prior to read error */
        if (op->verbose > 1)
            pr2serr("short read, requested %d blocks, got %d blocks\n",
                    csp->icbpt, blks_read);
        ++csp->leave_after_write;
        /* csp->leave_reason = 0; assume at end rather than error */
        csp->icbpt = blks_read;
        /* round down since don't do partial writes from pt reads */
        csp->ocbpt = (blks_read * op->ibs) / op->obs;
    }
    op->in_full += csp->icbpt;
    return 0;
}

/* Error occurred on block/regular read. coe active so assume all full
 * blocks prior to error are good (if any) and start to read from the
 * block containing the error, one block at a time, until ibpt. Supply
 * zeros for unreadable blocks. Return 0 if successful, SG_LIB_CAT_OTHER
 * if error other than EIO or EREMOTEIO, SG_LIB_FILE_ERROR if lseek fails,
 * and SG_LIB_CAT_MEDIUM_HARD if the coe_limit is exceeded. */
static int
coe_cp_read_block_reg(struct opts_t * op, struct cp_state_t * csp,
                      unsigned char * bp, int numread_errno)
{
    int res, res2, k, total_read, num_read;
    int ibs = op->ibs_pi;
    int64_t offset, off_res, my_skip;

    if (0 == numread_errno) {
        csp->icbpt = 0;
        csp->ocbpt = 0;
        ++csp->leave_after_write;
        csp->leave_reason = 0;
        return 0;       /* EOF */
    } else if (numread_errno < 0) {
        if ((-EIO == numread_errno) || (-EREMOTEIO == numread_errno)) {
            num_read = 0;
            if (1 == csp->icbpt) {
                // Don't read again, this must be bad block
                memset(bp, 0, ibs);
                if ((res2 = coe_process_eio(op, op->skip)))
                    return res2;
                ++op->in_full;
                csp->bytes_read += ibs;
                return 0;
            }
        } else
            return SG_LIB_CAT_OTHER;
    } else
        num_read = (numread_errno / ibs) * ibs;

    k = num_read / ibs;
    if (k > 0) {
        op->in_full += k;
        zero_coe_limit_count(op);
    }
    csp->bytes_read = num_read;
    my_skip = op->skip + k;
    offset = my_skip * ibs;
    bp += num_read;
    for ( ; k < csp->icbpt; ++k, ++my_skip, bp += ibs, offset += ibs) {
        if (offset != csp->if_filepos) {
            if (op->verbose > 2)
                pr2serr("moving if filepos: new_pos=%" PRId64 "\n",
                        (int64_t)offset);
            off_res = lseek(op->idip->fd, offset, SEEK_SET);
            if (off_res < 0) {
                pr2serr("failed moving if filepos: new_pos="
                        "%" PRId64 "\nlseek on input: %s\n", (int64_t)offset,
                        safe_strerror(errno));
                return SG_LIB_FILE_ERROR;
            }
            csp->if_filepos = offset;
        }
        memset(bp, 0, ibs);
        while (((res = read(op->idip->fd, bp, ibs)) < 0) &&
               (EINTR == errno))
            ++op->interrupted_retries;
        if (0 == res) {
            csp->leave_reason = 0;
            goto short_read;
        } else if (res < 0) {
            if ((EIO == errno) || (EREMOTEIO == errno)) {
                if ((res2 = coe_process_eio(op, my_skip)))
                    return res2;
            } else {
                pr2serr("reading 1 block, skip=%" PRId64 " : %s\n", my_skip,
                        safe_strerror(errno));
                csp->leave_reason = SG_LIB_CAT_OTHER;
                goto short_read;
            }
        } else if (res < ibs) {
            if (op->verbose)
                pr2serr("short read at skip=%" PRId64 " , wanted=%d, "
                        "got=%d bytes\n", my_skip, ibs, res);
            csp->leave_reason = 0;  /* assume EOF */
            goto short_read;
        } else { /* if (res == ibs) */
            zero_coe_limit_count(op);
            csp->if_filepos += ibs;
            if (op->verbose > 2)
                pr2serr("reading 1 block, skip=%" PRId64 " : okay\n",
                        my_skip);
        }
        ++op->in_full;
        csp->bytes_read += ibs;
    }
    return 0;

short_read:
    total_read = (ibs * k) + ((res > 0) ? res : 0);
    csp->icbpt = total_read / ibs;
    if ((total_read % ibs) > 0) {
        ++csp->icbpt;
        ++op->in_partial;
    }
    csp->ocbpt = total_read / op->obs;
    ++csp->leave_after_write;
    if (0 == csp->leave_reason) {
        csp->partial_write_bytes = total_read % op->obs;
    } else {
        /* if short read (not EOF) implies partial writes, bump obpt */
        if ((total_read % op->obs) > 0)
            ++csp->ocbpt;
    }
    return 0;
}

/* Main copy loop's read (input) for block device or regular file.
 * Returns 0 on success, else SG_LIB_FILE_ERROR, SG_LIB_CAT_MEDIUM_HARD,
 * SG_LIB_CAT_OTHER or -1 . */
static int
cp_read_block_reg(struct opts_t * op, struct cp_state_t * csp,
                  unsigned char * bp)
{
    int res, res2, in_type;
    int64_t offset = op->skip * op->ibs_pi;
    int numbytes = csp->icbpt * op->ibs_pi;
    int ibs = op->ibs_pi;

    if (op->verbose > 4)
        pr2serr("%s: offset=0x%" PRIx64 ", numbytes=%d\n", __func__, offset,
                numbytes);
    in_type = op->idip->d_type;
#ifdef SG_LIB_WIN32
    if (FT_BLOCK & in_type) {
        int ifull_extra;

        if ((res = win32_cp_read_block(op, csp, bp, &ifull_extra,
                                       op->verbose)))
            return res;
        op->in_full += ifull_extra;
        return 0;
    }
#endif
    if (offset != csp->if_filepos) {
        int64_t off_res;

        if (op->verbose > 2)
            pr2serr("moving if filepos: new_pos=%" PRId64 "\n",
                    (int64_t)offset);
        off_res = lseek(op->idip->fd, offset, SEEK_SET);
        if (off_res < 0) {
            pr2serr("failed moving if filepos: new_pos="
                    "%" PRId64 "\nlseek on input: %s\n", (int64_t)offset,
                    safe_strerror(errno));
            return SG_LIB_FILE_ERROR;
        }
        csp->if_filepos = offset;
    }
    while (((res = read(op->idip->fd, bp, numbytes)) < 0) &&
           (EINTR == errno))
        ++op->interrupted_retries;

    if (op->verbose > 2)
        pr2serr("read(unix): requested bytes=%d, res=%d\n", numbytes, res);
    if ((op->iflagp->coe) && (res < numbytes)) {
        res2 = (res >= 0) ? res : -errno;
        if ((res < 0) && op->verbose) {
            pr2serr("reading, skip=%" PRId64 " : %s, go to coe\n",
                    op->skip, safe_strerror(errno));
        } else if (op->verbose)
            pr2serr("reading, skip=%" PRId64 " : short read, go to coe\n",
                    op->skip);
        if (res2 > 0)
            csp->if_filepos += res2;
        return coe_cp_read_block_reg(op, csp, bp, res2);
    }
    if (res < 0) {
        pr2serr("reading, skip=%" PRId64 " : %s\n", op->skip,
                safe_strerror(errno));
        if ((EIO == errno) || (EREMOTEIO == errno))
            return SG_LIB_CAT_MEDIUM_HARD;
        else
            return SG_LIB_CAT_OTHER;
    } else if (res < numbytes) {
        csp->icbpt = res / ibs;
        if ((res % ibs) > 0) {
            ++csp->icbpt;
            ++op->in_partial;
            --op->in_full;
        }
        csp->ocbpt = res / op->obs;
        ++csp->leave_after_write;
        csp->leave_reason = 0;  /* fall through is assumed EOF */
        if (op->verbose > 1) {
            if (FT_BLOCK & in_type)
                pr2serr("short read at skip=%" PRId64 ", requested "
                        "%d blocks, got %d blocks\n", op->skip,
                        numbytes / ibs, csp->icbpt);
            else
                pr2serr("short read, requested %d bytes, got %d bytes\n",
                        numbytes, res);
        }
        res2 = 0;
        if ((res >= ibs) && (res <= (numbytes - ibs))) {
            /* Want to check for a EIO lurking */
            while (((res2 = read(op->idip->fd, bp + res, ibs)) < 0) &&
                   (EINTR == errno))
                ++op->interrupted_retries;
            if (res2 < 0) {
                if ((EIO == errno) || (EREMOTEIO == errno)) {
                    csp->leave_reason = SG_LIB_CAT_MEDIUM_HARD;
                    ++op->unrecovered_errs;
                } else
                    csp->leave_reason = SG_LIB_CAT_OTHER;
                if (op->verbose)
                    pr2serr("after short read, read at skip=%" PRId64
                            ": %s\n", op->skip + csp->icbpt,
                            safe_strerror(errno));
            } else {    /* actually expect 0==res2 indicating EOF */
                csp->if_filepos += res2;   /* could have moved filepos */
                if (op->verbose > 1)
                    pr2serr("extra read after short read, res=%d\n", res2);
            }
        }
        if (0 == csp->leave_reason)    /* if EOF, allow for partial write */
            csp->partial_write_bytes = (res + res2) % op->obs;
        else if ((res % op->obs) > 0) /* else if extra bytes bump obpt */
            ++csp->ocbpt;
    }
    csp->if_filepos += res;
    csp->bytes_read = res;
    op->in_full += csp->icbpt;
    return 0;
}

#ifdef SG_LIB_LINUX

/* Main copy loop's read (input) for tape device. Returns 0 on success,
 * else SG_LIB_CAT_MEDIUM_HARD, SG_LIB_CAT_OTHER or -1 . */
static int
cp_read_tape(struct opts_t * op, struct cp_state_t * csp, unsigned char * bp)
{
    int res, err, num;

    num = csp->icbpt * op->ibs;
    op->read_tape_numbytes = num;
    while (((res = read(op->idip->fd, bp, num)) < 0) && (EINTR == errno))
        ++op->interrupted_retries;

    err = errno;

    /* Summarise previous consecutive same-length reads. */
    print_tape_summary(op, res, "");

    if (op->verbose > 2)
        pr2serr("read(tape%s): requested bytes=%d, res=%d\n",
                ((res >= num) || (res < 0)) ? "" : ", short", num, res);

    if (op->verbose > 3)
        print_tape_pos("", "", op);

    if (res < 0) {
        /* If a tape block larger than the requested read length is
         * encountered, the Linux st driver returns ENOMEM. Handle that case
         * otherwise we would print a confusing/incorrect message
         * "Cannot allocate memory". */
        pr2serr("reading, skip=%" PRId64 " : %s\n", op->skip,
                (ENOMEM == err) ? "Tape block larger than requested read"
                " length" : safe_strerror(err));

        /* So print_stats() doesn't print summary. */
        op->last_tape_read_len = 0;

        if ((EIO == err) || (EREMOTEIO == err))
            return SG_LIB_CAT_MEDIUM_HARD;
        else
            return SG_LIB_CAT_OTHER;
    } else {
        if (op->verbose > 1) {
            if (res == op->last_tape_read_len)
                op->consec_same_len_reads++;
            else {
                op->last_tape_read_len = res;
                op->consec_same_len_reads = 1;
            }
        }
        if (res < num) {
            csp->icbpt = res / op->ibs;
            if ((res % op->ibs) > 0) {
                ++csp->icbpt;
                ++op->in_partial;
                --op->in_full;
            }
            csp->ocbpt = res / op->obs;
            ++csp->leave_after_write;
            csp->leave_reason = REASON_TAPE_SHORT_READ;
            csp->partial_write_bytes = res % op->obs;
            if ((op->verbose == 2) && (op->consec_same_len_reads == 1))
                pr2serr("short read: requested %d bytes, got %d\n",
                        op->read_tape_numbytes, res);
        }
    }
    csp->if_filepos += res;
    csp->bytes_read = res;
    op->in_full += csp->icbpt;
    return 0;
}

#endif /* SG_LIB_LINUX */

/* Main copy loop's read (input) for a fifo. Returns 0 on success, else
 * SG_LIB_CAT_OTHER or -1 . */
static int
cp_read_fifo(struct opts_t * op, struct cp_state_t * csp, unsigned char * bp)
{
    int res, k, err;
    int64_t offset = op->skip * op->ibs;
    int numbytes = csp->icbpt * op->ibs;

    if (offset != csp->if_filepos) {
        if (op->verbose > 2)
            pr2serr("%s: _not_ moving IFILE filepos to %" PRId64 "\n",
                    __func__, (int64_t)offset);
        csp->if_filepos = offset;
    }

    for (k = 0; k < numbytes; k += res) {
        while (((res = read(op->idip->fd, bp + k, numbytes - k)) < 0) &&
               (EINTR == errno))
            ++op->interrupted_retries;

        err = errno;
        if (op->verbose > 2)
            pr2serr("%s: requested bytes=%d, res=%d\n", __func__, numbytes,
                    res);
        if (res < 0) {
            pr2serr("%s: skip=%" PRId64 " : %s\n", __func__, op->skip,
                    safe_strerror(err));
            return SG_LIB_CAT_OTHER;
        } else if (0 == res) {
            csp->icbpt = k / op->ibs;
            if ((k % op->ibs) > 0) {
                ++csp->icbpt;
                ++op->in_partial;
                --op->in_full;
            }
            csp->ocbpt = k / op->obs;
            ++csp->leave_after_write;
            csp->leave_reason = 0;  /* EOF */
            csp->partial_write_bytes = k % op->obs;
            break;
        }
    }
    csp->if_filepos += k;
    csp->bytes_read = k;
    op->in_full += csp->icbpt;
    return 0;
}

/* Main copy loop's write (to of2) for regular file. Returns 0 if success,
 * else -1 on error. */
static int
cp_write_of2(struct opts_t * op, struct cp_state_t * csp,
             const unsigned char * bp)
{
    int res, off, part, err;
    int numbytes = (csp->ocbpt * op->obs) + csp->partial_write_bytes;

    // write to fifo (reg file ?) is non-atomic so loop if making progress
    off = 0;
    part = 0;
    do {
        while (((res = write(op->o2dip->fd, bp + off, numbytes - off)) < 0) &&
               (EINTR == errno))
            ++op->interrupted_retries;
        err = errno;
        if ((res > 0) && (res < (numbytes - off)))
            ++part;
    } while ((FT_FIFO & op->o2dip->d_type) && (res > 0) &&
             ((off += res) < numbytes));
    if (off >= numbytes) {
        res = numbytes;
        if (part && op->verbose)
            pr2serr("write to of2 splintered\n");
    } else if (off > 0)
        pr2serr("write to of2 fifo problem: count=%d, off=%d, res=%d\n",
                numbytes, off, res);
    if ((op->verbose > 2) && (0 == off))
        pr2serr("write to of2: count=%d, res=%d\n", numbytes, res);
    if (res < 0) {
        pr2serr("writing to of2, seek=%" PRId64 " : %s\n", op->seek,
                safe_strerror(err));
        return -1;
    }
    csp->bytes_of2 = res;
    return 0;
}

/* Main copy loop's read (output (of)) via pt. Returns 0 on success, else
 * see pt_read()'s return values. */
static int
cp_read_of_pt(struct opts_t * op, struct cp_state_t * csp, unsigned char * bp)
{
    int res, blks_read;

    res = pt_read(op, 1, bp, csp->ocbpt, &blks_read);
    if (res) {
        pr2serr("pt_read(sparing) failed, at or after "
                "lba=%" PRId64 " [0x%" PRIx64 "]\n", op->seek,
                op->seek);
        return res;
    } else if (blks_read != csp->ocbpt)
        return 1;
    return 0;
}

/* Main copy loop's read (output (of)) for block device or regular file.
 * Returns 0 on success, else SG_LIB_FILE_ERROR, SG_LIB_CAT_MEDIUM_HARD
 * or -1 . */
static int
cp_read_of_block_reg(struct opts_t * op, struct cp_state_t * csp,
                     unsigned char * bp)
{
    int res, err;
    int64_t offset = op->seek * op->obs;
    int numbytes = csp->ocbpt * op->obs;

#ifdef SG_LIB_WIN32
    if (FT_BLOCK & op->odip->d_type) {
        if (offset != csp->of_filepos) {
            if (op->verbose > 2)
                pr2serr("moving of filepos: new_pos=%" PRId64 "\n",
                        (int64_t)offset);
            if (win32_set_file_pos(op, DDPT_ARG_OUT, offset, op->verbose))
                return SG_LIB_FILE_ERROR;
            csp->of_filepos = offset;
        }
        res = win32_block_read_from_of(op, bp, numbytes, op->verbose);
        if (op->verbose > 2)
            pr2serr("read(sparing): requested bytes=%d, res=%d\n", numbytes,
                    res);
        if (res < 0) {
            pr2serr("read(sparing), seek=%" PRId64 "\n", op->seek);
            return (-SG_LIB_CAT_MEDIUM_HARD == res) ? -res : -1;
        } else if (res == numbytes) {
            csp->of_filepos += numbytes;
            return 0;
        } else {
            if (op->verbose > 2)
                pr2serr("short read\n");
            return -1;
        }
    } else
#endif
    {
        if (offset != csp->of_filepos) {
            int64_t off_res;

            if (op->verbose > 2)
                pr2serr("moving of filepos: new_pos=%" PRId64 "\n",
                        (int64_t)offset);
            off_res = lseek(op->odip->fd, offset, SEEK_SET);
            if (off_res < 0) {
                pr2serr("failed moving of filepos: new_pos="
                        "%" PRId64 "\nlseek on output: %s\n", (int64_t)offset,
                        safe_strerror(errno));
                return SG_LIB_FILE_ERROR;
            }
            csp->of_filepos = offset;
        }
        if (csp->partial_write_bytes > 0) {
            numbytes += csp->partial_write_bytes;
            if (op->verbose)
                pr2serr("read(sparing): %d bytes extra to fetch "
                        "due to partial read\n", csp->partial_write_bytes);
        }
        while (((res = read(op->odip->fd, bp, numbytes)) < 0) &&
               (EINTR == errno))
            ++op->interrupted_retries;

        err = errno;
        if (op->verbose > 2)
            pr2serr("read(sparing): requested bytes=%d, res=%d\n", numbytes,
                    res);
        if (res < 0) {
            pr2serr("read(sparing), seek=%" PRId64 " : %s\n", op->seek,
                    safe_strerror(err));
            return -1;
        } else if (res == numbytes) {
            csp->of_filepos += numbytes;
            return 0;
        } else {
            if (op->verbose > 2)
                pr2serr("short read\n");
            return 1;
        }
    }
}


/* Main copy loop's write (output (of)) via pt. Returns 0 on success, else
 * see pt_write()'s return values. */
static int
cp_write_pt(struct opts_t * op, struct cp_state_t * csp, int seek_delta,
            int blks, const unsigned char * bp)
{
    int res;
    int numbytes;
    int64_t aseek = op->seek + seek_delta;

    if (op->oflagp->nowrite)
        return 0;
    if (csp->partial_write_bytes > 0) {
        if (op->oflagp->pad) {
            unsigned char * ncbp = (unsigned char *)bp;

            numbytes = blks * op->obs;
            numbytes += csp->partial_write_bytes;
            ++csp->ocbpt;
            ++blks;
            res = blks * op->obs;
            if (res > numbytes)
                memset(ncbp + numbytes, 0, res - numbytes);
            if (op->verbose > 1)
                pr2serr("%s: padding probable final write at seek=%" PRId64
                        "\n", __func__, aseek);
        } else
            pr2serr(">>> ignore partial write of %d bytes to pt "
                    "(unless oflag=pad given)\n", csp->partial_write_bytes);
    }
    res = pt_write(op, bp, blks, aseek);
    if (0 != res) {
        pr2serr("%s: failed,%s seek=%" PRId64 "\n", __func__,
                ((-2 == res) ? " try reducing bpt," : ""), aseek);
        return res;
    } else
        op->out_full += blks;
    return 0;
}

#ifdef SG_LIB_LINUX

/* Main copy loop's write (output (of)) for a tape device.
 * Returns 0 on success, else SG_LIB_CAT_OTHER, SG_LIB_CAT_MEDIUM_HARD
 * or -1 . */
static int
cp_write_tape(struct opts_t * op, struct cp_state_t * csp,
              const unsigned char * bp, int could_be_last)
{
    int res, err;
    int numbytes;
    int partial = 0;
    int blks = csp->ocbpt;
    int64_t aseek = op->seek;
    int got_early_warning = 0;
/* Only print early warning message once when verbose=2 */
    static int printed_ew_message = 0;

    numbytes = blks * op->obs;
    if (op->oflagp->nowrite)
        return 0;
    if (csp->partial_write_bytes > 0) {
        ++partial;
        numbytes += csp->partial_write_bytes;
        if (op->oflagp->nopad)
            ++op->out_partial;
        else {
            unsigned char * ncbp = (unsigned char *)bp;

            ++csp->ocbpt;
            ++blks;
            res = blks * op->obs;
            if (res > numbytes)
                memset(ncbp + numbytes, 0, res - numbytes);
            numbytes = res;
        }
    }

ew_retry:
    while (((res = write(op->odip->fd, bp, numbytes)) < 0) &&
           (EINTR == errno))
        ++op->interrupted_retries;

    err = errno;
    if ((op->verbose > 2) || ((op->verbose > 0) && could_be_last)) {
        const char * cp;

        cp = ((! op->oflagp->nopad) && partial) ? ", padded" : "";
        pr2serr("write(tape%s%s): requested bytes=%d, res=%d\n",
                (partial ? ", partial" : ""), cp, numbytes, res);
    }

/* Handle EOM early warning. */
/* The Linux st driver returns -1 and ENOSPC to indicate the drive has reached
 * end of medium early warning. It is still possible to write a significant
 * amount of data before reaching end of tape (e.g. over 200MB for LTO 1). If
 * the user specified oflag=ignoreew (ignore early warning) retry the write.
 * The st driver should allow it; writes alternate until EOM, i.e. write okay,
 * ENOSPC, write okay, ENOSPC, etc. Exit if more than one ENOSPC in a row. */
    if ((op->oflagp->ignoreew) && (-1 == res) && (ENOSPC == err) &&
        (0 == got_early_warning)) {
        got_early_warning = 1;
        if (0 == printed_ew_message) {
            if (op->verbose > 1)
                pr2serr("writing, seek=%" PRId64 " : EOM early "
                        "warning, continuing...\n", aseek);
             if (2 == op->verbose) {
                pr2serr("(suppressing further early warning messages)\n");
                printed_ew_message = 1;
            }
        }
        goto ew_retry;
    }

    if (op->verbose > 3)
        print_tape_pos("", "", op);

    if (res < 0) {
        pr2serr("writing, seek=%" PRId64 " : %s\n", aseek,
                safe_strerror(err));
        if ((EIO == err) || (EREMOTEIO == err))
            return SG_LIB_CAT_MEDIUM_HARD;
        else
            return SG_LIB_CAT_OTHER;
    } else if (res < numbytes) {
        pr2serr("write(tape): wrote less than requested, exit\n");
        csp->of_filepos += res;
        csp->bytes_of = res;
        op->out_full += res / op->obs;
        /* can get a partial write due to a short write */
        if ((res % op->obs) > 0) {
            ++op->out_partial;
            ++op->out_full;
        }
        return -1;
    } else {    /* successful write */
        csp->of_filepos += numbytes;
        csp->bytes_of = numbytes;
        op->out_full += blks;
    }
    return 0;
}

#endif /* SG_LIB_LINUX */

/* Main copy loop's write (output (of)) for block device fifo or regular
 * file. Returns 0 on success, else SG_LIB_FILE_ERROR,
 * SG_LIB_CAT_MEDIUM_HARD or -1 . */
static int
cp_write_block_reg(struct opts_t * op, struct cp_state_t * csp,
                   int seek_delta, int blks, const unsigned char * bp)
{
    int64_t offset;
    int64_t aseek = op->seek + seek_delta;
    int res, off, part, out_type, err;
    int numbytes = blks * op->obs_pi;
    int obs = op->obs_pi;

    if (op->oflagp->nowrite)
        return 0;
    out_type = op->odip->d_type;
    offset = aseek * obs;
#ifdef SG_LIB_WIN32
    if (FT_BLOCK & out_type) {
        if (csp->partial_write_bytes > 0) {
            if (op->oflagp->pad) {
                numbytes += csp->partial_write_bytes;
                ++csp->ocbpt;
                ++blks;
                res = blks * obs;
                if (res > numbytes)
                    memset((unsigned char *)bp + numbytes, 0,
                           res - numbytes);
                numbytes = res;
                if (op->verbose > 1)
                    pr2serr("write(win32_block): padding probable "
                            "final write at seek=%" PRId64 "\n", aseek);
            } else
                pr2serr(">>> ignore partial write of %d bytes to "
                        "block device\n", csp->partial_write_bytes);
        }
        if (offset != csp->of_filepos) {
            if (op->verbose > 2)
                pr2serr("moving of filepos: new_pos=%" PRId64 "\n",
                        (int64_t)offset);
            if (win32_set_file_pos(op, DDPT_ARG_OUT, offset, op->verbose))
                return SG_LIB_FILE_ERROR;
            csp->of_filepos = offset;
        }
        res = win32_block_write(op, bp, numbytes, op->verbose);
        if (res < 0) {
            pr2serr("write(win32_block), seek=%" PRId64 " ", aseek);
            return (-SG_LIB_CAT_MEDIUM_HARD == res) ? -res : -1;
        } else if (res < numbytes) {
            pr2serr("output file probably full, seek=%" PRId64 " ",
                    aseek);
            csp->of_filepos += res;
            csp->bytes_of = res;
            op->out_full += res / obs;
            /* can get a partial write due to a short write */
            if ((res % obs) > 0) {
                ++op->out_partial;
                ++op->out_full;
            }
            return -1;
        } else {
            csp->of_filepos += numbytes;
            csp->bytes_of = numbytes;
            op->out_full += blks;
        }
        return 0;
    } else
#endif
    {
        if (csp->partial_write_bytes > 0) {
            if (op->oflagp->pad) {
                unsigned char * ncbp = (unsigned char *)bp;

                numbytes += csp->partial_write_bytes;
                ++csp->ocbpt;
                ++blks;
                res = blks * obs;
                if (res > numbytes)
                    memset(ncbp + numbytes, 0, res - numbytes);
                numbytes = res;
                if (op->verbose > 1)
                    pr2serr("write(unix): padding probable final "
                            "write at seek=%" PRId64 "\n", aseek);
            } else {
                if (FT_BLOCK & out_type)
                    pr2serr(">>> ignore partial write of %d bytes to block "
                            "device\n", csp->partial_write_bytes);
                else {
                    numbytes += csp->partial_write_bytes;
                    ++op->out_partial;
                }
            }
        }
        if ((offset != csp->of_filepos) &&
            (! (REASON_TAPE_SHORT_READ == csp->leave_reason))) {
            int64_t off_res;

            if (op->verbose > 2)
                pr2serr("moving of filepos: new_pos=%" PRId64 "\n",
                        (int64_t)offset);
            off_res = lseek(op->odip->fd, offset, SEEK_SET);
            if (off_res < 0) {
                pr2serr("failed moving of filepos: new_pos="
                        "%" PRId64 "\nlseek on output: %s\n", (int64_t)offset,
                        safe_strerror(errno));
                return SG_LIB_FILE_ERROR;
            }
            csp->of_filepos = offset;
        }
        // write to fifo (reg file ?) is non-atomic so loop if making progress
        off = 0;
        part = 0;
        do {
            while (((res = write(op->odip->fd, bp + off,
                                 numbytes - off)) < 0) && (EINTR == errno))
                ++op->interrupted_retries;
            err = errno;
            if ((res > 0) && (res < (numbytes - off)))
                ++part;
        } while ((FT_FIFO & out_type) && (res > 0) &&
                 ((off += res) < numbytes));
        if (off >= numbytes) {
            res = numbytes;
            if (part && op->verbose)
                pr2serr("write to output file splintered\n");
        } else if (off > 0)
            pr2serr("write to of fifo problem: count=%d, off=%d, "
                    "res=%d\n", numbytes, off, res);
        if ((op->verbose > 2) && (0 == off))
            pr2serr("write(unix): requested bytes=%d, res=%d\n", numbytes,
                    res);
        if (res < 0) {
            pr2serr("writing, seek=%" PRId64 " : %s\n", aseek,
                    safe_strerror(err));
            if ((EIO == err) || (EREMOTEIO == err))
                return SG_LIB_CAT_MEDIUM_HARD;
            else
                return SG_LIB_CAT_OTHER;
        } else if (res < numbytes) {
            pr2serr("output file probably full, seek=%" PRId64 "\n", aseek);
            csp->of_filepos += res;
            csp->bytes_of = res;
            op->out_full += res / obs;
            /* can get a partial write due to a short write */
            if ((res % obs) > 0) {
                ++op->out_partial;
                ++op->out_full;
            }
            return -1;
        } else {    /* successful write */
            csp->of_filepos += numbytes;
            csp->bytes_of = numbytes;
            op->out_full += blks;
        }
        return 0;
    }
}

/* Only for regular OFILE. Check what to do if last blocks where
 * not written, may require OFILE length adjustment */
static void
cp_sparse_cleanup(struct opts_t * op, struct cp_state_t * csp)
{
    int64_t offset = (op->seek * op->obs) + csp->partial_write_bytes;
    struct stat a_st;

    if (offset > csp->of_filepos) {
        if ((0 == op->oflagp->strunc) && (op->oflagp->sparse > 1)) {
            if (op->verbose > 1)
                pr2serr("asked to bypass writing sparse last block of "
                        "zeros\n");
            return;
        }
        if (fstat(op->odip->fd, &a_st) < 0) {
            pr2serr("%s: fstat: %s\n", __func__, safe_strerror(errno));
            return;
        }
        if (offset == a_st.st_size) {
            if (op->verbose > 1)
                pr2serr("%s: OFILE already correct length\n", __func__);
            return;
        }
        if (offset < a_st.st_size) {
            if (op->verbose > 1)
                pr2serr("%s: OFILE longer than required, do nothing\n",
                        __func__);
            return;
        }
        if (op->oflagp->strunc) {
            if (op->verbose > 1)
                pr2serr("About to truncate %s to byte offset "
                        "%" PRId64 "\n", op->odip->fn, offset);
            if (ftruncate(op->odip->fd, offset) < 0) {
                pr2serr("could not ftruncate after copy: %s\n",
                        safe_strerror(errno));
                return;
            }
            /* N.B. file offset (pointer) not changed by ftruncate */
        } else if (1 == op->oflagp->sparse) {
            if (op->verbose > 1)
                pr2serr("writing sparse last block of zeros\n");
            signals_process_delay(op, DELAY_WRITE);
            if (cp_write_block_reg(op, csp, -1, 1, op->zeros_buff) < 0)
                pr2serr("writing sparse last block of zeros "
                        "error, seek=%" PRId64 "\n", op->seek - 1);
            else
                --op->out_sparse;
        }
    } else if (op->verbose > 1)
        pr2serr("%s: bypass as output_offset <= output_filepos\n", __func__);
}

/* Main copy loop's finer grain comparison and possible write (to OFILE)
 * for all file types. Returns 0 on success. */
static int
cp_finer_comp_wr(struct opts_t * op, struct cp_state_t * csp,
                 const unsigned char * b1p, const unsigned char * b2p)
{
    int res, k, n, oblks, numbytes, chunk, need_wr, wr_len, wr_k, obs;
    int trim_check, need_tr, tr_len, tr_k, out_type;
    int done_sigs_delay = 0;

    oblks = csp->ocbpt;
    obs = op->obs;
    out_type = op->odip->d_type;
    if (op->obpch >= oblks) {
        if (FT_DEV_NULL & out_type)
            ;
        else if (FT_PT & out_type) {
            signals_process_delay(op, DELAY_WRITE);
            if ((res = cp_write_pt(op, csp, 0, oblks, b1p)))
                return res;
        } else {
            signals_process_delay(op, DELAY_WRITE);
            if ((res = cp_write_block_reg(op, csp, 0, oblks, b1p)))
                return res;
        }
        return 0;
    }
    numbytes = oblks * obs;
    if ((FT_REG & out_type) && (csp->partial_write_bytes > 0))
        numbytes += csp->partial_write_bytes;
    chunk = op->obpch * obs;
    trim_check = (op->oflagp->sparse && op->oflagp->wsame16 &&
                  (FT_PT & out_type));
    need_tr = 0;
    tr_len = 0;
    tr_k = 0;
    for (k = 0, need_wr = 0, wr_len = 0, wr_k = 0; k < numbytes; k += chunk) {
        n = ((k + chunk) < numbytes) ? chunk : (numbytes - k);
        if (0 == memcmp(b1p + k, b2p + k, n)) {
            if (need_wr) {
                if (FT_DEV_NULL & out_type)
                    ;
                else if (FT_PT & out_type) {
                    if (! done_sigs_delay) {
                        done_sigs_delay = 1;
                        signals_process_delay(op, DELAY_WRITE);
                    }
                    if ((res = cp_write_pt(op, csp, wr_k / obs,
                                           wr_len / obs, b1p + wr_k)))
                        return res;
                } else {
                    if (! done_sigs_delay) {
                        done_sigs_delay = 1;
                        signals_process_delay(op, DELAY_WRITE);
                    }
                    if ((res = cp_write_block_reg(op, csp, wr_k / obs,
                                                  wr_len / obs, b1p + wr_k)))
                        return res;
                }
                need_wr = 0;
            }
            if (need_tr)
                tr_len += n;
            else if (trim_check) {
                need_tr = 1;
                tr_len = n;
                tr_k = k;
            }
            op->out_sparse += (n / obs);
        } else {   /* look for a sequence of unequals */
            if (need_wr)
                wr_len += n;
            else {
                need_wr = 1;
                wr_len = n;
                wr_k = k;
            }
            if (need_tr) {
                    if (! done_sigs_delay) {
                        done_sigs_delay = 1;
                        signals_process_delay(op, DELAY_WRITE);
                    }
                res = pt_write_same16(op, b2p, obs, tr_len / obs,
                                      op->seek + (tr_k / obs));
                if (res)
                    ++op->trim_errs;
                /* continue past trim errors */
                need_tr = 0;
            }
        }
    }
    if (need_wr) {
        if (FT_DEV_NULL & out_type)
            ;
        else if (FT_PT & out_type) {
            if (! done_sigs_delay) {
                done_sigs_delay = 1;
                signals_process_delay(op, DELAY_WRITE);
            }
            if ((res = cp_write_pt(op, csp, wr_k / obs, wr_len / obs,
                                   b1p + wr_k)))
                return res;
        } else {
            if (! done_sigs_delay) {
                done_sigs_delay = 1;
                signals_process_delay(op, DELAY_WRITE);
            }
            if ((res = cp_write_block_reg(op, csp, wr_k / obs, wr_len / obs,
                                          b1p + wr_k)))
                return res;
        }
    }
    if (need_tr) {
        if (! done_sigs_delay)
            signals_process_delay(op, DELAY_WRITE);
        res = pt_write_same16(op, b2p, obs, tr_len / obs,
                              op->seek + (tr_k / obs));
        if (res)
            ++op->trim_errs;
        /* continue past trim errors */
    }
    return 0;
}

static int
cp_construct_pt_zero_buff(struct opts_t * op, int obpt)
{
    if ((FT_PT & op->idip->d_type) && (NULL == op->idip->ptvp)) {
        op->idip->ptvp = (struct sg_pt_base *)pt_construct_obj();
        if (NULL == op->idip->ptvp)
            return -1;
    }
    if ((FT_PT & op->odip->d_type) && (NULL == op->odip->ptvp)) {
        op->odip->ptvp = (struct sg_pt_base *)pt_construct_obj();
        if (NULL == op->odip->ptvp)
            return -1;
    }
    if ((op->oflagp->sparse) && (NULL == op->zeros_buff)) {
        op->zeros_buff = (unsigned char *)calloc(obpt * op->obs, 1);
        if (NULL == op->zeros_buff) {
            pr2serr("zeros_buff calloc failed\n");
            return -1;
        }
    }
    return 0;
}

/* Look at IFILE and OFILE lengths and blocks sizes. If dd_count
 * not given, try to deduce a value for it. If oflag=resume do skip,
 * seek, dd_count adjustments. Returns 0 to start copy, otherwise
 * bypass copy and exit */
static int
count_calculate(struct opts_t * op)
{
    int64_t in_num_blks = DDPT_COUNT_INDEFINITE;
    int64_t out_num_blks = DDPT_COUNT_INDEFINITE;
    int64_t ibytes, obytes, ibk;
    int valid_resume = 0;
    int res;

    if ((res = calc_count(op, &in_num_blks, &out_num_blks)))
        return res;
    if ((0 == op->oflagp->resume) && (op->dd_count > 0))
        return 0;
    if (op->verbose > 1)
        pr2serr("%s: in_num_blks=%" PRId64 ", out_num_blks=%" PRId64 "\n",
                __func__, in_num_blks, out_num_blks);
    if (op->skip && (FT_REG == op->idip->d_type) &&
        (op->skip > in_num_blks)) {
        pr2serr("cannot skip to specified offset on %s\n", op->idip->fn);
        op->dd_count = 0;
        return -1;
    }
    if (op->oflagp->resume) {
        if (FT_REG == op->odip->d_type) {
            if (out_num_blks < 0)
                pr2serr("resume cannot determine size of OFILE, ignore\n");
            else
                valid_resume = 1;
        } else
            pr2serr("resume expects OFILE to be regular, ignore\n");
    }
    if ((op->dd_count < 0) && (! valid_resume)) {
        /* Scale back in_num_blks by value of skip */
        if (op->skip && (in_num_blks > op->skip))
            in_num_blks -= op->skip;
        /* Scale back out_num_blks by value of seek */
        if (op->seek && (out_num_blks > op->seek))
            out_num_blks -= op->seek;

        if ((out_num_blks < 0) && (in_num_blks > 0))
            op->dd_count = in_num_blks;
        else if ((op->reading_fifo) && (FT_REG == op->odip->d_type))
            ;
        else if ((op->reading_fifo) && (out_num_blks < 0))
            ;
        else if ((out_num_blks < 0) && (in_num_blks <= 0))
            ;
        else {
            ibytes = (in_num_blks > 0) ? (op->ibs * in_num_blks) : 0;
            obytes = op->obs * out_num_blks;
            if (0 == ibytes)
                op->dd_count = obytes / op->ibs;
            else if ((ibytes > obytes) && (FT_REG != op->odip->d_type)) {
                op->dd_count = obytes / op->ibs;
            } else
                op->dd_count = in_num_blks;
        }
    }
    if (valid_resume) {
        if (op->dd_count < 0)
            op->dd_count = in_num_blks - op->skip;
        if (out_num_blks <= op->seek)
            pr2serr("resume finds no previous copy, restarting\n");
        else {
            obytes = op->obs * (out_num_blks - op->seek);
            ibk = obytes / op->ibs;
            if (ibk >= op->dd_count) {
                pr2serr("resume finds copy complete, exiting\n");
                op->dd_count = 0;
                return -1;
            }
            /* align to bpt multiple */
            ibk = (ibk / op->bpt_i) * op->bpt_i;
            op->skip += ibk;
            op->seek += (ibk * op->ibs) / op->obs;
            op->dd_count -= ibk;
            pr2serr("resume adjusting skip=%" PRId64 ", seek=%"
                    PRId64 ", and count=%" PRId64 "\n", op->skip, op->seek,
                    op->dd_count);
        }
    }
    if (op->verbose > 1)
        pr2serr("%s: dd_count=%" PRId64 "\n", __func__, op->dd_count);
    return 0;
}

/* This is the main copy loop (unless an offloaded copy is requested).
 * Attempts to copy 'dd_count' blocks (size given by bs or ibs) in chunks
 * of op->bpt_i blocks. Returns 0 if successful.  */
static int
do_rw_copy(struct opts_t * op)
{
    int ibpt, obpt, res, n, sparse_skip, sparing_skip, continual_read;
    int ret = 0;
    int first_time = 1;
    int first_time_ff = 1;
    int id_type = op->idip->d_type;
    int od_type = op->odip->d_type;
    struct cp_state_t cp_st;
    struct cp_state_t * csp;
    unsigned char * wPos = op->wrkPos;

    continual_read = op->reading_fifo && (op->dd_count < 0);
    if (op->verbose > 3) {
        if (continual_read)
            pr2serr("do_rw_copy: reading fifo continually\n");
        else
            pr2serr("do_rw_copy: dd_count=%" PRId64 "\n", op->dd_count);
    }
    if ((op->dd_count <= 0) && (! op->reading_fifo))
        return 0;
    csp = &cp_st;
    memset(csp, 0, sizeof(struct cp_state_t));
    ibpt = op->bpt_i;
    obpt = (op->ibs * op->bpt_i) / op->obs;
    if ((ret = cp_construct_pt_zero_buff(op, obpt)))
        goto copy_end;
    /* Both csp->if_filepos and csp->of_filepos are 0 */

    /* <<< main loop that does the copy >>> */
    while ((op->dd_count > 0) || continual_read) {
        if (first_time)
            first_time = 0;
        else
            signals_process_delay(op, DELAY_COPY_SEGMENT);
        csp->bytes_read = 0;
        csp->bytes_of = 0;
        csp->bytes_of2 = 0;
        sparing_skip = 0;
        sparse_skip = 0;
        if ((op->dd_count >= ibpt) || continual_read) {
            csp->icbpt = ibpt;
            csp->ocbpt = obpt;
        } else {
            csp->icbpt = op->dd_count;
            res = op->dd_count;
            n = res * op->ibs;
            csp->ocbpt = n / op->obs;
            if (n % op->obs) {
                ++csp->ocbpt;
                memset(wPos, 0, op->ibs * ibpt);
            }
        }

        /* Start of reading section */
        if (FT_PT & id_type) {
            if ((ret = cp_read_pt(op, csp, wPos)))
                break;
        } else if (FT_FIFO & id_type) {
             if ((ret = cp_read_fifo(op, csp, wPos)))
                break;
        } else if (FT_TAPE & id_type) {
#ifdef SG_LIB_LINUX
             if ((ret = cp_read_tape(op, csp, wPos)))
                break;
#else
            pr2serr("reading from tape not supported in this OS\n");
            ret = SG_LIB_CAT_OTHER;
            break;
#endif
        } else if (FT_ALL_FF & id_type) {
            if (first_time_ff) {
                first_time_ff = 0;
                memset(wPos, 0xff, op->ibs * ibpt);
            }
            op->in_full += csp->icbpt;
        } else {
             if ((ret = cp_read_block_reg(op, csp, wPos)))
                break;
        }
        if (0 == csp->icbpt)
            break;      /* nothing read so leave loop */

        if ((op->o2dip->fd >= 0) &&
            ((ret = cp_write_of2(op, csp, wPos))))
            break;

        if (op->oflagp->sparse) {
            n = (csp->ocbpt * op->obs) + csp->partial_write_bytes;
            if (0 == memcmp(wPos, op->zeros_buff, n)) {
                sparse_skip = 1;
                if (op->oflagp->wsame16 && (FT_PT & od_type)) {
                    signals_process_delay(op, DELAY_WRITE);
                    res = pt_write_same16(op, op->zeros_buff, op->obs,
                                          csp->ocbpt, op->seek);
                    if (res)
                        ++op->trim_errs;
                }
            } else if (op->obpch) {
                ret = cp_finer_comp_wr(op, csp, wPos, op->zeros_buff);
                if (ret)
                    break;
                goto bypass_write;
            }
        }
        if (op->oflagp->sparing && (! sparse_skip)) {
            /* In write sparing, we read from the output */
            if (FT_PT & od_type)
                res = cp_read_of_pt(op, csp, op->wrkPos2);
            else
                res = cp_read_of_block_reg(op, csp, op->wrkPos2);
            if (0 == res) {
                n = (csp->ocbpt * op->obs) + csp->partial_write_bytes;
                if (0 == memcmp(wPos, op->wrkPos2, n))
                    sparing_skip = 1;
                else if (op->obpch) {
                    ret = cp_finer_comp_wr(op, csp, wPos, op->wrkPos2);
                    if (ret)
                        break;
                    goto bypass_write;
                }
            } else {
                ret = res;
                break;
            }
        }

        /* Start of writing section */
        if (sparing_skip || sparse_skip) {
            op->out_sparse += csp->ocbpt;
            if (csp->partial_write_bytes > 0)
                ++op->out_sparse_partial;
        } else {
            if (FT_DEV_NULL & od_type)
                ;  /* don't bump out_full (earlier revs did) */
            else {
                signals_process_delay(op, DELAY_WRITE);
                if (FT_PT & od_type) {
                    if ((ret = cp_write_pt(op, csp, 0, csp->ocbpt, wPos)))
                        break;
                } else if (FT_TAPE & od_type) {
#ifdef SG_LIB_LINUX
                    int could_be_last;

                    could_be_last = ((! continual_read) &&
                                     (csp->icbpt >= op->dd_count));
                    if ((ret = cp_write_tape(op, csp, wPos, could_be_last)))
                        break;
#else
                    pr2serr("writing to tape not supported in this OS\n");
                    ret = SG_LIB_CAT_OTHER;
                    break;
#endif
                } else if ((ret = cp_write_block_reg(op, csp, 0, csp->ocbpt,
                                                     wPos))) /* plus fifo */
                    break;
            }
        }
bypass_write:
#ifdef HAVE_POSIX_FADVISE
        do_fadvise(op, csp->bytes_read, csp->bytes_of, csp->bytes_of2);
#endif
        if (op->dd_count > 0)
            op->dd_count -= csp->icbpt;
        op->skip += csp->icbpt;
        op->seek += csp->ocbpt;
        if (csp->leave_after_write) {
            if (REASON_TAPE_SHORT_READ == csp->leave_reason) {
                /* allow multiple partial writes for tape */
                csp->partial_write_bytes = 0;
                csp->leave_after_write = 0;
            } else {
                /* other cases: stop copy after partial write */
                ret = csp->leave_reason;
                break;
            }
        }
    } /* end of main loop that does the copy ... */

    /* sparse: clean up ofile length when last block(s) were not written */
    if ((FT_REG & od_type) && (0 == op->oflagp->nowrite) &&
        op->oflagp->sparse)
        cp_sparse_cleanup(op, csp);

#ifdef HAVE_FDATASYNC
    else if (op->oflagp->fdatasync) {
        if (fdatasync(op->odip->fd) < 0)
            perror("fdatasync() error");
        if (op->verbose)
            pr2serr("Called fdatasync() on %s successfully\n", op->odip->fn);
    }
#endif
#ifdef HAVE_FSYNC
    else if (op->oflagp->fsync) {
        if (fsync(op->odip->fd) < 0)
            perror("fsync() error");
        if (op->verbose)
            pr2serr("Called fsync() on %s successfully\n", op->odip->fn);
    }
#endif

copy_end:
    if (op->idip->ptvp) {
        pt_destruct_obj(op->idip->ptvp);
        op->idip->ptvp = NULL;
    }
    if (op->odip->ptvp) {
        pt_destruct_obj(op->odip->ptvp);
        op->odip->ptvp = NULL;
    }
    return ret;
}

static int
prepare_pi(struct opts_t * op)
{
#define PI_WORK 1       /* Protection Information */
#ifdef PI_WORK
    int res;

    op->ibs_pi = op->ibs;
    op->obs_pi = op->obs;
    if (op->rdprotect) {
        if ((0 == op->idip->prot_type) || (! (FT_PT & op->idip->d_type))) {
            pr2serr("IFILE is not a pt device or doesn't have "
                    "protection information\n");
            return SG_LIB_CAT_OTHER;
        }
        if (op->ibs != op->obs) {
            pr2serr("protect: don't support IFILE and OFILE "
                    "with different block sizes\n");
            return SG_LIB_CAT_OTHER;
        }
        if (op->wrprotect) {
            if (op->idip->p_i_exp != op->odip->p_i_exp) {
                pr2serr("Don't support IFILE and OFILE with "
                        "different P_I_EXP fields\n");
                return SG_LIB_CAT_OTHER;
            }
        }
        res = (op->idip->p_i_exp ? (1 << op->idip->p_i_exp) : 1) * 8;
        op->ibs_pi += res;
        op->obs_pi += res;
    }
    if (op->wrprotect) {
        if ((0 == op->odip->prot_type) || (! (FT_PT & op->odip->d_type))) {
            pr2serr("OFILE is not a pt device or doesn't have "
                    "protection information\n");
            return SG_LIB_CAT_OTHER;
        }
        if (op->ibs != op->obs) {
            pr2serr("protect: don't support IFILE and OFILE "
                    "with different block sizes\n");
            return SG_LIB_CAT_OTHER;
        }
        res = (op->odip->p_i_exp ? (1 << op->odip->p_i_exp) : 1) * 8;
        op->ibs_pi += res;
        op->obs_pi += res;
    }
#else
    if (op) { ; }       /* suppress warning */
#endif  /* PI_WORK */
    return 0;
}

static int
open_files_devices(struct opts_t * op)
{
    int fd, ret;
    struct dev_info_t * idip = op->idip;
    struct dev_info_t * odip = op->odip;
    struct dev_info_t * o2dip = op->o2dip;

#ifdef SG_LIB_WIN32
    win32_adjust_fns_pt(op);
#endif
    if (idip->fn[0]) {
        if (('-' == idip->fn[0]) && ('\0' == idip->fn[1])) {
            fd = STDIN_FILENO;
            idip->d_type = FT_FIFO;
            ++op->reading_fifo;
            if (op->verbose)
                pr2serr(" >> Input file type: fifo [stdin, stdout, named "
                        "pipe]\n");
        } else {
            fd = open_if(op);
            if (fd < 0)
                return -fd;
        }
        idip->fd = fd;
    } else if (op->iflagp->ff) {
        idip->d_type = FT_ALL_FF;
        idip->fd = 9999;        /* unlikely file descriptor */
    } else {
        pr2serr("'if=IFILE' option must be given. For stdin as input use "
                "'if=-'\n");
        pr2serr("For more information use '--help'\n");
        return SG_LIB_SYNTAX_ERROR;
    }

    if ('\0' == odip->fn[0])
        strcpy(odip->fn, "."); /* treat no 'of=OFILE' option as /dev/null */
    if (('-' == odip->fn[0]) && ('\0' == odip->fn[1])) {
        fd = STDOUT_FILENO;
        odip->d_type = FT_FIFO;
        odip->d_type_hold = odip->d_type;
        if (op->verbose)
            pr2serr(" >> Output file type: fifo [stdin, stdout, named "
                    "pipe]\n");
    } else {
        fd = open_of(op);
        if (fd < -1)
            return -fd;
    }
    odip->fd = fd;

    if (o2dip->fn[0]) {
        if (('-' == o2dip->fn[0]) && ('\0' == o2dip->fn[1])) {
            fd = STDOUT_FILENO;
            o2dip->d_type = FT_FIFO;
            if (op->verbose)
                pr2serr(" >> Output 2 file type: fifo  [stdin, stdout, "
                        "named pipe]\n");
        } else {
            o2dip->d_type = dd_filetype(o2dip->fn, op->verbose);
            if (FT_DEV_NULL & o2dip->d_type)
                fd = -1;
            else if (! ((FT_REG | FT_FIFO) & o2dip->d_type)) {
                pr2serr("Error: output 2 file type must be regular "
                        "file or fifo\n");
                return SG_LIB_FILE_ERROR;
            } else {
                if ((fd = open(o2dip->fn, O_WRONLY | O_CREAT, 0666)) < 0) {
                    ret = errno;
                    pr2serr("could not open %s for writing: %s\n", o2dip->fn,
                            safe_strerror(errno));
                    return ret;
                }
                if (sg_set_binary_mode(fd) < 0)
                    perror("sg_set_binary_mode");
                if (op->verbose)
                    pr2serr(" >> Output 2 file type: regular\n");
            }
        }
    } else
        fd = -1;
    o2dip->fd = fd;
    return 0;
}

static void
block_size_bpt_check(struct opts_t * op)
{
    if (0 == op->bpt_given) {
/* If reading from or writing to tape, use default bpt 1 if user did not
 * specify. Avoids inadvertent/accidental use of wrong tape block size. */
        if ((FT_TAPE & op->idip->d_type) || (FT_TAPE & op->odip->d_type)) {
            op->bpt_i = 1;
        }
#ifdef SG_LIB_FREEBSD
        else {
     /* FreeBSD (7+8 [DFLTPHYS]) doesn't like buffers larger than 64 KB being
     * sent to its pt interface (CAM), so take that into account when choosing
     * the default bpt value. There is overhead in the pt interface so reduce
     * default bpt value so bpt*ibs <= 32 KB .*/
        if (((FT_PT & op->idip->d_type) || (FT_PT & op->odip->d_type)) &&
            ((op->ibs <= 32768) && (op->bpt_i * op->ibs) > 32768))
            op->bpt_i = 32768 / op->ibs;
        }
#endif
    }
}

static void
sparse_sparing_check(struct opts_t * op)
{
    if (op->iflagp->sparse && (! op->oflagp->sparse)) {
        if (FT_DEV_NULL & op->odip->d_type) {
            pr2serr("sparse flag usually ignored on input; set it "
                    "on output in this case\n");
            ++op->oflagp->sparse;
        } else
            pr2serr("sparse flag ignored on input\n");
    }
    if (op->oflagp->sparse) {
        if ((FT_FIFO | FT_TAPE) & op->odip->d_type) {
            pr2serr("oflag=sparse needs seekable output file, ignore\n");
            op->oflagp->sparse = 0;
        } else {
            op->out_sparse_active = 1;
            if (op->oflagp->wsame16)
                op->out_trim_active = 1;
        }
    }
    if (op->oflagp->sparing) {
        if ((FT_DEV_NULL | FT_FIFO | FT_TAPE) & op->odip->d_type) {
            pr2serr("oflag=sparing needs a readable and seekable "
                    "output file, ignore\n");
            op->oflagp->sparing = 0;
        } else
            op->out_sparing_active = 1;
    }
}

static void
cdb_size_prealloc(struct opts_t * op)
{
    if (op->oflagp->prealloc) {
        if ((FT_DEV_NULL | FT_FIFO | FT_TAPE | FT_PT) & op->odip->d_type) {
            pr2serr("oflag=pre-alloc needs a normal output file, ignore\n");
            op->oflagp->prealloc = 0;
        }
    }
    if (! op->cdbsz_given) {
        if ((FT_PT & op->idip->d_type) && (op->iflagp->cdbsz < 16) &&
            (((op->dd_count + op->skip) > UINT_MAX) ||
             (op->bpt_i > USHRT_MAX))) {
            if (op->verbose > 0)
                pr2serr("SCSI command size increased from 10 to 16 "
                        "bytes on %s\n", op->idip->fn);
            op->iflagp->cdbsz = 16;
        }
        if ((FT_PT & op->odip->d_type) && (op->oflagp->cdbsz < 16) &&
            (((op->dd_count + op->seek) > UINT_MAX) ||
             (((op->ibs * op->bpt_i) / op->obs) > USHRT_MAX))) {
            if (op->verbose)
                pr2serr("SCSI command size increased from 10 to 16 "
                        "bytes on %s\n", op->odip->fn);
            op->oflagp->cdbsz = 16;
        }
    }
}

#ifdef SG_LIB_LINUX

static void
tape_cleanup_of(struct opts_t * op)
{
    /* Before closing OFILE, if writing to tape handle suppressing the
     * writing of a filemark and/or flushing the drive buffer which the
     * Linux st driver normally does when tape file is closed after writing.
     * Possibilities depend on oflag arguments:
     * nofm:         MTWEOFI 0 if possible (kernel 2.6.37+), else MTBSR 0
     * nofm & fsync: MTWEOF 0
     * fsync:        Do nothing; st writes filemark & flushes buffer on close.
     * neither:      MTWEOFI 1 if possible (2.6.37+), else nothing (drive
     *               buffer will be flushed if MTWEOFI not possible). */
    struct mtop mt_cmd;
    int res;

    if (op->oflagp->nofm || !op->oflagp->fsync) {
        mt_cmd.mt_op = (op->oflagp->fsync) ? MTWEOF : MTWEOFI;
        mt_cmd.mt_count = (op->oflagp->nofm) ? 0 : 1;
        res = ioctl(op->odip->fd, MTIOCTOP, &mt_cmd);
        if (res != 0) {
            if (op->verbose > 0)
                pr2serr("MTWEOF%s %d failed: %s\n",
                        (op->oflagp->fsync) ? "" : "I", mt_cmd.mt_count,
                        safe_strerror(errno));
            if (op->oflagp->nofm && !op->oflagp->fsync) {
                if (op->verbose > 0)
                    pr2serr("Trying MTBSR 0 instead\n");
                mt_cmd.mt_op = MTBSR; /* mt_cmd.mt_count = 0 from above */
                res = ioctl(op->odip->fd, MTIOCTOP, &mt_cmd);
                if (res != 0)
                    pr2serr("MTBSR 0 failed: %s\n(Filemark will be written "
                            "when tape file is closed)\n",
                            safe_strerror(errno));
            }
        }
    }
}

#endif  /* SG_LIB_LINUX */

static int
do_falloc(struct opts_t * op)
{
#ifdef SG_LIB_LINUX
#ifdef HAVE_FALLOCATE
    /* Try to pre-allocate space in the output file.
     *
     * If fallocate() does not succeed, exit with an error message. The user
     * can then either free up some disk space or invoke ddpt without
     * oflag=pre-alloc (at the risk of running out of disk space).
     *
     * TODO/DISCUSSION: Some filesystems (e.g. FAT32) don't support
     * fallocate(). In that case we should probably have a way to continue if
     * fallocate() fails, rather than exiting; useful for use in scripts
     * where the user would like to pre-allocate space when possible.
     *
     * On Linux, try fallocate() with the FALLOC_FL_KEEP_SIZE flag, which
     * allocates space but doesn't change the apparent file size (useful
     * since oflag=resume can be used).
     *
     * If fallocate() with FALLOC_FL_KEEP_SIZE returns ENOTTY, EINVAL or
     * EOPNOTSUPP, retry without that flag (since the flag is only supported
     * in recent Linux kernels). */
    int res;

#ifdef PREALLOC_DEBUG
    pr2serr("About to call fallocate() with FALLOC_FL_KEEP_SIZE\n");
#endif
    res = fallocate(op->odip->fd, FALLOC_FL_KEEP_SIZE, op->obs*op->seek,
                    op->obs*op->dd_count);
#ifdef PREALLOC_DEBUG
    pr2serr("fallocate() returned %d\n", res);
#endif
    /* fallocate() fails if the kernel does not support
     * FALLOC_FL_KEEP_SIZE, so retry without that flag. */
    if (-1 == res) {
        if ((ENOTTY == errno) || (EINVAL == errno)
             || (EOPNOTSUPP == errno)) {
            if (op->verbose)
                pr2serr("Could not pre-allocate with "
                        "FALLOC_FL_KEEP_SIZE (%s), retrying without "
                        "...\n", safe_strerror(errno));
            res = fallocate(op->odip->fd, 0, op->obs*op->seek,
                            op->obs*op->dd_count);
#ifdef PREALLOC_DEBUG
            pr2serr("fallocate() without FALLOC_FL_KEEP_SIZE "
                    " returned %d\n", res);
#endif
        }
    } else {
        /* fallocate() with FALLOC_FL_KEEP_SIZE succeeded. Set
         * op->oflagp->prealloc to 0 so the possible message about using
         * oflag=resume is not suppressed later. */
        op->oflagp->prealloc = 0;
    }
    if (-1 == res) {
            pr2serr("Unable to pre-allocate space: %s\n",
                    safe_strerror(errno));
            return SG_LIB_CAT_OTHER;
    }
    if (op->verbose > 1)
        pr2serr("Pre-allocated %" PRId64 " bytes at offset %"
                PRId64 "\n", op->obs*op->dd_count, op->obs*op->seek);

#endif  /* HAVE_FALLOCATE */
#else   /* other than SG_LIB_LINUX */
#ifdef HAVE_POSIX_FALLOCATE
    int res;

    /* If not on Linux, use posix_fallocate(). (That sets the file size to its
     * full length, so re-invoking ddpt with oflag=resume will do nothing.) */
    res = posix_fallocate(op->odip->fd, op->obs*op->seek,
                          op->obs*op->dd_count);
    if (-1 == res) {
            pr2serr("Unable to pre-allocate space: %s\n",
                    safe_strerror(errno));
            return SG_LIB_CAT_OTHER;
    }
    if (op->verbose > 1)
        pr2serr("Pre-allocated %" PRId64 " bytes at offset %" PRId64 "\n",
                op->obs*op->dd_count, op->obs*op->seek);
#else   /* do not HAVE_POSIX_FALLOCATE */
    if (op) { ; }
#endif  /* HAVE_POSIX_FALLOCATE else */
#endif  /* SG_LIB_LINUX else */
    return 0;
}

static void
details_pre_copy_print(struct opts_t * op)
{
    pr2serr("skip=%" PRId64 " (blocks on input), seek=%" PRId64
            " (blocks on output)\n", op->skip, op->seek);
    if (op->verbose > 1) {
        pr2serr("  ibs=%d bytes, obs=%d bytes, OBPC=%d\n",
                op->ibs, op->obs, op->obpch);
        if (op->ibs != op->ibs_pi)
            pr2serr("  due to protect ibs_pi=%d bytes, "
                    "obs_pi=%d bytes\n", op->ibs_pi, op->obs_pi);
    }
    if (op->reading_fifo && (op->dd_count < 0))
        pr2serr("  reading fifo, blocks_per_transfer=%d\n", op->bpt_i);
    else
        pr2serr("  initial count=%" PRId64 " (blocks of input), "
                "blocks_per_transfer=%d\n", op->dd_count, op->bpt_i);
    if ((op->delay > 0) || (op->wdelay > 0))
        pr2serr("  delay=%d ms, wdelay=%d ms\n", op->delay, op->wdelay);
}

static int
wrk_buffers_init(struct opts_t * op)
{
    int len = op->ibs_pi * op->bpt_i;

    if (op->has_xcopy)
        return 0;
    if (op->iflagp->direct || op->oflagp->direct) {
        size_t psz;

#if defined(HAVE_SYSCONF) && defined(_SC_PAGESIZE)
        psz = sysconf(_SC_PAGESIZE); /* POSIX.1 (was getpagesize()) */
#elif defined(SG_LIB_WIN32)
        psz = win32_pagesize();
#else
        psz = 4096;     /* give up, pick likely figure */
#endif

#ifdef HAVE_POSIX_MEMALIGN
        {
            int err;
            void * wp;

            wp = op->wrkBuff;
            err = posix_memalign(&wp, psz, len);
            if (err) {
                pr2serr("posix_memalign: error [%d] out of memory?\n", err);
                return SG_LIB_CAT_OTHER;
            }
            op->wrkBuff = (unsigned char *)wp;
            memset(op->wrkBuff, 0, len);
            op->wrkPos = op->wrkBuff;
            if (op->oflagp->sparing) {
                wp = op->wrkBuff2;
                err = posix_memalign(&wp, psz, len);
                if (err) {
                    pr2serr("posix_memalign(2): error [%d] out of memory?\n",
                             err);
                    return SG_LIB_CAT_OTHER;
                }
                op->wrkBuff2 = (unsigned char *)wp;
                memset(op->wrkBuff2, 0, len);
                op->wrkPos2 = op->wrkBuff2;
            }
        }
#else   /* do not HAVE_POSIX_MEMALIGN */
        op->wrkBuff = (unsigned char*)calloc(len + psz, 1);
        if (0 == op->wrkBuff) {
            pr2serr("Not enough user memory for aligned usage\n");
            return SG_LIB_CAT_OTHER;
        }
        op->wrkPos = (unsigned char *)(((uintptr_t)op->wrkBuff + psz - 1) &
                                       (~((uintptr_t)psz - 1)));
        if (op->oflagp->sparing) {
            op->wrkBuff2 = (unsigned char*)calloc(len + psz, 1);
            if (0 == op->wrkBuff2) {
                pr2serr("Not enough user memory for aligned usage(2)\n");
                return SG_LIB_CAT_OTHER;
            }
            op->wrkPos2 = (unsigned char *)
                           (((uintptr_t)op->wrkBuff2 + psz - 1) &
                            (~((uintptr_t)psz - 1)));
        }
#endif  /* HAVE_POSIX_MEMALIGN */
    } else {
        op->wrkBuff = (unsigned char*)calloc(op->ibs_pi * op->bpt_i, 1);
        if (0 == op->wrkBuff) {
            pr2serr("Not enough user memory\n");
            return SG_LIB_CAT_OTHER;
        }
        op->wrkPos = op->wrkBuff;
        if (op->oflagp->sparing) {
            op->wrkBuff2 = (unsigned char*)calloc(op->ibs_pi * op->bpt_i, 1);
            if (0 == op->wrkBuff2) {
                pr2serr("Not enough user memory(2)\n");
                return SG_LIB_CAT_OTHER;
            }
            op->wrkPos2 = op->wrkBuff2;
        }
    }
    return 0;
}

static void
cleanup_resources(struct opts_t * op)
{
#ifdef SG_LIB_LINUX
    if ((FT_TAPE & op->idip->d_type) || (FT_TAPE & op->odip->d_type)) {
        /* For writing, the st driver writes a filemark on closing the file
         * (unless user specified oflag=nofm), so make clear that the
         * position shown is prior to closing. */
        print_tape_pos("Final ", " (before closing file)", op);
        if ((FT_TAPE & op->odip->d_type) && (op->verbose > 1) &&
            op->oflagp->nofm)
            pr2serr("(suppressing writing of filemark on close)\n");
    }
#endif

    if (op->iflagp->errblk)
        errblk_close(op);

    if (op->wrkBuff)
        free(op->wrkBuff);
    if (op->wrkBuff2)
        free(op->wrkBuff2);
    if (op->zeros_buff)
        free(op->zeros_buff);
    if (FT_PT & op->idip->d_type)
        pt_close(op->idip->fd);
    else if ((op->idip->fd >= 0) && (STDIN_FILENO != op->idip->fd))
        close(op->idip->fd);
    if (FT_PT & op->odip->d_type)
        pt_close(op->odip->fd);
    if ((op->odip->fd >= 0) && (STDOUT_FILENO != op->odip->fd) &&
        !(FT_DEV_NULL & op->odip->d_type)) {
#ifdef SG_LIB_LINUX
        if (FT_TAPE & op->odip->d_type)
            tape_cleanup_of(op);
#endif
        close(op->odip->fd);
    }
    if ((op->o2dip->fd >= 0) && (STDOUT_FILENO != op->o2dip->fd))
        close(op->o2dip->fd);
}

static int
chk_sgl_for_non_offload(struct opts_t * op)
{
    if (op->in_sgl) {
        if (op->in_sgl_elems > 1) {
            pr2serr("Only accept a multiple element skip= (gather) list for "
                    "%s with odx\n", op->idip->fn[0] ? op->idip->fn : "?");
            return SG_LIB_SYNTAX_ERROR;
        }
        if ((op->dd_count >= 0) && (op->dd_count != op->in_sgl[0].num)) {
            pr2serr("dd_count [%" PRIu64 "] and skip (sgl num) [%" PRIu32 "] "
                    "contradict\n", op->dd_count, op->in_sgl[0].num);
            return SG_LIB_SYNTAX_ERROR;
        }
        op->skip = op->in_sgl[0].lba;
        op->dd_count = op->in_sgl[0].num;
    }
    if (op->out_sgl) {
        if (op->out_sgl_elems > 1) {
            pr2serr("Only accept a multiple element seek= (scatter) list for "
                    "%s with odx\n", op->odip->fn[0] ? op->odip->fn : "?");
            return SG_LIB_SYNTAX_ERROR;
        }
        /* assuming ibs==obs, revisit xxxxxxx */
        if ((op->dd_count >= 0) && (op->dd_count != op->out_sgl[0].num)) {
            pr2serr("dd_count [%" PRIu64 "] and seek (sgl num) [%" PRIu32 "] "
                    "too confusing\n", op->dd_count, op->out_sgl[0].num);
            return SG_LIB_SYNTAX_ERROR;
        }
        op->seek = op->out_sgl[0].lba;
        op->dd_count = op->out_sgl[0].num;
    }
    return 0;
}


/* The main() function: much of the its complex logic is spawned off to
 * helper functions shown directly above. */
int
main(int argc, char * argv[])
{
    int ret = 0;
    int started_copy = 0;
    int jf_depth = 0;
    struct opts_t ops;
    struct flags_t iflag, oflag;
    struct dev_info_t ids, ods, o2ds;
    struct opts_t * op;

    op = &ops;
    state_init(op, &iflag, &oflag, &ids, &ods, &o2ds);
    ret = cl_process(op, argc, argv, ddpt_version_str, jf_depth);
    if (op->do_help > 0) {
        ddpt_usage(op->do_help);
        return 0;
    } else if (ret)
        return (ret < 0) ? 0 : ret;

    if (op->quiet) {
        if (NULL == freopen("/dev/null", "w", stderr))
            pr2serr("freopen: failed to redirect stderr to /dev/null : %s\n",
                    safe_strerror(errno));
    }

#ifdef SG_LIB_WIN32
    if (op->wscan)
        return sg_do_wscan('\0', op->wscan, op->verbose);
#endif

    install_signal_handlers(op);

    if (op->has_odx) {
        started_copy = 1;
        ret = do_odx(op);
        goto cleanup;
    }

    /* may allow scatter gather lists for non-odx copies in future */
    ret = chk_sgl_for_non_offload(op);
    if (ret)
        return ret;

    if ((ret = open_files_devices(op)))
        return ret;

    block_size_bpt_check(op);
    sparse_sparing_check(op);

    if ((ret = count_calculate(op))) {
        if (op->verbose)
            pr2serr("count_calculate() returned %d, exit\n", ret);
        goto cleanup;
    }

    if ((ret = prepare_pi(op)))
        goto cleanup;

    if ((op->dd_count < 0) && (! op->reading_fifo)) {
        pr2serr("Couldn't calculate count, please give one\n");
        ret = SG_LIB_CAT_OTHER;
        goto cleanup;
    }

    cdb_size_prealloc(op);

    if ((ret = wrk_buffers_init(op)))
        goto cleanup;

    if (op->verbose)
        details_pre_copy_print(op);

    op->read1_or_transfer = !! (FT_DEV_NULL & op->odip->d_type);
    op->dd_count_start = op->dd_count;
    if (op->read1_or_transfer && (! op->outf_given) &&
        ((op->dd_count > 0) || op->reading_fifo))
        pr2serr("Output file not specified so no copy, just reading input\n");

    if (op->do_time)
        calc_duration_init(op);

    if (op->iflagp->errblk)
        errblk_open(op);

#ifdef SG_LIB_LINUX
    if ((FT_TAPE & op->idip->d_type) || (FT_TAPE & op->odip->d_type))
        print_tape_pos("Initial ", "", op);
#endif

    if (op->oflagp->prealloc) {
        if ((ret = do_falloc(op)))
            goto cleanup;
    }

    ++started_copy;
    if (op->has_xcopy)
        ret = do_xcopy_lid1(op);
    else
        ret = do_rw_copy(op);

    if (0 == op->status_none)
        print_stats("", op, 0);

    if ((op->oflagp->ssync) && (FT_PT & op->odip->d_type)) {
        if (0 == op->status_none)
            pr2serr(">> SCSI synchronizing cache on %s\n", op->odip->fn);
        pt_sync_cache(op->odip->fd);
    }
    if (op->do_time)
        calc_duration_throughput("", 0, op);

    if (op->sum_of_resids)
        pr2serr(">> Non-zero sum of residual counts=%d\n", op->sum_of_resids);

cleanup:
    cleanup_resources(op);
    if ((0 == ret) && op->err_to_report)
        ret = op->err_to_report;
    if (started_copy && (0 != op->dd_count) && (! op->reading_fifo)) {
        if (0 == ret)
            pr2serr("Early termination, EOF on input?\n");
        else if (ret > 0)
            print_exit_status_msg("Early termination", ret, 1);
        else {
            if (op->verbose < 2)
                pr2serr("Early termination: some error occurred; try again "
                        "with '-vv'\n");
            else
                pr2serr("Early termination: some error occurred\n");
        }
    }
    return (ret >= 0) ? ret : SG_LIB_CAT_OTHER;
}
