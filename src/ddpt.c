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
#include <sys/stat.h>

/* N.B. config.h must precede anything that depends on HAVE_*  */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


static const char * ddpt_version_str = "0.96 20180813 [svn: r364]";

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

#include "ddpt.h"
#include "sg_lib.h"
#include "sg_pr2serr.h"

#ifndef EREMOTEIO
#define EREMOTEIO EIO
#endif

/* Used for outputting diagnostic messages for oflag=prealloc */
#define PREALLOC_DEBUG 1


static int cp_read_of_pt(struct opts_t * op, struct cp_state_t * csp,
                         uint8_t * bp);
static int cp_read_of_block_reg(struct opts_t * op, struct cp_state_t * csp,
                                uint8_t * bp);



/* Returns open input file descriptor (>= 0) or a negative value
 * (sg3_utils style error code) if error.
 */
static int
open_if(struct opts_t * op)
{
    int fd = -SG_LIB_FILE_ERROR;
    int flags, err;
    int vb = op->verbose;
    char ebuff[EBUFF_SZ];
    struct flags_t * ifp = op->iflagp;
    struct dev_info_t * idip = op->idip;
    const char * ifn = idip->fn;

    idip->d_type = dd_filetype(ifn, vb);
    if (FT_ERROR & idip->d_type) {
        pr2serr("unable to access %s\n", ifn);
        return -SG_LIB_FILE_ERROR;
    } else if (((FT_BLOCK | FT_TAPE | FT_OTHER) & idip->d_type) && ifp->pt)
        idip->d_type |= FT_PT;
    if (vb)
        pr2serr(" >> Input file type: %s\n",
                dd_filetype_str(idip->d_type, ebuff, EBUFF_SZ, ifn));
    if (!(FT_PT & idip->d_type) && op->rdprotect)
        pr2serr("rdprotect ignored on non-pt device\n");
    if ((FT_FIFO | FT_CHAR | FT_TAPE) & idip->d_type)
        op->reading_fifo = true;

    if ((FT_TAPE & idip->d_type) && (FT_PT & idip->d_type)) {
        pr2serr("SCSI tape device %s not supported via pt\n", ifn);
        return -SG_LIB_FILE_ERROR;
    }
    if (FT_PT & idip->d_type) {
        fd = pt_open_if(op, NULL);
        if (fd < 0)
            return fd;
    }
#ifdef SG_LIB_WIN32
    else if (FT_BLOCK & idip->d_type) {
        if (win32_open_if(op, (ifp->excl ? O_EXCL : 0), vb))
            return -SG_LIB_FILE_ERROR;
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
            err = errno;
            pr2serr("could not open %s for reading: %s\n", ifn,
                    safe_strerror(err));
            return -sg_convert_errno(err);
        } else {
            if (sg_set_binary_mode(fd) < 0)
                perror("sg_set_binary_mode");
            if (vb)
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

        /* implicit unlock operation when fd closed when utility exits */
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
}

/* Returns open output file descriptor (>= 0), -9999 for don't
 * bother opening (e.g. /dev/null), or a more negative value
 * (sg3_utils style error code) if error.
 */
static int
open_of(struct opts_t * op)
{
    bool outf_exists = false;
    int fd = -SG_LIB_FILE_ERROR;
    int flags, err;
    int vb = op->verbose;
    struct flags_t * ofp = op->oflagp;
    struct dev_info_t * odip = op->odip;
    const char * ofn = odip->fn;
    char ebuff[EBUFF_SZ];
    struct stat st;

    odip->d_type = dd_filetype(ofn, vb);
    if (((FT_BLOCK | FT_TAPE | FT_OTHER) & odip->d_type) && ofp->pt)
        odip->d_type |= FT_PT;
    odip->d_type_hold = odip->d_type;
    if (vb)
        pr2serr(" >> Output file type: %s\n",
                dd_filetype_str(odip->d_type, ebuff, EBUFF_SZ, ofn));
    if (!(FT_PT & odip->d_type) && op->wrprotect)
        pr2serr("wrprotect ignored on non-pt device\n");

    if ((FT_TAPE & odip->d_type) && (FT_PT & odip->d_type)) {
        pr2serr("SCSI tape device %s not supported via pt\n", ofn);
        return -SG_LIB_FILE_ERROR;
    }
    if (FT_PT & odip->d_type) {
        fd = pt_open_of(op, NULL);
        if (fd < 0)
            return fd;
    } else if (FT_DEV_NULL & odip->d_type)
        fd = -9999; /* don't bother opening */
#ifdef SG_LIB_WIN32
    else if (FT_BLOCK & odip->d_type) {
        if (win32_open_of(op, (ofp->excl ? O_EXCL : 0), vb))
            return -SG_LIB_FILE_ERROR;
        fd = 0;
    }
#endif
    else {      /* typically regular file or block device node */
        bool needs_ftruncate = false;
        int64_t offset = 0;

        memset(&st, 0, sizeof(st));
        if (0 == stat(ofn, &st))
            outf_exists = true;
        else if (ofp->pt) {
            /* if oflag=pt, then creating a regular file is unhelpful */
            pr2serr("Cannot create a regular file called %s as a pt\n", ofn);
            return -SG_LIB_FILE_ERROR;
        }
        flags = ofp->sparing ? O_RDWR : O_WRONLY;
        if (! outf_exists)
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
            if (op->o_sgli.lowest_lba > 0) {
                offset = op->o_sgli.lowest_lba * op->obs_pi;
                if (st.st_size > offset)
                    needs_ftruncate = true;  // only truncate to shorten
            } else
                flags |= O_TRUNC;
        }
        if ((fd = open(ofn, flags, 0666)) < 0) {
            err = errno;
            pr2serr("could not open %s for writing: %s\n", ofn,
                    safe_strerror(err));
            return -sg_convert_errno(err);
        }
        if (needs_ftruncate && (offset > 0)) {
            if (ftruncate(fd, offset) < 0) {
                err = errno;
                pr2serr("could not ftruncate %s after open (seek): %s\n",
                        ofn, safe_strerror(err));
                return -sg_convert_errno(err);
            }
            /* N.B. file offset (pointer) not changed by ftruncate */
        }
        if ((! outf_exists) && (FT_ERROR & odip->d_type)) {
            odip->d_type = FT_REG;   /* exists now */
            odip->d_type_hold = odip->d_type;
        }
        if (sg_set_binary_mode(fd) < 0)
            perror("sg_set_binary_mode");
        if (vb) {
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
}

/* Set file pointer (byte position) of file associated with dip (assumed to
 * be open) to wanted unless it already equals cur_pos. Returns 0 on success.
 * cur_pos is ignored if negative and the filepos is set to wanted. Returns
 * -1 if wanted is negative (or dip is NULL). */
static int
set_filepos_if_needed(struct dev_info_t * dip, int64_t wanted,
                      int64_t cur_pos, const char * caller, bool noisy,
                      int vb)
{
    const char * cp = caller ? caller : "unknown";
    const char * dp;

    if ((NULL == dip) || (wanted < 0)) {
        pr2serr("%s: %s(%s): invalid arguments, wanted=%" PRId64 "\n",
                __func__, cp, (dip ? dip->dir_n : "??"), wanted);
        return -1;
    }
    dp = dip->dir_n;
    if (wanted != cur_pos) {
        if (noisy)
            pr2serr("%s(%s): moving filepos: new_pos=%" PRId64 " [0x%" PRIx64
                    ", prev=0x%" PRIx64 "]\n", cp, dp, wanted,
                    (uint64_t)wanted, ((cur_pos < 0) ? 0 : cur_pos));
#ifdef SG_LIB_WIN32
        if (win32_set_file_pos(dip, wanted, vb))
            return SG_LIB_FILE_ERROR;
#else
        {
            int err;
            int64_t off_res;

            off_res = lseek(dip->fd, wanted, SEEK_SET);
            if (off_res < 0) {
                err = errno;
                if (vb)
                    pr2serr("%s(%s): failed moving filepos: wanted=%" PRId64
                            " [0x%" PRIx64 "]\nlseek error: %s\n", cp, dp,
                            wanted, wanted, safe_strerror(errno));
                return sg_convert_errno(err);
            }
        }
#endif
    }
    return 0;
}

/* Helper for calc_count_both(). Attempts to size IFILE. Returns 0 if no
 * error detected. */
static int
calc_count_in(struct opts_t * op, int64_t * in_num_blksp)
{
    int res;
    int vb = op->verbose;
    int ibs_lb = op->ibs_lb;
    struct stat st;
    int in_lb_sz, id_type;
    struct dev_info_t * idip = op->idip;
#ifndef SG_LIB_WIN32
    int64_t num_blks, t;
    int blk_sz;
#endif
    const char * ifn = idip->fn;

    *in_num_blksp = DDPT_COUNT_INDEFINITE;
    id_type = idip->d_type;
    if (FT_PT & id_type) {
        if (op->iflagp->norcap) {
            if ((FT_BLOCK & id_type) && (0 == op->iflagp->force)) {
                pr2serr(">> warning: norcap (no Read Capacity) on input "
                        "block device accessed via pt is risky.\n");
                pr2serr(">> Abort copy, use iflag=force to override.\n");
                return -1;
            }
            return 0;
        }
        res = pt_read_capacity(op, DDPT_ARG_IN, in_num_blksp, &in_lb_sz);
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            pr2serr("Unit attention (readcap in), continuing\n");
            res = pt_read_capacity(op, DDPT_ARG_IN, in_num_blksp,
                                   &in_lb_sz);
        } else if (SG_LIB_CAT_ABORTED_COMMAND == res) {
            pr2serr("Aborted command (readcap in), continuing\n");
            res = pt_read_capacity(op, DDPT_ARG_IN, in_num_blksp,
                                   &in_lb_sz);
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
            if (vb) {
                print_blk_sizes(ifn, "readcap", *in_num_blksp, in_lb_sz,
                                true /* to_stderr */);
                if (idip->prot_type > 0)
                    pr2serr("    reports Protection_type=%d, p_i_exp=%d\n",
                            idip->prot_type, idip->p_i_exp);
            }
            if ((*in_num_blksp > 0) && (in_lb_sz != ibs_lb)) {
                pr2serr(">> warning: %s block size confusion: ibs=%d, "
                        "device claims=%d\n", ifn, ibs_lb, in_lb_sz);
                if (0 == op->iflagp->force) {
                    pr2serr(">> abort copy, use iflag=force to override\n");
                    return -1;
                }
            }
        }
#ifndef SG_LIB_WIN32
        /* aleady have (FT_sg & in_type), check overall size for both */
        if ((FT_BLOCK & id_type) && (0 == op->iflagp->force) &&
            (0 == get_blkdev_capacity(op, DDPT_ARG_IN, &num_blks,
                                      &blk_sz))) {
            t = (*in_num_blksp) * in_lb_sz;
            if (t != (num_blks * blk_sz)) {
                pr2serr(">> warning: Size of input block device is "
                        "different from pt size.\n>> Pass-through on block "
                        "partition can give unexpected offsets.\n");
                pr2serr(">> Abort copy, use iflag=force to override.\n");
                return -1;
            }
        }
#endif
    } else if (FT_BLOCK & id_type) {
        if (0 != get_blkdev_capacity(op, DDPT_ARG_IN, in_num_blksp,
                                     &in_lb_sz)) {
            pr2serr("Unable to read block capacity on %s\n", ifn);
            *in_num_blksp = DDPT_COUNT_INDEFINITE;
        } else {
            if (vb)
                print_blk_sizes(ifn, "blk", *in_num_blksp, in_lb_sz, true);
            if ((*in_num_blksp > 0) && (ibs_lb != in_lb_sz)) {
                pr2serr(">> warning: %s block size confusion: bs=%d, "
                        "device claims=%d\n", ifn, ibs_lb, in_lb_sz);
                *in_num_blksp = DDPT_COUNT_INDEFINITE;
                if (0 == op->iflagp->force) {
                    pr2serr(">> abort copy, use iflag=force to override\n");
                    return -1;
                }
            }
        }
    }
#if 0
// don't understand why this is here
    else if ((op->dd_count > 0) && (! op->oflagp->resume))
        return 0;
#endif
    else if (FT_REG & id_type) {
        if (fstat(idip->fd, &st) < 0) {
            perror("fstat(idip->fd) error");
            *in_num_blksp = DDPT_COUNT_INDEFINITE;
        } else {
            idip->reg_sz = st.st_size;
            *in_num_blksp = st.st_size / ibs_lb;
            res = st.st_size % ibs_lb;
            if (res) /* round up, for dd_count */
                ++*in_num_blksp;
            if (vb) {
                print_blk_sizes(ifn, "reg", *in_num_blksp, ibs_lb, true);
                pr2serr("    residual_bytes=%d\n", res);
            }
        }
    }
    return 0;
}

/* Helper for calc_count_both(). Attempts to size OFILE. Returns 0 if no
 * error detected. */
static int
calc_count_out(struct opts_t * op, int64_t * out_num_blksp)
{
    int res;
    int vb = op->verbose;
    int obs_lb = op->obs_lb;
    struct stat st;
    struct dev_info_t * odip = op->odip;
    int out_lb_sz;
    int od_type = odip->d_type;
#ifndef SG_LIB_WIN32
    int64_t num_blks, t;
    int blk_sz;
#endif
    const char * ofn = odip->fn;

    *out_num_blksp = DDPT_COUNT_INDEFINITE;
    if (FT_PT & od_type) {
        if (op->oflagp->norcap) {
            if ((FT_BLOCK & od_type) && (0 == op->oflagp->force)) {
                pr2serr(">> warning: norcap (no Read Capacity) on output "
                        "block device accessed via pt is risky.\n");
                pr2serr(">> Abort copy, use oflag=force to override.\n");
                return -1;
            }
            return 0;
        }
        res = pt_read_capacity(op, DDPT_ARG_OUT, out_num_blksp, &out_lb_sz);
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            pr2serr("Unit attention (readcap out), continuing\n");
            res = pt_read_capacity(op, DDPT_ARG_OUT, out_num_blksp,
                                   &out_lb_sz);
        } else if (SG_LIB_CAT_ABORTED_COMMAND == res) {
            pr2serr("Aborted command (readcap out), continuing\n");
            res = pt_read_capacity(op, DDPT_ARG_OUT, out_num_blksp,
                                   &out_lb_sz);
        }
        if (0 != res) {
            if (res == SG_LIB_CAT_INVALID_OP)
                pr2serr("read capacity not supported on %s\n", ofn);
            else
                pr2serr("Unable to read capacity on %s\n", ofn);
            *out_num_blksp = DDPT_COUNT_INDEFINITE;
            return res;
        } else {
            if (vb) {
                print_blk_sizes(ofn, "readcap", *out_num_blksp, out_lb_sz,
                                true /* to_stderr */);
                if (odip->prot_type > 0)
                    pr2serr("    reports Protection_type=%d, p_i_exp=%d\n",
                            odip->prot_type, odip->p_i_exp);
            }
            if ((*out_num_blksp > 0) && (obs_lb != out_lb_sz)) {
                pr2serr(">> warning: %s block size confusion: "
                        "obs=%d, device claims=%d\n", ofn, obs_lb, out_lb_sz);
                if (0 == op->oflagp->force) {
                    pr2serr(">> abort copy, use oflag=force to override\n");
                    return -1;
                }
            }
        }
#ifndef SG_LIB_WIN32
        if ((FT_BLOCK & od_type) && (0 == op->oflagp->force) &&
             (0 == get_blkdev_capacity(op, DDPT_ARG_OUT, &num_blks,
                                       &blk_sz))) {
            t = (*out_num_blksp) * out_lb_sz;
            if (t != (num_blks * blk_sz)) {
                pr2serr(">> warning: size of output block device is "
                        "different from pt size.\n>> Pass-through on block "
                        "partition can give unexpected results.\n");
                pr2serr(">> abort copy, use oflag=force to override\n");
                return -1;
            }
        }
#endif
    } else if (FT_BLOCK & od_type) {
        if (0 != get_blkdev_capacity(op, DDPT_ARG_OUT, out_num_blksp,
                                     &out_lb_sz)) {
            pr2serr("Unable to read block capacity on %s\n", ofn);
            *out_num_blksp = DDPT_COUNT_INDEFINITE;
        } else {
            if (vb)
                print_blk_sizes(ofn, "blk", *out_num_blksp, out_lb_sz, true);
            if ((*out_num_blksp > 0) && (obs_lb != out_lb_sz)) {
                pr2serr(">> warning: %s block size confusion: obs=%d, "
                        "device claims=%d\n", ofn, obs_lb, out_lb_sz);
                *out_num_blksp = DDPT_COUNT_INDEFINITE;
                if (0 == op->oflagp->force) {
                    pr2serr(">> abort copy, use oflag=force to override\n");
                    return -1;
                }
            }
        }
    }
#if 0
// don't understand why this is here
    else if ((op->dd_count > 0) && (! op->oflagp->resume))
        return 0;
#endif
    else if (FT_REG & od_type) {
        if (fstat(odip->fd, &st) < 0) {
            perror("fstat(odip->fd) error");
            *out_num_blksp = DDPT_COUNT_INDEFINITE;
        } else {
            odip->reg_sz = st.st_size;
            *out_num_blksp = st.st_size / obs_lb;
            res = st.st_size % obs_lb;
            if (res)
                ++*out_num_blksp;
            if (vb) {
                print_blk_sizes(ofn, "reg", *out_num_blksp, obs_lb, true);
                pr2serr("    residual_bytes=%d\n", res);
            }
        }
    }
    return 0;
}


/* Calculates the number of blocks associated with the in and out files.
 * May also yield the block size in bytes of devices. For regular files
 * uses ibs or obs as the logical block size. Returns 0 for continue,
 * otherwise bypass copy and exit. */
static int
calc_count_both(struct opts_t * op, int64_t * in_num_blksp,
                int64_t * out_num_blksp)
{
    int res;

    if ((op->dd_count > 0) && op->i_sgli.sum_hard &&
        (op->i_sgli.sum != op->dd_count)) {
        pr2serr("count=%" PRId64 " differs from sum of skip <num>s (%"
                PRId64 ")\n", op->dd_count, op->i_sgli.sum);
        if (op->iflagp->force)
            pr2serr("Continue due to force flag\n");
        else
            return SG_LIB_SYNTAX_ERROR;
    }
    res = calc_count_in(op, in_num_blksp);
    if (res)
        return res;

    if ((op->dd_count > 0) && op->o_sgli.sum_hard) {
        int64_t o_dd_count = (op->dd_count * op->ibs_pi) / op->obs_pi;

        if (op->o_sgli.sum != o_dd_count) {
            pr2serr("(out) count=%" PRId64 " differs from sum of seek "
                    "<num>s (%" PRId64 ")\n", o_dd_count, op->o_sgli.sum);
            if (op->oflagp->force)
                pr2serr("Continue due to force flag\n");
            else
                return SG_LIB_SYNTAX_ERROR;
        }
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
    bool in_valid, out2_valid, out_valid;
    int rt, id_type, od_type, o2d_type;
    int ibs = op->ibs_pi;
    int obs = op->obs_pi;
    int64_t lskip = op->lowest_skip;    /* initialized to -1 */
    int64_t l_seek = op->lowest_seek;

    /* dip->fd < 0 for the next 3 (if=, of= and of2=) if not given */
    id_type = op->idip->d_type;
    od_type = op->odip->d_type;
    o2d_type = op->o2dip->d_type;  /* of2=OFILE2 */
    in_valid = ((FT_REG & id_type) || (FT_BLOCK & id_type));
    out2_valid = ((FT_REG & o2d_type) || (FT_BLOCK & o2d_type));
    out_valid = ((FT_REG & od_type) || (FT_BLOCK & od_type));
    if (op->iflagp->nocache && (bytes_if > 0) && in_valid) {
        if ((lskip < 0) || (op->i_sgli.lowest_lba > lskip)) {
            lskip = op->i_sgli.lowest_lba;
            op->lowest_skip = lskip;
        }
        rt = posix_fadvise(op->idip->fd, (lskip * ibs),
                           ((op->i_sgli.lowest_lba - lskip) * ibs) + bytes_if,
                           POSIX_FADV_DONTNEED);
        if (rt)         /* returns error as result */
            pr2serr("posix_fadvise on read, skip=%" PRId64 " ,err=%d\n",
                    lskip, rt);
    }
    if ((op->oflagp->nocache & 2) && (bytes_of2 > 0) && out2_valid) {
        rt = posix_fadvise(op->o2dip->fd, 0, 0, POSIX_FADV_DONTNEED);
        if (rt)
            pr2serr("posix_fadvise on of2, err=%d\n", rt);
    }
    if ((op->oflagp->nocache & 1) && (bytes_of > 0) && out_valid) {
        if ((l_seek < 0) || (op->o_sgli.lowest_lba > l_seek)) {
            l_seek = op->o_sgli.lowest_lba;
            op->lowest_seek = l_seek;
        }
        rt = posix_fadvise(op->odip->fd, (l_seek * obs),
                   ((l_seek - op->lowest_seek) * obs) + bytes_of,
                           POSIX_FADV_DONTNEED);
        if (rt)
            pr2serr("posix_fadvise on output, seek=%" PRId64 " , err=%d\n",
                    l_seek, rt);
    }
}
#endif

static void
deep_dry_run_lba(const char * caller, bool dry_run, bool dir_in, int64_t lba,
                 int blks, int bp_off, const char * bname)
{
    const char * cp = caller ? caller : "unknown";

    pr2serr("%s%s offset=%d %s lba=0x%" PRIx64 ", num_blks=0x%x,%d  "
            "[%s]\n", (dry_run ? "ddr: " : ""), bname, bp_off,
            (dir_in ? "<<<  " : "  >>>"), (uint64_t)lba, (uint32_t)blks,
            blks, cp);
}

static void
deep_dry_run_fpos(const char * caller, bool dry_run, bool dir_in,
                  int64_t fpos, int nbytes, int bp_off, const char * bname)
{
    const char * cp = caller ? caller : "unknown";

    if (fpos >= 0)
        pr2serr("%s%s offset=%d  %s  fpos=0x%" PRIx64 ", bytes=0x%x,%d  "
                "[%s]\n", (dry_run ? "ddr: " : ""), bname, bp_off,
                (dir_in ? "<<<  " : "  >>>"), (uint64_t)fpos,
                (uint32_t)nbytes, nbytes, cp);
    else
        pr2serr("%s%s offset=%d  %s  bytes=0x%x,%d  [%s]\n",
                (dry_run ? "ddr: " : ""), bname, bp_off,
                (dir_in ? "<<<  " : "  >>>"), (uint32_t)nbytes, nbytes, cp);
}

/* Main copy loop's read (input) via pt. Returns 0 on success, else see
 * pt_read()'s return values. */
static int
cp_read_pt(struct opts_t * op, struct cp_state_t * csp, uint8_t * bp)
{
    bool dr = (bool)op->dry_run;
    int res;
    int blks = csp->cur_in_num;
    int blks_read = 0;
    int vb = op->verbose;

    if (dr || (vb > 3)) {
        if ((vb > 3) || (dr && (vb > 1)))
            deep_dry_run_lba(__func__, dr, true, csp->cur_in_lba, blks,
                             bp - csp->low_bp, csp->buf_name);
        if (dr)
            goto fini;
    }

    res = pt_read(op, 0, bp, blks, csp->cur_in_lba, &blks_read);
    if (res) {
        if (0 == blks_read) {
            pr2serr("pt_read failed,%s at or after lba=%" PRId64 " "
                    "[0x%" PRIx64 "]\n",
                    ((-2 == res) ?  " try reducing bpt," : ""),
                    csp->cur_in_lba, csp->cur_in_lba);
            return res;
        }
        /* limp on if data, should stop after write; hold err number */
        op->err_to_report = res;
    }
    if (blks_read < blks) {
        /* assume close to end, or some data prior to read error */
        if (op->verbose > 1)
            pr2serr("short read(pt), requested %d blocks, got %d blocks\n",
                    blks, blks_read);
        csp->leave_after_write = true;
        // csp->icbpt = blks_read;
        /* round down since don't do partial writes from pt reads */
        // yyyy csp->ocbpt = x_mult_div(blks_read, op->ibs_pi, op->obs_pi);
    }
fini:
    csp->blks_xfer = blks_read;
    csp->bytes_xfer = -1;  /* block device as far as reads are concerned */
    csp->bytes_read += (blks_read * op->ibs_pi);
    return 0;
}

static int
cp_read_pt_wrap(struct dev_info_t * dip, struct cp_state_t * csp,
                struct opts_t * op)
{
    uint8_t * bp = csp->cur_bp;

    switch (dip->ddpt_arg) {
    case DDPT_ARG_IN:
        return cp_read_pt(op, csp, bp);
    case DDPT_ARG_OUT:
        return cp_read_of_pt(op, csp, bp);
    case DDPT_ARG_OUT2:
    default:
        pr2serr("%s: logic error, unexpected ddpt_arg=%d\n", __func__,
                dip->ddpt_arg);
        return SG_LIB_CAT_OTHER;
    }
}

static int
cp_write_same_wrap(struct dev_info_t * dip, struct cp_state_t * csp,
                   struct opts_t * op)
{
    bool dr = (bool)op->dry_run;
    int vb = op->verbose;
    uint8_t * bp = csp->cur_bp;

    if (dr || (vb > 3)) {
        if ((vb > 3) || (dr && (vb > 1)))
            deep_dry_run_lba("pt_write_same16", dr, false, csp->cur_out_lba,
                             csp->cur_out_num, bp - csp->low_bp,
                             csp->buf_name);
        if (dr)
            return 0;
    }
    if (op->oflagp->nowrite)
        return 0;
    switch (dip->ddpt_arg) {
    case DDPT_ARG_OUT:
        return pt_write_same16(op, bp, op->obs_pi, csp->cur_out_num,
                               csp->cur_out_lba);
    case DDPT_ARG_IN:
    case DDPT_ARG_OUT2:
    default:
        pr2serr("%s: logic error, unexpected ddpt_arg=%d\n", __func__,
                dip->ddpt_arg);
        return SG_LIB_CAT_OTHER;
    }
}

/* Error occurred on block/regular read. coe>0 so assume all full
 * blocks prior to error are good (if any) and start to read from the
 * block containing the error, one block at a time, until ibpt. Supply
 * zeros for unreadable blocks. Return 0 if successful, SG_LIB_CAT_OTHER
 * if error other than EIO or EREMOTEIO, SG_LIB_FILE_ERROR if lseek fails,
 * and SG_LIB_CAT_MEDIUM_HARD if the coe_limit is exceeded. */
static int
coe_cp_read_block_reg(struct opts_t * op, struct cp_state_t * csp,
                      uint8_t * bp, int numread_errno)
{
    int res, res2, k, total_read, num_read, err;
    int ibs = op->ibs_pi;
    int obs = op->obs_pi;
    int blks = csp->cur_in_num;
    int vb = op->verbose;
    int64_t offset, my_skip;

    err = (numread_errno < 0) ? -numread_errno : 0;
    if (0 == numread_errno) {
        csp->blks_xfer = 0;
        csp->bytes_read = 0;
        csp->bytes_of = 0;
        /* yyyy csp->ocbpt = 0; */
        csp->leave_after_write = true;
        csp->leave_reason = DDPT_REASON_EOF_ON_READ;
        return 0;       /* EOF */
    } else if (err) {
        if ((EIO == err) || (EREMOTEIO == err)) {
            num_read = 0;
            if (1 == blks) {
                // Don't read again, this must be bad block
                memset(bp, 0, ibs);
                if ((res2 = coe_process_eio(op, csp->cur_in_lba)))
                    return res2;
                ++csp->stats.in_full;
                csp->blks_xfer += 1;
                csp->bytes_xfer += ibs;
                csp->bytes_read += ibs;
                return 0;
            }
        } else
            return sg_convert_errno(err);
    } else
        num_read = (numread_errno / ibs) * ibs; /* round down to n*ibs */

    k = num_read / ibs;
    if (k > 0) {
        csp->stats.in_full += k;
        zero_coe_limit_count(op);
    }
    csp->bytes_read = num_read;
    my_skip = csp->cur_in_lba + k;
    offset = my_skip * ibs;
    bp += num_read;
    for ( ; k < blks; ++k, ++my_skip, bp += ibs, offset += ibs) {
        res = set_filepos_if_needed(op->idip, offset, csp->in_iter.filepos,
                                    __func__, vb > 1, vb);
        if (res)
            return res;
        csp->in_iter.filepos = offset;
        memset(bp, 0, ibs);
        while (((res = read(op->idip->fd, bp, ibs)) < 0) &&
               (EINTR == errno))
            ++csp->stats.interrupted_retries;
        if (0 == res) {
            csp->leave_reason = DDPT_REASON_EOF_ON_READ;
            goto short_read;
        } else if (res < 0) {
            err = errno;
            if ((EIO == err) || (EREMOTEIO == err)) {
                if ((res2 = coe_process_eio(op, my_skip)))
                    return res2;
            } else {
                pr2serr("reading 1 block, skip=%" PRId64 " : %s\n", my_skip,
                        safe_strerror(err));
                csp->leave_reason = sg_convert_errno(err);
                goto short_read;
            }
        } else if (res < ibs) {
            if (vb)
                pr2serr("short read at skip=%" PRId64 " , wanted=%d, "
                        "got=%d bytes\n", my_skip, ibs, res);
            csp->leave_reason = DDPT_REASON_EOF_ON_READ;  /* assume EOF */
            goto short_read;
        } else { /* if (res == ibs) */
            zero_coe_limit_count(op);
            csp->in_iter.filepos += ibs;
            if (vb > 2)
                pr2serr("reading 1 block, skip=%" PRId64 " : okay\n",
                        my_skip);
        }
        ++csp->stats.in_full;
        csp->bytes_read += ibs;
    }
    csp->bytes_xfer = csp->bytes_read;
    csp->blks_xfer = csp->bytes_read / ibs;
    return 0;

short_read:
    total_read = (ibs * k) + ((res > 0) ? res : 0);
    csp->blks_xfer = total_read / ibs;
    csp->bytes_read = total_read;
    csp->bytes_xfer = total_read;
    if ((total_read % ibs) > 0) {
        // yyyy ++csp->icbpt;
        ++csp->stats.in_partial;
    }
    // yyyy csp->ocbpt = total_read / obs;
    csp->leave_after_write = true;
    if (DDPT_REASON_EOF_ON_READ == csp->leave_reason) {
        csp->partial_write_bytes = total_read % obs;
    } else {
        /* if short read (not EOF) implies partial writes, bump obpt */
        if ((total_read % obs) > 0) {
            ; // yyyy ++csp->ocbpt;
        }
    }
    return 0;
}

/* Main copy loop's read (input) for block device or regular file.
 * Returns 0 on success, else SG_LIB_FILE_ERROR, SG_LIB_CAT_MEDIUM_HARD,
 * SG_LIB_CAT_OTHER or -1 . */
static int
cp_read_block_reg(struct opts_t * op, struct cp_state_t * csp, uint8_t * bp)
{
    bool dr = (bool)op->dry_run;
    int res, res2, id_type, err;
    int ibs = op->ibs_pi;
    int obs = op->obs_pi;
    int req_blks = csp->cur_in_num;
    int numbytes = req_blks * ibs;
    int vb = op->verbose;
    int64_t offset = csp->cur_in_lba * ibs;

    if (dr || (vb > 3)) {
        if ((vb > 3) || (dr && (vb > 1)))
            deep_dry_run_fpos(__func__, dr, true, offset, numbytes,
                              bp - csp->low_bp, csp->buf_name);
        if (dr) {
            res = numbytes;
            goto fini;
        }
    }

    id_type = op->idip->d_type;
#ifdef SG_LIB_WIN32
    if (FT_BLOCK & id_type) {
        int ifull_extra;

        if ((res = win32_cp_read_block(op, csp, bp, &ifull_extra, vb)))
            return res;
        csp->blks_xfer = ifull_extra;
        csp->bytes_xfer = ifull_extra * ibs;
        return 0;
    }
#endif
    res = set_filepos_if_needed(op->idip, offset, csp->in_iter.filepos,
                                __func__, vb > 1, vb);
    if (res)
        return res;
    csp->in_iter.filepos = offset;
    /* read */
    while (((res = read(op->idip->fd, bp, numbytes)) < 0) &&
           (EINTR == errno))
        ++csp->stats.interrupted_retries;

    err = errno;
    if (vb > 2)
        pr2serr("read(unix): requested bytes=%d, res=%d\n", numbytes, res);
    if ((op->iflagp->coe) && (res < numbytes)) {
        res2 = (res >= 0) ? res : -err;
        if ((res < 0) && vb) {
            pr2serr("reading, skip=%" PRId64 " : %s, go to coe\n",
                    csp->cur_in_lba, safe_strerror(err));
        } else if (vb)
            pr2serr("reading, skip=%" PRId64 " : short read, go to coe\n",
                    csp->cur_in_lba);
        if (res2 > 0)
            csp->in_iter.filepos += res2;
        return coe_cp_read_block_reg(op, csp, bp, res2);
    }
    if (res < 0) {
        pr2serr("reading, skip=%" PRId64 " : %s\n", csp->cur_in_lba,
                safe_strerror(err));
        if ((EIO == err) || (EREMOTEIO == err))
            return SG_LIB_CAT_MEDIUM_HARD;
        else
            return sg_convert_errno(err);
    } else if (res < numbytes) {        /* short read */
        csp->leave_after_write = true;
        csp->leave_reason = DDPT_REASON_EOF_ON_READ;  /* assume EOF */
        if ((res % ibs) > 0)
            ++csp->stats.in_partial;
        if (vb > 1) {
            if (FT_BLOCK & id_type)
                pr2serr("short read at lba=%" PRId64 ", requested "
                        "%d blocks, got %d blocks\n", csp->cur_in_lba,
                        numbytes / ibs, csp->blks_xfer);
            else
                pr2serr("short read, requested %d bytes, got %d bytes, "
                        "from filepos=0x%" PRIx64 "\n", numbytes, res,
                        offset);
        }
        res2 = 0;
        if ((res >= ibs) && (res <= (numbytes - ibs))) {
            /* Want to check for a EIO lurking */
            while (((res2 = read(op->idip->fd, bp + res, ibs)) < 0) &&
                   (EINTR == errno))
                ++csp->stats.interrupted_retries;
            err = errno;
            if (res2 < 0) {
                if ((EIO == err) || (EREMOTEIO == err)) {
                    csp->leave_reason = SG_LIB_CAT_MEDIUM_HARD;
                    ++csp->stats.unrecovered_errs;
                } else
                    csp->leave_reason = sg_convert_errno(err);
                if (vb)
                    pr2serr("after short read, read at lba=%" PRId64
                            ": %s\n", csp->cur_in_lba + csp->blks_xfer,
                            safe_strerror(err));
            } else {    /* actually expect 0==res2 indicating EOF */
                csp->in_iter.filepos += res2;   /* could have moved filepos */
                if (vb > 1)
                    pr2serr("extra read after short read, res=%d\n", res2);
            }
        }
        /* allow for partial write */
        if (DDPT_REASON_EOF_ON_READ == csp->leave_reason)
            csp->partial_write_bytes = (res + res2) % obs;
        else if ((res % obs) > 0) { /* else if extra bytes bump obpt */
            ;  /* yyyy ++oocbpt;     // do this at higher level */
        }
    }
fini:
    csp->in_iter.filepos += res;
    csp->bytes_read = res;
    csp->bytes_xfer = res;
    csp->blks_xfer = res / ibs;
    return 0;
}

static int
cp_read_block_reg_wrap(struct dev_info_t * dip, struct cp_state_t * csp,
                       struct opts_t * op)

{
    uint8_t * bp = csp->cur_bp;

    switch (dip->ddpt_arg) {
    case DDPT_ARG_IN:
        return cp_read_block_reg(op, csp, bp);
    case DDPT_ARG_OUT:
        return cp_read_of_block_reg(op, csp, bp);
    case DDPT_ARG_OUT2:
    default:
        pr2serr("%s: logic error, unexpected ddpt_arg=%d\n", __func__,
                dip->ddpt_arg);
        return SG_LIB_CAT_OTHER;
    }
}

#ifdef SG_LIB_LINUX

/* Main copy loop's read (input) for tape device. Returns 0 on success,
 * else SG_LIB_CAT_MEDIUM_HARD, SG_LIB_CAT_OTHER or -1 . */
static int
cp_read_tape(struct opts_t * op, struct cp_state_t * csp, uint8_t * bp)
{
    bool dr = (bool)op->dry_run;
    int res, err;
    int num = csp->cur_in_num;
    int ibs = op->ibs_pi;
    int obs = op->obs_pi;
    int numbytes = num * ibs;
    int vb = op->verbose;

    op->read_tape_numbytes = numbytes;
    if (dr || (vb > 3)) {
        if ((vb > 3) || (dr && (vb > 1)))
            deep_dry_run_fpos(__func__, dr, true, -1, numbytes,
                              bp - csp->low_bp, csp->buf_name);
        if (dr) {
            res = num * ibs;
            csp->blks_xfer = num;
            csp->bytes_xfer = numbytes;
            goto fini;
        }
    }

    while (((res = read(op->idip->fd, bp, num)) < 0) && (EINTR == errno))
        ++csp->stats.interrupted_retries;

    err = errno;

    /* Summarise previous consecutive same-length reads. */
    print_tape_summary(op, res, "");

    if (vb > 2)
        pr2serr("read(tape%s): requested bytes=%d, res=%d\n",
                ((res >= num) || (res < 0)) ? "" : ", short", num, res);

    if (vb > 3)
        print_tape_pos("", "", op);

    if (res < 0) {
        /* If a tape block larger than the requested read length is
         * encountered, the Linux st driver returns ENOMEM. Handle that case
         * otherwise we would print a confusing/incorrect message
         * "Cannot allocate memory". */
        pr2serr("reading, skip=%" PRId64 " : %s\n", csp->cur_in_lba,
                (ENOMEM == err) ? "Tape block larger than requested read"
                " length" : safe_strerror(err));

        /* So print_stats() doesn't print summary. */
        op->last_tape_read_len = 0;

        csp->blks_xfer = 0;
        csp->bytes_xfer = 0;
        if ((EIO == err) || (EREMOTEIO == err))
            return SG_LIB_CAT_MEDIUM_HARD;
        else
            return sg_convert_errno(err);
    } else {
        if (vb > 1) {
            if (res == op->last_tape_read_len)
                op->consec_same_len_reads++;
            else {
                op->last_tape_read_len = res;
                op->consec_same_len_reads = 1;
            }
        }
        if (res < num) {
            csp->bytes_xfer = res;
            csp->blks_xfer = res / ibs;
            if ((res % ibs) > 0) {
                // yyyy ++csp->icbpt;
                ++csp->stats.in_partial;
                --csp->stats.in_full;
            }
            // yyyy csp->ocbpt = res / obs;
            csp->leave_after_write = true;
            csp->leave_reason = DDPT_REASON_TAPE_SHORT_READ;
            csp->partial_write_bytes = res % obs;
            if ((vb == 2) && (op->consec_same_len_reads == 1))
                pr2serr("short read: requested %d bytes, got %d\n",
                        op->read_tape_numbytes, res);
        } else {
            csp->blks_xfer = num / ibs;
            csp->bytes_xfer = num;
        }
    }
fini:
    csp->in_iter.filepos += res;
    csp->bytes_read = res;
    return 0;
}

#endif /* SG_LIB_LINUX */

/* Main copy loop's read (input) for a fifo. Returns 0 on success, else
 * SG_LIB_CAT_OTHER or -1 . */
static int
cp_read_fifo(struct opts_t * op, struct cp_state_t * csp, uint8_t * bp)
{
    bool dr = (bool)op->dry_run;
    int res, k, err;
    int ibs = op->ibs_pi;
    int obs = op->obs_pi;
    int num = csp->cur_in_num;
    int numbytes = num * ibs;
    int vb = op->verbose;
    int64_t offset = csp->cur_in_lba * ibs;

    if (dr || (vb > 3)) {
        if ((vb > 3) || (dr && (vb > 1)))
            deep_dry_run_fpos(__func__, dr, true, offset, numbytes,
                              bp - csp->low_bp, csp->buf_name);
        if (dr) {
            k = numbytes;
            goto fini;
        }
    }
    if (offset != csp->in_iter.filepos) {
        if (vb > 2)
            pr2serr("%s: _not_ moving IFILE filepos to %" PRId64 "\n",
                    __func__, (int64_t)offset);
        csp->in_iter.filepos = offset;
    }

    for (k = 0; k < numbytes; k += res) {
        while (((res = read(op->idip->fd, bp + k, numbytes - k)) < 0) &&
               (EINTR == errno))
            ++csp->stats.interrupted_retries;

        err = errno;
        if (vb > 2)
            pr2serr("%s: requested bytes=%d, res=%d\n", __func__, numbytes,
                    res);
        if (res < 0) {
            pr2serr("%s: skip=%" PRId64 " : %s\n", __func__, csp->cur_in_lba,
                    safe_strerror(err));
            return sg_convert_errno(err);
        } else if (0 == res) {
            if ((k % ibs) > 0)
                ++csp->stats.in_partial;
            csp->leave_after_write = true;
            csp->leave_reason = DDPT_REASON_EOF_ON_READ;
            csp->partial_write_bytes = k % obs;
            break;
        }
    }
fini:
    csp->in_iter.filepos += k;
    csp->bytes_read = k;
    csp->bytes_xfer = k;
    csp->blks_xfer = k / ibs;
    csp->stats.in_full += csp->blks_xfer;
    return 0;
}

/* Main copy loop's write (to of2) for regular file. Returns 0 if success,
 * else -1 on error. */
static int
cp_write_of2(struct opts_t * op, struct cp_state_t * csp, const uint8_t * bp)
{
    bool dr = (bool)op->dry_run;
    bool got_part;
    int res, off, err;
    int num = csp->cur_out_num;
    int obs = op->obs_pi;
    int numbytes = (num * obs) + csp->partial_write_bytes;
    int vb = op->verbose;

    if (dr || (vb > 3)) {
        if ((vb > 3) || (dr && (vb > 1)))
            deep_dry_run_fpos(__func__, dr, false, -1, numbytes,
                              bp - csp->low_bp, csp->buf_name);
        if (dr)
            return 0;
    }

    // write to fifo (reg file ?) is non-atomic so loop if making progress
    off = 0;
    got_part = false;
    do {
        while (((res = write(op->o2dip->fd, bp + off, numbytes - off)) < 0) &&
               (EINTR == errno))
            ++csp->stats.interrupted_retries;
        err = errno;
        if ((res > 0) && (res < (numbytes - off)))
            got_part = true;
    } while ((FT_FIFO & op->o2dip->d_type) && (res > 0) &&
             ((off += res) < numbytes));
    if (off >= numbytes) {
        res = numbytes;
        if (got_part && vb)
            pr2serr("write to of2 splintered\n");
    } else if (off > 0)
        pr2serr("write to of2 fifo problem: count=%d, off=%d, res=%d\n",
                numbytes, off, res);
    if ((vb > 2) && (0 == off))
        pr2serr("write to of2: count=%d, res=%d\n", numbytes, res);
    if (res < 0) {
        pr2serr("writing to of2: %s\n", safe_strerror(err));
        return -1;
    }
    csp->bytes_of2 = res;
    return 0;
}

/* Main copy loop's read (output (of)) via pt. Returns 0 on success, else
 * see pt_read()'s return values. */
static int
cp_read_of_pt(struct opts_t * op, struct cp_state_t * csp, uint8_t * bp)
{
    bool dr = (bool)op->dry_run;
    int res, blks_read;
    int num = csp->cur_out_num;
    int vb = op->verbose;

    if (dr || (vb > 3)) {
        if ((vb > 3) || (dr && (vb > 1)))
            deep_dry_run_lba(__func__, dr, true, csp->cur_out_lba, num,
                             bp - csp->low_bp, csp->buf_name);
        if (dr) {
            csp->blks_xfer = num;
            csp->bytes_xfer = -1;
            return 0;
        }
    }

    res = pt_read(op, DDPT_ARG_OUT, bp, num, csp->cur_out_lba, &blks_read);
    if (res) {
        pr2serr("pt_read(sparing) failed, at or after "
                "lba=%" PRId64 " [0x%" PRIx64 "]\n", csp->cur_out_lba,
                csp->cur_out_lba);
        return res;
    } else if (blks_read != num)
        return SG_LIB_CAT_OTHER;
    csp->blks_xfer = blks_read;
    csp->bytes_xfer = -1;
    /* don't updates statistics */
    return 0;
}

/* Main copy loop's read (output (of)) for block device or regular file.
 * Returns 0 on success, else SG_LIB_FILE_ERROR, SG_LIB_CAT_MEDIUM_HARD
 * or -1 . */
static int
cp_read_of_block_reg(struct opts_t * op, struct cp_state_t * csp,
                     uint8_t * bp)
{
    bool dr = (bool)op->dry_run;
    int res, err;
    int obs = op->obs_pi;
    int64_t offset = csp->cur_out_lba * obs;
    int numbytes = csp->cur_out_num * obs;
    int vb = op->verbose;

    if (dr || (vb > 3)) {
        if ((vb > 3) || (dr && (vb > 1)))
            deep_dry_run_fpos(__func__, dr, true, offset, numbytes,
                              bp - csp->low_bp, csp->buf_name);
        if (dr) {
            csp->blks_xfer = numbytes / obs;
            csp->bytes_xfer = numbytes;
            csp->out_iter.filepos = offset;
            csp->out_iter.filepos += numbytes;
            return 0;
        }
    }

#ifdef SG_LIB_WIN32
    if (FT_BLOCK & op->odip->d_type) {
        res = set_filepos_if_needed(op->odip, offset, csp->out_iter.filepos,
                                    __func__, vb > 1, vb);
        if (res)
            return res;
        csp->out_iter.filepos = offset;
        res = win32_block_read_from_of(op, bp, numbytes, vb);
        if (vb > 2)
            pr2serr("read(sparing): requested bytes=%d, res=%d\n", numbytes,
                    res);
        if (res < 0) {
            pr2serr("read(sparing), seek=%" PRId64 "\n", csp->cur_out_lba);
            return (-SG_LIB_CAT_MEDIUM_HARD == res) ? -res : -1;
        } else {
            csp->blks_xfer = res / obs;
            csp->bytes_xfer = res;
            csp->out_iter.filepos += res;
            if (res == numbytes)
                return 0;
            else {
                if (vb > 2)
                    pr2serr("%s: short read\n", __func__);
                return -1;
            }
        }
    } else
#endif
    {
        res = set_filepos_if_needed(op->odip, offset, csp->out_iter.filepos,
                                    __func__, vb > 1, vb);
        if (res)
            return res;
        csp->out_iter.filepos = offset;
        if (csp->partial_write_bytes > 0) {
            numbytes += csp->partial_write_bytes;
            if (vb)
                pr2serr("read(sparing): %d bytes extra to fetch "
                        "due to partial read\n", csp->partial_write_bytes);
        }
        while (((res = read(op->odip->fd, bp, numbytes)) < 0) &&
               (EINTR == errno))
            ++csp->stats.interrupted_retries;

        err = errno;
        if (vb > 2)
            pr2serr("read(sparing): requested bytes=%d, res=%d\n", numbytes,
                    res);
        if (res < 0) {
            pr2serr("read(sparing), seek=%" PRId64 " : %s\n", offset,
                    safe_strerror(err));
            return sg_convert_errno(err);
        }
        csp->blks_xfer = res / obs;
        csp->bytes_xfer = res;
        csp->out_iter.filepos += res;
        if (res == numbytes)
            return 0;
        else {
            if (vb > 2)
                pr2serr("%s: short read\n", __func__);
            return -1;
        }
    }
}


/* Main copy loop's write (output (of)) via pt. Returns 0 on success, else
 * see pt_write()'s return values. */
static int
cp_write_pt(struct opts_t * op, struct cp_state_t * csp, const uint8_t * bp)
{
    bool dr = (bool)op->dry_run;
    int obs = op->obs_pi;
    int res;
    int numbytes;
    int vb = op->verbose;
    uint32_t blks = csp->cur_out_num;
    int64_t aseek = csp->cur_out_lba;

    if (dr || (vb > 3)) {
        if ((vb > 3) || (dr && (vb > 1)))
            deep_dry_run_lba(__func__, dr, false, aseek, blks,
                             bp - csp->low_bp, csp->buf_name);
        if (dr)
            goto fini;
    }

    if (op->oflagp->nowrite)
        goto fini;
    if (csp->partial_write_bytes > 0) {
        if (op->oflagp->pad) {
            uint8_t * ncbp = (uint8_t *)bp;

            numbytes = blks * obs;
            numbytes += csp->partial_write_bytes;
            // yyyy ++csp->ocbpt;
            ++blks;
            res = blks * obs;
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
    }
fini:
    csp->blks_xfer = blks;
    csp->bytes_xfer = -1;
    return 0;
}

static int
cp_write_pt_wrap(struct dev_info_t * dip, struct cp_state_t * csp,
                 struct opts_t * op)
{
    uint8_t * bp = csp->cur_bp;
    struct dev_info_t * der_dip;

    switch (dip->ddpt_arg) {
    case DDPT_ARG_OUT:
        der_dip = op->odip;
        break;
    case DDPT_ARG_IN:
    case DDPT_ARG_OUT2:
    default:
        pr2serr("%s: logic error, unexpected ddpt_arg=%d\n", __func__,
                dip->ddpt_arg);
        return SG_LIB_CAT_OTHER;
    }
    if (dip != der_dip) {
        pr2serr("%s: logic error, unexpected dip\n", __func__);
        return SG_LIB_CAT_OTHER;
    }
    return cp_write_pt(op, csp, bp);
}

#ifdef SG_LIB_LINUX

/* Main copy loop's write (output (of)) for a tape device.
 * Returns 0 on success, else SG_LIB_CAT_OTHER, SG_LIB_CAT_MEDIUM_HARD
 * or -1 . */
static int
cp_write_tape(struct opts_t * op, struct cp_state_t * csp,
              const uint8_t * bp, bool could_be_last)
{
    static bool printed_ew_message = false;     /* <<<< static local */
    bool dr = (bool)op->dry_run;
    bool got_early_warning = false;
    bool got_partial = false;
    int res, err;
    int numbytes;
    int blks = csp->cur_out_num;
    int obs = op->obs_pi;
    int64_t aseek = csp->cur_out_lba;
    int vb = op->verbose;
/* Only print early warning message once when verbose=2 */

    numbytes = blks * obs;
    if (dr || (vb > 3)) {
        if ((vb > 3) || (dr && (vb > 1)))
            deep_dry_run_fpos(__func__, dr, false, -1, numbytes,
                              bp - csp->low_bp, csp->buf_name);
        if (dr)
            return 0;
    }

    if (op->oflagp->nowrite)
        return 0;
    if (csp->partial_write_bytes > 0) {
        got_partial = true;
        numbytes += csp->partial_write_bytes;
        if (op->oflagp->nopad)
            ++csp->stats.out_partial;
        else {
            uint8_t * ncbp = (uint8_t *)bp;

            // yyyy ++csp->ocbpt;
            ++blks;
            res = blks * obs;
            if (res > numbytes)
                memset(ncbp + numbytes, 0, res - numbytes);
            numbytes = res;
        }
    }

ew_retry:
    while (((res = write(op->odip->fd, bp, numbytes)) < 0) &&
           (EINTR == errno))
        ++csp->stats.interrupted_retries;

    err = errno;
    if ((vb > 2) || ((vb > 0) && could_be_last)) {
        const char * cp;

        cp = ((! op->oflagp->nopad) && got_partial) ? ", padded" : "";
        pr2serr("write(tape%s%s): requested bytes=%d, res=%d\n",
                (got_partial ? ", partial" : ""), cp, numbytes, res);
    }

/* Handle EOM early warning. */
/* The Linux st driver returns -1 and ENOSPC to indicate the drive has reached
 * end of medium early warning. It is still possible to write a significant
 * amount of data before reaching end of tape (e.g. over 200MB for LTO 1). If
 * the user specified oflag=ignoreew (ignore early warning) retry the write.
 * The st driver should allow it; writes alternate until EOM, i.e. write okay,
 * ENOSPC, write okay, ENOSPC, etc. Exit if more than one ENOSPC in a row. */
    if ((op->oflagp->ignoreew) && (-1 == res) && (ENOSPC == err) &&
        (! got_early_warning)) {
        got_early_warning = true;
        if (! printed_ew_message) {
            if (vb > 1)
                pr2serr("writing, seek=%" PRId64 " : EOM early "
                        "warning, continuing...\n", aseek);
             if (2 == vb) {
                pr2serr("(suppressing further early warning messages)\n");
                printed_ew_message = true;
            }
        }
        goto ew_retry;
    }

    if (vb > 3)
        print_tape_pos("", "", op);

    if (res < 0) {
        pr2serr("writing, seek=%" PRId64 " : %s\n", aseek,
                safe_strerror(err));
        if ((EIO == err) || (EREMOTEIO == err))
            return SG_LIB_CAT_MEDIUM_HARD;
        else
            return sg_convert_errno(err);
    } else if (res < numbytes) {
        pr2serr("write(tape): wrote less than requested, exit\n");
        csp->out_iter.filepos += res;
        csp->blks_xfer = res / obs;
        csp->bytes_xfer = res;
        csp->bytes_of = res;
        /* can get a partial write due to a short write */
        if ((res % obs) > 0) {
            ++csp->stats.out_partial;
            ++csp->stats.out_full;
        }
        return -1;
    } else {    /* successful write */
        csp->out_iter.filepos += numbytes;
        csp->blks_xfer = blks;
        csp->bytes_xfer = numbytes;
        csp->bytes_of = numbytes;
    }
    csp->stats.out_full += csp->blks_xfer;
    return 0;
}

#endif /* SG_LIB_LINUX */

/* Main copy loop's write (output (of)) for block device, fifo or regular
 * file. Returns 0 on success, else sg3_utils error code. */
static int
cp_write_block_reg(struct opts_t * op, struct cp_state_t * csp,
                   const uint8_t * bp)
{
    bool dr = (bool)op->dry_run;
    bool got_part = false;
    bool got_sparse_sparing = (op->oflagp->sparse || op->oflagp->sparing);
    uint32_t blks = csp->cur_out_num;
    int64_t offset;
    int64_t aseek = csp->cur_out_lba;
    int res, off, err, rem;
    int obs = op->obs_pi;
    int od_type = op->odip->d_type;
    int numbytes = blks * obs;
    int vb = op->verbose;
    int vb_thresh_filepos_change = got_sparse_sparing ? 3 : 2;

    offset = aseek * obs;
    if (dr || (vb > 3)) {
        if ((vb > 3) || (dr && (vb > 1)))
            deep_dry_run_fpos(__func__, dr, false, aseek * obs, numbytes,
                              bp - csp->low_bp, csp->buf_name);
        if (dr)
            goto fini;
    }
    if (op->oflagp->nowrite)
        goto fini;

#ifdef SG_LIB_WIN32
    if (FT_BLOCK & od_type) {
        if (csp->partial_write_bytes > 0) {
            rem = csp->partial_write_bytes;
            if (op->oflagp->pad) {
                numbytes += rem;
                // yyyy ++csp->ocbpt;
                ++blks;
                res = blks * obs;
                if (res > numbytes)
                    memset((uint8_t *)bp + numbytes, 0,
                           res - numbytes);
                numbytes = res;
                if (vb > 1)
                    pr2serr("write(win32_block): padding probable "
                            "final write at seek=%" PRId64 "\n", aseek);
            } else
                pr2serr(">>> ignore partial write of %d bytes to "
                        "block device\n", rem);
        }
        res = set_filepos_if_needed(op->odip, offset, csp->out_iter.filepos,
                                    __func__, vb >= vb_thresh_filepos_change,
                                    vb);
        if (res)
            return res;
        csp->out_iter.filepos = offset;
        res = win32_block_write(op, bp, numbytes, vb);
        if (res < 0) {
            pr2serr("write(win32_block), seek=%" PRId64 " ", aseek);
            return (-SG_LIB_CAT_MEDIUM_HARD == res) ? -res : -1;
        } else if (res < numbytes) {
            pr2serr("output file probably full, seek=%" PRId64 " ",
                    aseek);
            csp->out_iter.filepos += res;
            csp->blks_xfer = res / obs;
            csp->bytes_xfer = res;
            csp->bytes_of = res;
            /* can get a partial write due to a short write */
            if ((res % obs) > 0) {
                ++csp->stats.out_partial;
                ++csp->stats.out_full;
            }
            return sg_convert_errno(ENOSPC);
        } else {
            csp->out_iter.filepos += numbytes;
            csp->blks_xfer = blks;
            csp->bytes_xfer = numbytes;
            csp->bytes_of = numbytes;
        }
        return 0;
    } else
#endif
    {
        if (csp->partial_write_bytes > 0) {
            rem = csp->partial_write_bytes;
            if (op->oflagp->pad) {
                uint8_t * ncbp = (uint8_t *)bp;

                numbytes += rem;
                // yyyy ++csp->ocbpt;
                ++blks;
                res = blks * obs;
                if (res > numbytes)
                    memset(ncbp + numbytes, 0, res - numbytes);
                numbytes = res;
                if (vb > 1)
                    pr2serr("write(unix): padding probable final "
                            "write at seek=%" PRId64 "\n", aseek);
            } else {
                if (FT_BLOCK & od_type)
                    pr2serr(">>> ignore partial write of %d bytes to block "
                            "device\n", rem);
                else {
                    numbytes += rem;
                    ++csp->stats.out_partial;
                }
            }
        } else if (csp->last_segment && (csp->last_seg_wbytes > 0)) {
            rem = csp->last_seg_wbytes;
            /* assume only when OFILE is regular file */
            if (vb > 1)
                pr2serr("%s(reg): reducing last segment write to %d bytes\n",
                        __func__, rem);
            numbytes = rem;
            /* tweak stats to reflect what is actually happening */
            --csp->stats.out_full;
            ++csp->stats.out_partial;
        }
        if (DDPT_REASON_TAPE_SHORT_READ != csp->leave_reason) {
            res = set_filepos_if_needed(op->odip, offset,
                                        csp->out_iter.filepos, __func__,
                                        vb >= vb_thresh_filepos_change, vb);
            if (res)
                return res;
            csp->out_iter.filepos = offset;
        }
        // write to fifo (reg file ?) is non-atomic so loop if making progress
        off = 0;
        got_part = false;
        do {
            while (((res = write(op->odip->fd, bp + off,
                                 numbytes - off)) < 0) && (EINTR == errno))
                ++csp->stats.interrupted_retries;
            err = errno;
            if ((res > 0) && (res < (numbytes - off)))
                got_part = true;
        } while ((FT_FIFO & od_type) && (res > 0) &&
                 ((off += res) < numbytes));
        if (off >= numbytes) {
            res = numbytes;
            if (got_part && vb)
                pr2serr("write to output file splintered\n");
        } else if (off > 0)
            pr2serr("write to of fifo problem: count=%d, off=%d, "
                    "res=%d\n", numbytes, off, res);
        if ((vb > 2) && (0 == off))
            pr2serr("write(unix): requested bytes=%d, res=%d\n", numbytes,
                    res);
        if (res < 0) {
            pr2serr("writing, seek=%" PRId64 " : %s\n", aseek,
                    safe_strerror(err));
            if ((EIO == err) || (EREMOTEIO == err))
                return SG_LIB_CAT_MEDIUM_HARD;
            return sg_convert_errno(err);
        } else if (res < numbytes) {
            pr2serr("output file probably full, seek=%" PRId64 "\n", aseek);
            csp->out_iter.filepos += res;
            csp->blks_xfer = res / obs;
            csp->bytes_xfer = res;
            csp->bytes_of = res;
            /* can get a partial write due to a short write */
            if ((res % obs) > 0) {
                ++csp->stats.out_partial;
                ++csp->stats.out_full;
            }
            return sg_convert_errno(ENOSPC);
        }
        /* successful write */
        csp->out_iter.filepos += numbytes;
fini:
        csp->blks_xfer = blks;
        csp->bytes_xfer = numbytes;
        csp->bytes_of = numbytes;
        return 0;
    }
}

static int
cp_write_block_reg_wrap(struct dev_info_t * dip, struct cp_state_t * csp,
                        struct opts_t * op)
{
    uint8_t * bp = csp->cur_bp;
    struct dev_info_t * der_dip;

    switch (dip->ddpt_arg) {
    case DDPT_ARG_OUT:
        der_dip = op->odip;
        break;
    case DDPT_ARG_IN:
    case DDPT_ARG_OUT2:
    default:
        pr2serr("%s: logic error, unexpected ddpt_arg=%d\n", __func__,
                dip->ddpt_arg);
        return SG_LIB_CAT_OTHER;
    }
    if (dip != der_dip) {
        pr2serr("%s: logic error, unexpected dip\n", __func__);
        return SG_LIB_CAT_OTHER;
    }
    return cp_write_block_reg(op, csp, bp);
}


/* Only for regular OFILE. Check what to do if last blocks where
 * not written, may require OFILE length adjustment */
static void
cp_sparse_cleanup(struct opts_t * op, struct cp_state_t * csp)
{
    bool dr = (bool)op->dry_run;
    int num;
    int obs = op->obs_pi;
    int vb = op->verbose;
    int64_t lba, offset;
    struct sgl_info_t * sglip = &op->o_sgli;
    struct sgl_iter_t * itp = &csp->out_iter;
    struct stat a_st;

    if (dr || (vb > 3)) {
        if ((vb > 3) || (dr && (vb > 1)))
            deep_dry_run_fpos(__func__, dr, false, csp->cur_out_lba * obs,
                              csp->cur_out_num *obs,
                              0, csp->buf_name);
        if (dr)
            return;
    }
    if (! is_iter_linear(sglip)) {
        if (vb)
            pr2serr("%s: only write last sector, or 'truncate' to that "
                    "length when OFILE has linear sgl\n", __func__);
        return;
    }
    if (sgl_iter_at_end(itp)) {
        if (! sgl_iter_sub(itp, 1)) {
            pr2serr("%s: logic error: can't move out iterator 1 back\n",
                    __func__);
            return;
        }
        num = obs;
        csp->partial_write_bytes = obs;
    } else {
        if (csp->partial_write_bytes < 1) {
            if (! sgl_iter_sub(itp, 1)) {
                pr2serr("%s: logic error(2): can't move out iterator 1 "
                        "back\n", __func__);
                return;
            }
            num = obs;
            csp->partial_write_bytes = obs;
        } else
            num = csp->partial_write_bytes;
    }
    lba = sgl_iter_lba(itp);
    offset = (lba * obs) + num;
    if (vb > 1)
        pr2serr("%s: offset=0x%" PRIx64 ", num_bytes=%d\n", __func__, offset,
                num);
    if (num < 1)
        pr2serr("%s: offset=0x%" PRIx64 ", num_bytes=%d << too small\n",
                __func__, offset, num);

    if (offset > csp->out_iter.filepos) {
        if ((! op->oflagp->strunc) && (op->oflagp->sparse > 1)) {
            if (vb > 1)
                pr2serr("asked to bypass writing sparse last block of "
                        "zeros\n");
            return;
        }
        if (fstat(op->odip->fd, &a_st) < 0) {
            pr2serr("%s: fstat: %s\n", __func__, safe_strerror(errno));
            return;
        }
        if (offset == a_st.st_size) {
            if (vb > 1)
                pr2serr("%s: OFILE already correct length\n", __func__);
            return;
        }
        if (offset < a_st.st_size) {
            if (vb > 1)
                pr2serr("%s: OFILE longer than required, do nothing\n",
                        __func__);
            return;
        }
        if (op->oflagp->strunc) {
            if (vb > 1)
                pr2serr("About to truncate %s to byte offset %" PRId64 "\n",
                        op->odip->fn, offset);
            if (ftruncate(op->odip->fd, offset) < 0) {
                pr2serr("could not ftruncate after copy: %s\n",
                        safe_strerror(errno));
                return;
            }
            /* N.B. file offset (pointer) not changed by ftruncate */
        } else if (1 == op->oflagp->sparse) {
            if (vb > 1)
                pr2serr("writing sparse last block of zeros\n");
            signals_process_delay(op, DELAY_WRITE);
            csp->low_bp = op->zeros_buff;
            csp->subseg_bp = csp->low_bp;
            csp->cur_bp = csp->low_bp;
            csp->cur_countp = &csp->stats.out_full;
            csp->buf_name = "zb";
            csp->cur_out_lba = lba;
            csp->cur_out_num = 0;       /* partial_write_bytes > 0 */

            if (cp_write_block_reg(op, csp, csp->cur_bp) < 0)
                pr2serr("writing sparse last block of zeros "
                        "error, lba=%" PRId64 "\n", offset);
            else
                --csp->stats.out_sparse;
        }
    } else if (vb > 1)
        pr2serr("%s: bypass as output_offset <= output_filepos\n", __func__);
}

/* Main copy loop's finer grain comparison and possible write (to OFILE)
 * for most file types. This function is called when the OBPC option is
 * given (i.e. bpt=BPT,OBPC). Returns 0 on success. */
static int
cp_finer_comp_wr(struct opts_t * op, struct cp_state_t * csp,
                 const uint8_t * b1p, const uint8_t * b2p)
{
    bool done_sigs_delay = false;
    bool dr = (bool)op->dry_run;
    bool need_wr, trim_check, trim_needed;
    int res, k, n, chunk, wr_n_s, wr_k, num, tr_len /*, tr_k */ ;
    int od_type = op->odip->d_type;
    int oblks = csp->cur_out_num;
    int obs = op->obs_pi;
    int numbytes = oblks * obs;

    if (dr || (op->verbose > 3)) {
        if ((op->verbose > 3) || (dr && (op->verbose > 1)))
            deep_dry_run_fpos(__func__, dr, false, csp->cur_out_lba * obs,
                              oblks * obs, b2p - csp->low_bp, csp->buf_name);
        if (dr)
            return 0;
    }

    csp->reading = false;
    if (op->obpch >= oblks) {   /* output blocks per check */
        csp->low_bp = (uint8_t *)b1p;
        csp->subseg_bp = csp->low_bp;
        csp->cur_bp = csp->low_bp;
        csp->cur_countp = &csp->stats.out_full;
        csp->buf_name = "b1p";
        if (FT_DEV_NULL & od_type)
            res = 0;
        else if (FT_PT & od_type) {
            signals_process_delay(op, DELAY_WRITE);
            res = cp_via_sgl_iter(op->odip, csp, oblks, cp_write_pt_wrap, op);
        } else {
            signals_process_delay(op, DELAY_WRITE);
            res = cp_via_sgl_iter(op->odip, csp, oblks,
                                  cp_write_block_reg_wrap, op);
        }
        return res;
    }
    if ((FT_REG & od_type) && (csp->partial_write_bytes > 0))
        numbytes += csp->partial_write_bytes;
    chunk = op->obpch * obs;
    trim_check = (op->oflagp->sparse && op->oflagp->wsame16 &&
                  (FT_PT & od_type));
    trim_needed = false;
    tr_len = 0;
    /* tr_k = 0; */
    for (k = 0, n = 0, need_wr = false, wr_n_s = 0, wr_k = 0; k < numbytes;
         k += chunk) {
        n = ((k + chunk) < numbytes) ? chunk : (numbytes - k);
        if (0 == memcmp(b1p + k, b2p + k, n)) {         /* chunk equal */
            if (need_wr) {      /* write prior unequals */
                num = wr_n_s / obs;
                csp->low_bp = (uint8_t *)b1p;
                csp->subseg_bp = (uint8_t *)(b1p + wr_k);
                csp->cur_bp = csp->subseg_bp;
                csp->cur_countp = &csp->stats.out_full;
                if (FT_DEV_NULL & od_type)
                    ;
                else if (FT_PT & od_type) {
                    if (! done_sigs_delay) {
                        done_sigs_delay = true;
                        signals_process_delay(op, DELAY_WRITE);
                    }
                    res = cp_via_sgl_iter(op->odip, csp, num,
                                          cp_write_pt_wrap, op);
                } else {
                    if (! done_sigs_delay) {
                        done_sigs_delay = true;
                        signals_process_delay(op, DELAY_WRITE);
                    }
                    res = cp_via_sgl_iter(op->odip, csp, num,
                                          cp_write_block_reg_wrap, op);
                }
                need_wr = false;
            }
            num = n / obs;
            sgl_iter_add(&csp->out_iter, num, true /* relative */);
            csp->stats.out_sparse += num;
            if (trim_needed)
                tr_len += n;
            else if (trim_check) {
                trim_needed = true;
                tr_len = n;
                /* tr_k = k; */
            }
        } else {   /* unequal, look for a sequence of unequals */
            if (need_wr)
                wr_n_s += n;
            else {
                need_wr = true;
                wr_n_s = n;
                wr_k = k;
            }
            if (trim_needed) {
                if (! done_sigs_delay) {
                    done_sigs_delay = true;
                    signals_process_delay(op, DELAY_WRITE);
                }
                num = tr_len / obs;
                csp->low_bp = (uint8_t *)op->zeros_buff;
                csp->subseg_bp = csp->low_bp;
                csp->cur_bp = csp->low_bp;
                csp->cur_countp = &csp->stats.out_full;
                csp->buf_name = "zb";
                res = cp_via_sgl_iter(op->odip, csp, num, cp_write_same_wrap,
                                      op);
                if (res)
                    ++csp->stats.trim_errs;
                /* continue past trim errors */
                trim_needed = false;
            }
        }
    }   /* end of for loop */
    if (need_wr) {      /* finished loop but work to do */
        num = wr_n_s / obs;
        csp->low_bp = (uint8_t *)b1p;
        csp->subseg_bp = (uint8_t *)(b1p + wr_k);
        csp->cur_bp = csp->low_bp;
        csp->buf_name = "b1p+";
        csp->cur_countp = &csp->stats.out_full;
        if (FT_DEV_NULL & od_type)
            ;
        else if (FT_PT & od_type) {
            if (! done_sigs_delay) {
                done_sigs_delay = true;
                signals_process_delay(op, DELAY_WRITE);
            }
            res = cp_via_sgl_iter(op->odip, csp, num, cp_write_pt_wrap, op);
        } else {
            if (! done_sigs_delay) {
                done_sigs_delay = true;
                signals_process_delay(op, DELAY_WRITE);
            }
            res = cp_via_sgl_iter(op->odip, csp, num,
                                  cp_write_block_reg_wrap, op);
        }
    }
    if (trim_needed) {
        if (! done_sigs_delay)
            signals_process_delay(op, DELAY_WRITE);
        num = tr_len / obs;
        csp->low_bp = (uint8_t *)b2p;   /* data doesn't matter */
        csp->subseg_bp = csp->low_bp;
        csp->cur_bp = csp->low_bp;
        csp->cur_countp = &csp->stats.out_full;
        csp->buf_name = "b2p";
        res = cp_via_sgl_iter(op->odip, csp, num, cp_write_same_wrap, op);
        if (res)
            ++csp->stats.trim_errs;
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
        uint32_t pg_sz = sg_get_page_size();

        op->zeros_buff = sg_memalign(obpt * op->obs_pi, pg_sz,
                                     &op->free_zeros_buff, false);
        if (NULL == op->zeros_buff) {
            pr2serr("zeros_buff sg_memalign failed\n");
            return sg_convert_errno(ENOMEM);
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
    int64_t in_num_blks = DDPT_COUNT_INDEFINITE;        /* -1 */
    int64_t out_num_blks = DDPT_COUNT_INDEFINITE;
    int64_t ibytes, obytes, ibk, lba;
    bool valid_resume = false;
    int ibs = op->ibs_pi;
    int id_type = op->idip->d_type;
    int obs = op->obs_pi;
    int od_type = op->odip->d_type;
    int res, n;
    int vb = op->verbose;
    struct sgl_info_t * isglip = &op->i_sgli;
    struct sgl_info_t * osglip = &op->o_sgli;
    struct scat_gath_elem * sgep;
    const char * reasonp = "count= given";

    if ((res = calc_count_both(op, &in_num_blks, &out_num_blks)))
        return res;
    if (vb > 1)
        pr2serr("%s: in_num_blks=%" PRId64 ", out_num_blks=%" PRId64 "\n",
                __func__, in_num_blks, out_num_blks);
    if ((in_num_blks > 0) && (isglip->elems > 0) &&
        (isglip->lowest_lba > 0)) {
        if ((FT_REG & id_type) && (isglip->lowest_lba > in_num_blks)) {
            pr2serr("cannot skip to specified offset on %s\n", op->idip->fn);
            op->dd_count = 0;
            return SG_LIB_SYNTAX_ERROR;
        }
    }
    if ((! op->oflagp->resume) && (op->dd_count > 0))
        goto the_end;
    if (op->oflagp->resume) {
        if (FT_REG & od_type) {
            if (osglip->monotonic && (! osglip->fragmented)) {
                if (out_num_blks < 0)
                    pr2serr("resume cannot determine size of OFILE, "
                            "ignore\n");
                else
                    valid_resume = true;
            } else      /* output sgl not linear */
                pr2serr("resume requires linear sgl for OFILE, ignore\n");
        } else
            pr2serr("resume expects OFILE to be regular, ignore\n");
    }
    if (op->dd_count < 0) {
        if ((isglip->elems > 0) && isglip->sum_hard) {
            reasonp = "skip= (input) sgl";
            op->dd_count = isglip->sum;
            if (vb)
                pr2serr("%s: set COUNT from sum of skip <num>s\n", __func__);
        } else if ((osglip->elems > 0) && osglip->sum_hard) {
            obytes = obs * osglip->sum;
            reasonp = "seek= (output) sgl sz";
            op->dd_count = obytes / ibs;
            if (vb)
                pr2serr("%s: set COUNT from sum of seek <num>s\n", __func__);
        }
    }
    if ((op->dd_count < 0) && (! valid_resume)) {
        /* Scale back in_num_blks by value of skip */
        if ((isglip->lowest_lba > 0) &&
            (in_num_blks > isglip->lowest_lba))
            in_num_blks -= isglip->lowest_lba;
        /* Scale back out_num_blks by value of seek */
        if ((osglip->lowest_lba > 0) && (out_num_blks > osglip->lowest_lba))
            out_num_blks -= osglip->lowest_lba;

        if ((out_num_blks < 0) && (in_num_blks > 0)) {
            reasonp = "in file/device sz";
            op->dd_count = in_num_blks;
            op->idip->limits_xfer = true;
        } else if (op->reading_fifo && (FT_REG & od_type))
            reasonp = "reading fifo/pipe";
        else if (op->reading_fifo && (out_num_blks < 0))
            reasonp = "reading fifo/pipe";
        else if ((out_num_blks < 0) && (in_num_blks <= 0))
            reasonp = "? ?";
        else {  /* have both in and out file/device size */
            ibytes = (in_num_blks > 0) ? (ibs * in_num_blks) : 0;
            obytes = obs * out_num_blks;
            if (0 == ibytes) {
                reasonp = "in-side zero length, take out sz";
                op->dd_count = obytes / ibs;
                op->odip->limits_xfer = true;
            } else if ((ibytes > obytes) && (! (FT_REG & od_type))) {
                reasonp = "in > out(not_reg), take out sz";
                op->dd_count = obytes / ibs;
                op->odip->limits_xfer = true;
            } else {
                reasonp = (ibytes > obytes) ? "take in; in > out(reg) but "
                                "reg can grow" : "take in; in <= out";
                op->dd_count = in_num_blks;
                op->idip->limits_xfer = true;
                if ((FT_REG & od_type) && (ibytes < obytes)) {
                    if (! op->quiet)
                        pr2serr(">> Warning: %s will be partially "
                                "overwritten;\n>> Use 'oflag=trunc' "
                                "(truncate before write) to avoid this.\n",
                                op->odip->fn);
                }
            }
        }
    }
    if (valid_resume) {
        reasonp = "resume adjusted sz";
        if (op->dd_count < 0)
            op->dd_count = in_num_blks - op->i_sgli.lowest_lba;
        if (out_num_blks <= op->o_sgli.lowest_lba)
            pr2serr("resume finds no previous copy, restarting\n");
        else {
            obytes = obs * (out_num_blks - op->o_sgli.lowest_lba);
            ibk = obytes / ibs;
            if (ibk >= op->dd_count) {
                pr2serr("resume finds copy complete, exiting\n");
                op->dd_count = 0;
                return SG_LIB_FILE_ERROR;
            }
            /* align to bpt multiple */
            ibk = (ibk / op->bpt_i) * op->bpt_i;
            /* set up iterator offset and reduce count */
            op->resume_iblks = ibk;
            op->dd_count -= ibk;
            pr2serr("resume adjusting in side by %" PRId64 " and out side "
                    "by %" PRId64 " blocks,\n   reducing count to %" PRId64
                    " blocks\n", ibk, (ibk * ibs) / obs, op->dd_count);
        }
    }
the_end:
    if (vb > 1)
        pr2serr("%s: dd_count=%" PRId64 " [reason: %s]\n", __func__,
                op->dd_count, reasonp);
    if ((1 == isglip->elems) && ((isglip->sglp + 0)->num > 0))
        ;       /* 1 element, hard sgl */
    else if (isglip->elems < 2) {    /* 1 element soft sgl(in) or no sgl */
        lba = 0;
        if (1 == isglip->elems)
            lba = (isglip->sglp + 0)->lba;
        /* dd_count might be large requiring split into many elements */
        n = build_sgl(&sgep, op->dd_count, lba);
        if (n < 0)
            return -n;
        else if (n > 0) {       /* need to replace existing sgl */
            if (1 == isglip->elems)
                free(isglip->sglp);
            isglip->sglp = sgep;
            isglip->elems = n;
            sgl_sum_scan(isglip, "count_calculate[in]", vb > 1);
        }
    }
    if ((isglip->elems > 0) && (NULL == isglip->sglp)) {
        pr2serr("%s: in_sg_elems=%d but in_sgl NULL\n", __func__,
                isglip->elems);
        return SG_LIB_LOGIC_ERROR;
    }

    if ((1 == osglip->elems) && ((osglip->sglp + 0)->num > 0))
        ;
    else if (osglip->elems < 2) {
        lba = 0;
        if (1 == osglip->elems)
            lba = (osglip->sglp + 0)->lba;
        if (FT_DEV_NULL & od_type)
            n = build_degen_sgl(&sgep, lba);
        else if (op->odip->limits_xfer)
            n = build_sgl(&sgep, out_num_blks, lba);
        else
            n = build_sgl(&sgep, (op->dd_count * ibs) / obs, lba);
        if (n < 0)
            return -n;
        else if (n > 0) {
            if (1 == osglip->elems)
                free(osglip->sglp);
            osglip->sglp = sgep;
            osglip->elems = n;
            sgl_sum_scan(osglip, "count_calculate[out]", vb > 1);
        }
    }
    if ((osglip->elems > 0) && (NULL == osglip->sglp)) {
        pr2serr("%s: out_sg_elems=%d but out_sgl is NULL\n", __func__,
                osglip->elems);
        return SG_LIB_LOGIC_ERROR;
    }
    return 0;
}

static void
last_seg_helper(struct cp_state_t * csp, uint8_t * wPos, int ibs, int obs,
                struct opts_t * op)
{
    int res, rem, n;
    int vb = op->verbose;
    const struct dev_info_t * odip = op->odip;

    csp->last_segment = true;
    csp->icbpt = op->dd_count;
    res = op->dd_count; /* assume remaining count fits in 31 bits */
    if (ibs == obs)
        csp->ocbpt = res;
    else
        csp->ocbpt = x_mult_div(res, ibs, obs); /* (res * ibs) / obs */
    if (odip->limits_xfer && (odip->reg_sz >= 0)) {
        rem = 0;
        if (odip->reg_sz < csp->out_iter.filepos)
            pr2serr("%s: reg_sz=%" PRId64 " less than filepos=%"
                    PRId64 "\n", __func__, odip->reg_sz,
                    csp->out_iter.filepos);
        else if ((odip->reg_sz - csp->out_iter.filepos) >
                 (ibs * op->bpt_i))
            pr2serr("%s: reg_sz=%" PRId64 " and filepos=%" PRId64
                    "too far apart\n", __func__, odip->reg_sz,
                    csp->out_iter.filepos);
        else
            rem = (int)(odip->reg_sz - csp->out_iter.filepos);
        n = (rem / obs);
        if (n < csp->ocbpt) {
            csp->last_seg_wbytes = rem;
            csp->ocbpt = n;
            if (rem % obs)
                ++csp->ocbpt;
            if (vb)
                pr2serr("%s: out limited xfer, last_seg_wbytes=%d, ocbpt=%d"
                        "\n", __func__, csp->last_seg_wbytes, csp->ocbpt);
        } else {
            if (vb)
                pr2serr("%s: keep dd_count based ocbpt=%d, ignore calculated"
                        "=%d\n", __func__, csp->ocbpt, n);
        }
    } else {    /* xfer not limited by size of regular file */
        if (ibs == obs)
            rem = 0;
        else
            rem = x_mult_rem(res, ibs, obs);  /* (res * ibs) % obs */
        if (rem > 0) {
            ++csp->ocbpt;
            memset(wPos, 0, ibs * op->bpt_i);
        }
    }
}

static int
rw_reg_blk_see_leave(struct opts_t * op, struct cp_state_t * csp, int num,
                     int * id_typep, bool * change_overp, uint8_t * wPos)
{
    int n, rem_blks;
    int ibpt = op->bpt_i;
    int ibs = op->ibs_pi;

    n = csp->rem_seg_bytes;
    if (DDPT_REASON_EOF_ON_READ == csp->leave_reason) {
        if (op->iflagp->zero || op->iflagp->ff) {
            rem_blks = num - csp->blks_xfer;
            csp->leave_after_write = false;
            if (op->iflagp->zero) {
                memset(wPos + n, 0x0, ibs * ibpt - n);
                *id_typep = FT_DEV_NULL;
                op->idip->d_type = FT_DEV_NULL;
            } else {
                memset(wPos + n, 0xff, ibs * ibpt - n);
                *id_typep = FT_ALL_FF;
                op->idip->d_type = FT_ALL_FF;
            }
            *change_overp = true;
            csp->stats.in_full += rem_blks;
            if (! op->quiet)
                pr2serr("Reached EOF of %s, start %s fill\n", op->idip->fn,
                        (op->iflagp->zero ? "zero" : "ff"));
            return 0;
        } else {
            csp->leave_after_write = true;
            if (n <= 0)
                return 0;   /* finished without error */
            csp->ocbpt = n / op->obs_pi;
            csp->icbpt = n / ibs;   /* so dd_count accurate */
            if (n % ibs)   /* for dd_count, round up */
                ++csp->icbpt;
            csp->partial_write_bytes = n % op->obs_pi;
            /* drop through to write */
            return 0;
        }
    } else if (DDPT_REASON_TAPE_SHORT_READ == csp->leave_reason)
        return 0; // xxxxx        ????
    else if ((csp->leave_reason < 0) || (csp->leave_reason > 120))
        return SG_LIB_CAT_OTHER;
    else
        return csp->leave_reason;
}

/* As an extension of the normal return value (zero means good), -888
 * instructs caller to bypass its write stage. */
static int
rw_sparse_sparing(struct opts_t * op, struct cp_state_t * csp, int obs,
                  int num, bool * sparse_skipp, bool * sparing_skipp,
                  uint8_t * wPos)
{
    int n, res;
    int od_type = op->odip->d_type;
    int ret = 0;

    if (op->oflagp->sparse) {
        n = (num * obs) + csp->partial_write_bytes;
        if (0 == memcmp(wPos, op->zeros_buff, n)) {
            *sparse_skipp = true;
            if (op->oflagp->wsame16 && (FT_PT & od_type)) {
                signals_process_delay(op, DELAY_WRITE);
                csp->low_bp = (uint8_t *)op->zeros_buff;
                csp->subseg_bp = csp->low_bp;
                csp->cur_countp = NULL;     /* don't count */
                csp->buf_name = "zb";
                res = cp_via_sgl_iter(op->odip, csp, num,
                                      cp_write_same_wrap, op);
                if (res)
                    ++csp->stats.trim_errs;
                /* record error, otherwise ignore */
            } else
                sgl_iter_add(&csp->out_iter, num, true /* relative */);
        } else if (op->obpch) {     /* output blocks per check */
            csp->cur_out_num = num;
            ret = cp_finer_comp_wr(op, csp, wPos, op->zeros_buff);
            if (ret)
                return ret;
            return -888;
        }
    }
    /* oflag=sparing handling, read output to check if proposed write
     * will change it, if not then bypass write */
    if (op->oflagp->sparing && (! *sparse_skipp)) {
        /* In write sparing, Note: _read_ from the output */
        csp->reading = true;
        csp->cur_out_num = num;
        if (FT_PT & od_type) {
            csp->low_bp = op->wrkPos2;
            csp->subseg_bp = csp->low_bp;
            csp->cur_countp = NULL;
            csp->buf_name = "wPos2";
            ret = cp_via_sgl_iter(op->odip, csp, num, cp_read_pt_wrap, op);
            if (ret)
                return ret;
        } else {    /* otherwise assume regular file or block device */
            csp->low_bp = op->wrkPos2;
            csp->subseg_bp = csp->low_bp;
            csp->cur_countp = NULL;
            csp->buf_name = "wPos2";
            ret = cp_via_sgl_iter(op->odip, csp, num, cp_read_block_reg_wrap,
                                  op);
            if (ret)
                return ret;
        }
        n = (num * obs) + csp->partial_write_bytes;
        if (0 == memcmp(wPos, op->wrkPos2, n))
            *sparing_skipp = true;
        else {
            if (op->iflagp->nowrite)
                ;  /* no need to back up if not writing */
            else
                sgl_iter_sub(&csp->out_iter, num);
            if (op->obpch) {
                csp->reading = false;
                csp->low_bp = op->wrkPos2;
                csp->subseg_bp = csp->low_bp;
                csp->cur_countp = &csp->stats.out_full;
                csp->buf_name = "b1p=wPos; b2p=wPos2";
                ret = cp_finer_comp_wr(op, csp, wPos, op->wrkPos2);
                if (ret)
                    return ret;
                return -888;
            } /* else use write section below */
        }
    }
    return ret;
}

/* This is the main copy loop (unless an offloaded copy is requested).
 * Attempts to copy 'dd_count' blocks (size given by bs or ibs) in chunks
 * of op->bpt_i blocks. Returns 0 if successful.  */
static int
do_rw_copy(struct opts_t * op)
{
    bool sparse_skip, sparing_skip, continual_read;
    bool first_time = true;
    bool change_over = false;
    int k, n, ibpt, obpt, num, bump;
    int ret = 0;
    int vb = op->verbose;
    int id_type = op->idip->d_type;
    int od_type = op->odip->d_type;
    int ibs = op->ibs_pi;
    int obs = op->obs_pi;
    uint8_t * wPos = op->wrkPos;
    struct cp_state_t * csp;
    struct cp_state_t cp_st;

    csp = &cp_st;
    cp_state_init(csp, op);
    continual_read = (op->reading_fifo && (op->dd_count < 0));
    if (vb > 3) {
        if (continual_read)
            pr2serr("%s: reading fifo continually\n", __func__);
        else
            pr2serr("%s: dd_count=%" PRId64 "\n", __func__, op->dd_count);
    }
    if ((op->dd_count <= 0) && (! op->reading_fifo)) {
        ret = 0;
        goto copy_end;
    }
    ibpt = op->bpt_i;
    obpt = x_mult_div(ibs, ibpt, obs);
    if ((ret = cp_construct_pt_zero_buff(op, obpt)))
        goto copy_end;
    /* Both csp->in_iter.filepos and csp->out_iter.filepos are 0 */

    /* <<< main loop that does the copy >>> */
    while ((op->dd_count > 0) || continual_read) {
        if (! first_time)
            signals_process_delay(op, DELAY_COPY_SEGMENT);
        csp->bytes_read = 0;
        csp->bytes_of = 0;
        csp->bytes_of2 = 0;
        sparing_skip = false;
        sparse_skip = false;
        if (continual_read) {
            csp->icbpt = ibpt;
            csp->ocbpt = obpt;
        } else if (op->dd_count > ibpt) {
            csp->icbpt = ibpt;
            csp->ocbpt = obpt;
        } else        /* last segment's write could be shorter */
            last_seg_helper(csp, wPos, ibs, obs, op);

        /* Start of reading section */
        ret = 0;
        num = csp->icbpt;
        csp->reading = true;
        if (FT_PT & id_type) {
            csp->low_bp = wPos;
            csp->subseg_bp = csp->low_bp;
            csp->cur_countp = &csp->stats.in_full;
            csp->buf_name = "wPos";
            ret = cp_via_sgl_iter(op->idip, csp, num, cp_read_pt_wrap, op);
            if (ret) {
                if (csp->leave_after_write &&
                    (DDPT_CAT_SEE_LEAVE_REASON == ret)) {
                    ret = 0;
                    csp->icbpt = csp->blks_xfer;
                    if (ibs == obs)
                        csp->ocbpt = csp->blks_xfer;
                    else {
                        csp->ocbpt = (csp->blks_xfer * ibs) / obs;
                        csp->partial_write_bytes =
                                 (csp->blks_xfer * ibs) % obs;
                    }
                } else
                    break;
            }
        } else if (FT_FIFO & id_type) {
            csp->low_bp = wPos;
            csp->subseg_bp = wPos;
            csp->cur_countp = &csp->stats.in_full;
            csp->buf_name = "wPos";
            csp->cur_in_num = num;
            if (first_time && (op->i_sgli.lowest_lba > 0)) {
                int64_t kk;

                /* implement skip on fifo by read and ignore */
                for (kk = 0; kk < op->i_sgli.lowest_lba; kk += bump) {
                    bump = ((kk + num) > op->i_sgli.lowest_lba) ?
                           (op->i_sgli.lowest_lba - kk) : num;
                    csp->cur_in_num = bump;
                    if ((ret = cp_read_fifo(op, csp, wPos)))
                        break;
                    csp->cur_in_lba += bump;
                }
            }
            csp->subseg_bp = wPos;
            csp->cur_in_num = num;

            if ((ret = cp_read_fifo(op, csp, wPos)))
                break;
            if (csp->leave_after_write) {
                csp->icbpt = csp->blks_xfer;
                if (ibs == obs)
                    csp->ocbpt = csp->blks_xfer;
                else {
                    csp->ocbpt = (csp->blks_xfer * ibs) / obs;
                    csp->partial_write_bytes =
                             (csp->blks_xfer * ibs) % obs;
                }
            }
            csp->cur_in_lba += num;
        } else if (FT_TAPE & id_type) {
#ifdef SG_LIB_LINUX
            if ((ret = cp_read_tape(op, csp, wPos)))
                break;
            if (csp->leave_after_write) {
                csp->icbpt = csp->blks_xfer;
                if (ibs == obs)
                    csp->ocbpt = csp->blks_xfer;
                else {
                    csp->ocbpt = (csp->blks_xfer * ibs) / obs;
                    csp->partial_write_bytes =
                             (csp->blks_xfer * ibs) % obs;
                }
            }
#else
            pr2serr("reading from tape not supported in this OS\n");
            ret = SG_LIB_CAT_OTHER;
            break;
#endif
        } else if (FT_DEV_NULL & id_type) {
            if (first_time || change_over) {
                memset(wPos, 0x0, ibs * ibpt);
                change_over = false;
            }
            csp->stats.in_full += csp->icbpt;
        } else if (FT_ALL_FF & id_type) {
            if (first_time || change_over) {
                n = ibs * ibpt;
                if (op->iflagp->zero) { /* when iflag=00,ff build ascending */
                    for (k = 0; k < n; ++k)
                        wPos[k] = 0xff & k; /* 0,1...9,0xa...0xff,0,1... */
                } else
                    memset(wPos, 0xff, ibs * ibpt);
                change_over = false;
            }
            csp->stats.in_full += num;
        } else {        /* assume regular file or block device */
            csp->low_bp = wPos;
            csp->subseg_bp = wPos;
            csp->cur_countp = &csp->stats.in_full;
            csp->buf_name = "wPos";
            ret = cp_via_sgl_iter(op->idip, csp, num, cp_read_block_reg_wrap,
                                  op);
            if (ret) {
                if (DDPT_CAT_SEE_LEAVE_REASON == ret) {
                    if ((ret = rw_reg_blk_see_leave(op, csp, num, &id_type,
                                                    &change_over, wPos)))
                        break;
                } else
                    break;
            }
        }
        if (0 == csp->icbpt)
            break;      /* nothing read so leave loop */

        /* finished reading side, now write to of2 if required, do sparse
         * and sparing work */

        num = csp->ocbpt;

        /* if required write segment (wPos) to of2 now, no iterators */
        if (op->o2dip->fd >= 0) {
            csp->cur_out_num = num;
            ret = cp_write_of2(op, csp, wPos);
            if (ret)
                break;      /* some error writing to of2 */
        }

        /* oflag=sparse handling */
        ret = rw_sparse_sparing(op, csp, obs, num, &sparse_skip,
                                &sparing_skip, wPos);
        if (-888 == ret)
            goto bypass_write;
        if (ret)
            break;

        /* Start of writing section */
        csp->reading = false;
        if (sparing_skip || sparse_skip) {
            csp->stats.out_sparse += num;
            if (csp->partial_write_bytes > 0)
                ++csp->stats.out_sparse_partial;
            goto bypass_write;
        }
        if (FT_DEV_NULL & od_type)
            goto bypass_write;  /* don't bump out_full (earlier revs did) */

        signals_process_delay(op, DELAY_WRITE);
        if (FT_PT & od_type) {
            csp->low_bp = wPos;
            csp->subseg_bp = csp->low_bp;
            csp->cur_countp = &csp->stats.out_full;
            csp->buf_name = "wPos";
            ret = cp_via_sgl_iter(op->odip, csp, num, cp_write_pt_wrap, op);
            if (ret)
                break;
        } else if (FT_TAPE & od_type) {
#ifdef SG_LIB_LINUX
            bool could_be_last;

            could_be_last = ((! continual_read) &&
                             (csp->icbpt >= op->dd_count));
            if ((ret = cp_write_tape(op, csp, wPos, could_be_last)))
                break;
#else
            pr2serr("writing to tape not supported in this OS\n");
            ret = SG_LIB_CAT_OTHER;
            break;
#endif

        } else if (FT_FIFO & od_type) { /* may need to bypass some blocks */
            /* FT_FIFO only permitted one sgl element */
            uint64_t seek_lba = csp->out_iter.sglp->lba;

            if (csp->cur_out_lba >= seek_lba) {
                csp->low_bp = wPos;
                goto like_reg_blk; /* beyond seek_lba so like regular ofile */
            } else if ((csp->cur_out_lba + num) <= seek_lba) {
                csp->prev_out_lba = csp->cur_out_lba;
                csp->cur_out_lba += num;
                csp->out_iter.filepos += (num * obs);
            } else { /* we are astride seek_lba */
                n = (int)(seek_lba - csp->cur_out_lba);
                csp->prev_out_lba = csp->cur_out_lba;
                csp->cur_out_lba += n;
                csp->out_iter.filepos += (n * obs);
                /* bypassed n blocks, now want to write num-n blocks */
                csp->low_bp = wPos + (n * obs);
                num -= n;
                goto like_reg_blk;
            }
        } else {    /* regular file or block device */
            csp->low_bp = wPos;
like_reg_blk:
            csp->subseg_bp = csp->low_bp;
            csp->cur_countp = &csp->stats.out_full;
            csp->buf_name = "wPos";
            ret = cp_via_sgl_iter(op->odip, csp, num,
                                  cp_write_block_reg_wrap, op);
            if (ret) {
                if (DDPT_CAT_SEE_LEAVE_REASON == ret) {
                    ret = csp->leave_reason;
                    if (DDPT_REASON_EOF_ON_READ == ret) {
                        pr2serr("%s: don't expect REASON_EOF_ON_READ on "
                                "write\n", __func__);
                        ret = SG_LIB_FILE_ERROR;
                    } else if (SG_LIB_LBA_OUT_OF_RANGE == ret)
                        pr2serr("%s seems to be full, exit\n", op->odip->fn);
                    else if (sg_convert_errno(ENOSPC) == ret)
                        pr2serr("%s (or file system) seems to be full, "
                                "exit\n", op->odip->fn);
                    else {
                        char b[120];

                        sg_exit2str(ret, true, sizeof(b), b);
                        pr2serr("Error writing: %s, exit\n", b);
                    }
                }
                break;
            }
        }
bypass_write:

#ifdef HAVE_POSIX_FADVISE
        if (op->iflagp->nocache || op->oflagp->nocache)
            do_fadvise(op, csp->bytes_read, csp->bytes_of, csp->bytes_of2);
#endif
        if (op->dd_count > 0)
            op->dd_count -= csp->icbpt;
        csp->prev_in_lba = csp->cur_in_lba + csp->cur_in_num;
        csp->prev_out_lba = csp->cur_out_lba + csp->cur_out_num;
        if (csp->leave_after_write) {
            if (DDPT_REASON_TAPE_SHORT_READ == csp->leave_reason) {
                /* allow multiple partial writes for tape */
                csp->partial_write_bytes = 0;
                csp->leave_after_write = false;
            } else {
                /* other cases: stop copy after partial write */
                ret = csp->leave_reason;
                break;
            }
        }
        if (first_time)
            first_time = false;
    } /* end of main while loop that does the copy ... */

    if (op->oflagp->nowrite)
        goto copy_end;
    /* sparse: clean up ofile length when last block(s) were not written */
    if ((FT_REG & od_type) && op->oflagp->sparse && op->o_sgli.monotonic)
        cp_sparse_cleanup(op, csp);

#ifdef HAVE_FDATASYNC
    else if (op->oflagp->fdatasync) {
        if (fdatasync(op->odip->fd) < 0)
            perror("fdatasync() error");
        if (vb)
            pr2serr("Called fdatasync() on %s successfully\n", op->odip->fn);
    }
#endif
#ifdef HAVE_FSYNC
    else if (op->oflagp->fsync) {
        if (fsync(op->odip->fd) < 0)
            perror("fsync() error");
        if (vb)
            pr2serr("Called fsync() on %s successfully\n", op->odip->fn);
    }
#endif

copy_end:
    if (DDPT_CAT_SEE_LEAVE_REASON == ret)
        ret = csp->leave_reason;
    op->stats = csp->stats;
    op->stats.copied_from_working = true;
    op->stp = NULL;
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

    if (op->rdprotect) {
        if ((0 == op->idip->prot_type) || (! (FT_PT & op->idip->d_type))) {
            pr2serr("IFILE is not a pt device or doesn't have "
                    "protection information\n");
            return SG_LIB_CAT_OTHER;
        }
        if (op->ibs_lb != op->obs_lb) {
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
        if ((op->ibs_lb == op->obs_lb) && (0 == op->wrprotect))
            op->obs_pi += res;  /* move in sympathy, message to user ?? */
    }
    if (op->wrprotect) {
        if ((0 == op->odip->prot_type) || (! (FT_PT & op->odip->d_type))) {
            pr2serr("OFILE is not a pt device or doesn't have "
                    "protection information\n");
            return SG_LIB_CAT_OTHER;
        }
        if (op->ibs_lb != op->obs_lb) {
            pr2serr("protect: don't support IFILE and OFILE "
                    "with different block sizes\n");
            return SG_LIB_CAT_OTHER;
        }
        res = (op->odip->p_i_exp ? (1 << op->odip->p_i_exp) : 1) * 8;
        op->obs_pi += res;
        if ((op->ibs_lb == op->obs_lb) && (0 == op->rdprotect))
            op->ibs_pi += res;  /* move in sympathy, message to user ?? */
    }
    op->bs_same = (op->ibs_pi == op->obs_pi);
#else
    if (op) { ; }       /* suppress warning */
#endif  /* PI_WORK */
    return 0;
}

static int
open_files_devices(struct opts_t * op)
{
    int fd, ret;
    int vb = op->verbose;
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
            op->reading_fifo = true;
            if (vb)
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
    } else if (op->iflagp->zero) {
        idip->d_type = FT_DEV_NULL;
        idip->fd = 9998;        /* unlikely file descriptor */
    } else {
        pr2serr("'if=IFILE' operand must be given. For stdin as input use "
                "'if=-'\n");
        pr2serr("For more information use '--help'\n");
        return SG_LIB_SYNTAX_ERROR;
    }

    if ('\0' == odip->fn[0])
        strcpy(odip->fn, "."); /* treat no 'of=OFILE' operand as /dev/null */
    if (('-' == odip->fn[0]) && ('\0' == odip->fn[1])) {
        fd = STDOUT_FILENO;
        odip->d_type = FT_FIFO;
        odip->d_type_hold = odip->d_type;
        if (vb)
            pr2serr(" >> Output file type: fifo [stdin, stdout, named "
                    "pipe]\n");
    } else {
        fd = open_of(op);
        if (fd < 0) {
            if (-9999 == fd)    /* -9999 indicates /dev/null, didn't open */
                fd = -1;
            else
                return -fd;     /* negated sg3_utils style error code */
        }
    }
    odip->fd = fd;

    if (o2dip->fn[0]) {
        if (('-' == o2dip->fn[0]) && ('\0' == o2dip->fn[1])) {
            fd = STDOUT_FILENO;
            o2dip->d_type = FT_FIFO;
            if (vb)
                pr2serr(" >> Output 2 file type: fifo  [stdin, stdout, "
                        "named pipe]\n");
        } else {
            o2dip->d_type = dd_filetype(o2dip->fn, vb);
            if (FT_DEV_NULL & o2dip->d_type)
                fd = -1;
            else if (! ((FT_REG | FT_FIFO | FT_ERROR) & o2dip->d_type)) {
                /* assume FT_ERROR is file doesn't exist */
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
                if (vb)
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
    if (! op->bpt_given) {
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
            ((op->ibs_lb <= 32768) && (op->bpt_i * op->ibs_lb) > 32768))
            op->bpt_i = 32768 / op->ibs_lb;
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
            op->out_sparse_active = true;
            if (op->oflagp->wsame16)
                op->out_trim_active = true;
        }
    }
    if (op->oflagp->sparing) {
        if ((FT_DEV_NULL | FT_FIFO | FT_TAPE) & op->odip->d_type) {
            pr2serr("oflag=sparing needs a readable and seekable "
                    "output file, ignore\n");
            op->oflagp->sparing = false;
        } else
            op->out_sparing_active = true;
    }
}

static void
cdb_size_prealloc(struct opts_t * op)
{
    int ibs = op->ibs_pi;
    int obs = op->obs_pi;

    if (op->oflagp->prealloc) {
        if ((FT_DEV_NULL | FT_FIFO | FT_TAPE | FT_PT) & op->odip->d_type) {
            pr2serr("oflag=prealloc needs a normal output file, ignore\n");
            op->oflagp->prealloc = false;
        }
    }
    if (! op->cdbsz_given) {
        if ((FT_PT & op->idip->d_type) && (op->iflagp->cdbsz < 16) &&
            ((op->i_sgli.high_lba_p1 > UINT_MAX) ||
             (op->bpt_i > USHRT_MAX))) {
            if (op->verbose > 0)
                pr2serr("SCSI command size increased from 10 to 16 "
                        "bytes on %s\n", op->idip->fn);
            op->iflagp->cdbsz = 16;
        }
        if ((FT_PT & op->odip->d_type) && (op->oflagp->cdbsz < 16) &&
            ((op->o_sgli.high_lba_p1 > UINT_MAX) ||
             (((op->bpt_i * ibs) / obs) > USHRT_MAX))) {
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

    if (op->oflagp->nofm || (! op->oflagp->fsync)) {
        mt_cmd.mt_op = op->oflagp->fsync ? MTWEOF : MTWEOFI;
        mt_cmd.mt_count = (op->oflagp->nofm) ? 0 : 1;
        res = ioctl(op->odip->fd, MTIOCTOP, &mt_cmd);
        if (res != 0) {
            if (op->verbose > 0)
                pr2serr("MTWEOF%s %d failed: %s\n",
                        op->oflagp->fsync ? "" : "I", mt_cmd.mt_count,
                        safe_strerror(errno));
            if (op->oflagp->nofm && (! op->oflagp->fsync)) {
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
     * oflag=prealloc (at the risk of running out of disk space).
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
    int res, err;
    int obs = op->obs_pi;
    int64_t oseek = op->o_sgli.lowest_lba * obs;        /* fpos */
    int64_t o_count = op->dd_count * op->ibs_pi;        /* bytes */

#ifdef PREALLOC_DEBUG
    pr2serr("About to call fallocate() with FALLOC_FL_KEEP_SIZE\n");
#endif
    res = fallocate(op->odip->fd, FALLOC_FL_KEEP_SIZE, oseek, o_count);
#ifdef PREALLOC_DEBUG
    pr2serr("fallocate() returned %d\n", res);
#endif
    /* fallocate() fails if the kernel does not support
     * FALLOC_FL_KEEP_SIZE, so retry without that flag. */
    if (-1 == res) {
        err = errno;
        if ((ENOTTY == err) || (EINVAL == err) || (EOPNOTSUPP == err)) {
            if (op->verbose)
                pr2serr("Could not pre-allocate with "
                        "FALLOC_FL_KEEP_SIZE (%s), retrying without "
                        "...\n", safe_strerror(err));
            res = fallocate(op->odip->fd, 0 /* no flags */, oseek, o_count);
#ifdef PREALLOC_DEBUG
            pr2serr("fallocate() without FALLOC_FL_KEEP_SIZE "
                    " returned %d\n", res);
#endif
        }
    } else {
        /* fallocate() with FALLOC_FL_KEEP_SIZE succeeded. Set
         * op->oflagp->prealloc to 0 so the possible message about using
         * oflag=resume is not suppressed later. */
        op->oflagp->prealloc = false;
    }
    if (-1 == res) {
        pr2serr("Unable to pre-allocate space: %s\n", safe_strerror(err));
        return sg_convert_errno(err);
    }
    if (op->verbose > 1)
        pr2serr("Pre-allocated %" PRId64 " bytes at offset %" PRId64 "\n",
                o_count, oseek);

#endif  /* HAVE_FALLOCATE */
#else   /* other than SG_LIB_LINUX */
#ifdef HAVE_POSIX_FALLOCATE
    int res, err;
    int obs = op->obs_pi;
    int64_t oseek = op->o_sgli.lowest_lba * obs;        /* fpos */
    int64_t o_count = op->dd_count * op->ibs_pi;        /* bytes */

    /* If not on Linux, use posix_fallocate(). (That sets the file size to its
     * full length, so re-invoking ddpt with oflag=resume will do nothing.) */
    res = posix_fallocate(op->odip->fd, oseek, o_count);
    if (-1 == res) {
        err = errno;
        pr2serr("Unable to pre-allocate space: %s\n", safe_strerror(err));
            return sg_convert_errno(err);
    }
    if (op->verbose > 1)
        pr2serr("Pre-allocated %" PRId64 " bytes at offset %" PRId64 "\n",
                o_count, oseek);
#else   /* do not HAVE_POSIX_FALLOCATE */
    if (op) { ; }
#endif  /* HAVE_POSIX_FALLOCATE else */
#endif  /* SG_LIB_LINUX else */
    return 0;
}

static void
details_pre_copy_print(struct opts_t * op)
{
    pr2serr("lowest in lba=%" PRId64 " [0x%" PRIx64 "], lowest out lba=%"
            PRId64 " [0x%" PRIx64 "]\n", op->i_sgli.lowest_lba,
            op->i_sgli.lowest_lba, op->o_sgli.lowest_lba,
            op->o_sgli.lowest_lba);
    if (op->verbose > 1) {
        pr2serr("  ibs=%d bytes, obs=%d bytes; in bpt=%d, OBPC=%d\n",
                op->ibs_lb, op->obs_lb, op->bpt_i, op->obpch);
        if (op->ibs_lb != op->ibs_pi)
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
    op->wrkPos = sg_memalign(len, 0 /* page align */, &op->free_wrkPos,
                             false);
    if (NULL == op->wrkPos) {
        pr2serr("%s: sg_memalign: error, out of memory?\n", __func__);
        return sg_convert_errno(ENOMEM);
    }
    op->wrkPos2 = sg_memalign(len, 0, &op->free_wrkPos2, false);
    if (NULL == op->wrkPos2) {
        pr2serr("%s: sg_memalign: error, out of memory 2\n", __func__);
        return sg_convert_errno(ENOMEM);
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

    if (op->free_wrkPos)
        free(op->free_wrkPos);
    if (op->free_wrkPos2)
        free(op->free_wrkPos2);
    if (op->free_zeros_buff)
        free(op->free_zeros_buff);
    if (op->i_sgli.sglp)
        free(op->i_sgli.sglp);
    if (op->o_sgli.sglp)
        free(op->o_sgli.sglp);
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

/* Files should be open, sgl setup, PI checked.
 *                                                    */
static int
final_check(struct opts_t * op)
{
    int id_type = op->idip->d_type;
    int od_type = op->odip->d_type;
#if 0
    int ibs = op->ibs_pi;
    int obs = op->obs_pi;
#endif
    struct sgl_info_t * isglip = &op->i_sgli;
    struct sgl_info_t * osglip = &op->o_sgli;

    if (FT_FIFO & id_type) {
        if (isglip->monotonic && (! isglip->fragmented))
            ;
        else {
            pr2serr("Input file is FIFO, but doesn't have linear sgl\n");
            return SG_LIB_SYNTAX_ERROR;
        }
    }
    if (FT_FIFO & od_type) {
        if (osglip->monotonic && (! osglip->fragmented))
            ;
        else {
            pr2serr("Output file is FIFO, but doesn't have linear sgl\n");
            return SG_LIB_SYNTAX_ERROR;
        }
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
    int ibs, obs, vb;
    int64_t d_count, nn;
    struct opts_t ops;
    struct flags_t iflag, oflag;
    struct dev_info_t ids, ods, o2ds;
    struct opts_t * op;

    op = &ops;
    state_init(op, &iflag, &oflag, &ids, &ods, &o2ds);
    op->primary_ddpt = true;
    ret = cl_parse(op, argc, argv, ddpt_version_str, jf_depth);
    if (op->do_help > 0) {
        ddpt_usage(op->do_help);
        return 0;
    }

#ifdef DEBUG
    pr2serr("In DEBUG mode, ");
    if (op->verbose_given && op->version_given) {
        pr2serr("but override: '-vV' given, zero verbose and continue\n");
        op->verbose_given = false;
        op->version_given = false;
        op->verbose = 0;
    } else if (! op->verbose_given) {
        pr2serr("set '-vv'\n");
        op->verbose = 2;
    } else
        pr2serr("keep verbose=%d\n", op->verbose);
#else
    if (op->verbose_given && op->version_given)
        pr2serr("Not in DEBUG mode, so '-vV' has no special action\n");
#endif
    if (op->version_given) {
        pr2serr("version: %s\n", ddpt_version_str);
        return 0;
    }

    if (ret)
        return (ret < 0) ? 0 : ret;
    vb = op->verbose;

#ifdef SG_LIB_WIN32
    if (op->wscan)
        return sg_do_wscan('\0', op->wscan, vb);
#endif

    if (op->quiet) {
        if (NULL == freopen("/dev/null", "w", stderr))
            pr2serr("freopen: failed to redirect stderr to /dev/null : %s\n",
                    safe_strerror(errno));
    }

    install_signal_handlers(op);

    if (op->has_odx) {
        started_copy = 1;
        if (1 == op->dry_run) { /* let op->dry_run > 1 go deeper */
            if (! op->quiet)
                pr2serr("Bypass copy due to --dry-run\n");
            ret = 0;
        } else
            ret = do_odx(op);
        goto cleanup;
    }

    if ((ret = open_files_devices(op)))
        return ret;

    block_size_bpt_check(op);
    sparse_sparing_check(op);

    if ((ret = count_calculate(op))) {
        if (vb)
            pr2serr("count_calculate() returned %d, exit\n", ret);
        goto cleanup;
    }

    if ((ret = prepare_pi(op)))
        goto cleanup;
    ibs = op->ibs_pi;
    obs = op->obs_pi;
    op->idip->bs_pi = ibs;
    op->odip->bs_pi = obs;
    op->o2dip->bs_pi = obs;
    d_count = op->dd_count;
    nn = ibs;
    nn *= op->bpt_i;
    if ((nn >= INT_MAX) || (nn < 0)) {
        pr2serr("Product ibs*bpt is too large, try reducing bpt (%d)\n",
                op->bpt_i);
        ret = SG_LIB_CAT_OTHER;
        goto cleanup;
    }

    if ((d_count < 0) && (! op->reading_fifo)) {
        pr2serr("Couldn't calculate count, please give one\n");
        ret = SG_LIB_SYNTAX_ERROR;
        goto cleanup;
    }

    if (d_count > 0) {     /* see if we need append2sgl */
        if ((! op->i_sgli.sum_hard) && (op->i_sgli.sum < d_count)) {
            struct sgl_info_t * sglip = &op->i_sgli;
            struct scat_gath_elem ** sgepp = &sglip->sglp;
            struct scat_gath_elem * hold_sgep = *sgepp;

            sglip->elems = append2sgl(sgepp, sglip->elems,
                                      d_count - sglip->sum,
                                      sglip->high_lba_p1);
            if (sglip->elems < 0) {
                ret = -sglip->elems;
                goto cleanup;
            }
            if (*sgepp != hold_sgep)
                sgl_sum_scan(sglip, "[in] append to sgl", vb > 1);
        }
        if (! (op->o_sgli.sum_hard || (FT_DEV_NULL & op->odip->d_type))) {
            int64_t odd_count = (ibs == obs) ? d_count :
                                               ((d_count * ibs) / obs);
            struct sgl_info_t * sglip = &op->o_sgli;
            struct scat_gath_elem ** sgepp = &sglip->sglp;
            struct scat_gath_elem * hold_sgep = *sgepp;

            sglip->elems = append2sgl(sgepp, sglip->elems,
                                      odd_count - sglip->sum,
                                      sglip->high_lba_p1);
            if (sglip->elems < 0) {
                ret = -sglip->elems;
                goto cleanup;
            }
            if (*sgepp != hold_sgep)
                sgl_sum_scan(sglip, "[out] append to sgl", vb > 1);
        }
    } else if (op->reading_fifo && (op->dd_count < 0) &&
               (! op->o_sgli.sum_hard)) {
        struct sgl_info_t * sglip = &op->o_sgli;
        struct scat_gath_elem ** sgepp = &sglip->sglp;
        struct scat_gath_elem * hold_sgep = *sgepp;

        sglip->elems = append2sgl(sgepp, sglip->elems,
                                  DDPT_BIGGEST_CONTINUAL,
                                  sglip->high_lba_p1);
        if (sglip->elems < 0) {
            ret = -sglip->elems;
            goto cleanup;
        }
        if (*sgepp != hold_sgep)
            sgl_sum_scan(sglip, "[out] append 'biggest' to sgl", vb > 1);
    }

    /* Check dd_count doesn't overflow out block/sg device */
    if (op->ibs_pi != op->obs_pi) {
        nn = op->ibs_pi * op->dd_count;
        if (nn % op->obs_pi) {
            int64_t orig_dd_count = op->dd_count;

            /* Definitely needed for block/sg devs but what about regs? */
            op->dd_count = ((nn / op->obs_pi) * op->obs_pi) / op->ibs_pi;
            pr2serr("Reducing dd_count from %" PRId64 " to %" PRId64 " to "
                    "fit last out block\n", orig_dd_count, op->dd_count);
        }
    }

    cdb_size_prealloc(op);

    if ((ret = wrk_buffers_init(op)))
        goto cleanup;

    if (vb)
        details_pre_copy_print(op);

    ret = final_check(op);
    if (ret)
        goto cleanup;

    op->read1_or_transfer = !! (FT_DEV_NULL & op->odip->d_type);
    op->stats.dd_count_start = op->dd_count;
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
    if (vb && (op->dry_run > 1))
        pr2serr("dry_run=%d goes deep: lines start with 'ddr:'\n   then "
                "'<<<' indicates in from %s\n   and '>>>' indicates out "
                "to %s\n", op->dry_run, op->idip->fn, op->odip->fn);

/* Copy done here vvvvvvvvvvvvvvvvvvv */
    if (1 == op->dry_run) {     /* let op->dry_run > 1 go deeper */
        if (! op->quiet)
            pr2serr("Bypass copy due to --dry-run\n");
    } else if (op->has_xcopy)
        ret = do_xcopy_lid1(op);
    else
        ret = do_rw_copy(op);
/* Copy done here ^^^^^^^^^^^^^^^^^^^ */

    if (op->progress > 0)
        pr2serr("\nCompleted:\n");
    if (! op->status_none)
        print_stats("", op, 0 /* both in and out */, true);

    if (op->oflagp->ssync && (FT_PT & op->odip->d_type)) {
        if (! op->status_none)
            pr2serr(">> SCSI synchronizing cache on %s\n", op->odip->fn);
        pt_sync_cache(op->odip->fd);
    }
    if (op->do_time)
        calc_duration_throughput("", false /* contin */, op);

    if (op->stats.sum_of_resids)
        pr2serr(">> Non-zero sum of residual counts=%d\n",
                op->stats.sum_of_resids);

cleanup:
    cleanup_resources(op);
    if ((0 == ret) && op->err_to_report)
        ret = op->err_to_report;
    if (op->quiet)
        goto the_end;
    if (started_copy && (0 != op->dd_count) && (! op->reading_fifo)) {
        const char * et = "Early termination";

        if (0 == ret) {
            if (! op->dry_run)
                pr2serr("%s, EOF on input?\n", et);
        } else if (ret > 0)
            print_exit_status_msg(et, ret, true /* to stderr */);
        else {
            if (vb < 2)
                pr2serr("%s: some error occurred; try again with '-vv'\n",
                        et);
            else
                pr2serr("%s: some error occurred\n", et);
        }
    } else if (! started_copy) {
        if (ret > 0)
            print_exit_status_msg("During preparation", ret, true);
    } else if (ret > 0)
        print_exit_status_msg("Problem", ret, true);
the_end:
    return (ret >= 0) ? ret : SG_LIB_CAT_OTHER;
}


