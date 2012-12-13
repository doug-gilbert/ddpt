/*
 * Copyright (c) 2008-2012 Douglas Gilbert.
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
 * This file contains SCSI pass-through (pt) helper functions for ddpt.
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

#include "ddpt.h"

#include "sg_lib.h"
#include "sg_cmds_basic.h"
#include "sg_cmds_extra.h"
#include "sg_pt.h"


void *
pt_construct_obj(void)
{
    void * vp;

    if (NULL == (vp = construct_scsi_pt_obj()))
        fprintf(stderr, "construct_scsi_pt_obj: out of memory\n");
    return vp;
}

void
pt_destruct_obj(void * vp)
{
    destruct_scsi_pt_obj(vp);
}

int
pt_open_if(struct opts_t * op)
{
    int verb, flags, fl, fd;
    struct sg_simple_inquiry_resp sir;
    struct flags_t * ifp = op->iflagp;
    const char * inf = op->inf;

    verb = (op->verbose ? op->verbose - 1: 0);
    flags = O_NONBLOCK;
    if (ifp->direct)
        flags |= O_DIRECT;
    if (ifp->excl)
        flags |= O_EXCL;
    if (ifp->sync)
        flags |= O_SYNC;
    fl = O_RDWR;
    if ((fd = scsi_pt_open_flags(inf, (fl | flags), op->verbose)) < 0) {
        fl = O_RDONLY;
        if ((fd = scsi_pt_open_flags(inf, (fl | flags), op->verbose)) < 0) {
            fprintf(stderr, "could not open %s for pt reading: %s\n", inf,
                    safe_strerror(-fd));
            return -1;
        }
    }
    if (sg_simple_inquiry(fd, &sir, 0, verb)) {
        fprintf(stderr, "INQUIRY failed on %s\n", inf);
        return -2;
    }
    ifp->pdt = sir.peripheral_type;
    if (op->verbose)
        fprintf(stderr, "    %s: %.8s  %.16s  %.4s  [pdt=%d]\n",
                inf, sir.vendor, sir.product, sir.revision, ifp->pdt);
    return fd;
}

int
pt_open_of(struct opts_t * op)
{
    int verb, flags, fd;
    struct sg_simple_inquiry_resp sir;
    struct flags_t * ofp = op->oflagp;
    const char * outf = op->outf;

    verb = (op->verbose ? op->verbose - 1: 0);
    flags = O_RDWR | O_NONBLOCK;
    if (ofp->direct)
        flags |= O_DIRECT;
    if (ofp->excl)
        flags |= O_EXCL;
    if (ofp->sync)
        flags |= O_SYNC;
    if ((fd = scsi_pt_open_flags(outf, flags, op->verbose)) < 0) {
        fprintf(stderr, "could not open %s for pt writing: %s\n",
                outf, safe_strerror(-fd));
        return -1;
    }
    if (sg_simple_inquiry(fd, &sir, 0, verb)) {
        fprintf(stderr, "INQUIRY failed on %s\n", outf);
        return -2;
    }
    ofp->pdt = sir.peripheral_type;
    if (op->verbose)
        fprintf(stderr, "    %s: %.8s  %.16s  %.4s  [pdt=%d]\n",
                outf, sir.vendor, sir.product, sir.revision, ofp->pdt);
    return fd;
}

void
pt_close(int fd)
{
    scsi_pt_close_device(fd);
}

/* Fetch number of blocks and block size of a pt device.
 * Return of 0 -> success, see sg_ll_read_capacity*() otherwise. */
int
pt_read_capacity(struct opts_t * op, int in0_out1, int64_t * num_sect,
                 int * sect_sz)
{
    int k, res;
    unsigned int ui;
    unsigned char rcBuff[RCAP16_REPLY_LEN];
    int verb;
    int sg_fd = (in0_out1 ? op->outfd : op->infd);
    int protect = (in0_out1 ? op->wrprotect : op->rdprotect);

    verb = (op->verbose ? op->verbose - 1: 0);
    memset(rcBuff, 0, sizeof(rcBuff));
    if (! protect) {
        res = sg_ll_readcap_10(sg_fd, 0, 0, rcBuff, READ_CAP_REPLY_LEN, 0,
			       verb);
        if (0 != res)
            return res;
    }

    if (protect || ((0xff == rcBuff[0]) && (0xff == rcBuff[1]) &&
                    (0xff == rcBuff[2]) && (0xff == rcBuff[3]))) {
        int64_t ls;
        int prot_typ, p_i_exp;

        if (verb && ! protect)
            fprintf(stderr, "    READ CAPACITY (10) response cannot "
                    "represent this capacity\n");
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
        if (protect) {
            if (rcBuff[12] & 0x1) {     /* PROT_EN */
                prot_typ = ((rcBuff[12] >> 1) & 0x7) + 1;
                p_i_exp = ((rcBuff[13] >> 4) & 0xf);
                if (in0_out1) {
                    op->wrprot_typ = prot_typ;
                    op->wrp_i_exp = p_i_exp;
                } else {
                    op->rdprot_typ = prot_typ;
                    op->rdp_i_exp = p_i_exp;
                }
            }
        }
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

/* Build a SCSI READ or WRITE CDB. */
static int
pt_build_scsi_cdb(unsigned char * cdbp, int cdb_sz, unsigned int blocks,
                  int64_t start_block, int write_true, int fua, int fua_nv,
                  int dpo, int protect)
{
    int rd_opcode[] = {0x8, 0x28, 0xa8, 0x88};
    int wr_opcode[] = {0xa, 0x2a, 0xaa, 0x8a};
    int sz_ind;

    memset(cdbp, 0, cdb_sz);
    if (cdb_sz > 6) {
        if (dpo)
            cdbp[1] |= 0x10;
        if (fua)
            cdbp[1] |= 0x8;
        if (fua_nv)
            cdbp[1] |= 0x2;
        if (protect)
            cdbp[1] |= ((protect & 0x7) << 5);
    }
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
            fprintf(stderr, "for 6 byte commands, maximum number of "
                            "blocks is 256\n");
            return 1;
        }
        if ((start_block + blocks - 1) & (~0x1fffff)) {
            fprintf(stderr, "for 6 byte commands, can't address blocks"
                            " beyond %d\n", 0x1fffff);
            return 1;
        }
        if (dpo || fua) {
            fprintf(stderr, "for 6 byte commands, neither dpo nor fua"
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
            fprintf(stderr, "for 10 byte commands, maximum number of "
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
        fprintf(stderr, "expected cdb size of 6, 10, 12, or 16 but got"
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
pt_low_read(struct opts_t * op, int in0_out1, unsigned char * buff,
            int blocks, int64_t from_block, int bs,
            uint64_t * io_addrp)
{
    unsigned char rdCmd[MAX_SCSI_CDBSZ];
    unsigned char sense_b[SENSE_BUFF_LEN];
    int res, k, info_valid, slen, sense_cat, ret, vt;
    struct sg_pt_base * ptvp = (in0_out1 ? op->of_ptvp : op->if_ptvp);
    const struct flags_t * fp = (in0_out1 ? op->oflagp : op->iflagp);
    int sg_fd = (in0_out1 ? op->outfd : op->infd);
    int protect = (in0_out1 ? op->wrprotect : op->rdprotect);
    struct sg_scsi_sense_hdr ssh;

    if (pt_build_scsi_cdb(rdCmd, fp->cdbsz, blocks, from_block, 0,
                          fp->fua, fp->fua_nv, fp->dpo, protect)) {
        fprintf(stderr, "bad rd cdb build, from_block=%"PRId64", "
                "blocks=%d\n", from_block, blocks);
        return SG_LIB_SYNTAX_ERROR;
    }
    if (op->verbose > 2) {
        fprintf(stderr, "    READ cdb: ");
        for (k = 0; k < fp->cdbsz; ++k)
            fprintf(stderr, "%02x ", rdCmd[k]);
        fprintf(stderr, "\n");
    }

    if (NULL == ptvp) {
        fprintf(stderr, "pt_low_read: ptvp NULL?\n");
        return -1;
    }
    clear_scsi_pt_obj(ptvp);
    set_scsi_pt_cdb(ptvp, rdCmd, fp->cdbsz);
    set_scsi_pt_sense(ptvp, sense_b, sizeof(sense_b));
    set_scsi_pt_data_in(ptvp, buff, bs * blocks);
#ifdef SCSI_PT_FLAGS_FUNCTION
    set_scsi_pt_flags(ptvp, SCSI_PT_FLAGS_QUEUE_AT_TAIL);
#endif
    vt = (op->verbose ? (op->verbose - 1) : 0);
    while (((res = do_scsi_pt(ptvp, sg_fd, DEF_TIMEOUT, vt)) < 0) &&
           (-EINTR == res))
        ++op->interrupted_retries; /* resubmit if interrupted system call */

    vt = ((op->verbose > 1) ? (op->verbose - 1) : op->verbose);
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
            ++op->unrecovered_errs;
            break;
        case SG_LIB_CAT_UNIT_ATTENTION:
            break;
        case SG_LIB_CAT_ABORTED_COMMAND:
            if (sg_scsi_normalize_sense(sense_b, slen, &ssh) &&
                (0x10 == ssh.asc)) {    /* Protection problem, so no retry */
                ++op->unrecovered_errs;
                info_valid = sg_get_sense_info_fld(sense_b, slen, io_addrp);
                if (info_valid)
                    ret = SG_LIB_CAT_PROTECTION_WITH_INFO;
                else
                    ret = SG_LIB_CAT_PROTECTION;
            }
            break;
        case SG_LIB_CAT_RECOVERED:
            ++op->recovered_errs;
            info_valid = sg_get_sense_info_fld(sense_b, slen, io_addrp);
            if (info_valid)
                fprintf(stderr, "    lba of last recovered error in this "
                        "READ=0x%"PRIx64"\n", *io_addrp);
            else
                fprintf(stderr, "Recovered error: [no info] reading from "
                        "block=0x%"PRIx64", num=%d\n", from_block, blocks);
            break;
        case SG_LIB_CAT_MEDIUM_HARD:
            ++op->unrecovered_errs;
            info_valid = sg_get_sense_info_fld(sense_b, slen, io_addrp);
            /* MMC and MO devices don't necessarily set VALID bit */
            if (info_valid || ((*io_addrp > 0) &&
                               ((5 == fp->pdt) || (7 == fp->pdt))))
                ret = SG_LIB_CAT_MEDIUM_HARD_WITH_INFO; // <<<<<<<<<<<<
            else
                fprintf(stderr, "Medium, hardware or blank check error but "
                        "no lba of failure in sense data\n");
            break;
        case SG_LIB_CAT_NO_SENSE:
            ret = 0;
            break;
        case SG_LIB_CAT_ILLEGAL_REQ:
            if (5 == fp->pdt) {    /* MMC READs can go down this path */
                int ili;

                if (sg_scsi_normalize_sense(sense_b, slen, &ssh) &&
                    (0x64 == ssh.asc) && (0x0 == ssh.ascq)) {
                    if (sg_get_sense_filemark_eom_ili(sense_b, slen, NULL,
                                                      NULL, &ili) && ili) {
                        info_valid = sg_get_sense_info_fld(sense_b, slen,
                                                           io_addrp);
                        if (*io_addrp > 0) {
                            ++op->unrecovered_errs;
                            ret = SG_LIB_CAT_MEDIUM_HARD_WITH_INFO;
                        } else
                            fprintf(stderr, "MMC READ gave 'illegal mode for "
                                    "this track' and ILI but no LBA of "
                                    "failure\n");
                    }
                    ++op->unrecovered_errs;
                    ret = SG_LIB_CAT_MEDIUM_HARD;
                }
            }
        default:
            break;
        }
    } else
        ret = 0;

    /* We are going to re-read those good blocks */
    if ((SG_LIB_CAT_MEDIUM_HARD_WITH_INFO != ret) &&
        (SG_LIB_CAT_PROTECTION_WITH_INFO != ret))
        op->sum_of_resids += get_scsi_pt_resid(ptvp);
    return ret;
}

/* Control pass-through read retries and coe (continue on error).
 * Fast path is a call to pt_low_read() that succeeds. If medium
 * error then back up and re-read good blocks prior to bad block;
 * then, if coe, use zero for bad block and continue reading at
 * the next LBA (N.B. more medium errors could occur).
 * 0 -> successful, SG_LIB_SYNTAX_ERROR -> unable to build cdb,
 * SG_LIB_CAT_UNIT_ATTENTION -> try again, SG_LIB_CAT_NOT_READY,
 * SG_LIB_CAT_MEDIUM_HARD, SG_LIB_CAT_ABORTED_COMMAND,
 * -2 -> ENOMEM, -1 other errors */
int
pt_read(struct opts_t * op, int in0_out1, unsigned char * buff, int blocks,
        int * blks_readp)
{
    uint64_t io_addr;
    int64_t from_block;
    int64_t lba;
    int bs;
    struct flags_t * fp;
    int res, blks, use_io_addr, xferred, pi_len;
    unsigned char * bp;
    int retries_tmp;
    int ret = 0;
    int may_coe = 0;

    if (in0_out1) {
        from_block = op->seek;
        bs = op->obs_pi;
        pi_len = op->obs_pi - op->obs;
        fp = op->oflagp;
    } else {
        from_block = op->skip;
        bs = op->ibs_pi;
        pi_len = op->ibs_pi - op->ibs;
        fp = op->iflagp;
    }
    retries_tmp = fp->retries;
    for (xferred = 0, blks = blocks, lba = from_block, bp = buff;
         blks > 0; blks = blocks - xferred) {
        io_addr = 0;
        use_io_addr = 0;
        may_coe = 0;
        res = pt_low_read(op, in0_out1, bp, blks, lba, bs, &io_addr);
        switch (res) {
        case 0:         /* this is the fast path after good pt_low_read() */
            if (blks_readp)
                *blks_readp = xferred + blks;
            if (0 == in0_out1)
                zero_coe_limit_count(op);
            return 0;
        case -2:        /* ENOMEM */
            return res;
        case SG_LIB_CAT_NOT_READY:
            fprintf(stderr, "Device (r) not ready\n");
            return res;
        case SG_LIB_CAT_ABORTED_COMMAND:
            if (--op->max_aborted > 0)
                fprintf(stderr, "Aborted command, continuing (r)\n");
            else {
                fprintf(stderr, "Aborted command, too many (r)\n");
                return res;
            }
            break;
        case SG_LIB_CAT_UNIT_ATTENTION:
            if (--op->max_uas > 0)
                fprintf(stderr, "Unit attention, continuing (r)\n");
            else {
                fprintf(stderr, "Unit attention, too many (r)\n");
                return res;
            }
            break;
        case SG_LIB_CAT_PROTECTION_WITH_INFO:
            use_io_addr = 1;
            ret = res;
            break; /* unrecovered read error at lba=io_addr */
        case SG_LIB_CAT_MEDIUM_HARD_WITH_INFO:
            if (retries_tmp > 0) {
                fprintf(stderr, ">>> retrying pt read: starting "
                        "lba=%"PRId64" [0x%"PRIx64"] blocks=%d\n", lba,
                        (uint64_t)lba, blks);
                --retries_tmp;
                ++op->num_retries;
                if (op->unrecovered_errs > 0)
                    --op->unrecovered_errs;
            } else
                use_io_addr = 1;
            ret = SG_LIB_CAT_MEDIUM_HARD;
            break; /* unrecovered read error at lba=io_addr */
        case SG_LIB_SYNTAX_ERROR:
            fp->coe = 0;
            ret = res;
            goto err_out;
        case -1:
            ret = res;
            goto err_out;
        case SG_LIB_CAT_MEDIUM_HARD:
            may_coe = 1;
            /* No VALID+INFO field but we know the range of lba_s */
            if (0 == retries_tmp)
                put_range_errblk(lba, blks, op);
            /* fall through */
        default:
            if (retries_tmp > 0) {
                fprintf(stderr, ">>> retrying pt read: starting "
                        "lba=%"PRId64" [0x%"PRIx64"] blocks=%d\n", lba,
                        (uint64_t)lba, blks);
                --retries_tmp;
                ++op->num_retries;
                if (op->unrecovered_errs > 0)
                    --op->unrecovered_errs;
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
        if (op->highest_unrecovered < 0) {
            op->highest_unrecovered = io_addr;
            op->lowest_unrecovered = io_addr;
        } else {
            if ((int64_t)io_addr < op->lowest_unrecovered)
                op->lowest_unrecovered = io_addr;
            if ((int64_t)io_addr > op->highest_unrecovered)
                op->highest_unrecovered = io_addr;
        }
        put_errblk(io_addr, op);
        if (fp->coe) {
            ++op->in_partial;
            --op->in_full;
        }
        blks = (int)(io_addr - (uint64_t)lba);
        if (blks > 0) {
            if (op->verbose)
                fprintf(stderr, "  partial re-read of %d blocks prior to "
                        "medium error\n", blks);
            res = pt_low_read(op, in0_out1, bp, blks, lba, bs, &io_addr);
            switch (res) {
            case 0:
                break;
            case -1:
                fp->coe = 0;
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
            case SG_LIB_CAT_PROTECTION_WITH_INFO:
            case SG_LIB_CAT_PROTECTION:
                ret = SG_LIB_CAT_PROTECTION;
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
        if (0 == fp->coe) {
            /* give up at block before problem unless 'coe' */
            if (blks_readp)
                *blks_readp = xferred;
            return ret;
        }
        bp += (blks * bs);
        lba += blks;
        fprintf(stderr, ">> unrecovered read error at blk=%"PRId64", "
                "substitute zeros%s\n", lba,
                ((pi_len > 0) ? " (PI with 0xFFs)" : ""));
        if (pi_len > 0) {
            memset(bp, 0, bs - pi_len);
            memset(bp + bs - pi_len, 0xff, pi_len);
        } else
            memset(bp, 0, bs);
        ++xferred;
        bp += bs;
        ++lba;
        if ((op->coe_limit > 0) && (++op->coe_count > op->coe_limit)) {
            if (blks_readp)
                *blks_readp = xferred + blks;
            fprintf(stderr, ">> coe_limit on consecutive reads exceeded\n");
            return SG_LIB_CAT_MEDIUM_HARD;
        }
        retries_tmp = fp->retries;
    }
    if (blks_readp)
        *blks_readp = xferred;
    return 0;

err_out:
    if (fp->coe) {
        memset(bp, 0, bs * blks);
        fprintf(stderr, ">> unable to read at blk=%"PRId64" for "
                "%d bytes, use zeros\n", lba, bs * blks);
        if (blks > 1)
            fprintf(stderr, ">>   try reducing bpt to limit number "
                    "of zeros written near bad block(s)\n");
        /* fudge success */
        if (blks_readp)
            *blks_readp = xferred + blks;
        if ((op->coe_limit > 0) && (++op->coe_count > op->coe_limit)) {
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
pt_low_write(struct opts_t * op, unsigned char * buff, int blocks,
             int64_t to_block, int bs)
{
    unsigned char wrCmd[MAX_SCSI_CDBSZ];
    unsigned char sense_b[SENSE_BUFF_LEN];
    int res, k, info_valid, ret, sense_cat, slen, vt;
    int sg_fd = op->outfd;
    uint64_t io_addr = 0;
    struct sg_pt_base * ptvp = op->of_ptvp;
    const struct flags_t * ofp = op->oflagp;
    struct sg_scsi_sense_hdr ssh;

    if (pt_build_scsi_cdb(wrCmd, ofp->cdbsz, blocks, to_block, 1, ofp->fua,
                          ofp->fua_nv, ofp->dpo, op->wrprotect)) {
        fprintf(stderr, "bad wr cdb build, to_block=%"PRId64", blocks=%d\n",
                to_block, blocks);
        return SG_LIB_SYNTAX_ERROR;
    }
    if (op->verbose > 2) {
        fprintf(stderr, "    WRITE cdb: ");
        for (k = 0; k < ofp->cdbsz; ++k)
            fprintf(stderr, "%02x ", wrCmd[k]);
        fprintf(stderr, "\n");
    }

    if (NULL == ptvp) {
        fprintf(stderr, "pt_low_write: of_ptvp NULL?\n");
        return -1;
    }
    clear_scsi_pt_obj(ptvp);
    set_scsi_pt_cdb(ptvp, wrCmd, ofp->cdbsz);
    set_scsi_pt_sense(ptvp, sense_b, sizeof(sense_b));
    set_scsi_pt_data_out(ptvp, buff, bs * blocks);
    vt = (op->verbose ? (op->verbose - 1) : 0);
    while (((res = do_scsi_pt(ptvp, sg_fd, DEF_TIMEOUT, vt)) < 0) &&
           (-EINTR == res))
        ++op->interrupted_retries; /* resubmit if interrupted system call */

    vt = ((op->verbose > 1) ? (op->verbose - 1) : op->verbose);
    ret = sg_cmds_process_resp(ptvp, "WRITE", res, bs * blocks, sense_b,
                               0 /* noisy */, vt, &sense_cat);
    if (-1 == ret)
        ;
    else if (-2 == ret) {
        slen = get_scsi_pt_sense_len(ptvp);
        ret = sense_cat;

        switch (sense_cat) {
        case SG_LIB_CAT_RECOVERED:
            ++op->wr_recovered_errs;
            info_valid = sg_get_sense_info_fld(sense_b, slen, &io_addr);
            if (info_valid)
                fprintf(stderr, "    lba of last recovered error in this "
                        "WRITE=0x%"PRIx64"\n", io_addr);
            else
                fprintf(stderr, "Recovered error: [no info] writing to "
                        "block=0x%"PRIx64", num=%d\n", to_block, blocks);
            break;
        case SG_LIB_CAT_ABORTED_COMMAND:
            if (sg_scsi_normalize_sense(sense_b, slen, &ssh) &&
                (0x10 == ssh.asc)) {    /* Protection problem, so no retry */
                ++op->wr_unrecovered_errs;
                ret = SG_LIB_CAT_PROTECTION;
            }
            break;
        case SG_LIB_CAT_UNIT_ATTENTION:
            break;
        case SG_LIB_CAT_NOT_READY:
        case SG_LIB_CAT_ILLEGAL_REQ:
        case SG_LIB_CAT_INVALID_OP:
        case SG_LIB_CAT_SENSE:
            ++op->wr_unrecovered_errs;
            break;
        case SG_LIB_CAT_MEDIUM_HARD:
        default:
            ++op->wr_unrecovered_errs;
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
int
pt_write(struct opts_t * op, unsigned char * buff, int blocks,
         int64_t to_block)
{
    int retries_tmp;
    int first = 1;
    int ret = 0;
    int bs = op->obs_pi;

    retries_tmp = op->oflagp->retries;
    while (1) {
        ret = pt_low_write(op, buff, blocks, to_block, bs);
        if (0 == ret)
            break;
        if ((SG_LIB_CAT_NOT_READY == ret) ||
            (SG_LIB_SYNTAX_ERROR == ret))
            break;
        else if ((SG_LIB_CAT_UNIT_ATTENTION == ret) && first) {
            if (--op->max_uas > 0)
                fprintf(stderr, "Unit attention, continuing (w)\n");
            else {
                fprintf(stderr, "Unit attention, too many (w)\n");
                break;
            }
        } else if ((SG_LIB_CAT_ABORTED_COMMAND == ret) && first) {
            if (--op->max_aborted > 0)
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
            ++op->num_retries;
            if (op->wr_unrecovered_errs > 0)
                --op->wr_unrecovered_errs;
        } else
            break;
        first = 0;
    }
    return ret;
}

/* This function performs a "trim" on a pt device. In the SCSI command set
 * this is either done with the UNMAP command or WRITE SAME command. This
 * function uses WRITE SAME(16) with the unmap bit set. In Linux libata
 * translates this to the ATA DATA SET MANAGEMENT command with the trim
 * field set. Returns 0 on success. */
int
pt_write_same16(struct opts_t * op, unsigned char * buff, int bs, int blocks,
                int64_t start_block)
{
    int k, ret, res, sense_cat, vt;
    uint64_t llba;
    uint32_t unum;
    unsigned char wsCmdBlk[16];
    unsigned char sense_b[SENSE_BUFF_LEN];
    struct sg_pt_base * ptvp = op->of_ptvp;
    int sg_fd = op->outfd;

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
    if (op->verbose > 2) {
        fprintf(stderr, "    WRITE SAME(16) cdb: ");
        for (k = 0; k < (int)sizeof(wsCmdBlk); ++k)
            fprintf(stderr, "%02x ", wsCmdBlk[k]);
        fprintf(stderr, "\n");
        if (op->verbose > 4)
            fprintf(stderr, "    Data-out buffer length=%d\n",
                    bs);
    }

    if (NULL == ptvp) {
        fprintf(stderr, "pt_write_same16: ptvp NULL?\n");
        return -1;
    }
    clear_scsi_pt_obj(ptvp);
    set_scsi_pt_cdb(ptvp, wsCmdBlk, sizeof(wsCmdBlk));
    set_scsi_pt_sense(ptvp, sense_b, sizeof(sense_b));
    set_scsi_pt_data_out(ptvp, buff, bs);
    vt = ((op->verbose > 1) ? (op->verbose - 1) : 0);
    while (((res = do_scsi_pt(ptvp, sg_fd, WRITE_SAME16_TIMEOUT, vt)) < 0) &&
           (-EINTR == res))
        ++op->interrupted_retries; /* resubmit if interrupted system call */
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

void
pt_sync_cache(int fd)
{
    int res;

    res = sg_ll_sync_cache_10(fd, 0, 0, 0, 0, 0, 0, 0);
    if (SG_LIB_CAT_UNIT_ATTENTION == res) {
        fprintf(stderr, "Unit attention (out, sync cache), "
                "continuing\n");
        res = sg_ll_sync_cache_10(fd, 0, 0, 0, 0, 0, 0, 0);
    }
    if (0 != res)
        fprintf(stderr, "Unable to do SCSI synchronize cache\n");
}
