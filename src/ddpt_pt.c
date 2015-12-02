/*
 * Copyright (c) 2008-2015 Douglas Gilbert.
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

#include "ddpt.h"       /* includes <signal.h> */

#include "sg_lib.h"
#include "sg_cmds_basic.h"
#include "sg_cmds_extra.h"
#include "sg_pt.h"
#include "sg_unaligned.h"

#define DDPT_READ6_OC 0x8
#define DDPT_READ10_OC 0x28
#define DDPT_READ12_OC 0xa8
#define DDPT_READ16_OC 0x88
#define DDPT_WRITE6_OC 0xa
#define DDPT_WRITE10_OC 0x2a
#define DDPT_WRITE12_OC 0xaa
#define DDPT_WRITE16_OC 0x8a
#define DDPT_WRITE_ATOMIC16_OC 0x9c     /* sbc4r02 */
#define DDPT_WRITE_VERIFY10_OC 0x2e
#define DDPT_WRITE_VERIFY16_OC 0x8e
#define DDPT_VARIABLE_LEN_OC 0x7f
#define DDPT_READ32_SA 0x9
#define DDPT_WRITE32_SA 0xb
#define DDPT_TPC_OUT_CMD 0x83
#define DDPT_TPC_OUT_CMDLEN 16
#define DDPT_TPC_IN_CMD 0x84
#define DDPT_TPC_IN_CMDLEN 16

#define ASC_GENERAL_0 0x0
#define ASQ_OP_IN_PROGRESS 0x16

#define ASC_3PC_GEN 0xd
#define ASQ_TARGET_UNDERRUN 0x4
#define ASQ_TARGET_OVERRUN 0x5

#define ASC_PARAM_LST_LEN_ERR 0x1a      /* only asq=00 defined */

#define ASC_INVALID_TOKOP 0x23
#define ASQ_TOKOP_UNK 0x0
/* There are 11 of these ASQs, just add asq to DDPT_CAT base */

#define ASC_INVALID_PARAM 0x26
#define ASQ_INVALID_FLD_IN_PARAM 0x0
#define ASQ_TOO_MANY_SEGS_IN_PARAM 0x8

#define ASC_CMDS_CLEARED 0x2f
#define ASQ_CMDS_CLEARED_BY_DEV_SVR 0x2

#define ASC_INSUFF_RES 0x55
#define ASQ_INSUFF_RES_CREATE_ROD 0xc
#define ASQ_INSUFF_RES_CREATE_RODTOK 0xd

#define DEF_PT_TIMEOUT 60       /* 60 seconds */


void *
pt_construct_obj(void)
{
    void * vp;

    if (NULL == (vp = construct_scsi_pt_obj()))
        pr2serr("construct_scsi_pt_obj: out of memory\n");
    return vp;
}

void
pt_destruct_obj(void * vp)
{
    destruct_scsi_pt_obj((struct sg_pt_base *)vp);
}

/* Opens given device with the pass-through interface. If successful sends
 * a standard INQUIRY request. Returns file descriptor (>=0) if successful,
 * -1 if open fails, -2 if standard INQUIRY fails. */
int
pt_open_if(struct opts_t * op, struct sg_simple_inquiry_resp * sirp)
{
    int verb, flags, fl, fd;
    struct sg_simple_inquiry_resp sir;
    struct flags_t * fp = op->iflagp;
    struct dev_info_t * dip = op->idip;
    const char * fn = op->idip->fn;

    verb = (op->verbose ? op->verbose - 1: 0);
    flags = fp->block ? 0 : O_NONBLOCK;
    if (fp->direct)
        flags |= O_DIRECT;
    if (fp->excl)
        flags |= O_EXCL;
    if (fp->sync)
        flags |= O_SYNC;
    fl = op->o_readonly ? O_RDONLY : O_RDWR;
    if ((fd = scsi_pt_open_flags(fn, (fl | flags), op->verbose)) < 0) {
        if (-EBUSY == fd) {
            pr2serr("open %s for pass-through reports BUSY,\n"
                    "  use iflag=block to wait until ready\n", fn);
            return -1;
        }
        if (op->o_readonly) {
            pr2serr("could not open %s ro as pass-through: %s\n", fn,
                        safe_strerror(-fd));
            return -1;
        } else {
            fl = O_RDONLY;
            if (op->verbose)
                pr2serr("could not open %s read-write so try read-only\n",
                        fn);
            if ((fd = scsi_pt_open_flags(fn, (fl | flags), op->verbose))
                < 0) {
                pr2serr("could not open %s [rw or ro] as pass-through: %s\n",
                        fn, safe_strerror(-fd));
                return -1;
            }
        }
    }
    if (sg_simple_inquiry(fd, &sir, 0, verb)) {
        pr2serr("INQUIRY failed on %s\n", fn);
        return -2;
    }
    dip->pdt = sir.peripheral_type;
    if (op->verbose) {
        if (op->has_xcopy || op->has_odx)
            pr2serr("    %s: %.8s  %.16s  %.4s  [pdt=%d, 3pc=%d]\n",
                    fn, sir.vendor, sir.product, sir.revision, dip->pdt,
                    !! (0x8 & sir.byte_5));
        else
            pr2serr("    %s: %.8s  %.16s  %.4s  [pdt=%d]\n",
                    fn, sir.vendor, sir.product, sir.revision, dip->pdt);
    }
    if (sirp)
        *sirp = sir;
    return fd;
}

/* Opens given device with the pass-through interface. If successful sends
 * a standard INQUIRY request. Returns file descriptor (>=0) if successful,
 * -1 if open fails, -2 if standard INQUIRY fails. */
int
pt_open_of(struct opts_t * op, struct sg_simple_inquiry_resp * sirp)
{
    int verb, flags, fd;
    struct sg_simple_inquiry_resp sir;
    struct flags_t * fp = op->oflagp;
    struct dev_info_t * dip = op->odip;
    const char * fn = dip->fn;

    verb = (op->verbose ? op->verbose - 1: 0);
    flags = fp->block ? 0 : O_NONBLOCK;
    flags |= O_RDWR;
    if (fp->direct)
        flags |= O_DIRECT;
    if (fp->excl)
        flags |= O_EXCL;
    if (fp->sync)
        flags |= O_SYNC;
    if ((fd = scsi_pt_open_flags(fn, flags, op->verbose)) < 0) {
        if (-EBUSY == fd) {
            pr2serr("open %s for pass-through reports BUSY,\n"
                    "  use oflag=block to wait until ready\n", fn);
            return -1;
        }
        pr2serr("could not open %s for pass-through: %s\n", fn,
                safe_strerror(-fd));
        return -1;
    }
    if (sg_simple_inquiry(fd, &sir, 0, verb)) {
        pr2serr("INQUIRY failed on %s\n", fn);
        return -2;
    }
    dip->pdt = sir.peripheral_type;
    if (op->verbose) {
        if (op->has_xcopy || op->has_odx)
            pr2serr("    %s: %.8s  %.16s  %.4s  [pdt=%d, 3pc=%d]\n",
                    fn, sir.vendor, sir.product, sir.revision, dip->pdt,
                    !! (0x8 & sir.byte_5));
        else
            pr2serr("    %s: %.8s  %.16s  %.4s  [pdt=%d]\n",
                    fn, sir.vendor, sir.product, sir.revision, dip->pdt);
    }
    if (sirp)
        *sirp = sir;
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
pt_read_capacity(struct opts_t * op, int in0_out1, int64_t * num_blks,
                 int * blk_sz)
{
    int res;
    unsigned int ui;
    unsigned char rcBuff[RCAP16_REPLY_LEN];
    int verb;
    int sg_fd = (in0_out1 ? op->odip->fd : op->idip->fd);
    int protect = (in0_out1 ? op->wrprotect : op->rdprotect);

    verb = (op->verbose ? op->verbose - 1: 0);
    memset(rcBuff, 0, sizeof(rcBuff));
    if (! protect) {
        res = sg_ll_readcap_10(sg_fd, 0, 0, rcBuff, READ_CAP_REPLY_LEN, 1,
                               verb);
        if (0 != res)
            return res;
    }

    if (protect || ((0xff == rcBuff[0]) && (0xff == rcBuff[1]) &&
                    (0xff == rcBuff[2]) && (0xff == rcBuff[3]))) {
        int64_t ls;
        int prot_typ = 0;
        int p_i_exp = 0;

        if (verb && ! protect)
            pr2serr("    READ CAPACITY (10) response cannot represent this "
                    "capacity\n");
        res = sg_ll_readcap_16(sg_fd, 0, 0, rcBuff, RCAP16_REPLY_LEN, 1,
                               verb);
        if (0 != res)
            return res;
        ls = sg_get_unaligned_be64(rcBuff + 0);
        *num_blks = ls + 1;
        *blk_sz = sg_get_unaligned_be32(rcBuff + 8);
        if (rcBuff[12] & 0x1) {         /* PROT_EN */
            prot_typ = ((rcBuff[12] >> 1) & 0x7) + 1;
            p_i_exp = ((rcBuff[13] >> 4) & 0xf);
        }
        if (in0_out1) {
            op->odip->prot_type = prot_typ;
            op->odip->p_i_exp = p_i_exp;
        } else {
            op->idip->prot_type = prot_typ;
            op->idip->p_i_exp = p_i_exp;
        }
    } else {
        ui = sg_get_unaligned_be32(rcBuff + 0);
        /* take care not to sign extend values > 0x7fffffff */
        *num_blks = (int64_t)ui + 1;
        *blk_sz = sg_get_unaligned_be32(rcBuff + 4);
    }
    return 0;
}

/* Build a SCSI READ or WRITE CDB. */
static int
pt_build_scsi_cdb(unsigned char * cdbp, int cdb_sz, unsigned int blocks,
                  int64_t start_block, int write_true,
                  const struct flags_t * fp, int protect)
{
    int rd_opcode[] = {DDPT_READ6_OC, DDPT_READ10_OC, DDPT_READ12_OC,
                       DDPT_READ16_OC, DDPT_READ32_SA};
    int wr_opcode[] = {DDPT_WRITE6_OC, DDPT_WRITE10_OC, DDPT_WRITE12_OC,
                       DDPT_WRITE16_OC, DDPT_WRITE32_SA};
    int opcode_sa, options_byte, rw_sa;
    int * opc_arr;

    if (cdb_sz < 6) {
        pr2serr("cdb_sz too small\n");
        return 1;
    }
    memset(cdbp, 0, cdb_sz);
    opcode_sa = 0;
    options_byte = 0;
    opc_arr = NULL;

    if (write_true) {
        if (fp->atomic) {
            if (16 == cdb_sz)
                opcode_sa = DDPT_WRITE_ATOMIC16_OC;
            else {
                pr2serr("atomic flag only for WRITE_ATOMIC(16)\n");
                return 1;
            }
        } else if (fp->verify) {
            if (10 == cdb_sz)
                opcode_sa = DDPT_WRITE_VERIFY10_OC;
            else if (16 == cdb_sz)
                opcode_sa = DDPT_WRITE_VERIFY16_OC;
            else {
                pr2serr("verify flag for WRITE AND VERIFY (10 or 16) only\n");
                return 1;
            }
        } else
            opc_arr = wr_opcode;
    } else {    /* a READ */
        opc_arr = rd_opcode;
        if (fp->rarc)
            options_byte |= 0x4;
    }

    if (cdb_sz > 6) {
        if (fp->dpo)
            options_byte |= 0x10;
        if (fp->verify && write_true) {
            if (fp->bytchk)
                options_byte |= ((fp->bytchk & 0x3) << 1);
        } else {
            if (fp->fua)
                options_byte |= 0x8;
            if (fp->fua_nv)
                options_byte |= 0x2;
        }
        if (protect)
            options_byte |= ((protect & 0x7) << 5);
    }

    switch (cdb_sz) {
    case 6:
        if (0 == opcode_sa)
            opcode_sa = opc_arr[0];
        cdbp[0] = (unsigned char)opcode_sa;
        sg_put_unaligned_be24((uint32_t)(0x1fffff & start_block), cdbp + 1);
        cdbp[4] = (256 == blocks) ? 0 : (unsigned char)blocks;
        if (blocks > 256) {
            pr2serr("for 6 byte commands, maximum number of blocks is 256\n");
            return 1;
        }
        if ((start_block + blocks - 1) & (~0x1fffff)) {
            pr2serr("for 6 byte commands, can't address blocks beyond %d\n",
                    0x1fffff);
            return 1;
        }
        if (fp->dpo || fp->fua || fp->rarc) {
            pr2serr("for 6 byte commands, neither dpo, fua, nor rarc bits "
                    "supported\n");
            return 1;
        }
        break;
    case 10:
        if (0 == opcode_sa)
            opcode_sa = opc_arr[1];
        cdbp[0] = (unsigned char)opcode_sa;
        cdbp[1] = (unsigned char)options_byte;
        sg_put_unaligned_be32((uint32_t)start_block, cdbp + 2);
        sg_put_unaligned_be16((uint16_t)blocks, cdbp + 7);
        if (blocks & (~0xffff)) {
            pr2serr("for 10 byte commands, maximum number of blocks is %d\n",
                    0xffff);
            return 1;
        }
        break;
    case 12:
        if (0 == opcode_sa)
            opcode_sa = opc_arr[2];
        cdbp[0] = (unsigned char)opcode_sa;
        cdbp[1] = (unsigned char)options_byte;
        sg_put_unaligned_be32((uint32_t)start_block, cdbp + 2);
        sg_put_unaligned_be32((uint32_t)blocks, cdbp + 6);
        break;
    case 16:
        if (0 == opcode_sa)
            opcode_sa = opc_arr[3];
        cdbp[0] = (unsigned char)opcode_sa;
        cdbp[1] = (unsigned char)options_byte;
        sg_put_unaligned_be64(start_block, cdbp + 2);
        sg_put_unaligned_be32((uint32_t)blocks, cdbp + 10);
        break;
    case 32:
        if (0 == opcode_sa)
            opcode_sa = opc_arr[4];
        cdbp[0] = (unsigned char)DDPT_VARIABLE_LEN_OC;
        cdbp[7] = (unsigned char)0x18;  /* additional length=>32 byte cdb */
        rw_sa = opcode_sa;
        sg_put_unaligned_be16((uint16_t)rw_sa, cdbp + 8);
        cdbp[10] = (unsigned char)options_byte;
        sg_put_unaligned_be64(start_block, cdbp + 12);
        sg_put_unaligned_be32((uint32_t)blocks, cdbp + 28);
        break;
    default:
        pr2serr("expected cdb size of 6, 10, 12, 16 or 32 but got %d\n",
                cdb_sz);
        return 1;
    }
    return 0;
}

/* Read using the pass-through. No retries or remedial work here.
 * 0 -> successful, SG_LIB_SYNTAX_ERROR -> unable to build cdb,
 * SG_LIB_CAT_MEDIUM_HARD_WITH_INFO -> 'io_addrp' written to,
 * SG_LIB_CAT_MEDIUM_HARD -> no info field,
 * plus various other SG_LIB_CAT_* positive values,
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
    struct sg_pt_base * ptvp = (in0_out1 ? op->odip->ptvp : op->idip->ptvp);
    const struct flags_t * fp = (in0_out1 ? op->oflagp : op->iflagp);
    const struct dev_info_t * dip = (in0_out1 ? op->odip : op->idip);
    int protect = (in0_out1 ? op->wrprotect : op->rdprotect);
    struct sg_scsi_sense_hdr ssh;

    if (pt_build_scsi_cdb(rdCmd, fp->cdbsz, blocks, from_block, 0,
                          fp, protect)) {
        pr2serr("bad rd cdb build, from_block=%" PRId64 ", blocks=%d\n",
                from_block, blocks);
        return SG_LIB_SYNTAX_ERROR;
    }
    if (op->verbose > 2) {
        pr2serr("    READ cdb: ");
        for (k = 0; k < fp->cdbsz; ++k)
            pr2serr("%02x ", rdCmd[k]);
        pr2serr("\n");
    }

    if (NULL == ptvp) {
        pr2serr("pt_low_read: ptvp NULL?\n");
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
    while (((res = do_scsi_pt(ptvp, dip->fd, DEF_RW_TIMEOUT, vt)) < 0) &&
           ((-EINTR == res) || (-EAGAIN == res))) {
        /* resubmit in these cases */
        if (-EINTR == res)
            ++op->interrupted_retries;
        else
            ++op->io_eagains;
    }

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
        case SG_LIB_CAT_RES_CONFLICT:
        case SG_LIB_CAT_DATA_PROTECT:
        case SG_LIB_CAT_ABORTED_COMMAND:
            ++op->unrecovered_errs;
            break;
        case SG_LIB_CAT_UNIT_ATTENTION:
            break;
        case SG_LIB_CAT_PROTECTION:
            /* no retry, might have INFO field */
            ++op->unrecovered_errs;
            info_valid = sg_get_sense_info_fld(sense_b, slen, io_addrp);
            if (info_valid)
                ret = SG_LIB_CAT_PROTECTION_WITH_INFO;
            break;
        case SG_LIB_CAT_RECOVERED:
            ++op->recovered_errs;
            info_valid = sg_get_sense_info_fld(sense_b, slen, io_addrp);
            if (info_valid)
                pr2serr("    lba of last recovered error in this READ=0x%"
                        PRIx64 "\n", *io_addrp);
            else
                pr2serr("Recovered error: [no info] reading from block=0x%"
                        PRIx64 ", num=%d\n", from_block, blocks);
            break;
        case SG_LIB_CAT_MEDIUM_HARD:
            ++op->unrecovered_errs;
            info_valid = sg_get_sense_info_fld(sense_b, slen, io_addrp);
            /* MMC and MO devices don't necessarily set VALID bit */
            if (info_valid || ((*io_addrp > 0) &&
                               ((5 == dip->pdt) || (7 == dip->pdt))))
                ret = SG_LIB_CAT_MEDIUM_HARD_WITH_INFO; // <<<<<<<<<<<<
            else
                pr2serr("Medium, hardware or blank check error but no lba "
                        "of failure in sense data\n");
            break;
        case SG_LIB_CAT_NO_SENSE:
            ret = 0;
            break;
        case SG_LIB_CAT_ILLEGAL_REQ:
            if (5 == dip->pdt) {    /* MMC READs can go down this path */
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
                            pr2serr("MMC READ gave 'illegal mode for this "
                                    "track' and ILI but no LBA of failure\n");
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
    const char * iop;

    if (in0_out1) {
        from_block = op->seek;
        bs = op->obs_pi;
        pi_len = op->obs_pi - op->obs;
        fp = op->oflagp;
        iop = "ofile";
    } else {
        from_block = op->skip;
        bs = op->ibs_pi;
        pi_len = op->ibs_pi - op->ibs;
        fp = op->iflagp;
        iop = "ifile";
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
            pr2serr("%s: Device not ready [%s]\n", __func__, iop);
            return res;
        case SG_LIB_CAT_ABORTED_COMMAND:
            if (--op->max_aborted > 0)
                pr2serr("%s: Aborted command, continuing [%s]\n", __func__,
                        iop);
            else {
                pr2serr("%s: Aborted command, too many [%s]\n", __func__,
                        iop);
                return res;
            }
            break;
        case SG_LIB_CAT_UNIT_ATTENTION:
            if (--op->max_uas > 0)
                pr2serr("%s: Unit attention, continuing [%s]\n", __func__,
                        iop);
            else {
                pr2serr("%s: Unit attention, too many [%s]\n", __func__,
                        iop);
                return res;
            }
            break;
        case SG_LIB_CAT_PROTECTION_WITH_INFO:
            use_io_addr = 1;
            ret = res;
            break; /* unrecovered read error at lba=io_addr */
        case SG_LIB_CAT_MEDIUM_HARD_WITH_INFO:
            if (retries_tmp > 0) {
                pr2serr(">>> retrying pt read: starting lba=%" PRId64 " [0x%"
                        PRIx64 "] blocks=%d\n", lba, (uint64_t)lba, blks);
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
                errblk_put_range(lba, blks, op);
            /* fall through */
        case SG_LIB_CAT_RES_CONFLICT:
        case SG_LIB_CAT_DATA_PROTECT:
        case SG_LIB_CAT_PROTECTION:
        default:
            if (retries_tmp > 0) {
                pr2serr(">>> retrying pt read: starting lba=%" PRId64 " [0x%"
                        PRIx64 "] blocks=%d\n", lba, (uint64_t)lba, blks);
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
                pr2serr("  Unrecovered error lba 0x%" PRIx64 " not in "
                        "correct range:\n\t[0x%" PRIx64 ",0x%" PRIx64 "]\n",
                        io_addr, (uint64_t)lba, (uint64_t)(lba + blks - 1));
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
        errblk_put(io_addr, op);
        if (fp->coe) {
            ++op->in_partial;
            --op->in_full;
        }
        blks = (int)(io_addr - (uint64_t)lba);
        if (blks > 0) {
            if (op->verbose)
                pr2serr("  partial re-read of %d blocks prior to medium "
                        "error\n", blks);
            res = pt_low_read(op, in0_out1, bp, blks, lba, bs, &io_addr);
            switch (res) {
            case 0:
                break;
            case -1:
                fp->coe = 0;
                ret = res;
                goto err_out;
            case -2:
                pr2serr("%s: ENOMEM again, unexpected [%s]\n", __func__, iop);
                return -1;
            case SG_LIB_CAT_NOT_READY:
                pr2serr("%s: device not ready [%s]\n", __func__, iop);
                return res;
            case SG_LIB_CAT_UNIT_ATTENTION:
                pr2serr("%s: Unit attention, unexpected [%s]\n", __func__,
                        iop);
                return res;
            case SG_LIB_CAT_ABORTED_COMMAND:
                pr2serr("%s: Aborted command, unexpected [%s]\n", __func__,
                        iop);
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
                pr2serr(">> unexpected result=%d from pt_low_read() 2 [%s]\n",
                        res, iop);
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
        pr2serr(">> unrecovered read error at blk=%" PRId64 ", substitute "
                "zeros%s\n", lba, ((pi_len > 0) ? " (PI with 0xFFs)" : ""));
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
            pr2serr(">> coe_limit on consecutive reads exceeded\n");
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
        pr2serr(">> unable to read at blk=%" PRId64 " for %d bytes, use "
                "zeros\n", lba, bs * blks);
        if (blks > 1)
            pr2serr(">>   try reducing bpt to limit number of zeros written "
                    "near bad block(s)\n");
        /* fudge success */
        if (blks_readp)
            *blks_readp = xferred + blks;
        if ((op->coe_limit > 0) && (++op->coe_count > op->coe_limit)) {
            pr2serr(">> coe_limit on consecutive reads exceeded\n");
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
pt_low_write(struct opts_t * op, const unsigned char * buff, int blocks,
             int64_t to_block, int bs)
{
    unsigned char wrCmd[MAX_SCSI_CDBSZ];
    unsigned char sense_b[SENSE_BUFF_LEN];
    int res, k, info_valid, ret, sense_cat, slen, vt;
    int sg_fd = op->odip->fd;
    uint64_t io_addr = 0;
    struct sg_pt_base * ptvp = op->odip->ptvp;
    const struct flags_t * fp = op->oflagp;
    const char * desc;

    if (pt_build_scsi_cdb(wrCmd, fp->cdbsz, blocks, to_block, 1, fp,
                          op->wrprotect)) {
        pr2serr("bad wr cdb build, to_block=%" PRId64 ", blocks=%d\n",
                to_block, blocks);
        return SG_LIB_SYNTAX_ERROR;
    }
    if (DDPT_WRITE_ATOMIC16_OC == wrCmd[0])
        desc = "WRITE ATOMIC(16)";
    else if (0xe == (0xf & wrCmd[0]))
        desc = "WRITE AND VERIFY";
    else
        desc = "WRITE";
    if (op->verbose > 2) {
        pr2serr("    %s cdb: ", desc);
        for (k = 0; k < fp->cdbsz; ++k)
            pr2serr("%02x ", wrCmd[k]);
        pr2serr("\n");
    }

    if (NULL == ptvp) {
        pr2serr("pt_low_write: of_ptvp NULL?\n");
        return -1;
    }
    clear_scsi_pt_obj(ptvp);
    set_scsi_pt_cdb(ptvp, wrCmd, fp->cdbsz);
    set_scsi_pt_sense(ptvp, sense_b, sizeof(sense_b));
    set_scsi_pt_data_out(ptvp, buff, bs * blocks);
#ifdef SCSI_PT_FLAGS_FUNCTION
    set_scsi_pt_flags(ptvp, SCSI_PT_FLAGS_QUEUE_AT_TAIL);
#endif
    vt = (op->verbose ? (op->verbose - 1) : 0);
    while (((res = do_scsi_pt(ptvp, sg_fd, DEF_RW_TIMEOUT, vt)) < 0) &&
           ((-EINTR == res) || (-EAGAIN == res))) {
        /* resubmit in these cases */
        if (-EINTR == res)
            ++op->interrupted_retries;
        else
            ++op->io_eagains;
    }

    vt = ((op->verbose > 1) ? (op->verbose - 1) : op->verbose);
    ret = sg_cmds_process_resp(ptvp, desc, res, bs * blocks, sense_b,
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
                pr2serr("    lba of last recovered error in this WRITE=0x%"
                        PRIx64 "\n", io_addr);
            else
                pr2serr("Recovered error: [no info] writing to block=0x%"
                        PRIx64 ", num=%d\n", to_block, blocks);
            break;
        case SG_LIB_CAT_ABORTED_COMMAND:
        case SG_LIB_CAT_UNIT_ATTENTION:
            break;
        case SG_LIB_CAT_NOT_READY:
        case SG_LIB_CAT_ILLEGAL_REQ:
        case SG_LIB_CAT_INVALID_OP:
        case SG_LIB_CAT_SENSE:
        case SG_LIB_CAT_RES_CONFLICT:
        case SG_LIB_CAT_DATA_PROTECT:
        case SG_LIB_CAT_PROTECTION:
            ++op->wr_unrecovered_errs;
            break;
        case SG_LIB_CAT_MEDIUM_HARD:
        default:
            ++op->wr_unrecovered_errs;
            if (fp->coe) {
                pr2serr(">> ignored errors for out blk=%" PRId64 " for %d "
                        "bytes\n", to_block, bs * blocks);
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
pt_write(struct opts_t * op, const unsigned char * buff, int blocks,
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
                pr2serr("Unit attention, continuing (w)\n");
            else {
                pr2serr("Unit attention, too many (w)\n");
                break;
            }
        } else if ((SG_LIB_CAT_ABORTED_COMMAND == ret) && first) {
            if (--op->max_aborted > 0)
                pr2serr("Aborted command, continuing (w)\n");
            else {
                pr2serr("Aborted command, too many (w)\n");
                break;
            }
        } else if (ret < 0)
            break;
        else if (retries_tmp > 0) {
            pr2serr(">>> retrying pt write: starting lba=%" PRId64 " [0x%"
                    PRIx64 "] blocks=%d\n", to_block, (uint64_t)to_block,
                    blocks);
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
pt_write_same16(struct opts_t * op, const unsigned char * buff, int bs,
                int blocks, int64_t start_block)
{
    int k, ret, res, sense_cat, vt;
    uint64_t llba;
    uint32_t unum;
    unsigned char wsCmdBlk[16];
    unsigned char sense_b[SENSE_BUFF_LEN];
    struct sg_pt_base * ptvp = op->odip->ptvp;
    int sg_fd = op->odip->fd;

    memset(wsCmdBlk, 0, sizeof(wsCmdBlk));
    wsCmdBlk[0] = 0x93;         /* WRITE SAME(16) opcode */
    /* set UNMAP; clear wrprotect, anchor, pbdata, lbdata */
    wsCmdBlk[1] = 0x8;
    llba = start_block;
    sg_put_unaligned_be64(llba, wsCmdBlk + 2);
    unum = blocks;
    sg_put_unaligned_be32(unum, wsCmdBlk + 10);
    if (op->verbose > 2) {
        pr2serr("    WRITE SAME(16) cdb: ");
        for (k = 0; k < (int)sizeof(wsCmdBlk); ++k)
            pr2serr("%02x ", wsCmdBlk[k]);
        pr2serr("\n");
        if (op->verbose > 4)
            pr2serr("    Data-out buffer length=%d\n", bs);
    }

    if (NULL == ptvp) {
        pr2serr("pt_write_same16: ptvp NULL?\n");
        return -1;
    }
    clear_scsi_pt_obj(ptvp);
    set_scsi_pt_cdb(ptvp, wsCmdBlk, sizeof(wsCmdBlk));
    set_scsi_pt_sense(ptvp, sense_b, sizeof(sense_b));
    set_scsi_pt_data_out(ptvp, buff, bs);
    vt = ((op->verbose > 1) ? (op->verbose - 1) : 0);
    while (((res = do_scsi_pt(ptvp, sg_fd, WRITE_SAME16_TIMEOUT, vt)) < 0) &&
           ((-EINTR == res) || (-EAGAIN == res))) {
        /* resubmit in these cases */
        if (-EINTR == res)
            ++op->interrupted_retries;
        else
            ++op->io_eagains;
    }
    ret = sg_cmds_process_resp(ptvp, "Write same(16)", res, 0, sense_b,
                               1 /*noisy */, vt, &sense_cat);
    if (-1 == ret)
        ;
    else if (-2 == ret) {
        switch (sense_cat) {
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
                    pr2serr("Medium or hardware error starting at lba=%"
                            PRIu64 " [0x%" PRIx64 "]\n", ull, ull);
            }
            ret = sense_cat;
            break;
        default:
            ret = sense_cat;
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

    res = sg_ll_sync_cache_10(fd, 0, 0, 0, 0, 0, 1, 0);
    if (SG_LIB_CAT_UNIT_ATTENTION == res) {
        pr2serr("Unit attention (out, sync cache), continuing\n");
        res = sg_ll_sync_cache_10(fd, 0, 0, 0, 0, 0, 1, 0);
    }
    if (0 != res)
        pr2serr("Unable to do SCSI synchronize cache\n");
}

static int
pt_tpc_process_res(int cp_ret, int sense_cat, const unsigned char * sense_b,
                   int sense_len)
{
    int ret, sb_ok;

    if (-1 == cp_ret)
        ret = -1;
    else if (-2 == cp_ret) {
        struct sg_scsi_sense_hdr ssh;

        sb_ok = sg_scsi_normalize_sense(sense_b, sense_len, &ssh);
        switch (sense_cat) {
        case SG_LIB_CAT_ILLEGAL_REQ:
            if (sb_ok) {
                if ((ASC_GENERAL_0 == ssh.asc) &&
                    (ASQ_OP_IN_PROGRESS == ssh.ascq))
                    ret = DDPT_CAT_OP_IN_PROGRESS;
                else if (ASC_3PC_GEN == ssh.asc) {
                    if (ASQ_TARGET_UNDERRUN == ssh.ascq)
                        ret = DDPT_CAT_TARGET_UNDERRUN;
                    else if (ASQ_TARGET_OVERRUN == ssh.ascq)
                        ret = DDPT_CAT_TARGET_OVERRUN;
                    else
                        ret = sense_cat;
                } else if (ASC_PARAM_LST_LEN_ERR == ssh.asc)
                    ret = DDPT_CAT_PARAM_LST_LEN_ERR;
                else if (ASC_INVALID_PARAM == ssh.asc) {
                    if (ASQ_INVALID_FLD_IN_PARAM == ssh.ascq)
                        ret = DDPT_CAT_INVALID_FLD_IN_PARAM;
                    else if (ASQ_TOO_MANY_SEGS_IN_PARAM == ssh.ascq)
                        ret = DDPT_CAT_TOO_MANY_SEGS_IN_PARAM;
                    else
                        ret = sense_cat;
                } else if (ASC_INSUFF_RES == ssh.asc) {
                    if (ASQ_INSUFF_RES_CREATE_ROD == ssh.ascq)
                        ret = DDPT_CAT_INSUFF_RES_CREATE_ROD;
                    else if (ASQ_INSUFF_RES_CREATE_RODTOK == ssh.ascq)
                        ret = DDPT_CAT_INSUFF_RES_CREATE_RODTOK;
                    else
                        ret = sense_cat;
                } else if (ASC_INVALID_TOKOP == ssh.asc)
                    ret = DDPT_CAT_TOKOP_BASE + ssh.ascq;
                else
                    ret = sense_cat;
            } else
                ret = sense_cat;
            break;
        case SG_LIB_CAT_RECOVERED:
        case SG_LIB_CAT_NO_SENSE:
            ret = 0;
            break;
        default:
            ret = sense_cat;
            break;
        }
    } else
        ret = 0;
    return ret;
}

/* Handles various service actions associated with opcode 0x83 which is
 * called THIRD PARTY COPY OUT. These include the EXTENDED COPY(LID1 and
 * LID4), POPULATE TOKEN and WRITE USING TOKEN commands.
 * Return of 0 -> success,
 * SG_LIB_CAT_INVALID_OP -> opcode 0x83 not supported,
 * SG_LIB_CAT_ILLEGAL_REQ -> bad field in cdb, SG_LIB_CAT_UNIT_ATTENTION,
 * SG_LIB_CAT_NOT_READY -> device not ready, SG_LIB_CAT_ABORTED_COMMAND,
 * -1 -> other failure */
int
pt_3party_copy_out(int sg_fd, int sa, uint32_t list_id, int group_num,
                   int timeout_secs, void * paramp, int param_len,
                   int noisy, int vb, int err_vb)
{
    int k, res, ret, has_lid, sense_cat, tmout;
    unsigned char xcopyCmdBlk[DDPT_TPC_OUT_CMDLEN] =
      {DDPT_TPC_OUT_CMD, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    unsigned char sense_b[SENSE_BUFF_LEN];
    struct sg_pt_base * ptvp;
    char cname[80];

    if (vb < 0)
        vb = 0;
    if (err_vb < 0)
        err_vb = 0;
    sg_get_opcode_sa_name(DDPT_TPC_OUT_CMD, sa, 0, sizeof(cname),
                          cname);
    xcopyCmdBlk[1] = (unsigned char)(sa & 0x1f);
    switch (sa) {
    case 0x0:   /* XCOPY(LID1) */
    case 0x1:   /* XCOPY(LID4) */
        sg_put_unaligned_be32((uint32_t)param_len, xcopyCmdBlk + 10);
        has_lid = 0;
        break;
    case 0x10:  /* POPULATE TOKEN (SBC-3) */
    case 0x11:  /* WRITE USING TOKEN (SBC-3) */
        sg_put_unaligned_be32(list_id, xcopyCmdBlk + 6);
        has_lid = 1;
        sg_put_unaligned_be32((uint32_t)param_len, xcopyCmdBlk + 10);
        xcopyCmdBlk[14] = (unsigned char)(group_num & 0x1f);
        break;
    case 0x1c:  /* COPY OPERATION ABORT */
        sg_put_unaligned_be32(list_id, xcopyCmdBlk + 2);
        has_lid = 1;
        break;
    default:
        pr2serr("pt_3party_copy_out: unknown service action 0x%x\n", sa);
        return -1;
    }
    tmout = (timeout_secs > 0) ? timeout_secs : DEF_PT_TIMEOUT;

    if (vb) {
        if ((vb > 1) || (param_len <= 0)) {
            pr2serr("    %s cmd : ", cname);
            for (k = 0; k < DDPT_TPC_OUT_CMDLEN; ++k)
                pr2serr("%02x ", xcopyCmdBlk[k]);
            pr2serr("\n");
        } else if (has_lid)
            pr2serr("    %s for list_id=%" PRIu32 "\n", cname, list_id);
        if (paramp && param_len) {
            if (vb > 1) {
                if (param_len <= 16) {
                    pr2serr("    %s parameter list:\n", cname);
                    dStrHexErr((const char *)paramp, param_len, -1);
                } else {
                    pr2serr("    %s, first 16 of %d byte parameter list:\n",
                            cname, param_len);
                    dStrHexErr((const char *)paramp, 16, -1);
                }
            }
        }
    }

    ptvp = construct_scsi_pt_obj();
    if (NULL == ptvp) {
        pr2serr("%s: out of memory\n", cname);
        return -1;
    }
    set_scsi_pt_cdb(ptvp, xcopyCmdBlk, sizeof(xcopyCmdBlk));
    set_scsi_pt_sense(ptvp, sense_b, sizeof(sense_b));
    set_scsi_pt_data_out(ptvp, (unsigned char *)paramp, param_len);
    res = do_scsi_pt(ptvp, sg_fd, tmout, vb);
    ret = sg_cmds_process_resp(ptvp, cname, res, 0, sense_b, noisy,
                               ((err_vb > 0) ? err_vb : 0), &sense_cat);
    if ((-1 == ret) &&
        (SCSI_PT_RESULT_STATUS == get_scsi_pt_result_category(ptvp)) &&
        (SAM_STAT_RESERVATION_CONFLICT == get_scsi_pt_status_response(ptvp)))
        ret = SG_LIB_CAT_RES_CONFLICT;
    else
        ret = pt_tpc_process_res(ret, sense_cat, sense_b,
                                 get_scsi_pt_sense_len(ptvp));
    destruct_scsi_pt_obj(ptvp);
    return ret;
}

int
pt_3party_copy_in(int sg_fd, int sa, uint32_t list_id, int timeout_secs,
                  void * resp, int mx_resp_len, int noisy, int vb, int err_vb)
{
    int k, res, ret, sense_cat, tmout;
    unsigned char rcvcopyresCmdBlk[DDPT_TPC_IN_CMDLEN] =
      {DDPT_TPC_IN_CMD, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    unsigned char sense_b[SENSE_BUFF_LEN];
    struct sg_pt_base * ptvp;
    char cname[64];

    if (vb < 0)
        vb = 0;
    if (err_vb < 0)
        err_vb = 0;
    sg_get_opcode_sa_name(DDPT_TPC_IN_CMD, sa, 0, (int)sizeof(cname), cname);
    rcvcopyresCmdBlk[1] = (unsigned char)(sa & 0x1f);
    if (sa <= 4)        /* LID1 variants */
        rcvcopyresCmdBlk[2] = (unsigned char)(list_id);
    else if ((sa >= 5) && (sa <= 7))    /* LID4 variants */
        sg_put_unaligned_be32(list_id, rcvcopyresCmdBlk + 2);
    sg_put_unaligned_be32((uint32_t)mx_resp_len, rcvcopyresCmdBlk + 10);
    tmout = (timeout_secs > 0) ? timeout_secs : DEF_PT_TIMEOUT;

    if (vb > 1) {
        pr2serr("    %s cmd: ", cname);
        for (k = 0; k < DDPT_TPC_IN_CMDLEN; ++k)
            pr2serr("%02x ", rcvcopyresCmdBlk[k]);
        pr2serr("\n");
    }

    ptvp = construct_scsi_pt_obj();
    if (NULL == ptvp) {
        pr2serr("%s: out of memory\n", cname);
        return -1;
    }
    set_scsi_pt_cdb(ptvp, rcvcopyresCmdBlk, sizeof(rcvcopyresCmdBlk));
    set_scsi_pt_sense(ptvp, sense_b, sizeof(sense_b));
    set_scsi_pt_data_in(ptvp, (unsigned char *)resp, mx_resp_len);
    res = do_scsi_pt(ptvp, sg_fd, tmout, vb);
    ret = sg_cmds_process_resp(ptvp, cname, res, mx_resp_len, sense_b, noisy,
                               ((err_vb > 0) ? err_vb : 0), &sense_cat);
    if ((-1 == ret) &&
        (SCSI_PT_RESULT_STATUS == get_scsi_pt_result_category(ptvp)) &&
        (SAM_STAT_RESERVATION_CONFLICT == get_scsi_pt_status_response(ptvp)))
        ret = SG_LIB_CAT_RES_CONFLICT;
    else
        ret = pt_tpc_process_res(ret, sense_cat, sense_b,
                                 get_scsi_pt_sense_len(ptvp));
    destruct_scsi_pt_obj(ptvp);
    return ret;
}
