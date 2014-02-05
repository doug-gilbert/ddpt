/*
 * Copyright (c) 2008-2014 Douglas Gilbert.
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

#define DDPT_READ6_OC 0x8
#define DDPT_READ10_OC 0x28
#define DDPT_READ12_OC 0xa8
#define DDPT_READ16_OC 0x88
#define DDPT_WRITE6_OC 0xa
#define DDPT_WRITE10_OC 0x2a
#define DDPT_WRITE12_OC 0xaa
#define DDPT_WRITE16_OC 0x8a
#define DDPT_VARIABLE_LEN_OC 0x7f
#define DDPT_READ32_SA 0x9
#define DDPT_WRITE32_SA 0xb

#define ASC_PROTECTION_INFO_BAD 0x10


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

int
pt_open_if(struct opts_t * op)
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
    fl = O_RDWR;
    if ((fd = scsi_pt_open_flags(fn, (fl | flags), op->verbose)) < 0) {
        if (-EBUSY == fd) {
            pr2serr("open %s for pt reading reports BUSY, use iflag=block "
                    "to wait until ready\n", fn);
            return -1;
        }
        fl = O_RDONLY;
        if ((fd = scsi_pt_open_flags(fn, (fl | flags), op->verbose)) < 0) {
            pr2serr("could not open %s for pt reading: %s\n", fn,
                    safe_strerror(-fd));
            return -1;
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
    return fd;
}

int
pt_open_of(struct opts_t * op)
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
            pr2serr("open %s for pt writing reports BUSY, use oflag=block "
                    "to wait until ready\n", fn);
            return -1;
        }
        pr2serr("could not open %s for pt writing: %s\n", fn,
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
        for (k = 0, ls = 0; k < 8; ++k) {
            ls <<= 8;
            ls |= rcBuff[k];
        }
        *num_sect = ls + 1;
        *sect_sz = (rcBuff[8] << 24) | (rcBuff[9] << 16) |
                   (rcBuff[10] << 8) | rcBuff[11];
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
                  int64_t start_block, int write_true,
                  const struct flags_t * fp, int protect)
{
    int rd_opcode[] = {DDPT_READ6_OC, DDPT_READ10_OC, DDPT_READ12_OC,
                       DDPT_READ16_OC, DDPT_READ32_SA};
    int wr_opcode[] = {DDPT_WRITE6_OC, DDPT_WRITE10_OC, DDPT_WRITE12_OC,
                       DDPT_WRITE16_OC, DDPT_WRITE32_SA};
    int sz_ind, options_byte, rw_sa;

    memset(cdbp, 0, cdb_sz);
    options_byte = 0;
    if (cdb_sz < 6) {
        pr2serr("cdb_sz too small\n");
        return 1;
    }
    if (cdb_sz > 6) {
        if (fp->dpo)
            options_byte |= 0x10;
        if (fp->fua)
            options_byte |= 0x8;
        if (fp->fua_nv)
            options_byte |= 0x2;
        if (protect)
            options_byte |= ((protect & 0x7) << 5);
    }
    if ((! write_true) && fp->rarc)
        options_byte |= 0x4;

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
        sz_ind = 1;
        cdbp[0] = (unsigned char)(write_true ? wr_opcode[sz_ind] :
                                               rd_opcode[sz_ind]);
        cdbp[1] = (unsigned char)options_byte;
        cdbp[2] = (unsigned char)((start_block >> 24) & 0xff);
        cdbp[3] = (unsigned char)((start_block >> 16) & 0xff);
        cdbp[4] = (unsigned char)((start_block >> 8) & 0xff);
        cdbp[5] = (unsigned char)(start_block & 0xff);
        cdbp[7] = (unsigned char)((blocks >> 8) & 0xff);
        cdbp[8] = (unsigned char)(blocks & 0xff);
        if (blocks & (~0xffff)) {
            pr2serr("for 10 byte commands, maximum number of blocks is %d\n",
                    0xffff);
            return 1;
        }
        break;
    case 12:
        sz_ind = 2;
        cdbp[0] = (unsigned char)(write_true ? wr_opcode[sz_ind] :
                                               rd_opcode[sz_ind]);
        cdbp[1] = (unsigned char)options_byte;
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
        cdbp[1] = (unsigned char)options_byte;
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
    case 32:
        sz_ind = 4;
        cdbp[0] = (unsigned char)DDPT_VARIABLE_LEN_OC;
        cdbp[7] = (unsigned char)0x18;  /* additional length=>32 byte cdb */
        rw_sa = write_true ? wr_opcode[sz_ind] : rd_opcode[sz_ind];
        cdbp[8] = (unsigned char)((rw_sa >> 8) & 0xff);
        cdbp[9] = (unsigned char)(rw_sa & 0xff);
        cdbp[10] = (unsigned char)options_byte;
        cdbp[12] = (unsigned char)((start_block >> 56) & 0xff);
        cdbp[13] = (unsigned char)((start_block >> 48) & 0xff);
        cdbp[14] = (unsigned char)((start_block >> 40) & 0xff);
        cdbp[15] = (unsigned char)((start_block >> 32) & 0xff);
        cdbp[16] = (unsigned char)((start_block >> 24) & 0xff);
        cdbp[17] = (unsigned char)((start_block >> 16) & 0xff);
        cdbp[18] = (unsigned char)((start_block >> 8) & 0xff);
        cdbp[19] = (unsigned char)(start_block & 0xff);
        cdbp[28] = (unsigned char)((blocks >> 24) & 0xff);
        cdbp[29] = (unsigned char)((blocks >> 16) & 0xff);
        cdbp[30] = (unsigned char)((blocks >> 8) & 0xff);
        cdbp[31] = (unsigned char)(blocks & 0xff);
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
                (ASC_PROTECTION_INFO_BAD == ssh.asc)) {
                /* Protection problem, so no retry */
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
            pr2serr("Device (r) not ready\n");
            return res;
        case SG_LIB_CAT_ABORTED_COMMAND:
            if (--op->max_aborted > 0)
                pr2serr("Aborted command, continuing (r)\n");
            else {
                pr2serr("Aborted command, too many (r)\n");
                return res;
            }
            break;
        case SG_LIB_CAT_UNIT_ATTENTION:
            if (--op->max_uas > 0)
                pr2serr("Unit attention, continuing (r)\n");
            else {
                pr2serr("Unit attention, too many (r)\n");
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
                pr2serr("ENOMEM again, unexpected (r)\n");
                return -1;
            case SG_LIB_CAT_NOT_READY:
                pr2serr("device (r) not ready\n");
                return res;
            case SG_LIB_CAT_UNIT_ATTENTION:
                pr2serr("Unit attention, unexpected (r)\n");
                return res;
            case SG_LIB_CAT_ABORTED_COMMAND:
                pr2serr("Aborted command, unexpected (r)\n");
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
                pr2serr(">> unexpected result=%d from pt_low_read() 2\n",
                        res);
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
    struct sg_scsi_sense_hdr ssh;

    if (pt_build_scsi_cdb(wrCmd, fp->cdbsz, blocks, to_block, 1, fp,
                          op->wrprotect)) {
        pr2serr("bad wr cdb build, to_block=%" PRId64 ", blocks=%d\n",
                to_block, blocks);
        return SG_LIB_SYNTAX_ERROR;
    }
    if (op->verbose > 2) {
        pr2serr("    WRITE cdb: ");
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
    vt = (op->verbose ? (op->verbose - 1) : 0);
    while (((res = do_scsi_pt(ptvp, sg_fd, DEF_RW_TIMEOUT, vt)) < 0) &&
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
                pr2serr("    lba of last recovered error in this WRITE=0x%"
                        PRIx64 "\n", io_addr);
            else
                pr2serr("Recovered error: [no info] writing to block=0x%"
                        PRIx64 ", num=%d\n", to_block, blocks);
            break;
        case SG_LIB_CAT_ABORTED_COMMAND:
            if (sg_scsi_normalize_sense(sense_b, slen, &ssh) &&
                (ASC_PROTECTION_INFO_BAD == ssh.asc)) {
                /* Protection problem, so no retry */
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
                    pr2serr("Medium or hardware error starting at lba=%"
                            PRIu64 " [0x%" PRIx64 "]\n", ull, ull);
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

    res = sg_ll_sync_cache_10(fd, 0, 0, 0, 0, 0, 1, 0);
    if (SG_LIB_CAT_UNIT_ATTENTION == res) {
        pr2serr("Unit attention (out, sync cache), continuing\n");
        res = sg_ll_sync_cache_10(fd, 0, 0, 0, 0, 0, 1, 0);
    }
    if (0 != res)
        pr2serr("Unable to do SCSI synchronize cache\n");
}
