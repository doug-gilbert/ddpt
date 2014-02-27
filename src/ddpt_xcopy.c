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
 * This file contains SCSI Extended copy command helper functions for ddpt.
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


/* In SPC-4 the cdb opcodes have more generic names */
#define THIRD_PARTY_COPY_OUT_CMD 0x83
#define THIRD_PARTY_COPY_IN_CMD 0x84

/* Third party copy IN (opcode 0x84) and OUT (opcode 0x83) command service
 * actions */
#define SA_XCOPY_LID1           0x0     /* OUT, originate */
#define SA_XCOPY_LID4           0x1     /* OUT, originate */
#define SA_POP_TOK              0x10    /* OUT, originate */
#define SA_WR_USING_TOK         0x11    /* OUT, originate */
#define SA_COPY_ABORT           0x1C    /* OUT, abort */
#define SA_COPY_STATUS_LID1     0x0     /* IN, retrieve */
#define SA_COPY_DATA_LID1       0x1     /* IN, retrieve */
#define SA_COPY_OP_PARAMS       0x3     /* IN, retrieve */
#define SA_COPY_FAIL_DETAILS    0x4     /* IN, retrieve */
#define SA_COPY_STATUS_LID4     0x5     /* IN, retrieve */
#define SA_COPY_DATA_LID4       0x6     /* IN, retrieve */
#define SA_ROD_TOK_INFO         0x7     /* IN, retrieve */
#define SA_ALL_ROD_TOKS         0x8     /* IN, retrieve */

#define DEF_3PC_OUT_TIMEOUT (10 * 60)   /* is 10 minutes enough? */
#define DEF_3PC_IN_TIMEOUT 60           /* these should be fast */

/* Target descriptor variety */
#define TD_FC_WWPN 1
#define TD_FC_PORT 2
#define TD_FC_WWPN_AND_PORT 4
#define TD_SPI 8
#define TD_VPD 16
#define TD_IPV4 32
#define TD_ALIAS 64
#define TD_RDMA 128
#define TD_FW 256
#define TD_SAS 512
#define TD_IPV6 1024
#define TD_IP_COPY_SERVICE 2048
#define TD_ROD 4096


#define LOCAL_ROD_TOKEN_SIZE 1024
static unsigned char local_rod_token[LOCAL_ROD_TOKEN_SIZE];

static const char * rec_copy_op_params_str = "Receive copy operating "
                                             "parameters";

static struct val_str_t xcopy4_cpy_op_status[] = {
    {0x1, "Operation completed without errors"},
    {0x2, "Operation completed with errors"},
    {0x3, "Operation completed without errors but with partial ROD token "
     "usage"},
    {0x4, "Operation completed without errors but with residual data"},
    {0x10, "Operation in progress, foreground or background unknown"},
    {0x11, "Operation in progress in foreground"},
    {0x12, "Operation in progress in background"},
    {0x60, "Operation terminated"},
    {0x0, NULL},
};


static int
simplified_dt(const struct dev_info_t * dip)
{
    int d_type = dip->d_type;

    switch (d_type) {
    case FT_BLOCK:
    case FT_TAPE:
    case FT_REG:
    case FT_DEV_NULL:
    case FT_FIFO:
    case FT_ERROR:
        return d_type;
    default:
        if (FT_PT & d_type) {
            if ((0 == dip->pdt) || (0xe == dip->pdt)) /* D-A or RBC */
            return FT_BLOCK;
        else if (0x1 == dip->pdt)
            return FT_TAPE;
        }
        return FT_OTHER;
    }
}

static int
seg_desc_from_d_type(int in_dt, int in_off, int out_dt, int out_off)
{
    int desc_type = -1;

    switch (in_dt) {
    case FT_BLOCK:
        switch (out_dt) {
        case FT_TAPE:
            if (out_off)
                break;
            if (in_off)
                desc_type = 0x8;
            else
                desc_type = 0;
            break;
        case FT_BLOCK:
            if (in_off || out_off)
                desc_type = 0xA;
            else
                desc_type = 2;
            break;
        default:
            break;
        }
        break;
    case FT_TAPE:
        if (in_off)
            break;

        switch (out_dt) {
        case FT_TAPE:
            if (!out_off) {
                desc_type = 3;
                break;
            }
            break;
        case FT_BLOCK:
            if (out_off)
                desc_type = 9;
            else
                desc_type = 3;
            break;
        case FT_DEV_NULL:
            desc_type = 6;
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
    return desc_type;
}

static int
scsi_encode_seg_desc(struct opts_t * op, unsigned char *seg_desc,
                     int seg_desc_type, int64_t num_blk, uint64_t src_lba,
                     uint64_t dst_lba)
{
    int seg_desc_len = 0;

    seg_desc[0] = seg_desc_type;
    seg_desc[1] = (op->xc_dc << 1) | op->xc_cat;
    if (seg_desc_type == 0x02) {
        seg_desc_len = 0x18;
        seg_desc[4] = 0;
        seg_desc[5] = 0; /* Source target index */
        seg_desc[7] = 1; /* Destination target index */
        seg_desc[10] = (num_blk >> 8) & 0xff;
        seg_desc[11] = num_blk & 0xff;
        seg_desc[12] = (src_lba >> 56) & 0xff;
        seg_desc[13] = (src_lba >> 48) & 0xff;
        seg_desc[14] = (src_lba >> 40) & 0xff;
        seg_desc[15] = (src_lba >> 32) & 0xff;
        seg_desc[16] = (src_lba >> 24) & 0xff;
        seg_desc[17] = (src_lba >> 16) & 0xff;
        seg_desc[18] = (src_lba >> 8) & 0xff;
        seg_desc[19] = src_lba & 0xff;
        seg_desc[20] = (dst_lba >> 56) & 0xff;
        seg_desc[21] = (dst_lba >> 48) & 0xff;
        seg_desc[22] = (dst_lba >> 40) & 0xff;
        seg_desc[23] = (dst_lba >> 32) & 0xff;
        seg_desc[24] = (dst_lba >> 24) & 0xff;
        seg_desc[25] = (dst_lba >> 16) & 0xff;
        seg_desc[26] = (dst_lba >> 8) & 0xff;
        seg_desc[27] = dst_lba & 0xff;
    }
    seg_desc[2] = (seg_desc_len >> 8) & 0xFF;
    seg_desc[3] = seg_desc_len & 0xFF;

    return seg_desc_len + 4;
}

static int
scsi_extended_copy(struct opts_t * op, unsigned char *src_desc,
                   int src_desc_len, unsigned char *dst_desc,
                   int dst_desc_len, int seg_desc_type, int64_t num_blk)
{
    unsigned char xcopyBuff[256];
    int desc_offset = 16;
    int seg_desc_len, verb, fd, tmout;
    uint64_t src_lba = op->skip;
    uint64_t dst_lba = op->seek;

    fd = (op->iflagp->xcopy) ? op->idip->fd : op->odip->fd;
    verb = (op->verbose > 1) ? (op->verbose - 2) : 0;

    memset(xcopyBuff, 0, 256);
    xcopyBuff[0] = op->list_id;
    xcopyBuff[1] = (op->id_usage << 3) | op->prio;
    xcopyBuff[2] = 0;
    xcopyBuff[3] = src_desc_len + dst_desc_len; /* Two target descriptors */
    memcpy(xcopyBuff + desc_offset, src_desc, src_desc_len);
    desc_offset += src_desc_len;
    memcpy(xcopyBuff + desc_offset, dst_desc, dst_desc_len);
    desc_offset += dst_desc_len;
    seg_desc_len = scsi_encode_seg_desc(op, xcopyBuff + desc_offset,
                                        seg_desc_type, num_blk,
                                        src_lba, dst_lba);
    xcopyBuff[11] = seg_desc_len; /* One segment descriptor */
    desc_offset += seg_desc_len;
    tmout = (op->timeout_xcopy < 1) ? DEF_3PC_OUT_TIMEOUT : op->timeout_xcopy;
    return pt_3party_copy_out(fd, SA_XCOPY_LID1, op->list_id, DEF_GROUP_NUM,
                              tmout, xcopyBuff, desc_offset, 1, verb);
}

/* Returns target descriptor variety encoded into an int. There may be
 * more than one, OR-ed together. A return value of zero or less is
 * considered as an error. */
static int
scsi_operating_parameter(struct opts_t * op, int is_dest)
{
    int res, fd, ftype, pdt, snlid, verb;
    unsigned char rcBuff[256];
    unsigned int rcBuffLen = 256, len, n, td_list = 0;
    unsigned long num, max_target_num, max_segment_num, max_segment_len;
    unsigned long max_desc_len, max_inline_data, held_data_limit;
    int valid = 0;
    struct dev_info_t * dip;

    verb = (op->verbose ? (op->verbose - 1) : 0);
    dip = is_dest ? op->odip : op->idip;
    fd = dip->fd;
    ftype = dip->d_type;
    if (FT_PT & ftype) {
        pdt = dip->pdt;
        if ((0 == pdt) || (0xe == pdt)) /* direct-access or RBC */
            ftype |= FT_BLOCK;
        else if (0x1 == pdt)
            ftype |= FT_TAPE;
    } else if (FT_FIFO & ftype)
        ftype |= FT_REG;
    if (FT_REG & ftype) {
        pr2serr("%s: not expecting a regular file here: %s\n",  __func__,
                dip->fn);
        return -SG_LIB_FILE_ERROR;
    }

    /* Third Party Copy IN command; sa: RECEIVE COPY OPERATING PARAMETERS */
    res = pt_3party_copy_in(fd, SA_COPY_OP_PARAMS, 0, DEF_3PC_IN_TIMEOUT,
                            rcBuff, rcBuffLen, 1, verb);
    if (0 != res)
        return -res;

    len = ((rcBuff[0] << 24) | (rcBuff[1] << 16) | (rcBuff[2] << 8) |
           rcBuff[3]) + 4;
    if (len > rcBuffLen) {
        pr2serr("  <<report too long for internal buffer, output "
                "truncated\n");
    }
    if (op->verbose > 2) {
        pr2serr("\nOutput %s response in hex:\n", rec_copy_op_params_str);
        dStrHexErr((const char *)rcBuff, len, 1);
    }
    snlid = rcBuff[4] & 0x1;
    max_target_num = rcBuff[8] << 8 | rcBuff[9];
    max_segment_num = rcBuff[10] << 8 | rcBuff[11];
    max_desc_len = rcBuff[12] << 24 | rcBuff[13] << 16 | rcBuff[14] << 8 |
                   rcBuff[15];
    max_segment_len = rcBuff[16] << 24 | rcBuff[17] << 16 |
                      rcBuff[18] << 8 | rcBuff[19];
    dip->xc_max_bytes = max_segment_len ? max_segment_len : ULONG_MAX;
    max_inline_data = rcBuff[20] << 24 | rcBuff[21] << 16 | rcBuff[22] << 8 |
                      rcBuff[23];
    if (op->verbose) {
        pr2serr(" >> %s, %sput [%s]:\n", rec_copy_op_params_str,
                (is_dest ? "out" : "in"), dip->fn);
        pr2serr("    Support No List IDentifier (SNLID): %d\n", snlid);
        pr2serr("    Maximum target descriptor count: %lu\n", max_target_num);
        pr2serr("    Maximum segment descriptor count: %lu\n",
                max_segment_num);
        pr2serr("    Maximum descriptor list length: %lu\n", max_desc_len);
        pr2serr("    Maximum segment length: %lu\n", max_segment_len);
        pr2serr("    Maximum inline data length: %lu\n", max_inline_data);
    }
    held_data_limit = rcBuff[24] << 24 | rcBuff[25] << 16 |
                      rcBuff[26] << 8 | rcBuff[27];
    if (op->id_usage < 0) {
        if (! held_data_limit)
            op->id_usage = 2;
        else
            op->id_usage = 0;
    }
    if (op->verbose) {
        pr2serr("    Held data limit: %lu (list_id_usage: %d)\n",
                held_data_limit, op->id_usage);
        num = rcBuff[28] << 24 | rcBuff[29] << 16 | rcBuff[30] << 8 |
              rcBuff[31];
        pr2serr("    Maximum stream device transfer size: %lu\n", num);
        pr2serr("    Maximum concurrent copies: %u\n", rcBuff[36]);
        pr2serr("    Data segment granularity: %u bytes\n", 1 << rcBuff[37]);
        pr2serr("    Inline data granularity: %u bytes\n", 1 << rcBuff[38]);
        pr2serr("    Held data granularity: %u bytes\n", 1 << rcBuff[39]);

        pr2serr("    Implemented descriptor list:\n");
    }
    dip->xc_min_bytes = 1 << rcBuff[37];

    for (n = 0; n < rcBuff[43]; n++) {
        switch(rcBuff[44 + n]) {
        case 0x00: /* copy block to stream device */
            if (!is_dest && (ftype & FT_BLOCK))
                valid++;
            if (is_dest && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy Block to Stream device\n");
            break;
        case 0x01: /* copy stream to block device */
            if (!is_dest && (ftype & FT_TAPE))
                valid++;
            if (is_dest && (ftype & FT_BLOCK))
                valid++;
            if (op->verbose)
                pr2serr("        Copy Stream to Block device\n");
            break;
        case 0x02: /* copy block to block device */
            if (!is_dest && (ftype & FT_BLOCK))
                valid++;
            if (is_dest && (ftype & FT_BLOCK))
                valid++;
            if (op->verbose)
                pr2serr("        Copy Block to Block device\n");
            break;
        case 0x03: /* copy stream to stream device */
            if (!is_dest && (ftype & FT_TAPE))
                valid++;
            if (is_dest && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy Stream to Stream device\n");
            break;
        case 0x04: /* copy inline data to stream device */
            if (!is_dest && (ftype & FT_REG))
                valid++;
            if (is_dest && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy inline data to Stream device\n");
            break;
        case 0x05: /* copy embedded data to stream device */
            if (!is_dest && (ftype & FT_REG))
                valid++;
            if (is_dest && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy embedded data to Stream device\n");
            break;
        case 0x06: /* Read from stream device and discard */
            if (!is_dest && (ftype & FT_TAPE))
                valid++;
            if (is_dest && (ftype & FT_DEV_NULL))
                valid++;
            if (op->verbose)
                pr2serr("        Read from stream device and discard\n");
            break;
        case 0x07: /* Verify block or stream device operation */
            if (!is_dest && (ftype & (FT_TAPE | FT_BLOCK)))
                valid++;
            if (is_dest && (ftype & (FT_TAPE | FT_BLOCK)))
                valid++;
            if (op->verbose)
                pr2serr("        Verify block or stream device operation\n");
            break;
        case 0x08: /* copy block device with offset to stream device */
            if (!is_dest && (ftype & FT_BLOCK))
                valid++;
            if (is_dest && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy block device with offset to stream "
                       "device\n");
            break;
        case 0x09: /* copy stream device to block device with offset */
            if (!is_dest && (ftype & FT_TAPE))
                valid++;
            if (is_dest && (ftype & FT_BLOCK))
                valid++;
            if (op->verbose)
                pr2serr("        Copy stream device to block device with "
                       "offset\n");
            break;
        case 0x0a: /* copy block device with offset to block device with
                    * offset */
            if (!is_dest && (ftype & FT_BLOCK))
                valid++;
            if (is_dest && (ftype & FT_BLOCK))
                valid++;
            if (op->verbose)
                pr2serr("        Copy block device with offset to block "
                       "device with offset\n");
            break;
        case 0x0b: /* copy block device to stream device and hold data */
            if (!is_dest && (ftype & FT_BLOCK))
                valid++;
            if (is_dest && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy block device to stream device and hold "
                       "data\n");
            break;
        case 0x0c: /* copy stream device to block device and hold data */
            if (!is_dest && (ftype & FT_TAPE))
                valid++;
            if (is_dest && (ftype & FT_BLOCK))
                valid++;
            if (op->verbose)
                pr2serr("        Copy stream device to block device and hold "
                       "data\n");
            break;
        case 0x0d: /* copy block device to block device and hold data */
            if (!is_dest && (ftype & FT_BLOCK))
                valid++;
            if (is_dest && (ftype & FT_BLOCK))
                valid++;
            if (op->verbose)
                pr2serr("        Copy block device to block device and hold "
                       "data\n");
            break;
        case 0x0e: /* copy stream device to stream device and hold data */
            if (!is_dest && (ftype & FT_TAPE))
                valid++;
            if (is_dest && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy block device to block device and hold "
                       "data\n");
            break;
        case 0x0f: /* read from stream device and hold data */
            if (!is_dest && (ftype & FT_TAPE))
                valid++;
            if (is_dest && (ftype & FT_DEV_NULL))
                valid++;
            if (op->verbose)
                pr2serr("        Read from stream device and hold data\n");
            break;
        case 0xe0: /* FC N_Port_Name */
            if (op->verbose)
                pr2serr("        FC N_Port_Name target descriptor\n");
            td_list |= TD_FC_WWPN;
            break;
        case 0xe1: /* FC Port_ID */
            if (op->verbose)
                pr2serr("        FC Port_ID target descriptor\n");
            td_list |= TD_FC_PORT;
            break;
        case 0xe2: /* FC N_Port_ID with N_Port_Name checking */
            if (op->verbose)
                pr2serr("        FC N_Port_ID with N_Port_Name target "
                       "descriptor\n");
            td_list |= TD_FC_WWPN_AND_PORT;
            break;
        case 0xe3: /* Parallel Interface T_L  */
            if (op->verbose)
                pr2serr("        SPI T_L target descriptor\n");
            td_list |= TD_SPI;
            break;
        case 0xe4: /* identification descriptor */
            if (op->verbose)
                pr2serr("        Identification target descriptor\n");
            td_list |= TD_VPD;
            break;
        case 0xe5: /* IPv4  */
            if (op->verbose)
                pr2serr("        IPv4 target descriptor\n");
            td_list |= TD_IPV4;
            break;
        case 0xe6: /* Alias */
            if (op->verbose)
                pr2serr("        Alias target descriptor\n");
            td_list |= TD_ALIAS;
            break;
        case 0xe7: /* RDMA */
            if (op->verbose)
                pr2serr("        RDMA target descriptor\n");
            td_list |= TD_RDMA;
            break;
        case 0xe8: /* FireWire */
            if (op->verbose)
                pr2serr("        IEEE 1394 target descriptor\n");
            td_list |= TD_FW;
            break;
        case 0xe9: /* SAS */
            if (op->verbose)
                pr2serr("        SAS target descriptor\n");
            td_list |= TD_SAS;
            break;
        case 0xea: /* IPv6 */
            if (op->verbose)
                pr2serr("        IPv6 target descriptor\n");
            td_list |= TD_IPV6;
            break;
        case 0xeb: /* IP Copy Service */
            if (op->verbose)
                pr2serr("        IP Copy Service target descriptor\n");
            td_list |= TD_IP_COPY_SERVICE;
            break;
        case 0xfe: /* ROD */
            if (op->verbose)
                pr2serr("        ROD target descriptor\n");
            td_list |= TD_ROD;
            break;
        default:
            pr2serr(">> Unhandled target descriptor 0x%02x\n",
                    rcBuff[44 + n]);
            break;
        }
    }
    if (!valid) {
        pr2serr(">> no matching target descriptor supported\n");
        td_list = 0;
    }
    return td_list;
}

static int
desc_from_vpd_id(struct opts_t * op, unsigned char *desc, int desc_len,
                 int is_dest)
{
    int fd, res, u, i_len, assoc, desig, verb;
    unsigned char rcBuff[256], *ucp, *best = NULL;
    unsigned int len = 254;
    unsigned int block_size;
    int off = -1;
    int best_len = 0;
    int f_desig = 0;
    struct flags_t * flp;
    struct dev_info_t * dip;

    verb = (op->verbose ? (op->verbose - 1) : 0);
    dip = is_dest ? op->odip : op->idip;
    fd = dip->fd;
    flp = is_dest ? op->oflagp : op->iflagp;
    block_size = is_dest ? op->obs : op->ibs;
    memset(rcBuff, 0xff, len);
    res = sg_ll_inquiry(fd, 0, 1, VPD_DEVICE_ID, rcBuff, 4, 1, verb);
    if (0 != res) {
        if (SG_LIB_CAT_ILLEGAL_REQ == res)
            pr2serr("Device identification VPD page not found\n");
        else
            pr2serr("VPD inquiry failed with %d, try again with '-vv'\n",
                    res);
        return res;
    } else if (rcBuff[1] != VPD_DEVICE_ID) {
        pr2serr("invalid VPD response\n");
        return SG_LIB_CAT_MALFORMED;
    }
    len = ((rcBuff[2] << 8) + rcBuff[3]) + 4;
    res = sg_ll_inquiry(fd, 0, 1, VPD_DEVICE_ID, rcBuff, len, 1, verb);
    if (0 != res) {
        pr2serr("VPD inquiry failed with %d\n", res);
        return res;
    } else if (rcBuff[1] != VPD_DEVICE_ID) {
        pr2serr("invalid VPD response\n");
        return SG_LIB_CAT_MALFORMED;
    }
    if (op->verbose > 2) {
        pr2serr("Output VPD_DEVICE_ID (0x83) page in hex:\n");
        dStrHexErr((const char *)rcBuff, len, 1);
    }

    while ((u = sg_vpd_dev_id_iter(rcBuff + 4, len - 4, &off, 0, -1, -1)) ==
           0) {
        ucp = rcBuff + 4 + off;
        i_len = ucp[3];
        if (((unsigned int)off + i_len + 4) > len) {
            pr2serr("    VPD page error: designator length %d longer than\n"
                    "     remaining response length=%d\n", i_len,
                    (len - off));
            return SG_LIB_CAT_MALFORMED;
        }
        assoc = ((ucp[1] >> 4) & 0x3);
        desig = (ucp[1] & 0xf);
        if (op->verbose > 2)
            pr2serr("    Desc %d: assoc %u desig %u len %d\n", off, assoc,
                    desig, i_len);
        /* Descriptor must be less than 16 bytes */
        if (i_len > 16)
            continue;
        if (desig == 3) {
            best = ucp;
            best_len = i_len;
            break;
        }
        if (desig == 2) {
            if (!best || f_desig < 2) {
                best = ucp;
                best_len = i_len;
                f_desig = 2;
            }
        } else if (desig == 1) {
            if (!best || f_desig == 0) {
                best = ucp;
                best_len = i_len;
                f_desig = desig;
            }
        } else if (desig == 0) {
            if (!best) {
                best = ucp;
                best_len = i_len;
                f_desig = desig;
            }
        }
    }
    if (best) {
        if (op->verbose)
            decode_designation_descriptor(best, best_len, op->verbose);
        if (best_len + 4 < desc_len) {
            memset(desc, 0, 32);
            desc[0] = 0xe4;
            memcpy(desc + 4, best, best_len + 4);
            desc[4] &= 0x1f;
            desc[28] = flp->pad << 2;
            desc[29] = (block_size >> 16) & 0xff;
            desc[30] = (block_size >> 8) & 0xff;
            desc[31] = block_size & 0xff;
            if (op->verbose > 3) {
                pr2serr("Descriptor in hex (bs %d):\n", block_size);
                dStrHexErr((const char *)desc, 32, 1);
            }
            return 32;
        }
        return  best_len + 8;
    }
    return 0;
}

/* Called from main() in ddpt.c . Returns 0 on success or a positive
 * errno value if problems. This is for a xcopy(LID1) disk->disk copy. */
int
do_xcopy(struct opts_t * op)
{
    int res, ibpt, obpt, bs_same, max_bpt, blocks, oblocks;
    int src_desc_len, dst_desc_len, seg_desc_type;
    unsigned char src_desc[256];
    unsigned char dst_desc[256];
    const struct flags_t * ifp = op->iflagp;
    const struct flags_t * ofp = op->oflagp;
    const struct dev_info_t * idip = op->idip;
    const struct dev_info_t * odip = op->odip;

    if (op->list_id_given && (op->list_id > UCHAR_MAX)) {
        pr2serr("list_id for xcopy(LID1) cannot exceed 255\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if (op->id_usage == 3) { /* list_id usage disabled */
        if (op->list_id_given && (0 != op->list_id)) {
            pr2serr("list_id disabled by id_usage flag\n");
            return SG_LIB_SYNTAX_ERROR;
        } else
            op->list_id = 0;
    }
    res = scsi_operating_parameter(op, 0);
    if (SG_LIB_CAT_UNIT_ATTENTION == -res)
        res = scsi_operating_parameter(op, 0);
    if (res < 0) {
        if (-res == SG_LIB_CAT_INVALID_OP) {
            pr2serr("%s command not supported on %s\n",
                    rec_copy_op_params_str, idip->fn);
            return EINVAL;
        } else if (-res == SG_LIB_CAT_NOT_READY)
            pr2serr("%s failed on %s - not ready\n",
                    rec_copy_op_params_str, idip->fn);
        else {
            pr2serr("Unable to %s on %s\n", rec_copy_op_params_str,
                    idip->fn);
            return -res;
        }
    } else if (res == 0)
        return SG_LIB_CAT_INVALID_OP;
    if (res & TD_VPD) {
        if (op->verbose)
            pr2serr("  >> using VPD identification for source %s\n",
                    op->idip->fn);
        src_desc_len = desc_from_vpd_id(op, src_desc, sizeof(src_desc), 0);
        if (src_desc_len > (int)sizeof(src_desc)) {
            pr2serr("source descriptor too large (%d bytes)\n", res);
            return SG_LIB_CAT_MALFORMED;
        }
    } else
        return SG_LIB_CAT_INVALID_OP;

    res = scsi_operating_parameter(op, 1);
    if (res < 0) {
        if (SG_LIB_CAT_UNIT_ATTENTION == -res) {
            pr2serr("Unit attention (%s), continuing\n",
                    rec_copy_op_params_str);
            res = scsi_operating_parameter(op, 1);
        } else {
            if (-res == SG_LIB_CAT_INVALID_OP) {
                pr2serr("%s command not supported on %s\n",
                        rec_copy_op_params_str, odip->fn);
                return EINVAL;
            } else if (-res == SG_LIB_CAT_NOT_READY)
                pr2serr("%s failed on %s - not ready\n",
                        rec_copy_op_params_str, odip->fn);
            else {
                pr2serr("Unable to %s on %s\n", rec_copy_op_params_str,
                        odip->fn);
                return -res;
            }
        }
    } else if (res == 0)
        return SG_LIB_CAT_INVALID_OP;
    if (res & TD_VPD) {
        if (op->verbose)
            pr2serr("  >> using VPD identification for destination %s\n",
                    odip->fn);
        dst_desc_len = desc_from_vpd_id(op, dst_desc, sizeof(dst_desc), 1);
        if (dst_desc_len > (int)sizeof(dst_desc)) {
            pr2serr("destination descriptor too large (%d bytes)\n", res);
            return SG_LIB_CAT_MALFORMED;
        }
    } else
        return SG_LIB_CAT_INVALID_OP;

    bs_same = (op->ibs == op->obs);
    max_bpt = bs_same ? MAX_XC_BPT : MAX_XC_BPT_POW2;
    /* Beware, xc_max_bytes may be ULONG_MAX hence unsigned long division */
    if (op->bpt_given) {
        ibpt = op->bpt_i;
        ibpt = (ibpt > max_bpt) ? max_bpt : ibpt;
        obpt = bs_same ? ibpt : ((op->ibs * op->bpt_i) / op->obs);
        if (ifp->dc || ofp->dc) {
            if ((unsigned long)obpt * op->obs > odip->xc_max_bytes) {
                pr2serr("bpt too large (max %ld blocks)\n",
                        odip->xc_max_bytes / op->obs);
                return SG_LIB_SYNTAX_ERROR;
            }
        } else {
            if ((unsigned long)ibpt * op->ibs > idip->xc_max_bytes) {
                pr2serr("bpt too large (max %ld blocks)\n",
                        idip->xc_max_bytes / op->ibs);
                return SG_LIB_SYNTAX_ERROR;
            }
        }
    } else {
        unsigned long r;

        if (ifp->dc || ofp->dc) {
            r = odip->xc_max_bytes / (unsigned long)op->obs;
            obpt = (r > INT_MAX) ? INT_MAX : (int)r;
            ibpt = bs_same ? obpt : ((op->obs * obpt) / op->ibs);
            ibpt = (ibpt > max_bpt) ? max_bpt : ibpt;
            obpt = bs_same ? ibpt : ((op->ibs * ibpt) / op->obs);
        } else {
            r = idip->xc_max_bytes / (unsigned long)op->ibs;
            ibpt = (r > (unsigned long)max_bpt) ? max_bpt : (int)r;
            obpt = bs_same ? ibpt : ((op->ibs * ibpt) / op->obs);
        }
    }
    if (op->verbose > 1)
        pr2serr("do_xcopy: xcopy->%s will use ibpt=%d, obpt=%d\n",
                (ifp->xcopy ? idip->fn : odip->fn),  ibpt, obpt);
    seg_desc_type = seg_desc_from_d_type(simplified_dt(op->idip), 0,
                                         simplified_dt(op->odip), 0);

    res = 0;
    while (op->dd_count > 0) {
        blocks = (op->dd_count > ibpt) ? ibpt : op->dd_count;
        oblocks = bs_same ? blocks : ((op->ibs * blocks) / op->obs);

        res = scsi_extended_copy(op, src_desc, src_desc_len, dst_desc,
                                 dst_desc_len, seg_desc_type, blocks);
        if (res != 0) {
            if ((op->verbose > 0) && (op->verbose < 3)) {
                pr2serr("scsi_extended_copy: ");
                switch (res) {
                case SG_LIB_CAT_INVALID_OP:
                    pr2serr("invalid opcode\n");
                    break;
                case SG_LIB_CAT_ILLEGAL_REQ:
                    pr2serr("illegal request\n");
                    break;
                case SG_LIB_CAT_UNIT_ATTENTION:
                    pr2serr("unit attention\n");
                    break;
                case SG_LIB_CAT_NOT_READY:
                    pr2serr("not ready\n");
                    break;
                case SG_LIB_CAT_ABORTED_COMMAND:
                    pr2serr("aborted command\n");
                    break;
                default:
                    pr2serr("unknown result=%d\n", res);
                    break;
                }
                pr2serr("  use 'verbose=3' (or '-vvv') for more "
                        "information\n");
            }
            break;
        }
        op->in_full += blocks;
        op->out_full += oblocks;
        op->skip += blocks;
        op->seek += oblocks;
        op->num_xcopy++;
        op->dd_count -= blocks;
        if (op->dd_count > 0)
            signals_process_delay(op, DELAY_COPY_SEGMENT);
    }
    return res;
}

/* vvvvvvvvv  ODX [SBC-3's POPULATE TOKEN + WRITE USING TOKEN vvvvvvv */


const char *
cpy_op_status_str(int cos, char * b, int blen)
{
    const struct val_str_t * vsp;
    const char * p = NULL;

    for (vsp = xcopy4_cpy_op_status; vsp->name; ++vsp) {
        if (cos == vsp->num) {
            p = vsp->name;
            break;
        }
    }
    if (p)
        snprintf(b, blen, "%s", p);
    else
        snprintf(b, blen, "copy operation status 0x%x not found\n", cos);
    return b;
}

/* This is xcopy(LID4) related: "ROD" == Representation Of Data
 * Used by VPD_3PARTY_COPY */
static void
decode_rod_descriptor(const unsigned char * buff, int len)
{
    const unsigned char * ucp = buff;
    int k, bump, j;
    uint64_t ull;

    for (k = 0; k < len; k += bump, ucp += bump) {
        bump = (ucp[2] << 8) + ucp[3];
        switch (ucp[0]) {
            case 0:
                /* Block ROD device type specific descriptor */
                printf("   Optimal block ROD length granularity: %d\n",
                       (ucp[6] << 8) + ucp[7]);
                ull = 0;
                for (j = 0; j < 8; j++) {
                    if (j > 0)
                        ull <<= 8;
                    ull |= ucp[8 + j];
                }
                printf("  Maximum Bytes in block ROD: %" PRIu64 "\n", ull);
                ull = 0;
                for (j = 0; j < 8; j++) {
                    if (j > 0)
                        ull <<= 8;
                    ull |= ucp[16 + j];
                }
                printf("  Optimal Bytes in block ROD transfer: %" PRIu64 "\n",
                       ull);
                ull = 0;
                for (j = 0; j < 8; j++) {
                    if (j > 0)
                        ull <<= 8;
                    ull |= ucp[24 + j];
                }
                printf("  Optimal Bytes to token per segment: %" PRIu64 "\n",
                       ull);
                ull = 0;
                for (j = 0; j < 8; j++) {
                    if (j > 0)
                        ull <<= 8;
                    ull |= ucp[32 + j];
                }
                printf("  Optimal Bytes from token per segment:"
                       " %" PRIu64 "\n", ull);
                break;
            case 1:
                /* Stream ROD device type specific descriptor */
                ull = 0;
                for (j = 0; j < 8; j++) {
                    if (j > 0)
                        ull <<= 8;
                    ull |= ucp[8 + j];
                }
                printf("  Maximum Bytes in stream ROD: %" PRIu64 "\n", ull);
                ull = 0;
                for (j = 0; j < 8; j++) {
                    if (j > 0)
                        ull <<= 8;
                    ull |= ucp[16 + j];
                }
                printf("  Optimal Bytes in stream ROD transfer:"
                       " %" PRIu64 "\n", ull);
                break;
            case 3:
                /* Copy manager ROD device type specific descriptor */
                ull = 0;
                for (j = 0; j < 8; j++) {
                    if (j > 0)
                        ull <<= 8;
                    ull |= ucp[8 + j];
                }
                printf("  Maximum Bytes in processor ROD:"
                       " %" PRIu64 "\n", ull);
                ull = 0;
                for (j = 0; j < 8; j++) {
                    if (j > 0)
                        ull <<= 8;
                    ull |= ucp[16 + j];
                }
                printf("  Optimal Bytes in processor ROD transfer:"
                       " %" PRIu64 "\n", ull);
                break;
            default:
                printf("  Unhandled descriptor (format %d, device type %d)\n",
                       ucp[0] >> 5, ucp[0] & 0x1F);
                break;
        }
    }
}

/* VPD_3PARTY_COPY [3PC, third party copy] */
static void
decode_3party_copy_vpd(unsigned char * buff, int len, int do_hex, int verbose)
{
    int j, k, bump, desc_type, desc_len, sa_len;
    unsigned int u;
    const unsigned char * ucp;
    uint64_t ull;
    char b[80];

    if (len < 4) {
        pr2serr("Third-party Copy VPD page length too short=%d\n", len);
        return;
    }
    len -= 4;
    ucp = buff + 4;
    for (k = 0; k < len; k += bump, ucp += bump) {
        desc_type = (ucp[0] << 8) + ucp[1];
        desc_len = (ucp[2] << 8) + ucp[3];
        if (verbose)
            printf("Descriptor type=%d, len %d\n", desc_type, desc_len);
        bump = 4 + desc_len;
        if ((k + bump) > len) {
            pr2serr("Third-party Copy VPD page, short descriptor length=%d, "
                    "left=%d\n", bump, (len - k));
            return;
        }
        if (0 == desc_len)
            continue;
        if (2 == do_hex)
            dStrHex((const char *)ucp + 4, desc_len, 1);
        else if (do_hex > 2)
            dStrHex((const char *)ucp, bump, 1);
        else {
            switch (desc_type) {
            case 0x0000:    /* Required if POPULATE TOKEN (or friend) used */
                printf(" Block Device ROD Token Limits:\n");
                printf("  Maximum Range Descriptors: %d\n",
                       (ucp[10] << 8) + ucp[11]);
                u = (ucp[12] << 24) | (ucp[13] << 16) | (ucp[14] << 8) |
                    ucp[15];
                printf("  Maximum Inactivity Timeout: %u seconds\n", u);
                u = (ucp[16] << 24) | (ucp[17] << 16) | (ucp[18] << 8) |
                    ucp[19];
                printf("  Default Inactivity Timeout: %u seconds\n", u);
                ull = 0;
                for (j = 0; j < 8; j++) {
                    if (j > 0)
                        ull <<= 8;
                    ull |= ucp[20 + j];
                }
                printf("  Maximum Token Transfer Size: %" PRIu64 "\n", ull);
                ull = 0;
                for (j = 0; j < 8; j++) {
                    if (j > 0)
                        ull <<= 8;
                    ull |= ucp[28 + j];
                }
                printf("  Optimal Transfer Count: %" PRIu64 "\n", ull);
                break;
            case 0x0001:    /* Mandatory (SPC-4) */
                printf(" Supported Commands:\n");
                j = 0;
                while (j < ucp[4]) {
                    sa_len = ucp[6 + j];
                    for (k = 0; k < sa_len; k++) {
                        sg_get_opcode_sa_name(ucp[5 + j], ucp[7 + j + k],
                                              0, sizeof(b), b);
                        printf("   %s\n", b);
                    }
                    j += sa_len;
                }
                break;
            case 0x0004:
                printf(" Parameter Data:\n");
                printf("  Maximum CSCD Descriptor Count: %d\n",
                       (ucp[8] << 8) + ucp[9]);
                printf("  Maximum Segment Descriptor Count: %d\n",
                       (ucp[10] << 8) + ucp[11]);
                u = (ucp[12] << 24) | (ucp[13] << 16) | (ucp[14] << 8) |
                    ucp[15];
                printf("  Maximum Descriptor List Length: %u\n", u);
                u = (ucp[16] << 24) | (ucp[17] << 16) | (ucp[18] << 8) |
                    ucp[19];
                printf("  Maximum Inline Data Length: %u\n", u);
                break;
            case 0x0008:
                printf(" Supported Descriptors:\n");
                for (j = 0; j < ucp[4]; j++) {
                    printf("  0x%x\n", ucp[5 + j]);
                }
                break;
            case 0x000C:
                printf(" Supported CSCD IDs:\n");
                for (j = 0; j < (ucp[4] << 8) + ucp[5]; j += 2) {
                    u = (ucp[6 + j] << 8) | ucp[7 + j];
                    printf("  0x%04x\n", u);
                }
                break;
            case 0x0106:
                printf(" ROD Token Features:\n");
                printf("  Remote Tokens: %d\n", ucp[4] & 0x0f);
                u = (ucp[16] << 24) | (ucp[17] << 16) | (ucp[18] << 8) |
                    ucp[19];
                printf("  Minimum Token Lifetime: %u seconds\n", u);
                u = (ucp[20] << 24) | (ucp[21] << 16) | (ucp[22] << 8) |
                    ucp[23];
                printf("  Maximum Token Lifetime: %u seconds\n", u);
                u = (ucp[24] << 24) | (ucp[25] << 16) | (ucp[26] << 8) |
                    ucp[27];
                printf("  Maximum Token inactivity timeout: %d\n", u);
                decode_rod_descriptor(&ucp[48], (ucp[46] << 8) + ucp[47]);
                break;
            case 0x0108:
                printf(" Supported ROD Token and ROD Types:\n");
                for (j = 0; j < (ucp[6] << 8) + ucp[7]; j+= 64) {
                    u = (ucp[8 + j] << 24) | (ucp[8 + j + 1] << 16) |
                        (ucp[8 + j + 2] << 8) | ucp[8 + j + 3];
                    printf("  ROD Type %u:\n", u);
                    printf("    Internal: %s\n",
                           ucp[8 + j + 4] & 0x80 ? "yes" : "no");
                    printf("    Token In: %s\n",
                           ucp[8 + j + 4] & 0x02 ? "yes" : "no");
                    printf("    Token Out: %s\n",
                           ucp[8 + j + 4] & 0x01 ? "yes" : "no");
                    printf("    Preference: %d\n",
                           (ucp[8 + j + 6] << 8) + ucp[8 + j + 7]);
                }
                break;
            case 0x8001:    /* Mandatory (SPC-4) */
                printf(" General Copy Operations:\n");
                u = (ucp[4] << 24) | (ucp[5] << 16) | (ucp[6] << 8) |
                    ucp[7];
                printf("  Total Concurrent Copies: %u\n", u);
                u = (ucp[8] << 24) | (ucp[9] << 16) | (ucp[10] << 8) |
                    ucp[11];
                printf("  Maximum Identified Concurrent Copies: %u\n", u);
                u = (ucp[12] << 24) | (ucp[13] << 16) | (ucp[14] << 8) |
                    ucp[15];
                printf("  Maximum Segment Length: %u\n", u);
                ull = (1 << ucp[16]);
                printf("  Data Segment Granularity: %" PRIu64 "\n", ull);
                ull = (1 << ucp[17]);
                printf("  Inline Data Granularity: %" PRIu64 "\n", ull);
                break;
            case 0x9101:
                printf(" Stream Copy Operations:\n");
                u = (ucp[4] << 24) | (ucp[5] << 16) | (ucp[6] << 8) |
                    ucp[7];
                printf("  Maximum Stream Device Transfer Size: %u\n", u);
                break;
            case 0xC001:
                printf(" Held Data:\n");
                u = (ucp[4] << 24) | (ucp[5] << 16) | (ucp[6] << 8) |
                    ucp[7];
                printf("  Held Data Limit: %u\n", u);
                ull = (1 << ucp[8]);
                printf("  Held Data Granularity: %" PRIu64 "\n", ull);
                break;
            default:
                pr2serr("Unexpected type=%d\n", desc_type);
                dStrHexErr((const char *)ucp, bump, 1);
                break;
            }
        }
    }
}


/* Note this function passes back a malloc-ed buffer if it returns 0 and
 * fixed_b != *alloc_bp which caller should free. Returns 0 on success. */
static int
fetch_3pc_vpd(int fd, unsigned char * fixed_b, int fixed_blen,
              unsigned char ** alloc_bp, int verb)
{
    int res, len;
    unsigned char * rp;

    rp = fixed_b;
    if (alloc_bp)
        *alloc_bp = fixed_b;
    res = sg_ll_inquiry(fd, 0, 1, VPD_3PARTY_COPY, rp, fixed_blen, 1, verb);
    if (res) {
        if (SG_LIB_CAT_ILLEGAL_REQ == res)
            pr2serr("Third Party Copy VPD page not found\n");
        else
            pr2serr("3PARTY_COPY VPD inquiry failed with %d, try again "
                    "with '-vv'\n", res);
        return res;
    } else if (rp[1] != VPD_3PARTY_COPY) {
        pr2serr("invalid 3PARTY_COPY VPD response\n");
        return SG_LIB_CAT_MALFORMED;
    }
    len = ((rp[2] << 8) + rp[3]) + 4;
    if (len > fixed_blen) {
        rp = (unsigned char *)malloc(len);
        if (NULL == rp) {
            pr2serr("Not enough user memory for fetch_3pc_vpd\n");
            return SG_LIB_CAT_OTHER;
        }
        if (alloc_bp)
            *alloc_bp = rp;
        res = sg_ll_inquiry(fd, 0, 1, VPD_3PARTY_COPY, rp, len, 1, verb);
        if (res) {
            pr2serr("3PARTY_COPY VPD inquiry failed with %d\n", res);
            if (fixed_b != rp)
                free(rp);
            return res;
        }
    }
    return 0;
}

static int
check_3pc_vpd(struct opts_t * op, struct dev_info_t * dip)
{
    unsigned char rBuff[256];
    unsigned char * rp;
    unsigned char * ucp;
    int res, verb, n, len, bump, desc_type, desc_len, k, j;
    int found = 0;
    uint32_t max_ito = 0;
    uint64_t ull;

    verb = (op->verbose ? (op->verbose - 1) : 0);
    rp = rBuff;
    n = (int)sizeof(rBuff);
    res = fetch_3pc_vpd(dip->fd, rBuff, n, &rp, verb);
    if (res)
        return res;
    len = ((rp[2] << 8) + rp[3]) + 4;
    len -= 4;
    ucp = rp + 4;
    for (k = 0; k < len; k += bump, ucp += bump) {
        desc_type = (ucp[0] << 8) + ucp[1];
        desc_len = (ucp[2] << 8) + ucp[3];
        if (op->verbose > 4)
            printf("Descriptor type=%d, len %d\n", desc_type, desc_len);
        bump = 4 + desc_len;
        if ((k + bump) > len) {
            pr2serr("3PARTY_COPY Copy VPD page, short descriptor length=%d, "
                    "left=%d\n", bump, (len - k));
            if (rBuff != rp)
                free(rp);
            return SG_LIB_CAT_OTHER;
        }
        if (0 == desc_len)
            continue;
        switch (desc_type) {
        case 0x0000:    /* Block Device ROD Token Limits */
            ++found;
            if (op->verbose > 3) {
                pr2serr("3PARTY_COPY Copy VPD, Block Device ROD Token "
                        "Limits descriptor:\n");
                dStrHexErr((const char *)ucp, desc_len, 1);
            }
            if (desc_len < 32) {
                pr2serr("3PARTY_COPY Copy VPD, Block Device ROD Token "
                        "Limits descriptor, too short, want 32 got %d\n",
                        desc_len);
                break;
            }
            dip->odxp->max_range_desc = (ucp[10] << 8) + ucp[11];
            max_ito = (ucp[12] << 24) | (ucp[13] << 16) | (ucp[14] << 8) |
                      ucp[15];
            dip->odxp->max_inactivity_to = max_ito;
            dip->odxp->def_inactivity_to = (ucp[16] << 24) | (ucp[17] << 16) |
                                           (ucp[18] << 8) | ucp[19];
            ull = 0;
            for (j = 0; j < 8; j++) {
                if (j > 0)
                    ull <<= 8;
                ull |= ucp[20 + j];
            }
            dip->odxp->max_tok_xfer_size = ull;
            ull = 0;
            for (j = 0; j < 8; j++) {
                if (j > 0)
                    ull <<= 8;
                ull |= ucp[28 + j];
            }
            dip->odxp->optimal_xfer_count = ull;
            break;
        default:
            break;
        }
    }
    if (rBuff != rp)
        free(rp);
    if (! found) {
        pr2serr("Did not find Block Device ROD Token Limits descriptor in "
                "3PARTY_COPY Copy VPD page\n");
        return SG_LIB_CAT_OTHER;
    }
    if ((max_ito > 0) && (op->inactivity_to > max_ito)) {
        pr2serr("Block Device ROD Token Limits: maximum inactivity timeout "
                "(%" PRIu32 ") exceeded\n", max_ito);
        if (! op->iflagp->force) {
            pr2serr("... exiting; can override with 'force' flag\n");
            return SG_LIB_CAT_OTHER;
        }
    }
    return 0;
}

int
print_3pc_vpd(struct opts_t * op)
{
    unsigned char rBuff[256];
    unsigned char * rp;
    int res, verb, len;

    verb = (op->verbose ? (op->verbose - 1) : 0);
    res = fetch_3pc_vpd(op->idip->fd, rBuff, (int)sizeof(rBuff), &rp, verb);
    if (res)
        return res;
    len = ((rp[2] << 8) + rp[3]) + 4;
    decode_3party_copy_vpd(rp, len, 0, verb);
    if (rBuff != rp)
        free(rp);
    return res;
}

static uint64_t
count_sgl_blocks(const struct scat_gath_elem * sglp, int elems)
{
    int k;
    uint64_t num;

    for (k = 0, num = 0; k < elems; ++k, ++sglp)
        num += sglp->num;
    return num;
}

/* Return maximum number of blocks from the available num_blks that are
 * available in the scatter gather list, given several constraints. First
 * bypass blk_off blocks in the list. Then check that elems and
 * max_descriptors is not exceeded. If max_descriptors is 0 then it is not
 * constraining. The return value is always <= num_blks. */
static uint64_t
count_restricted_sgl_blocks(const struct scat_gath_elem * sglp, int elems,
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
            res = (uint64_t)sglp->num - blk_off;
        else
            res += (uint64_t)sglp->num;
    }
    return (res < (uint64_t)num_blks) ? res : (uint64_t)num_blks;
}

/* Do POPULATE_TOKEN command, returns 0 on success */
static int
do_pop_tok(struct opts_t * op, uint64_t sgl_off, uint32_t num_blks)
{
    int res, k, n, verb, len, fd, tmout, sz_bdrd, pl_sz;
    uint64_t lba;
    uint32_t num;
    const struct scat_gath_elem * sglp = NULL;
    unsigned char * pl;

    verb = (op->verbose > 1) ? (op->verbose - 2) : 0;
    if (verb > 2)
        pr2serr("%s: enter; sgl_off=%" PRIu64 ", num_blks=%"  PRIu32 "\n",
                __func__, sgl_off, num_blks);
    fd = op->idip->fd;
    if (op->in_sgl) {
        sglp = op->in_sgl;
        for (k = 0; k < op->in_sgl_elems; ++k, ++sglp) {
            if ((uint64_t)sglp->num > sgl_off)
                break;
            sgl_off -= sglp->num;
        }
        if (k >= op->in_sgl_elems) {
            pr2serr("%s: exhausted sgl_elems [%d], miscalculation\n",
                    __func__, op->in_sgl_elems);
            return SG_LIB_CAT_MALFORMED;
        }
    }
    pl_sz = 16 + (16 * (op->in_sgl ? (op->in_sgl_elems - k) : 1));
    pl = (unsigned char *)malloc(pl_sz);
    memset(pl, 0, pl_sz);
    if (op->rod_type_given) {
        pl[2] = 0x2;            /* RTV bit */
        pl[8] = (unsigned char)((op->rod_type >> 24) & 0xff);
        pl[9] = (unsigned char)((op->rod_type >> 16) & 0xff);
        pl[10] = (unsigned char)((op->rod_type >> 8) & 0xff);
        pl[11] = (unsigned char)(op->rod_type & 0xff);
    }
    if (op->iflagp->immed)
        pl[2] |= 0x1;           /* IMMED bit */
    pl[4] = (unsigned char)((op->inactivity_to >> 24) & 0xff);
    pl[5] = (unsigned char)((op->inactivity_to >> 16) & 0xff);
    pl[6] = (unsigned char)((op->inactivity_to >> 8) & 0xff);
    pl[7] = (unsigned char)(op->inactivity_to & 0xff);

    if (sglp) {
        for (k = 0, n = 15; num_blks > 0; ++k, num_blks -= num, ++sglp) {
            lba = sglp->lba;
            if (0 == k)
                lba += sgl_off;
            pl[++n] = (unsigned char)((lba >> 56) & 0xff);
            pl[++n] = (unsigned char)((lba >> 48) & 0xff);
            pl[++n] = (unsigned char)((lba >> 40) & 0xff);
            pl[++n] = (unsigned char)((lba >> 32) & 0xff);
            pl[++n] = (unsigned char)((lba >> 24) & 0xff);
            pl[++n] = (unsigned char)((lba >> 16) & 0xff);
            pl[++n] = (unsigned char)((lba >> 8) & 0xff);
            pl[++n] = (unsigned char)(lba & 0xff);
            num = sglp->num;
            if (num > num_blks)
                num = num_blks;
            pl[++n] = (unsigned char)((num >> 24) & 0xff);
            pl[++n] = (unsigned char)((num >> 16) & 0xff);
            pl[++n] = (unsigned char)((num >> 8) & 0xff);
            pl[++n] = (unsigned char)(num & 0xff);
            n += 4;
        }
        sz_bdrd = k * 16;
        pl[14] = (unsigned char)((sz_bdrd >> 8) & 0xff);
        pl[15] = (unsigned char)(sz_bdrd & 0xff);
        len = n + 1;
    } else {    /* assume count= and possibly skip= given */
        sz_bdrd = 16;       /* single element */
        pl[14] = (unsigned char)((sz_bdrd >> 8) & 0xff);
        pl[15] = (unsigned char)(sz_bdrd & 0xff);
        lba = op->skip + sgl_off;
        pl[16] = (unsigned char)((lba >> 56) & 0xff);
        pl[17] = (unsigned char)((lba >> 48) & 0xff);
        pl[18] = (unsigned char)((lba >> 40) & 0xff);
        pl[19] = (unsigned char)((lba >> 32) & 0xff);
        pl[20] = (unsigned char)((lba >> 24) & 0xff);
        pl[21] = (unsigned char)((lba >> 16) & 0xff);
        pl[22] = (unsigned char)((lba >> 8) & 0xff);
        pl[23] = (unsigned char)(lba & 0xff);
        pl[24] = (unsigned char)((num_blks >> 24) & 0xff);
        pl[25] = (unsigned char)((num_blks >> 16) & 0xff);
        pl[26] = (unsigned char)((num_blks >> 8) & 0xff);
        pl[27] = (unsigned char)(num_blks & 0xff);
        len = 32;
    }
    n = len - 2;
    pl[0] = (unsigned char)((n >> 8) & 0xff);
    pl[1] = (unsigned char)(n & 0xff);

    tmout = (op->timeout_xcopy < 1) ? DEF_3PC_OUT_TIMEOUT : op->timeout_xcopy;
    res = pt_3party_copy_out(fd, SA_POP_TOK, op->list_id, DEF_GROUP_NUM,
                             tmout, pl, len, 1, verb);
    free(pl);
    return res;
}

int
fetch_rrti_after_odx(struct opts_t * op, int in0_out1, int * for_sap,
                     int * cstatp, uint64_t * tc_p, unsigned char * rtp,
                     int max_rt_sz, int * rt_lenp, int verb)
{
    int j, res, fd, for_sa, cstat, lsdf, off, rt_len;
    unsigned int len, rtdl;
    uint64_t ull;
    unsigned char rsp[1024];
    char b[400];
    char bb[80];
    const char * cp;

    fd = in0_out1 ? op->odip->fd : op->idip->fd;
    res = pt_3party_copy_in(fd, SA_ROD_TOK_INFO, op->list_id,
                            DEF_3PC_IN_TIMEOUT, rsp, sizeof(rsp), 1, verb);
    if (res)
        return res;

    len = ((rsp[0] << 24) | (rsp[1] << 16) | (rsp[2] << 8) | rsp[3]) + 4;
    if (len > sizeof(rsp)) {
        pr2serr("RRTI: ROD Token info too long for internal buffer, output "
                "truncated\n");
        len = sizeof(rsp);
    }
    if (verb > 3) {
        pr2serr("\nRRTI response in hex:\n");
        dStrHexErr((const char *)rsp, len, 1);
    }
    for_sa = 0x1f & rsp[4];
    if (SA_POP_TOK == for_sa)
        cp = "RRTI for PT";
    else if (SA_WR_USING_TOK == for_sa)
        cp = "RRTI for WUT";
    else
        cp = "RRTI for non ODX service action";
    if (verb > 2)
        pr2serr("%s\n", cp);
    if (for_sap)
        *for_sap = for_sa;
    cstat = 0x7f & rsp[5];
    if (verb > 3)
       pr2serr("%s: %s\n", cp, cpy_op_status_str(cstat, b, sizeof(b)));
    if (cstatp)
        *cstatp = cstat;
    ull = 0;
    for (j = 0; j < 8; j++) {
        if (j > 0)
            ull <<= 8;
        ull |= rsp[16 + j];
    }
    if (tc_p)
        *tc_p = ull;
    lsdf = rsp[13];
    if (lsdf > 0) {
        snprintf(bb, sizeof(bb), "%s: sense data", cp);
        sg_get_sense_str(cp, rsp + 32, rsp[14], verb, sizeof(b), b);
        pr2serr("%s\n", b);
    }
    off = 32 + lsdf;
    rtdl = (rsp[off] << 24) | (rsp[off + 1] << 16) | (rsp[off + 2] << 8) |
           rsp[off + 3];
    if ((rtdl > 2) && (verb > 2))
        pr2serr("RRTI: ROD Token received, length=%d bytes\n", rtdl - 2);
    if ((rtdl > 2) && rtp && rt_lenp) {
        rt_len = rtdl - 2;
        memcpy(rtp, rsp + off + 6, (rt_len > max_rt_sz) ? max_rt_sz : rt_len);
        *rt_lenp = rt_len;
    } else if (rt_lenp)
        *rt_lenp = 0;
    return 0;
}

static int
fetch_rt_after_poptok(struct opts_t * op, uint64_t * tcp)
{
    int res, fd, rt_len, verb, for_sa, cstat, err, sz;
    uint64_t tc;
    unsigned char rt_buf[600];
    char b[400];

    verb = (op->verbose > 1) ? (op->verbose - 2) : 0;
    sz = (int)sizeof(rt_buf);
    res = fetch_rrti_after_odx(op, DDPT_ARG_IN, &for_sa, &cstat, &tc, rt_buf,
                               sz, &rt_len, verb);
    if (res)
        return res;
    if (SA_POP_TOK != for_sa) {
        sg_get_opcode_sa_name(THIRD_PARTY_COPY_OUT_CMD, for_sa, 0, sizeof(b),
                              b);
        pr2serr("Receive ROD Token info expected response for Populate "
                "Token\n  but got response for %s\n", b);
    }
    if ((! ((0x1 == cstat) || (0x3 == cstat))) || verb)
        pr2serr("for PT: %s\n", cpy_op_status_str(cstat, b, sizeof(b)));
    if (verb)
        pr2serr("for PT: Transfer count=%" PRIu64 " [0x%" PRIx64 "]\n", tc,
                tc);
    if (tcp)
        *tcp = tc;
    if (rt_len > sz)
        rt_len = sz;
    if (rt_len > 0) {
        if (op->rtf[0]) {     /* write ROD Token to RTF */
            fd = open(op->rtf, O_WRONLY | O_CREAT | O_TRUNC, 0644);
            if (fd < 0) {
                err = errno;
                pr2serr("%s: unable to create file: %s [%s]\n", __func__,
                        op->rtf, safe_strerror(err));
                return SG_LIB_FILE_ERROR;
            }
            res = write(fd, rt_buf, rt_len);
            if (res < 0) {
                err = errno;
                pr2serr("%s: unable to write to file: %s [%s]\n", __func__,
                        op->rtf, safe_strerror(err));
                close(fd);
                return SG_LIB_FILE_ERROR;
            }
            close(fd);
            if (res < rt_len) {
                pr2serr("%s: short write to file: %s, wanted %d, got %d\n",
                        __func__, op->rtf, rt_len, res);
                return SG_LIB_CAT_OTHER;
            }
        } else {        /* write ROD Token to static */
            if (rt_len > LOCAL_ROD_TOKEN_SIZE) {
                pr2serr("%s: ROD token too large for static storage, try "
                        "'rtf=RTF'\n", __func__);
                return SG_LIB_CAT_OTHER;
            }
            memcpy(local_rod_token, rt_buf, rt_len);
        }
    }
    return 0;
}

/* Do WRITE USING TOKEN command, returns 0 on success */
static int
do_wut(struct opts_t * op, uint64_t sgl_off, uint32_t num_blks, uint64_t oir)
{
    int verb, len, k, n, fd, err, res, tmout, sz_bdrd, pl_sz, rodt_blk_zero;
    struct flags_t * flp;
    uint64_t lba;
    uint32_t num;
    const struct scat_gath_elem * sglp = NULL;
    unsigned char * pl;
    unsigned char rt[512];

    verb = (op->verbose > 1) ? (op->verbose - 2) : 0;
    if (verb > 2)
        pr2serr("%s: enter; sgl_off=%" PRIu64 ", num_blks=%"  PRIu32 ", "
                " oir=0x%" PRIx64 "\n", __func__, sgl_off, num_blks, oir);
    fd = op->odip->fd;
    flp = op->oflagp;
    rodt_blk_zero = (RODT_BLK_ZERO == op->rod_type);
    if (op->out_sgl) {
        sglp = op->out_sgl;
        for (k = 0; k < op->out_sgl_elems; ++k, ++sglp) {
            if ((uint64_t)sglp->num > sgl_off)
                break;
            sgl_off -= sglp->num;
        }
        if (k >= op->out_sgl_elems) {
            pr2serr("%s: exhausted sgl_elems [%d], miscalculation\n",
                    __func__, op->out_sgl_elems);
            return SG_LIB_CAT_MALFORMED;
        }
    }
    pl_sz = 540 + (16 * (op->out_sgl ? (op->out_sgl_elems - k) : 1));
    pl = (unsigned char *)malloc(pl_sz);
    memset(pl, 0, pl_sz);
    if (! rodt_blk_zero) {
        if (ODX_REQ_WUT == op->odx_request) {
            if (flp->del_tkn)
                pl[2] = 0x2;        /* DEL_TKN bit */
        } else {
            if (! flp->no_del_tkn)
                pl[2] = 0x2;        /* data->data default DEL_TKN unless */
        }
    }
    if (flp->immed)
        pl[2] |= 0x1;           /* IMMED bit */
    if (oir) {          /* Offset in ROD field */
        pl[8] = (unsigned char)((oir >> 56) & 0xff);
        pl[9] = (unsigned char)((oir >> 48) & 0xff);
        pl[10] = (unsigned char)((oir >> 40) & 0xff);
        pl[11] = (unsigned char)((oir >> 32) & 0xff);
        pl[12] = (unsigned char)((oir >> 24) & 0xff);
        pl[13] = (unsigned char)((oir >> 16) & 0xff);
        pl[14] = (unsigned char)((oir >> 8) & 0xff);
        pl[15] = (unsigned char)(oir & 0xff);
    }
    if (rodt_blk_zero) {
        if (verb > 3)
            pr2serr("%s: configure for block device zero ROD Token\n",
                    __func__);
        local_rod_token[0] = (unsigned char)((RODT_BLK_ZERO >> 24) & 0xff);
        local_rod_token[1] = (unsigned char)((RODT_BLK_ZERO >> 16) & 0xff);
        local_rod_token[2] = (unsigned char)((RODT_BLK_ZERO >> 8) & 0xff);
        local_rod_token[3] = (unsigned char)(RODT_BLK_ZERO & 0xff);
        local_rod_token[6] = (unsigned char)(0x1);
        local_rod_token[7] = (unsigned char)(0xf8);
        memcpy(pl + 16, local_rod_token, 512);
    } else if (op->rtf[0]) {
        if ((fd = open(op->rtf, O_RDONLY)) < 0) {
            err = errno;
            pr2serr("could not open '%s' for reading: %s\n", op->rtf,
                    safe_strerror(err));
            return SG_LIB_FILE_ERROR;
        }
        res = read(fd, rt, sizeof(rt));
        if (res < 0) {
            err = errno;
            pr2serr("could not read '%s': %s\n", op->rtf,
                    safe_strerror(err));
            close(fd);
            return SG_LIB_FILE_ERROR;
        }
        if (res < (int)sizeof(rt))
            pr2serr("unable to read %d bytes from '%s', only got %d bytes\n",
                     (int)sizeof(rt), op->rtf, res);
        memcpy(pl + 16, rt, res);
        close(fd);
    } else
        memcpy(pl + 16, local_rod_token, 512);

    if (sglp) {
        for (k = 0, n = 535; num_blks > 0; ++k, num_blks -= num, ++sglp) {
            lba = sglp->lba;
            if (0 == k)
                lba += sgl_off;
            pl[++n] = (unsigned char)((lba >> 56) & 0xff);
            pl[++n] = (unsigned char)((lba >> 48) & 0xff);
            pl[++n] = (unsigned char)((lba >> 40) & 0xff);
            pl[++n] = (unsigned char)((lba >> 32) & 0xff);
            pl[++n] = (unsigned char)((lba >> 24) & 0xff);
            pl[++n] = (unsigned char)((lba >> 16) & 0xff);
            pl[++n] = (unsigned char)((lba >> 8) & 0xff);
            pl[++n] = (unsigned char)(lba & 0xff);
            num = sglp->num;
            if (num > num_blks)
                num = num_blks;
            pl[++n] = (unsigned char)((num >> 24) & 0xff);
            pl[++n] = (unsigned char)((num >> 16) & 0xff);
            pl[++n] = (unsigned char)((num >> 8) & 0xff);
            pl[++n] = (unsigned char)(num & 0xff);
            n += 4;
        }
        sz_bdrd = 16 * k;
        pl[534] = (unsigned char)((sz_bdrd >> 8) & 0xff);
        pl[535] = (unsigned char)(sz_bdrd & 0xff);
    } else {
        sz_bdrd = 16;   /* single element */
        pl[534] = (unsigned char)((sz_bdrd >> 8) & 0xff);
        pl[535] = (unsigned char)(sz_bdrd & 0xff);
        lba = op->seek + sgl_off;
        pl[536] = (unsigned char)((lba >> 56) & 0xff);
        pl[537] = (unsigned char)((lba >> 48) & 0xff);
        pl[538] = (unsigned char)((lba >> 40) & 0xff);
        pl[539] = (unsigned char)((lba >> 32) & 0xff);
        pl[540] = (unsigned char)((lba >> 24) & 0xff);
        pl[541] = (unsigned char)((lba >> 16) & 0xff);
        pl[542] = (unsigned char)((lba >> 8) & 0xff);
        pl[543] = (unsigned char)(lba & 0xff);
        pl[544] = (unsigned char)((num_blks >> 24) & 0xff);
        pl[545] = (unsigned char)((num_blks >> 16) & 0xff);
        pl[546] = (unsigned char)((num_blks >> 8) & 0xff);
        pl[547] = (unsigned char)(num_blks & 0xff);
    }
    len = 536 +  sz_bdrd;
    n = len - 2;
    pl[0] = (unsigned char)((n >> 8) & 0xff);
    pl[1] = (unsigned char)(n & 0xff);
    fd = op->odip->fd;

    tmout = (op->timeout_xcopy < 1) ? DEF_3PC_OUT_TIMEOUT : op->timeout_xcopy;
    res = pt_3party_copy_out(fd, SA_WR_USING_TOK, op->list_id, DEF_GROUP_NUM,
                             tmout, pl, len, 1, verb);
    free(pl);
    return res;
}

static int
fetch_rrti_after_wut(struct opts_t * op, uint64_t * tcp)
{
    int res, verb, for_sa, cstat;
    uint64_t tc;
    char b[80];

    verb = (op->verbose > 1) ? (op->verbose - 2) : 0;
    res = fetch_rrti_after_odx(op, DDPT_ARG_OUT, &for_sa, &cstat, &tc, NULL,
                               0, NULL, verb);
    if (res)
        return res;
    if (SA_WR_USING_TOK != for_sa) {
        sg_get_opcode_sa_name(THIRD_PARTY_COPY_OUT_CMD, for_sa, 0, sizeof(b),
                              b);
        pr2serr("Receive ROD Token info expected response for Write "
                "Using Token\n  but got response for %s\n", b);
    }
    if ((! ((0x1 == cstat) || (0x3 == cstat))) || verb)
        pr2serr("for WUT: %s\n", cpy_op_status_str(cstat, b, sizeof(b)));
    if (tcp)
        *tcp = tc;
    if (verb)
        pr2serr("for WUT: Transfer count=%" PRIu64 " [0x%" PRIx64 "]\n", tc,
                tc);
    return 0;
}

static int
odx_check_sgl(struct opts_t * op, uint64_t num_blks, int in0_out1)
{
    uint32_t allowed_descs;
    struct dev_info_t * dip = in0_out1 ? op->odip : op->idip;
    struct flags_t * flp = in0_out1 ? op->oflagp : op->iflagp;
    uint32_t num_elems = in0_out1 ? op->out_sgl_elems : op->in_sgl_elems;
    const char * sgl_nm = in0_out1 ? "scatter" : "gather";

    if ((op->dd_count >= 0) && ((uint64_t)op->dd_count != num_blks)) {
        pr2serr("%s: count= value not equal to the sum of %s "
                "nums\n", __func__, sgl_nm);
        return SG_LIB_SYNTAX_ERROR;
    }
    if ((! flp->force) && dip->odxp) {
        allowed_descs = dip->odxp->max_range_desc;
        if ((allowed_descs > 0) && (num_elems > allowed_descs)) {
            pr2serr("%s: number of %s list elements exceeds what the "
                    "Block Device ROD\nToken Limits descriptor in the 3PC "
                    "VPD page permits (%d).\nCan try '%cflag=force'\n",
                     __func__, sgl_nm, allowed_descs, (in0_out1 ? 'o' : 'i'));
            return SG_LIB_CAT_OTHER;
        }
    }
    return 0;
}

/* This is called when rod_type=zero which implies the input is a dummy
 * (require 'if=/dev/null') and we want to write block of zeros to the
 * destination. Returns 0 when successful. */
static int
odx_full_zero_copy(struct opts_t * op)
{
    int k, got_count, res, out_blk_sz, out_num_elems;
    struct dev_info_t * odip = op->odip;
    uint64_t out_sgl_off, num, tc;
    int64_t out_num_blks, v;

    k = dd_filetype(op->idip->fn, op->verbose);
    got_count = (op->dd_count > 0);
    if (FT_DEV_NULL != k) {
        pr2serr("For single WUT version of ODX write blocks of zeros, "
                "don't give if=IFILE option\n");
        pr2serr("For full copy version of ODX write blocks of zeros, "
                "give if=/dev/null or equivalent\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if ((res = pt_read_capacity(op, DDPT_ARG_OUT, &out_num_blks,
                                &out_blk_sz))) {
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            pr2serr("Unit attention (readcap out), continuing\n");
            res = pt_read_capacity(op, DDPT_ARG_OUT, &out_num_blks,
                                   &out_blk_sz);
        }
        if (res)
            return res;
    }
    if (op->verbose) {
        print_blk_sizes(odip->fn, "pt", out_num_blks, out_blk_sz, 1);
        if (odip->prot_type > 0)
            pr2serr("    reports Protection_type=%d, p_i_exp=%d\n",
                    odip->prot_type, odip->p_i_exp);
    }
    if ((out_num_blks > 0) && (op->obs != out_blk_sz)) {
        pr2serr(">> warning: %s block size confusion: obs=%d, device "
                "claims=%d\n", odip->fn, op->obs, out_blk_sz);
        if (0 == op->oflagp->force) {
            pr2serr(">> abort copy, use oflag=force to override\n");
            return -1;
        }
    }
    v = out_num_blks;
    if (op->out_sgl) {  /* scatter list */
        out_num_elems = op->out_sgl_elems;
        out_num_blks = count_sgl_blocks(op->out_sgl, out_num_elems);
    } else { /* no scatter list */
        out_num_elems = 1;
        out_num_blks = got_count ? op->dd_count : 0;
    }
    if (0 == op->dd_count) {
        if (op->verbose)
            pr2serr("%s: enough checks, count=0 given so exit\n", __func__);
        return 0;
    }
    if ((op->dd_count < 0) && (0 == out_num_blks)) {
        if (op->verbose > 1)
            pr2serr("%s: zero the lot after scaling for seek=\n", __func__);
        v -= op->seek;
        if (v < 0) {
            pr2serr("%s: seek exceeds out device size\n", __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        out_num_blks = v;
    }
    out_sgl_off = 0;
    op->dd_count = out_num_blks;
    if (op->verbose > 1)
        pr2serr("%s: about to zero %" PRIi64 " blocks\n", __func__,
                out_num_blks);

    for (k = 0; out_num_blks > 0; out_num_blks -= num, ++k) {
        num = out_num_blks;
        if (op->bpt_given && ((uint64_t)op->bpt_i < num))
            num = op->bpt_i;    /* in this case BPT refers to OFILE */
        if ((odip->odxp->max_tok_xfer_size > 0) &&
            (num > odip->odxp->max_tok_xfer_size))
            num = odip->odxp->max_tok_xfer_size;
        if (op->out_sgl)
            num = count_restricted_sgl_blocks(op->out_sgl, out_num_elems,
                                              out_sgl_off, num,
                                              odip->odxp->max_range_desc);
        if ((res = do_wut(op, out_sgl_off, num, 0)))
            return res;
        if ((res = fetch_rrti_after_wut(op, &tc)))
            return res;
        if (tc != num) {
            pr2serr("%s: number requested differs from transfer count\n",
                    __func__);
            // ouch, think about this one
        }
        op->out_full += tc;
        out_sgl_off += num;
        op->dd_count -= tc;
    }
    return 0;
}

/* This function is designed to copy large amounts (terabytes) with
 * potentially different block sizes on input and output. Returns
 * 0 on success. */
static int
odx_full_copy(struct opts_t * op)
{
    int k, res, ok, in_blk_sz, out_blk_sz, oneto1, in_mult, out_mult;
    int got_count, in_num_elems, out_num_elems;
    uint64_t in_sgl_off, out_sgl_off, num, o_num, r_o_num, oir, tc;
    int64_t in_num_blks, out_num_blks, u, uu, v, vv;
    struct dev_info_t * idip = op->idip;
    struct dev_info_t * odip = op->odip;

    if (op->rod_type_given && (RODT_BLK_ZERO == op->rod_type))
        return odx_full_zero_copy(op);
    got_count = (op->dd_count > 0);
    /* need to know block size of input and output */
    if ((res = pt_read_capacity(op, DDPT_ARG_IN, &in_num_blks, &in_blk_sz))) {
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            pr2serr("Unit attention (readcap in), continuing\n");
            res = pt_read_capacity(op, DDPT_ARG_IN, &in_num_blks,
                                   &in_blk_sz);
        }
        if (res)
            return res;
    }
    if (op->verbose) {
        print_blk_sizes(idip->fn, "pt", in_num_blks, in_blk_sz, 1);
        if (idip->prot_type > 0)
            pr2serr("    reports Protection_type=%d, p_i_exp=%d\n",
                    idip->prot_type, idip->p_i_exp);
    }
    if ((in_num_blks > 0) && (in_blk_sz != op->ibs)) {
        pr2serr(">> warning: %s block size confusion: ibs=%d, device "
                "claims=%d\n", idip->fn, op->ibs, in_blk_sz);
        if (0 == op->iflagp->force) {
            pr2serr(">> abort copy, use iflag=force to override\n");
            return -1;
        }
    }
    u = in_num_blks;
    if ((res = pt_read_capacity(op, DDPT_ARG_OUT, &out_num_blks,
                                &out_blk_sz))) {
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            pr2serr("Unit attention (readcap out), continuing\n");
            res = pt_read_capacity(op, DDPT_ARG_OUT, &out_num_blks,
                                   &out_blk_sz);
        }
        if (res)
            return res;
    }
    if (op->verbose) {
        print_blk_sizes(odip->fn, "pt", out_num_blks, out_blk_sz, 1);
        if (odip->prot_type > 0)
            pr2serr("    reports Protection_type=%d, p_i_exp=%d\n",
                    odip->prot_type, odip->p_i_exp);
    }
    if ((out_num_blks > 0) && (op->obs != out_blk_sz)) {
        pr2serr(">> warning: %s block size confusion: obs=%d, device "
                "claims=%d\n", odip->fn, op->obs, out_blk_sz);
        if (0 == op->oflagp->force) {
            pr2serr(">> abort copy, use oflag=force to override\n");
            return -1;
        }
    }
    v = out_num_blks;
    oneto1 = (in_blk_sz == out_blk_sz);
    in_mult = 0;        /* so (in_blk_sz < out_blk_sz) */
    out_mult = 0;
    if (! oneto1) {
        out_mult = in_blk_sz / out_blk_sz;
        if (out_mult > 0)
            ok = (in_blk_sz == (out_mult * out_blk_sz));
        else {
            in_mult = out_blk_sz / in_blk_sz;
            ok = (out_blk_sz == (in_mult * in_blk_sz));
        }
        if (! ok) {
            pr2serr("%s: only accept different block sizes if one is a "
                    "multiple of the other.\n input block size=%d and "
                    "output block size=%d\n", __func__, in_blk_sz,
                     out_blk_sz);
            return SG_LIB_CAT_OTHER;
        }
    }
    if (op->in_sgl) {   /* gather list */
        in_num_elems = op->in_sgl_elems;
        in_num_blks = count_sgl_blocks(op->in_sgl, in_num_elems);
        if (got_count && (in_num_blks != op->dd_count)) {
            pr2serr("%s: count= value not equal to the sum of gather nums\n",
                    __func__);
            return SG_LIB_CAT_OTHER;
        }
    } else {
        in_num_elems = 1;
        in_num_blks = got_count ? op->dd_count : 0;
    }
    if (op->out_sgl) {  /* scatter list */
        out_num_elems = op->out_sgl_elems;
        out_num_blks = count_sgl_blocks(op->out_sgl, out_num_elems);
        if (oneto1) {
            if(got_count && (out_num_blks != op->dd_count)) {
                pr2serr("%s: count= value not equal to the sum of scatter "
                        "nums\n", __func__);
                return SG_LIB_SYNTAX_ERROR;
            }
            if (out_num_blks != in_num_blks) {
                pr2serr("%s: number of blocks in gather list differ from "
                        "scatter list\n", __func__);
                return SG_LIB_SYNTAX_ERROR;
            }
        } else { /* unequal block size */
            u = out_blk_sz * out_num_blks;
            if (op->in_sgl && (u != (in_blk_sz * in_num_blks))) {
                pr2serr("%s: number of blocks in both lists need to reflect "
                        "the same number of bytes, but don't\n", __func__);
                return SG_LIB_SYNTAX_ERROR;
            }
            if (got_count && (u != (in_blk_sz * in_num_blks))) {
                pr2serr("%s: number of scatter blocks and count need to "
                        "reflect the same number of bytes, but don't\n",
                         __func__);
                return SG_LIB_SYNTAX_ERROR;
            }
        }
    } else { /* no scatter list */
        out_num_elems = 1;
        if (got_count) {
            if (oneto1)
                out_num_blks = op->dd_count;
            else if (in_mult)
                out_num_blks = op->dd_count * out_blk_sz / in_blk_sz;
            else
                out_num_blks = op->dd_count * in_blk_sz / out_blk_sz;
        } else
            out_num_blks = 0;
    }
    if (0 == op->dd_count) {
        if (op->verbose)
            pr2serr("%s: enough checks, count=0 given so exit\n", __func__);
        return 0;
    }
    if ((op->dd_count < 0) && (0 == in_num_blks) && (0 == out_num_blks)) {
        if (op->verbose > 1)
            pr2serr("%s: copy the lot after scaling for skip= and seek=\n",
                    __func__);
        u -= op->skip;
        v -= op->seek;
        if (u < 0) {
            pr2serr("%s: skip exceeds input device size\n", __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        if (v < 0) {
            pr2serr("%s: seek exceeds out device size\n", __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        if (oneto1) {
            in_num_blks = (u < v) ? u : v;
            out_num_blks = in_num_blks;
        } else {
            uu = u * in_blk_sz;
            vv = v * out_blk_sz;
            if (uu == vv) {
                in_num_blks = u;
                out_num_blks = v;
            } else if (uu < vv) {
                in_num_blks = u;
                out_num_blks = uu / out_blk_sz;
            } else {
                in_num_blks = vv / in_blk_sz;
                out_num_blks = v;
            }
        }
    }

    in_sgl_off = 0;
    out_sgl_off = 0;
    op->dd_count = in_num_blks;
    if (op->verbose > 1)
        pr2serr("%s: about to copy %" PRIi64 " blocks (seen from input)\n",
                    __func__, in_num_blks);

    /* copy using PT, WUT, [WUT, ...], PT, WUT, [WUT, ...] sequence */
    for (k = 0; in_num_blks > 0; in_num_blks -= num, ++k) {
        num = in_num_blks;
        if (op->bpt_given && ((uint64_t)op->bpt_i < num))
            num = op->bpt_i;
        if ((idip->odxp->max_tok_xfer_size > 0) &&
            (num > idip->odxp->max_tok_xfer_size))
            num = idip->odxp->max_tok_xfer_size;
        if (op->in_sgl)
            num = count_restricted_sgl_blocks(op->in_sgl, in_num_elems,
                                              in_sgl_off, num,
                                              idip->odxp->max_range_desc);
        if (! oneto1) {
            if (in_mult) {
                o_num = num / in_mult;
                num = o_num * in_mult;
                if (0 == num) {
                    if (in_num_blks < in_mult) {
                        pr2serr("%s: unable to copy trailing blocks due to "
                                "block size mismatch\n", __func__);
                        return 0;
                    } else {
                        pr2serr("%s: block size mismatch problem, perhaps "
                                "BPT value too small\n", __func__);
                        return SG_LIB_SYNTAX_ERROR;
                    }
                }
            } else      /* out_mult must be >= 2 */
                o_num = num * out_mult;
        } else
            o_num = num;
        if (op->verbose > 2)
            pr2serr("%s: k=%d, in_sgl_off=0x%" PRIx64 ", i_num=%" PRIu64 ", "
                    "out_sgl_off=0x%" PRIx64 ", o_num=%" PRIu64 "\n",
                    __func__, k, in_sgl_off, num, out_sgl_off, o_num);

        if ((res = do_pop_tok(op, in_sgl_off, num)))
            return res;
        if ((res = fetch_rt_after_poptok(op, &tc)))
            return res;
        if (tc != num) {
            pr2serr("%s: number requested (in) differs from transfer "
                    "count\n", __func__);
            // ouch, think about this one
        }
        op->in_full += tc;
        op->dd_count -= tc;
        in_sgl_off += num;

        for (oir = 0; o_num > 0; oir += r_o_num, o_num -= r_o_num) {
            /* output dev might be more restricted than input, so multiple
             * WUT calls (latter ones using offset in ROD) may be needed */
            r_o_num = o_num;
            if ((odip->odxp->max_tok_xfer_size > 0) &&
                (r_o_num > odip->odxp->max_tok_xfer_size))
                r_o_num = odip->odxp->max_tok_xfer_size;
            if (op->out_sgl)
                r_o_num = count_restricted_sgl_blocks(op->out_sgl,
                                out_num_elems, out_sgl_off, r_o_num,
                                odip->odxp->max_range_desc);
            if ((res = do_wut(op, out_sgl_off, r_o_num, oir)))
                return res;
            if ((res = fetch_rrti_after_wut(op, &tc)))
                return res;
            if (tc != r_o_num) {
                pr2serr("%s: number requested (in) differs from transfer "
                        "count\n", __func__);
                // ouch, think about this one
            }
            op->out_full += tc;
            out_sgl_off += r_o_num;
        }
    }
    return 0;
}

static int
odx_setup(struct opts_t * op)
{
    int fd, res, num_elems, req;
    struct dev_info_t * dip;
    uint64_t num_blks, tc;
    uint64_t in_max_xfer = 0;
    uint64_t  out_max_xfer = 0;

    req = op->odx_request;
    if (! op->list_id_given)
        op->list_id = (ODX_REQ_WUT == req) ? 0x102 : 0x101;
    if ((ODX_REQ_PT == req) ||
        ((ODX_REQ_COPY == req) && (RODT_BLK_ZERO != op->rod_type))) {
        fd = pt_open_if(op, NULL);
        if (-1 == fd)
            return SG_LIB_FILE_ERROR;
        else if (fd < -1)
            return SG_LIB_CAT_OTHER;
        dip = op->idip;
        dip->fd = fd;
        dip->odxp = (struct block_rodtok_vpd *)malloc(sizeof(*dip->odxp));
        if (NULL == dip->odxp) {
            pr2serr("Not enough user memory for do_odx_copy\n");
            return SG_LIB_CAT_OTHER;
        }
        memset(dip->odxp, 0, sizeof(*dip->odxp));
        res = check_3pc_vpd(op, dip);
        if (res && (op->iflagp->force < 2))
            return res;
        in_max_xfer = dip->odxp->max_tok_xfer_size;
    }
    if ((ODX_REQ_WUT == req) || (ODX_REQ_COPY == req)) {
        fd = pt_open_of(op, NULL);
        if (-1 == fd)
            return SG_LIB_FILE_ERROR;
        else if (fd < -1)
            return SG_LIB_CAT_OTHER;
        dip = op->odip;
        dip->fd = fd;
        dip->odxp = (struct block_rodtok_vpd *)malloc(sizeof(*dip->odxp));
        if (NULL == dip->odxp) {
            pr2serr("Not enough user memory for do_odx_copy\n");
            return SG_LIB_CAT_OTHER;
        }
        memset(dip->odxp, 0, sizeof(*dip->odxp));
        res = check_3pc_vpd(op, dip);
        if (res && (op->oflagp->force < 2))
            return res;
        out_max_xfer = dip->odxp->max_tok_xfer_size;
    }

    if (ODX_REQ_PT == req) {
        if (op->bpt_given)
            pr2serr("warning: bpt=BPT ignored for ODX PT\n");
        if (op->in_sgl) {
            num_elems = op->in_sgl_elems;
            num_blks = count_sgl_blocks(op->in_sgl, num_elems);
            if ((res = odx_check_sgl(op, num_blks, 1)))
                return res;
            op->dd_count = (int64_t)num_blks;
        } else if (op->dd_count > 0)
            num_blks = op->dd_count;
        else {
            pr2serr("for ODX PT require either a count= (>0) or a gather "
                    "list\n");
            return SG_LIB_SYNTAX_ERROR;
        }
        if (num_blks > in_max_xfer)
            pr2serr("warning: number of blocks requested [%" PRIu64 "] "
                    "exceeds\n   VPD page maximum token transfer size [%"
                    PRIu64 "], may fail\n", num_blks, in_max_xfer);
        if ((res = do_pop_tok(op, 0, num_blks)))
            return res;
        else if (op->iflagp->immed)
            return 0;
        if ((res = fetch_rt_after_poptok(op, &tc)))
            return res;
        op->in_full += tc;
        op->dd_count -= tc;
    } else if (ODX_REQ_WUT == req) {
        if (op->bpt_given)
            pr2serr("warning: bpt=BPT ignored for ODX WUT\n");
        if (op->out_sgl) {
            num_elems = op->out_sgl_elems;
            num_blks = count_sgl_blocks(op->out_sgl, num_elems);
            if ((res = odx_check_sgl(op, num_blks, 0)))
                return res;
            op->dd_count = (int64_t)num_blks;
        } else if (op->dd_count > 0)
            num_blks = op->dd_count;
        else {
            pr2serr("for ODX WUT require either a count= (>0) or a scatter "
                    "list\n");
            return SG_LIB_SYNTAX_ERROR;
        }
        if (num_blks > out_max_xfer)
            pr2serr("warning: number of blocks requested [%" PRIu64 "] "
                    "exceeds\n   VPD page maximum token transfer size [%"
                    PRIu64 "], may fail\n", num_blks, out_max_xfer);
        if ((res = do_wut(op, 0, num_blks, 0)))
            return res;
        else if (op->oflagp->immed)
            return 0;
        if ((res = fetch_rrti_after_wut(op, &tc)))
            return res;
        op->out_full += tc;
        op->dd_count -= tc;
    } else if (ODX_REQ_COPY == req) {
        if ((res = odx_full_copy(op)))
            return res;
    }
    return 0;
}


/* Called from main() in ddpt.c . Returns 0 on success or a positive
 * errno value if problems. This is for ODX which is a subset of
 * xcopy(LID4) for disk->disk, disk->held and held-> disk copies. */
int
do_odx(struct opts_t * op)
{
    int ret, in_immed, out_immed, time_useful, full_cp;

    full_cp = (ODX_REQ_COPY == op->odx_request);
    in_immed = (ODX_REQ_PT == op->odx_request) && op->iflagp->immed;
    out_immed = ((ODX_REQ_WUT == op->odx_request) || full_cp) &&
                 op->oflagp->immed;
    if (in_immed && out_immed) {
        pr2serr("Can't do iflag=immed -and_ oflag=immed\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if (in_immed)
        time_useful = 0;
    else if (out_immed && (! full_cp))
        time_useful = 0;
    else
        time_useful = 1;
    if (op->do_time && time_useful)
        calc_duration_init(op);
    ret = odx_setup(op);
    if (time_useful && (0 == op->status_none))
        print_stats("", op, (out_immed && full_cp));
    if (op->do_time && time_useful)
        calc_duration_throughput("", 0, op);
    if (in_immed) {
        op->dd_count = 0;       /* mute early termination report */
        pr2serr("Started ODX read (populate token) in immediate "
                "mode.\nUser may need list_id=%d for completion\n",
                op->list_id);
    } else if (out_immed) {
        op->dd_count = 0;       /* mute early termination report */
        pr2serr("Started ODX write (write using token) in immediate "
                "mode.\nUser may need list_id=%d for completion\n",
                op->list_id);
    }
    return ret;
}
