/*
 * Copyright (c) 2013-2023, Douglas Gilbert
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
 * This file contains SCSI Extended copy command helper functions for ddpt.
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
#include "sg_unaligned.h"
#include "sg_pr2serr.h"


#ifndef UINT32_MAX
#define UINT32_MAX ((uint32_t)-1)
#endif

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

#define MAX_IN_PROGRESS 10


#define LOCAL_ROD_TOKEN_SIZE 1024
static uint8_t local_rod_token[LOCAL_ROD_TOKEN_SIZE];

static const char * rec_copy_op_params_str = "Receive copy operating "
                                             "parameters";

static const struct val_str_t xcopy4_cpy_op_status[] = {
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
scsi_encode_seg_desc(struct opts_t * op, uint8_t *seg_desc, int seg_desc_type,
                     int64_t num_blk, uint64_t src_lba, uint64_t dst_lba)
{
    int seg_desc_len = 0;

    seg_desc[0] = seg_desc_type;
    if (op->xc_cat)
        seg_desc[1] = 0x1;
    if (op->xc_dc)
        seg_desc[1] |= 0x2;
    if (seg_desc_type == 0x02) {
        seg_desc_len = 0x18;
        seg_desc[4] = 0;
        seg_desc[5] = 0; /* Source target index */
        seg_desc[7] = 1; /* Destination target index */
        sg_put_unaligned_be16((uint16_t)num_blk, seg_desc + 10);
        sg_put_unaligned_be64(src_lba, seg_desc + 12);
        sg_put_unaligned_be64(dst_lba, seg_desc + 20);
    }
    sg_put_unaligned_be16((uint16_t)seg_desc_len, seg_desc + 2);

    return seg_desc_len + 4;
}

static int
a_xcopy_lid1_cmd(struct opts_t * op, uint8_t *src_desc, int src_desc_len,
                 uint8_t *dst_desc, int dst_desc_len, int seg_desc_type,
                 int64_t num_blk)
{
    uint8_t xcopyBuff[256];
    int desc_offset = 16;
    int seg_desc_len, verb, err_vb, fd, tmout;
    uint64_t src_lba = op->i_sgli.lowest_lba;
    uint64_t dst_lba = op->o_sgli.lowest_lba;

    fd = (op->iflagp->xcopy) ? op->idip->fd : op->odip->fd;
    verb = (op->verbose > 1) ? (op->verbose - 2) : 0;
    if (op->verbose > 0)
        err_vb = (verb > 1) ? verb : 1;
    else
        err_vb = 0;
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
    if (verb)
        pr2serr("xcopy(LID1) cmd; src_lba=0x%" PRIx64 ", num_blks=%" PRId64
                "\n", src_lba, num_blk);
    return pt_3party_copy_out(fd, SA_XCOPY_LID1, op->list_id, DEF_GROUP_NUM,
                              tmout, xcopyBuff, desc_offset, true /* noisy */,
                              verb, err_vb);
}

/* Returns target descriptor variety encoded into an int. There may be
 * more than one, OR-ed together. A return value of zero or less is
 * considered as an error. */
static int
scsi_operating_parameter(struct opts_t * op, bool is_dest)
{
    bool valid = false;
    int res, fd, ftype, pdt, snlid, verb;
    uint8_t rcBuff[256];
    unsigned int rcBuffLen = 256, len, n, td_list = 0;
    uint32_t num, max_target_num, max_segment_num, max_segment_len;
    uint32_t max_desc_len, max_inline_data, held_data_limit;
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
                            rcBuff, rcBuffLen, true /* noisy */, verb,
                            op->verbose);
    if (0 != res)
        return -res;

    len = sg_get_unaligned_be32(rcBuff + 0) + 4;
    if (len > rcBuffLen) {
        pr2serr("  <<report len %d > %d too long for internal buffer, output "
                "truncated\n", len, rcBuffLen);
    }
    if (op->verbose > 2) {
        pr2serr("\nOutput %s response in hex:\n", rec_copy_op_params_str);
        hex2stderr(rcBuff, len, 1);
    }
    snlid = rcBuff[4] & 0x1;
    max_target_num = sg_get_unaligned_be16(rcBuff + 8);
    max_segment_num = sg_get_unaligned_be16(rcBuff + 10);
    max_desc_len = sg_get_unaligned_be32(rcBuff + 12);
    max_segment_len = sg_get_unaligned_be32(rcBuff + 16);
    dip->xc_max_bytes = max_segment_len ? max_segment_len : UINT32_MAX;
    max_inline_data = sg_get_unaligned_be32(rcBuff + 20);
    if (op->verbose) {
        pr2serr(" >> %s, %sput [%s]:\n", rec_copy_op_params_str,
                (is_dest ? "out" : "in"), dip->fn);
        pr2serr("    Support No List IDentifier (SNLID): %d\n", snlid);
        pr2serr("    Maximum target descriptor count: %" PRIu32 "\n",
                max_target_num);
        pr2serr("    Maximum segment descriptor count: %" PRIu32 "\n",
                max_segment_num);
        pr2serr("    Maximum descriptor list length: %" PRIu32 "\n",
                max_desc_len);
        pr2serr("    Maximum segment length: %" PRIu32 "\n", max_segment_len);
        pr2serr("    Maximum inline data length: %" PRIu32 "\n",
                max_inline_data);
    }
    held_data_limit = sg_get_unaligned_be32(rcBuff + 24);
    if (op->id_usage < 0) {
        if (! held_data_limit)
            op->id_usage = 2;
        else
            op->id_usage = 0;
    }
    if (op->verbose) {
        pr2serr("    Held data limit: %" PRIu32 " (list_id_usage: %d)\n",
                held_data_limit, op->id_usage);
        num = sg_get_unaligned_be32(rcBuff + 28);
        pr2serr("    Maximum stream device transfer size: %" PRIu32 "\n",
                num);
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
                valid = true;
            if (is_dest && (ftype & FT_TAPE))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy Block to Stream device\n");
            break;
        case 0x01: /* copy stream to block device */
            if (!is_dest && (ftype & FT_TAPE))
                valid = true;
            if (is_dest && (ftype & FT_BLOCK))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy Stream to Block device\n");
            break;
        case 0x02: /* copy block to block device */
            if (!is_dest && (ftype & FT_BLOCK))
                valid = true;
            if (is_dest && (ftype & FT_BLOCK))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy Block to Block device\n");
            break;
        case 0x03: /* copy stream to stream device */
            if (!is_dest && (ftype & FT_TAPE))
                valid = true;
            if (is_dest && (ftype & FT_TAPE))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy Stream to Stream device\n");
            break;
        case 0x04: /* copy inline data to stream device */
            if (!is_dest && (ftype & FT_REG))
                valid = true;
            if (is_dest && (ftype & FT_TAPE))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy inline data to Stream device\n");
            break;
        case 0x05: /* copy embedded data to stream device */
            if (!is_dest && (ftype & FT_REG))
                valid = true;
            if (is_dest && (ftype & FT_TAPE))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy embedded data to Stream device\n");
            break;
        case 0x06: /* Read from stream device and discard */
            if (!is_dest && (ftype & FT_TAPE))
                valid = true;
            if (is_dest && (ftype & FT_DEV_NULL))
                valid = true;
            if (op->verbose)
                pr2serr("        Read from stream device and discard\n");
            break;
        case 0x07: /* Verify block or stream device operation */
            if (!is_dest && (ftype & (FT_TAPE | FT_BLOCK)))
                valid = true;
            if (is_dest && (ftype & (FT_TAPE | FT_BLOCK)))
                valid = true;
            if (op->verbose)
                pr2serr("        Verify block or stream device operation\n");
            break;
        case 0x08: /* copy block device with offset to stream device */
            if (!is_dest && (ftype & FT_BLOCK))
                valid = true;
            if (is_dest && (ftype & FT_TAPE))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy block device with offset to stream "
                        "device\n");
            break;
        case 0x09: /* copy stream device to block device with offset */
            if (!is_dest && (ftype & FT_TAPE))
                valid = true;
            if (is_dest && (ftype & FT_BLOCK))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy stream device to block device with "
                        "offset\n");
            break;
        case 0x0a: /* copy block device with offset to block device with
                    * offset */
            if (!is_dest && (ftype & FT_BLOCK))
                valid = true;
            if (is_dest && (ftype & FT_BLOCK))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy block device with offset to block "
                        "device with offset\n");
            break;
        case 0x0b: /* copy block device to stream device and hold data */
            if (!is_dest && (ftype & FT_BLOCK))
                valid = true;
            if (is_dest && (ftype & FT_TAPE))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy block device to stream device and hold "
                        "data\n");
            break;
        case 0x0c: /* copy stream device to block device and hold data */
            if (!is_dest && (ftype & FT_TAPE))
                valid = true;
            if (is_dest && (ftype & FT_BLOCK))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy stream device to block device and hold "
                        "data\n");
            break;
        case 0x0d: /* copy block device to block device and hold data */
            if (!is_dest && (ftype & FT_BLOCK))
                valid = true;
            if (is_dest && (ftype & FT_BLOCK))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy block device to block device and hold "
                        "data\n");
            break;
        case 0x0e: /* copy stream device to stream device and hold data */
            if (!is_dest && (ftype & FT_TAPE))
                valid = true;
            if (is_dest && (ftype & FT_TAPE))
                valid = true;
            if (op->verbose)
                pr2serr("        Copy block device to block device and hold "
                        "data\n");
            break;
        case 0x0f: /* read from stream device and hold data */
            if (!is_dest && (ftype & FT_TAPE))
                valid = true;
            if (is_dest && (ftype & FT_DEV_NULL))
                valid = true;
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
    if (! valid) {
        pr2serr(">> no matching target descriptor supported\n");
        td_list = 0;
    }
    return td_list;
}

/* build xcopy(lid1) CSCD descriptor using device id VPD page */
static int
desc_from_vpd_id(struct opts_t * op, uint8_t *desc, int desc_len,
                 bool is_dest)
{
    int fd, res, u, i_len, assoc, desig, verb, resid;
    uint8_t rcBuff[256], *bp, *best = NULL;
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
    block_size = is_dest ? op->obs_lb : op->ibs_lb;
    memset(rcBuff, 0xff, len);
    res = sg_ll_inquiry_v2(fd, true, VPD_DEVICE_ID, rcBuff, 4, 0, &resid,
                           true, verb);
    if (0 != res) {
        if (SG_LIB_CAT_ILLEGAL_REQ == res)
            pr2serr("Device identification VPD page not found [%s]\n",
                    dip->fn);
        else
            pr2serr("VPD inquiry failed with %d [%s] , try again with "
                    "'-vv'\n", res, dip->fn);
        return res;
    } else if (resid > 0) {
        pr2serr("VPD response: resid indicates less than 4 bytes fetched\n");
        return SG_LIB_CAT_MALFORMED;
    } else if (rcBuff[1] != VPD_DEVICE_ID) {
        pr2serr("invalid VPD response\n");
        return SG_LIB_CAT_MALFORMED;
    }
    len = sg_get_unaligned_be16(rcBuff + 2) + 4;
    res = sg_ll_inquiry_v2(fd, true, VPD_DEVICE_ID, rcBuff, len, 0, &resid,
                           true, verb);
    if (0 != res) {
        pr2serr("VPD inquiry failed with %d\n", res);
        return res;
    } else if ((len - resid) < 4) {
        pr2serr("VPD response length only %d bytes\n", len - resid);
        return SG_LIB_CAT_MALFORMED;
    } else if (rcBuff[1] != VPD_DEVICE_ID) {
        pr2serr("invalid VPD response\n");
        return SG_LIB_CAT_MALFORMED;
    }
    if (op->verbose > 2) {
        pr2serr("Output VPD_DEVICE_ID (0x83) page in hex:\n");
        hex2stderr(rcBuff, len, 1);
    }

    while ((u = sg_vpd_dev_id_iter(rcBuff + 4, len - 4, &off, 0, -1, -1)) ==
           0) {
        bp = rcBuff + 4 + off;
        i_len = bp[3];
        if (((unsigned int)off + i_len + 4) > len) {
            pr2serr("    VPD page error: designator length %d longer than\n"
                    "     remaining response length=%d\n", i_len,
                    (len - off));
            return SG_LIB_CAT_MALFORMED;
        }
        assoc = ((bp[1] >> 4) & 0x3);
        desig = (bp[1] & 0xf);
        if (op->verbose > 2)
            pr2serr("    Desc %d: assoc %u desig %u len %d\n", off, assoc,
                    desig, i_len);
        /* Identification descriptor's Designator length must be <= 20. */
        if (i_len > 20)
            continue;
        if (desig == /*NAA=*/ 3) {
            best = bp;
            best_len = i_len;
            break;
        }
        if (desig == /*EUI64=*/ 2) {
            if (!best || f_desig < 2) {
                best = bp;
                best_len = i_len;
                f_desig = 2;
            }
        } else if (desig == /*T10*/ 1) {
            if (!best || f_desig == 0) {
                best = bp;
                best_len = i_len;
                f_desig = desig;
            }
        } else if (desig == /*Vendor specific=*/ 0) {
            if (!best) {
                best = bp;
                best_len = i_len;
                f_desig = desig;
            }
        }
    }
    if (best) {
        if (op->verbose)
            decode_designation_descriptor(best, best_len,
                                          true /* to_stderr */, op->verbose);
        if (best_len + 4 < desc_len) {
            memset(desc, 0, 32);
            desc[0] = 0xe4; /* Identification Descriptor */
            memcpy(desc + 4, best, best_len + 4);
            desc[4] &= 0x0f; /* code set */
            desc[5] &= 0x3f; /* association and designator type */
            if (flp->pad)
                desc[28] = 0x4;
            sg_put_unaligned_be24((uint32_t)block_size, desc + 29);
            if (op->verbose > 3) {
                pr2serr("Descriptor in hex (bs %d):\n", block_size);
                hex2stderr(desc, 32, 1);
            }
            return 32;
        }
        return  best_len + 8;
    }
    return 0;
}

/* Called from main() in ddpt.c . Returns 0 on success or a positive
 * errno value if problems. This is for a xcopy(LID1) disk-->disk copy. */
int
do_xcopy_lid1(struct opts_t * op)
{
    int res, ibpt, obpt, bs_same, max_bpt, blocks, oblocks;
    int src_desc_len, dst_desc_len, seg_desc_type;
    int vb = op->verbose;
    uint8_t src_desc[256];
    uint8_t dst_desc[256];
    const struct flags_t * ifp = op->iflagp;
    const struct flags_t * ofp = op->oflagp;
    const struct dev_info_t * idip = op->idip;
    const struct dev_info_t * odip = op->odip;
    struct cp_state_t * csp;
    struct cp_state_t cp_st;

    csp = &cp_st;
    cp_state_init(csp, op);
    memset(csp, 0, sizeof(*csp));
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
    res = scsi_operating_parameter(op, false /* is_dest */);
    if (SG_LIB_CAT_UNIT_ATTENTION == -res)
        res = scsi_operating_parameter(op, false);
    if (res < 0) {
        if (-res == SG_LIB_CAT_INVALID_OP) {
            pr2serr("%s command not supported on %s\n",
                    rec_copy_op_params_str, idip->fn);
            return EINVAL;
        } else if (-res == SG_LIB_CAT_NOT_READY)
            pr2serr("%s failed on %s - not ready\n",
                    rec_copy_op_params_str, idip->fn);
        else {
            pr2serr("Unable to %s on %s\n", rec_copy_op_params_str, idip->fn);
            return -res;
        }
    } else if (res == 0)
        return SG_LIB_CAT_INVALID_OP;
    if (res & TD_VPD) {
        if (vb)
            pr2serr("  >> using VPD identification for source %s\n",
                    op->idip->fn);
        src_desc_len = desc_from_vpd_id(op, src_desc, sizeof(src_desc),
                                        false /* is_dest */);
        if (src_desc_len > (int)sizeof(src_desc)) {
            pr2serr("source descriptor too large (%d bytes)\n", res);
            return SG_LIB_CAT_MALFORMED;
        }
    } else
        return SG_LIB_CAT_INVALID_OP;

    res = scsi_operating_parameter(op, true /* is_dest */);
    if (res < 0) {
        if (SG_LIB_CAT_UNIT_ATTENTION == -res) {
            pr2serr("Unit attention (%s), continuing\n",
                    rec_copy_op_params_str);
            res = scsi_operating_parameter(op, true);
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
        if (vb)
            pr2serr("  >> using VPD identification for destination %s\n",
                    odip->fn);
        dst_desc_len = desc_from_vpd_id(op, dst_desc, sizeof(dst_desc),
                                        true /* is_dest */);
        if (dst_desc_len > (int)sizeof(dst_desc)) {
            pr2serr("destination descriptor too large (%d bytes)\n", res);
            return SG_LIB_CAT_MALFORMED;
        }
    } else
        return SG_LIB_CAT_INVALID_OP;

    bs_same = (op->ibs_lb == op->obs_lb);
    max_bpt = bs_same ? MAX_XC_BPT : MAX_XC_BPT_POW2;
    if (op->bpt_given) {
        ibpt = op->bpt_i;
        ibpt = (ibpt > max_bpt) ? max_bpt : ibpt;
        obpt = bs_same ? ibpt : ((op->ibs_lb * op->bpt_i) / op->obs_lb);
        if (ifp->dc || ofp->dc) {
            if ((uint32_t)(obpt * op->obs_lb) > odip->xc_max_bytes) {
                pr2serr("bpt too large (max %" PRIu32 " blocks)\n",
                        odip->xc_max_bytes / (uint32_t)op->obs_lb);
                return SG_LIB_SYNTAX_ERROR;
            }
        } else {
            if ((uint32_t)(ibpt * op->ibs_lb) > idip->xc_max_bytes) {
                pr2serr("bpt too large (max %" PRIu32 " blocks)\n",
                        idip->xc_max_bytes / (uint32_t)op->ibs_lb);
                return SG_LIB_SYNTAX_ERROR;
            }
        }
    } else {
        uint32_t r;

        if (ifp->dc || ofp->dc) {
            r = odip->xc_max_bytes / (uint32_t)op->obs_lb;
            obpt = (r > INT_MAX) ? INT_MAX : (int)r;
            ibpt = bs_same ? obpt : ((op->obs_lb * obpt) / op->ibs_lb);
            ibpt = (ibpt > max_bpt) ? max_bpt : ibpt;
            obpt = bs_same ? ibpt : ((op->ibs_lb * ibpt) / op->obs_lb);
        } else {
            r = idip->xc_max_bytes / (uint32_t)op->ibs_lb;
            ibpt = (r > (uint32_t)max_bpt) ? max_bpt : (int)r;
            obpt = bs_same ? ibpt : ((op->ibs_lb * ibpt) / op->obs_lb);
        }
    }
    if (vb > 1)
        pr2serr("%s: xcopy->%s will use ibpt=%d, obpt=%d\n", __func__,
                (ifp->xcopy ? idip->fn : odip->fn),  ibpt, obpt);
    seg_desc_type = seg_desc_from_d_type(simplified_dt(op->idip), 0,
                                         simplified_dt(op->odip), 0);

    res = 0;
    while (op->dd_count > 0) {
        blocks = (op->dd_count > ibpt) ? ibpt : op->dd_count;
        oblocks = bs_same ? blocks : ((op->ibs_lb * blocks) / op->obs_lb);

        if (op->dry_run) {
            if (vb)
                pr2serr("%s: bypass a_xcopy_lid1_cmd() due to --dry-run\n",
                        __func__);
            res = 0;
            goto bypass;
        }
        res = a_xcopy_lid1_cmd(op, src_desc, src_desc_len, dst_desc,
                               dst_desc_len, seg_desc_type, blocks);
        if (res != 0) {
            if ((vb > 0) && (vb < 3)) {
                pr2serr("a_xcopy_lid1_cmd: ");
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
bypass:
        op->stats.in_full += blocks;
        op->stats.out_full += oblocks;
        if (! sgl_iter_add(&csp->in_iter, blocks, true /* relative */))
            pr2serr("%s: sgl_iter_add(in) failed\n", __func__);
        if (! sgl_iter_add(&csp->out_iter, oblocks, true))
            pr2serr("%s: sgl_iter_add(out) failed\n", __func__);
        op->num_xcopy++;
        op->dd_count -= blocks;
        if (op->dd_count > 0)
            signals_process_delay(op, DELAY_COPY_SEGMENT);
    }
    return res;
}

/* vvvvvvvvv  ODX [SBC-3's POPULATE TOKEN + WRITE USING TOKEN] vvvvvvv */

/* Returns 0 on success. Otherwise returns sg3_utils type error (positive) */
int
open_rtf(struct opts_t * op)
{
    bool r_w1, must_exist;
    int res, fd, flags, err;
    struct stat a_st;

    if (op->rtf_fd >= 0) {
        pr2serr("%s: rtf already open\n", __func__ );
#ifdef SG_LIB_MINGW
        return sg_convert_errno(EEXIST);
#else
        return sg_convert_errno(EADDRINUSE);
#endif
    }
    must_exist = false;
    switch (op->odx_request) {
    case ODX_COPY:
        if (RODT_BLK_ZERO == op->rod_type) {
            if (op->verbose)
                pr2serr("ignoring rtf %s since token is fixed\n", op->rtf);
            return 0;
        }
        r_w1 = true;
        break;
    case ODX_READ_INTO_RODS:
        r_w1 = true;
        break;
    case ODX_WRITE_FROM_RODS:
        r_w1 = false;
        must_exist = true;
        break;
    default:
        r_w1 = true;
        break;
    }
    if (! op->rtf[0])
        return must_exist ? sg_convert_errno(ENOENT) : 0;
    res = stat(op->rtf, &a_st);
    if (res < 0) {
        err = errno;
        if (ENOENT == err) {
            if (must_exist) {
                pr2serr("%s not found but rtf required\n", op->rtf);
                return sg_convert_errno(err);
            }
        } else {
            perror("rtf");
            return sg_convert_errno(err);
        }
        fd = creat(op->rtf, 0644);
        if (fd < 0) {
            perror(op->rtf);
            return sg_convert_errno(err);
        }
        op->rtf_fd = fd;
        return 0;
    }
    if (S_ISDIR(a_st.st_mode)) {
        pr2serr("%s: %s is a directory, expected a file\n", __func__,
                op->rtf);
        return sg_convert_errno(EINVAL);
    }
    if (S_ISBLK(a_st.st_mode) || S_ISCHR(a_st.st_mode)) {
        pr2serr("%s: %s is a block or char device, unexpected\n", __func__,
                op->rtf);
        return sg_convert_errno(EINVAL);
    }
    flags = (r_w1 ? O_WRONLY : O_RDONLY);
    if (S_ISREG(a_st.st_mode) && r_w1)
        flags |= (op->rtf_append ? O_APPEND : O_TRUNC);
    fd = open(op->rtf, flags);
    if (fd < 0) {
        err = errno;
        perror(op->rtf);
        return sg_convert_errno(err);
    }
    op->rtf_fd = fd;
    return 0;
}

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
decode_rod_descriptor(const uint8_t * buff, int len, bool to_stderr)
{
    const uint8_t * bp = buff;
    int k, bump;
    int (*print_p)(const char *, ...);

    print_p = to_stderr ? pr2serr : printf;
    for (k = 0; k < len; k += bump, bp += bump) {
        bump = sg_get_unaligned_be16(bp + 2) + 4;
        switch (bp[0]) {
            case 0:
                /* Block ROD device type specific descriptor */
                print_p("  Optimal block ROD length granularity: %d\n",
                        sg_get_unaligned_be16(bp + 6));
                print_p("  Maximum Bytes in block ROD: %" PRIu64 "\n",
                        sg_get_unaligned_be64(bp + 8));
                print_p("  Optimal Bytes in block ROD transfer: %" PRIu64 "\n",
                        sg_get_unaligned_be64(bp + 16));
                print_p("  Optimal Bytes to token per segment: %" PRIu64 "\n",
                        sg_get_unaligned_be64(bp + 24));
                print_p("  Optimal Bytes from token per segment:"
                        " %" PRIu64 "\n", sg_get_unaligned_be64(bp + 32));
                break;
            case 1:
                /* Stream ROD device type specific descriptor */
                print_p("  Maximum Bytes in stream ROD: %" PRIu64 "\n",
                        sg_get_unaligned_be64(bp + 8));
                print_p("  Optimal Bytes in stream ROD transfer:"
                        " %" PRIu64 "\n", sg_get_unaligned_be64(bp + 16));
                break;
            case 3:
                /* Copy manager ROD device type specific descriptor */
                print_p("  Maximum Bytes in processor ROD: %" PRIu64 "\n",
                        sg_get_unaligned_be64(bp + 8));
                print_p("  Optimal Bytes in processor ROD transfer:"
                        " %" PRIu64 "\n", sg_get_unaligned_be64(bp + 16));
                break;
            default:
                print_p("  Unhandled descriptor (format %d, device type %d)\n",
                        bp[0] >> 5, bp[0] & 0x1F);
                break;
        }
    }
}

struct tpc_desc_type {
    uint8_t code;
    const char * name;
};

static const struct tpc_desc_type tpc_desc_arr[] = {
    {0x0, "block -> stream"},
    {0x1, "stream -> block"},
    {0x2, "block -> block"},
    {0x3, "stream -> stream"},
    {0x4, "inline -> stream"},
    {0x5, "embedded -> stream"},
    {0x6, "stream -> discard"},
    {0x7, "verify CSCD"},
    {0x8, "block<o> -> stream"},
    {0x9, "stream -> block<o>"},
    {0xa, "block<o> -> block<o>"},
    {0xb, "block -> stream & application_client"},
    {0xc, "stream -> block & application_client"},
    {0xd, "block -> block & application_client"},
    {0xe, "stream -> stream&application_client"},
    {0xf, "stream -> discard&application_client"},
    {0x10, "filemark -> tape"},
    {0x11, "space -> tape"},
    {0x12, "locate -> tape"},
    {0x13, "<i>tape -> <i>tape"},
    {0x14, "register persistent reservation key"},
    {0x15, "third party persistent reservation source I_T nexus"},
    {0x16, "<i>block -> <i>block"},
    {0xbe, "ROD <- block range(n)"},
    {0xbf, "ROD <- block range(1)"},
    {0xe0, "CSCD: FC N_Port_Name"},
    {0xe1, "CSCD: FC N_Port_ID"},
    {0xe2, "CSCD: FC N_Port_ID with N_Port_Name, checking"},
    {0xe3, "CSCD: Parallel interface: I_T"},
    {0xe4, "CSCD: Identification Descriptor"},
    {0xe5, "CSCD: IPv4"},
    {0xe6, "CSCD: Alias"},
    {0xe7, "CSCD: RDMA"},
    {0xe8, "CSCD: IEEE 1394 EUI-64"},
    {0xe9, "CSCD: SAS SSP"},
    {0xea, "CSCD: IPv6"},
    {0xeb, "CSCD: IP copy service"},
    {0xfe, "CSCD: ROD"},
    {0xff, "CSCD: extension"},
    {0x0, NULL},
};

static const char *
get_tpc_desc_name(uint8_t code)
{
    const struct tpc_desc_type * dtp;

    for (dtp = tpc_desc_arr; dtp->name; ++dtp) {
        if (code == dtp->code)
            return dtp->name;
    }
    return "";
}

struct tpc_rod_type {
    uint32_t type;
    const char * name;
};

static const struct tpc_rod_type tpc_rod_arr[] = {
    {0x0, "copy manager internal"},
    {0x10000, "access upon reference"},
    {0x800000, "point in time copy - default"},
    {0x800001, "point in time copy - change vulnerable"},
    {0x800002, "point in time copy - persistent"},
    {0x80ffff, "point in time copy - any"},
    {0xffff0001, "block device zero"},
    {0x0, NULL},
};

static const char *
get_tpc_rod_name(uint32_t rod_type)
{
    const struct tpc_rod_type * rtp;

    for (rtp = tpc_rod_arr; rtp->name; ++rtp) {
        if (rod_type == rtp->type)
            return rtp->name;
    }
    return "";
}

/* VPD_3PARTY_COPY [3PC, third party copy] */
static void
decode_3party_copy_vpd(uint8_t * buff, int len, bool to_stderr, int verbose)
{
    int k, j, m, bump, desc_type, desc_len, sa_len, pdt, csll;
    unsigned int u;
    const uint8_t * bp;
    const char * cp;
    uint64_t ull;
    char b[80];
    int (*print_p)(const char *, ...);

    print_p = to_stderr ? pr2serr : printf;
    if (len < 4) {
        print_p("Third-party Copy VPD page length too short=%d\n", len);
        return;
    }
    pdt = buff[0] & 0x1f;
    len -= 4;
    bp = buff + 4;
    for (k = 0; k < len; k += bump, bp += bump) {
        desc_type = sg_get_unaligned_be16(bp);
        desc_len = sg_get_unaligned_be16(bp + 2);
        if (verbose)
            print_p("Descriptor type=%d, len=%d\n", desc_type, desc_len);
        bump = 4 + desc_len;
        if ((k + bump) > len) {
            print_p("Third-party Copy VPD page, short descriptor length=%d, "
                    "left=%d\n", bump, (len - k));
            return;
        }
        if (0 == desc_len)
            continue;
        switch (desc_type) {
        case 0x0000:    /* Required if POPULATE TOKEN (or friend) used */
            print_p(" Block Device ROD Token Limits:\n");
            print_p("  Maximum Range Descriptors: %d\n",
                    sg_get_unaligned_be16(bp + 10));
            u = sg_get_unaligned_be32(bp + 12);
            print_p("  Maximum Inactivity Timeout: %u seconds\n", u);
            u = sg_get_unaligned_be32(bp + 16);
            print_p("  Default Inactivity Timeout: %u seconds\n", u);
            ull = sg_get_unaligned_be64(bp + 20);
            print_p("  Maximum Token Transfer Size: %" PRIu64 "\n", ull);
            ull = sg_get_unaligned_be64(bp + 28);
            print_p("  Optimal Transfer Count: %" PRIu64 "\n", ull);
            break;
        case 0x0001:    /* Mandatory (SPC-4) */
            print_p(" Supported Commands:\n");
            j = 0;
            csll = bp[4];
            if (csll >= desc_len) {
                pr2serr("Command supported list length (%d) >= "
                        "descriptor length (%d), wrong so trim\n",
                        csll, desc_len);
                csll = desc_len - 1;
            }
            while (j < csll) {
                sa_len = bp[6 + j];
                for (m = 0; (m < sa_len) && ((j + m) < csll); ++m) {
                    sg_get_opcode_sa_name(bp[5 + j], bp[7 + j + m],
                                          pdt, sizeof(b), b);
                    print_p("  %s\n", b);
                }
                if (0 == sa_len) {
                    sg_get_opcode_name(bp[5 + j], pdt, sizeof(b), b);
                    print_p("  %s\n",  b);
                } else if (m < sa_len)
                    pr2serr("Supported service actions list length (%d) "
                            "is too large\n", sa_len);
                j += m + 2;
            }
            break;
        case 0x0004:
            print_p(" Parameter Data:\n");
            print_p("  Maximum CSCD Descriptor Count: %d\n",
                    sg_get_unaligned_be16(bp + 8));;
            print_p("  Maximum Segment Descriptor Count: %d\n",
                    sg_get_unaligned_be16(bp + 10));
            u = sg_get_unaligned_be32(bp + 12);
            print_p("  Maximum Descriptor List Length: %u\n", u);
            u = sg_get_unaligned_be32(bp + 16);
            print_p("  Maximum Inline Data Length: %u\n", u);
            break;
        case 0x0008:
            print_p(" Supported Descriptors:\n");
            for (j = 0; j < bp[4]; j++) {
                cp = get_tpc_desc_name(bp[5 + j]);
                if (strlen(cp) > 0)
                    printf("  %s [0x%x]\n", cp, bp[5 + j]);
                else
                    printf("  0x%x\n", bp[5 + j]);
            }
            break;
        case 0x000C:
            print_p(" Supported CSCD IDs:\n");
            for (j = 0; j < sg_get_unaligned_be16(bp + 4); j += 2) {
                u = sg_get_unaligned_be16(bp + 6 + j);
                print_p("  0x%04x\n", u);
            }
            break;
        case 0x0106:
            print_p(" ROD Token Features:\n");
            print_p("  Remote Tokens: %d\n", bp[4] & 0x0f);
            u = sg_get_unaligned_be32(bp + 16);
            print_p("  Minimum Token Lifetime: %u seconds\n", u);
            u = sg_get_unaligned_be32(bp + 20);
            print_p("  Maximum Token Lifetime: %u seconds\n", u);
            u = sg_get_unaligned_be32(bp + 24);
            print_p("  Maximum Token inactivity timeout: %d\n", u);
            decode_rod_descriptor(bp + 48,
                                  sg_get_unaligned_be16(bp + 46), to_stderr);
            break;
        case 0x0108:
            print_p(" Supported ROD Token and ROD Types:\n");
            for (j = 0; j < sg_get_unaligned_be16(bp + 6); j+= 64) {
                u = sg_get_unaligned_be32(bp + 8 + j);
                cp = get_tpc_rod_name(u);
                if (strlen(cp) > 0)
                    printf("  ROD Type: %s [0x%x]\n", cp, u);
                else
                   printf("  ROD Type: 0x%x\n", u);
                print_p("    Internal: %s\n",
                        bp[8 + j + 4] & 0x80 ? "yes" : "no");
                print_p("    Token In: %s\n",
                        bp[8 + j + 4] & 0x02 ? "yes" : "no");
                print_p("    Token Out: %s\n",
                        bp[8 + j + 4] & 0x01 ? "yes" : "no");
                print_p("    Preference: %d\n",
                        sg_get_unaligned_be16(bp + 8 + j + 6));
            }
            break;
        case 0x8001:    /* Mandatory (SPC-4) */
            print_p(" General Copy Operations:\n");
            u = sg_get_unaligned_be32(bp + 4);
            print_p("  Total Concurrent Copies: %u\n", u);
            u = sg_get_unaligned_be32(bp + 8);
            print_p("  Maximum Identified Concurrent Copies: %u\n", u);
            u = sg_get_unaligned_be32(bp + 12);
            print_p("  Maximum Segment Length: %u\n", u);
            ull = (1 << bp[16]);       /* field is power of 2 */
            print_p("  Data Segment Granularity: %" PRIu64 "\n", ull);
            ull = (1 << bp[17]);
            print_p("  Inline Data Granularity: %" PRIu64 "\n", ull);
            break;
        case 0x9101:
            print_p(" Stream Copy Operations:\n");
            u = sg_get_unaligned_be32(bp + 4);
            print_p("  Maximum Stream Device Transfer Size: %u\n", u);
            break;
        case 0xC001:
            print_p(" Held Data:\n");
            u = sg_get_unaligned_be32(bp + 4);
            print_p("  Held Data Limit: %u\n", u);
            ull = (1 << bp[8]);
            print_p("  Held Data Granularity: %" PRIu64 "\n", ull);
            break;
        default:
            print_p("Unexpected type=%d\n", desc_type);
            hex2stderr(bp, bump, 1);
            break;
        }
    }
}


/* Note this function passes back a malloc-ed buffer if it returns 0 and
 * fixed_b != *alloc_bp which caller should free. Returns 0 on success. */
static int
fetch_3pc_vpd(int fd, const char * fn, uint8_t * fixed_b, int fixed_blen,
              uint8_t ** alloc_bp, int verb)
{
    int res, len, resid;
    uint8_t * rp;

    rp = fixed_b;
    if (alloc_bp)
        *alloc_bp = fixed_b;
    res = sg_ll_inquiry_v2(fd, true, VPD_3PARTY_COPY, rp, fixed_blen, 0,
                           &resid, true, verb);
    if (res) {
        if (SG_LIB_CAT_ILLEGAL_REQ == res) {
            if (fn)
                pr2serr("Third Party Copy VPD page not found [%s]\n", fn);
            else
                pr2serr("Third Party Copy VPD page not found\n");
        } else
            pr2serr("Third Party Copy VPD inquiry failed with %d, try again "
                    "with '-vv'\n", res);
        return res;
    } else if ((fixed_blen - resid) < 4) {
        pr2serr("3PARTY_COPY VPD response length < 4 bytes after resid\n");
        return SG_LIB_CAT_MALFORMED;
    } else if (rp[1] != VPD_3PARTY_COPY) {
        pr2serr("invalid 3PARTY_COPY VPD response\n");
        return SG_LIB_CAT_MALFORMED;
    }
    len = sg_get_unaligned_be16(rp + 2) + 4;
    if (len > fixed_blen) {
        rp = (uint8_t *)malloc(len);
        if (NULL == rp) {
            pr2serr("Not enough user memory for %s\n", __func__);
            return sg_convert_errno(ENOMEM);
        }
        if (alloc_bp)
            *alloc_bp = rp;
        res = sg_ll_inquiry_v2(fd, true, VPD_3PARTY_COPY, rp, len, 0, &resid,
                               true, verb);
        if (res) {
            pr2serr("3PARTY_COPY VPD inquiry failed with %d\n", res);
            if (fixed_b != rp)
                free(rp);
            return res;
        } else if ((len - resid) < 4) {
            pr2serr("3PARTY_COPY VPD response length < 4 bytes after resid\n");
            return SG_LIB_CAT_MALFORMED;
        }
    }
    return 0;
}

/* Returns 0 on success */
static int
get_3pc_vpd_blkdev_lims(struct opts_t * op, struct dev_info_t * dip)
{
    bool found = false;
    int res, verb, n, len, bump, desc_type, desc_len, k;
    uint32_t max_ito = 0;
    uint8_t * rp;
    uint8_t * bp;
    uint8_t rBuff[256];

    verb = (op->verbose ? (op->verbose - 1) : 0);
    rp = rBuff;
    n = (int)sizeof(rBuff);
    res = fetch_3pc_vpd(dip->fd, dip->fn, rBuff, n, &rp, verb);
    if (res)
        return res;
    len = sg_get_unaligned_be16(rp + 2);
    bp = rp + 4;
    for (k = 0; k < len; k += bump, bp += bump) {
        desc_type = sg_get_unaligned_be16(bp + 0);
        desc_len = sg_get_unaligned_be16(bp + 2);
        if (op->verbose > 4)
            pr2serr("Descriptor type=%d, len=%d\n", desc_type, desc_len);
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
            found = true;
            if (op->verbose > 3) {
                pr2serr("3PARTY_COPY Copy VPD, Block Device ROD Token "
                        "Limits descriptor:\n");
                hex2stderr(bp, desc_len, 1);
            }
            if (desc_len < 32) {
                pr2serr("3PARTY_COPY Copy VPD, Block Device ROD Token "
                        "Limits descriptor, too short, want 32 got %d\n",
                        desc_len);
                break;
            }
            dip->odxp->max_range_desc = sg_get_unaligned_be16(bp + 10);
            max_ito = sg_get_unaligned_be32(bp + 12);
            dip->odxp->max_inactivity_to = max_ito;
            dip->odxp->def_inactivity_to = sg_get_unaligned_be32(bp + 16);
            dip->odxp->max_tok_xfer_size = sg_get_unaligned_be64(bp + 20);
            dip->odxp->optimal_xfer_count = sg_get_unaligned_be64(bp + 28);
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
        if (0 == op->iflagp->force) {
            pr2serr("... exiting; can override with 'force' flag\n");
            return SG_LIB_CAT_OTHER;
        }
    }
    return 0;
}

/* Returns 0 on success. */
int
print_3pc_vpd(struct opts_t * op, bool to_stderr)
{
    int res, verb, len;
    uint8_t * rp;
    uint8_t rBuff[256];

    verb = (op->verbose ? (op->verbose - 1) : 0);
    res = fetch_3pc_vpd(op->idip->fd, NULL, rBuff, (int)sizeof(rBuff),
                        &rp, verb);
    if (res)
        return res;
    len = sg_get_unaligned_be16(rp + 2) + 4;
    decode_3party_copy_vpd(rp, len, to_stderr, verb);
    if (rBuff != rp)
        free(rp);
    return res;
}

/* Do POPULATE_TOKEN command, returns 0 on success */
int
do_pop_tok(struct opts_t * op, uint64_t blk_off, uint32_t num_blks,
           bool walk_list_id, int vb_a)
{
    int res, k, j, n, len, fd, tmout, sz_bdrd, elems, pl_sz, err_vb;
    uint32_t num;
    uint32_t pg_sz = sg_get_page_size();
    uint64_t lba, sg0_off;
    const struct scat_gath_elem * sglp;
    uint8_t * pl;
    uint8_t * free_pl;
    struct cp_state_t * csp;
    struct cp_state_t cp_st;

    csp = &cp_st;
    cp_state_init(csp, op);
    if (vb_a)
        pr2serr("%s: blk_off=%" PRIu64 ", num_blks=%"  PRIu32 "\n", __func__,
                blk_off, num_blks);
    if (op->verbose == vb_a)
        err_vb = op->verbose;
    else if (op->verbose > 0)
        err_vb = (vb_a > 0) ? vb_a : 1;
    else
        err_vb = 0;
    fd = op->idip->fd;
    if (op->i_sgli.sglp) {
        sg0_off = blk_off;
        sglp = op->i_sgli.sglp;
        for (k = 0; k < op->i_sgli.elems; ++k, ++sglp) {
            if ((uint64_t)sglp->num >= sg0_off)
                break;
            sg0_off -= sglp->num;
        }
        if (k >= op->i_sgli.elems) {
            pr2serr("%s: exhausted sgl_elems [%d], miscalculation\n",
                    __func__, op->i_sgli.elems);
            return SG_LIB_CAT_MALFORMED;
        }
        /* remain sg elements is worst case, might use less */
        elems = op->i_sgli.elems - k;
        pl_sz = 16 + (16 * elems);
    } else {
        sg0_off = 0;    /* compilers should be smarter */
        sglp = NULL;
        elems = 1;
        pl_sz = 32;
    }
    pl = sg_memalign(pl_sz, pg_sz, &free_pl, false);
    if (NULL == pl) {
        pr2serr("%s: Not enough user memory\n", __func__);
        return sg_convert_errno(ENOMEM);
    }
    if (op->rod_type_given) {
        pl[2] = 0x2;            /* RTV bit */
        sg_put_unaligned_be32((uint32_t)op->rod_type, pl + 8);
    }
    if (op->iflagp->immed)
        pl[2] |= 0x1;           /* IMMED bit */
    /* if inactivity_to=0 then cm takes default in TPC VPD page */
    sg_put_unaligned_be32((uint32_t)op->inactivity_to, pl + 4);

    len = 32;           /* temporary, stop uninitialized warning */
    sz_bdrd = 16;       /* temporary, stop uninitialized warning */
    if (sglp) {
        lba = sglp->lba + sg0_off;
        num = sglp->num - sg0_off;
        for (k = 0, n = 15; k < elems; ++k, num_blks -= num, ++sglp) {
            if (k > 0) {
                lba = sglp->lba;
                num = sglp->num;
            }
            if (num > num_blks)
                num = num_blks;
            if (vb_a)
                pr2serr("  lba=0x%" PRIx64 ", num=%" PRIu32 ", k=%d\n", lba,
                        num, k);
            sg_put_unaligned_be64(lba, pl + n);
            n += 8;
            sg_put_unaligned_be32(num, pl + n);
            n += 4 + 4;
        }
        sz_bdrd = k * 16;
        sg_put_unaligned_be16(sz_bdrd, pl + 14);
        len = n + 1;
#if 0           /* should always have sgl. so this shouldn't ... */
    } else {    /* assume count= and possibly skip= given */
        sz_bdrd = 16;       /* single element */
        sg_put_unaligned_be16(sz_bdrd, pl + 14);
        lba = op->skip + blk_off;
        if (vb_a)
            pr2serr("  lba=0x%" PRIx64 ", num_blks=%" PRIu32 "\n", lba,
                    num_blks);
        sg_put_unaligned_be64(lba, pl + 16);
        sg_put_unaligned_be32(num_blks, pl + 24);
        len = 32;
#endif
    }
    n = len - 2;
    sg_put_unaligned_be16((uint16_t)n, pl + 0);

    tmout = (op->timeout_xcopy < 1) ? DEF_3PC_OUT_TIMEOUT : op->timeout_xcopy;
    res = pt_3party_copy_out(fd, SA_POP_TOK, op->list_id, DEF_GROUP_NUM,
                             tmout, pl, len, true /* noisy */, vb_a - 1,
                             err_vb);
    if ((DDPT_CAT_OP_IN_PROGRESS == res) && walk_list_id) {
        for (j = 0; j < MAX_IN_PROGRESS; ++j) {
            res = pt_3party_copy_out(fd, SA_POP_TOK, ++op->list_id,
                                     DEF_GROUP_NUM, tmout, pl, len, true,
                                     vb_a - 1, err_vb);
            if (DDPT_CAT_OP_IN_PROGRESS != res)
                break;
        }
        if (MAX_IN_PROGRESS == j) {
            if (vb_a)
                pr2serr("%s: too many list_id_s 'in progress'\n", __func__);
        }
    }
    free(free_pl);
    return res;
}

static int rrti_num = 0;

/* send Receive ROD Token Information command and process response */
int
do_rrti(struct opts_t * op, bool in0_out1, struct rrti_resp_t * rrp, int verb)
{
    int res, fd, off, err_vb;
    uint32_t len, rtdl;
    const char * cp;
    uint8_t rsp[1024];
    char b[400];
    char bb[80];

    /* want to suppress 'pass-through requested n bytes ...' messages with
     * 'ddpt verbose=2 ...' */
    err_vb = op->verbose;
    if ((verb != op->verbose) && (err_vb > 1))
        --err_vb;
    fd = in0_out1 ? op->odip->fd : op->idip->fd;
    ++rrti_num;
    res = pt_3party_copy_in(fd, SA_ROD_TOK_INFO, op->list_id,
                            DEF_3PC_IN_TIMEOUT, rsp, sizeof(rsp), true,
                            verb, err_vb);
    if (res)
        return res;

    len = sg_get_unaligned_be32(rsp + 0) + 4;
    if (len > sizeof(rsp)) {
        pr2serr("RRTI [%d]: ROD Token Info too long for internal buffer, "
                "output truncated\n", rrti_num);
        len = sizeof(rsp);
    }
    if (verb > 1) {
        pr2serr("\nRRTI [%d] response in hex:\n", rrti_num);
        hex2stderr(rsp, len, 1);
    }
    if (NULL == rrp)
        return 0;
    rrp->for_sa = 0x1f & rsp[4];
    switch(rrp->for_sa) {
    case SA_POP_TOK:
        cp = "PT";
        break;
    case SA_WR_USING_TOK:
        cp = "WUT";
        break;
    case SA_XCOPY_LID1:
        cp = "XCOPY(LID1)";
        break;
    case SA_XCOPY_LID4:
        cp = "XCOPY(LID4)";
        break;
    default:
        cp = "unknown originating xcopy command";
        break;
    }
    rrp->cstat = 0x7f & rsp[5];
    rrp->xc_cstatus = rsp[12];
    rrp->sense_len = rsp[14];
    rrp->esu_del = sg_get_unaligned_be32(rsp + 8);
    if (verb)
       pr2serr("RRTI [%d] for %s: %s\n", rrti_num, cp,
               cpy_op_status_str(rrp->cstat, b, sizeof(b)));
    rrp->tc = sg_get_unaligned_be64(rsp + 16);
    if (rrp->sense_len > 0) {
        snprintf(bb, sizeof(bb), "%s [%d]: sense data", cp, rrti_num);
        sg_get_sense_str(bb, rsp + 32, rrp->sense_len, verb, sizeof(b), b);
        pr2serr("%s\n", b);
    }
    off = 32 + rsp[13];
    rtdl = sg_get_unaligned_be32(rsp + off);
    rrp->rt_len = (rtdl > 2) ? rtdl - 2 : 0;
    if (rtdl > 2)
        memcpy(rrp->rod_tok, rsp + off + 6,
               ((rrp->rt_len > 512) ? 512 : rrp->rt_len));
    return 0;
}

static int rcs_num = 0;

/* send Receive Copy Status command and process response */
int
do_rcs(struct opts_t * op, bool in0_out1, struct rrti_resp_t * rrp, int verb)
{
    int res, fd, err_vb;
    uint32_t len;
    const char * cp;
    uint8_t rsp[1024];
    char b[400];
    char bb[80];

    /* want to suppress 'pass-through requested n bytes ...' messages with
     * 'ddpt verbose=2 ...' */
    err_vb = op->verbose;
    if ((verb != op->verbose) && (err_vb > 1))
        --err_vb;
    fd = in0_out1 ? op->odip->fd : op->idip->fd;
    ++rcs_num;
    res = pt_3party_copy_in(fd, SA_COPY_STATUS_LID4, op->list_id,
                            DEF_3PC_IN_TIMEOUT, rsp, sizeof(rsp), true,
                            verb, err_vb);
    if (res)
        return res;

    len = sg_get_unaligned_be32(rsp + 0) + 4;
    if (len > sizeof(rsp)) {
        pr2serr("RCS [%d]: too long for internal buffer, output truncated\n",
                rcs_num);
        len = sizeof(rsp);
    }
    if (verb > 1) {
        pr2serr("\nRCS [%d] response in hex:\n", rcs_num);
        hex2stderr(rsp, len, 1);
    }
    if (NULL == rrp)
        return 0;
    rrp->for_sa = 0x1f & rsp[4];
    switch(rrp->for_sa) {
    case SA_POP_TOK:
        cp = "PT";
        break;
    case SA_WR_USING_TOK:
        cp = "WUT";
        break;
    case SA_XCOPY_LID1:
        cp = "XCOPY(LID1)";
        break;
    case SA_XCOPY_LID4:
        cp = "XCOPY(LID4)";
        break;
    default:
        cp = "unknown originating xcopy command";
        break;
    }
    rrp->cstat = 0x7f & rsp[5];
    rrp->xc_cstatus = rsp[12];
    rrp->sense_len = rsp[14];
    rrp->esu_del = sg_get_unaligned_be32(rsp + 8);
    if (verb)
       pr2serr("RCS [%d] for %s: %s\n", rcs_num, cp,
               cpy_op_status_str(rrp->cstat, b, sizeof(b)));
    rrp->tc = sg_get_unaligned_be64(rsp + 16);
    if (rrp->sense_len > 0) {
        snprintf(bb, sizeof(bb), "%s [%d]: sense data", cp, rcs_num);
        sg_get_sense_str(bb, rsp + 32, rrp->sense_len, verb, sizeof(b), b);
        pr2serr("%s\n", b);
    }
    rrp->rt_len = 0;
    return 0;
}

int
process_after_poptok(struct opts_t * op, uint64_t * tcp, int vb_a)
{
    int res, len, vb_b, err, cont;
    uint32_t delay;
    uint64_t rod_sz;
    struct rrti_resp_t r;
    char b[400];
    uint8_t uc[8];

    if (op->verbose == vb_a)
        vb_b = op->verbose;
    else
        vb_b = (vb_a > 0) ? (vb_a - 1) : 0;
    do {
        res = do_rrti(op, DDPT_ARG_IN, &r, vb_b);
        if (res)
            return res;
        if (SA_POP_TOK != r.for_sa) {
            sg_get_opcode_sa_name(THIRD_PARTY_COPY_OUT_CMD, r.for_sa, 0,
                                  sizeof(b), b);
            pr2serr("RRTI [%d] expected response for Populate Token\n  "
                    "but got response for %s\n", rrti_num, b);
        }
        cont = ((r.cstat >= 0x10) && (r.cstat <= 0x12));
        if (cont) {
            delay = r.esu_del;
            if ((delay < 0xfffffffe) && (delay > 0)) {
                if (vb_b > 1)
                    pr2serr("[%d] using copy manager recommended delay of %"
                            PRIu32 " milliseconds\n", rrti_num, delay);
            } else {
                delay = DEF_ODX_POLL_DELAY_MS;
                if (vb_b > 1)
                    pr2serr("[%d] using default for poll delay\n", rrti_num);
            }
            if (delay)
                sleep_ms(delay);
        }
    } while (cont);
    if ((! ((0x1 == r.cstat) || (0x3 == r.cstat))) || (vb_b > 1))
        pr2serr("RRTI [%d] after PT: %s\n", rrti_num,
                cpy_op_status_str(r.cstat, b, sizeof(b)));
    if (vb_a)
        pr2serr("RRTI [%d] after PT: Transfer count=%" PRIu64 " [0x%" PRIx64
                "]\n", rrti_num, r.tc, r.tc);
    if (tcp)
        *tcp = r.tc;
    if (r.rt_len > 0) {
        len = (r.rt_len > 512) ? 512 : r.rt_len;
        if (vb_a) {
            pr2serr("RRTI [%d] after PT: copy manager ROD Token id: %s",
                    rrti_num,
                    rt_cm_id_str(r.rod_tok, r.rt_len, b, sizeof(b)));
            if (512 == r.rt_len)
                pr2serr("\n");
            else
                pr2serr(" [rt_len=%" PRIu32 "d]\n", r.rt_len);
        }
        if (op->rtf_fd >= 0) {     /* write ROD Token to RTF */
            res = write(op->rtf_fd, r.rod_tok, len);
            if (res < 0) {
                err = errno;
                pr2serr("%s: unable to write to file: %s [%s]\n", __func__,
                        op->rtf, safe_strerror(err));
                return sg_convert_errno(err);
            }
            if (res < len) {
                pr2serr("%s: short write to file: %s, wanted %d, got %d\n",
                        __func__, op->rtf, len, res);
                return SG_LIB_CAT_OTHER;
            }
            if (op->rtf_len_add) {
                rod_sz = r.tc * op->ibs_lb;
                sg_put_unaligned_be64(rod_sz, uc + 0);
                res = write(op->rtf_fd, uc, 8);
                if (res < 0) {
                    err = errno;
                    pr2serr("%s: unable to write length to file: %s [%s]\n",
                             __func__, op->rtf, safe_strerror(err));
                    return SG_LIB_FILE_ERROR;
                }
            }
        }
        /* write ROD Token to static, in any case; could be a copy */
        if (len > LOCAL_ROD_TOKEN_SIZE) {
            pr2serr("%s: ROD token too large for static storage, try "
                    "'rtf=RTF'\n", __func__);
            return SG_LIB_CAT_OTHER;
        }
        memcpy(local_rod_token, r.rod_tok, len);
    }
    return 0;
}

void
get_local_rod_tok(uint8_t * tokp, int max_tok_len)
{
    int len;

    if (tokp && (max_tok_len > 0)) {
        len = (max_tok_len > 512) ? 512 : max_tok_len;
        memcpy(tokp, local_rod_token, len);
    }
}

/* Do WRITE USING TOKEN command, returns 0 on success */
int
do_wut(struct opts_t * op, uint8_t * tokp, uint64_t blk_off,
       uint32_t num_blks, uint64_t oir, bool more_left, bool walk_list_id,
       int vb_a)
{
    bool rodt_blk_zero;
    int len, k, j, n, fd, res, tmout, sz_bdrd, elems, pl_sz;
    int err_vb = 0;
    uint32_t num;
    uint32_t pg_sz = sg_get_page_size();
    uint64_t lba, sg0_off;
    struct flags_t * flp;
    const struct scat_gath_elem * sglp;
    uint8_t * pl;
    uint8_t * free_pl;
    // uint8_t rt[512];

    if (op->verbose == vb_a)
        err_vb = op->verbose;
    else if (op->verbose > 0)
        err_vb = (vb_a > 0) ? vb_a : 1;
    if (vb_a)
        pr2serr("%s: enter; blk_off=%" PRIu64 ", num_blks=%"  PRIu32 ", "
                " oir=0x%" PRIx64 "\n", __func__, blk_off, num_blks, oir);
    flp = op->oflagp;
    rodt_blk_zero = (RODT_BLK_ZERO == op->rod_type);
    if (op->o_sgli.sglp) {
        sglp = op->o_sgli.sglp;
        for (k = 0, sg0_off = blk_off; k < op->o_sgli.elems; ++k, ++sglp) {
            if ((uint64_t)sglp->num >= sg0_off)
                break;
            sg0_off -= sglp->num;
        }
        if (k >= op->o_sgli.elems) {
            pr2serr("%s: exhausted sgl_elems [%d], miscalculation\n",
                    __func__, op->o_sgli.elems);
            return SG_LIB_CAT_MALFORMED;
        }
        /* remain sg elements is worst case, might use less */
        elems = op->o_sgli.elems - k;
        pl_sz = 540 + (16 * elems);
    } else {
        sg0_off = 0;    /* compilers should be smarter */
        sglp = NULL;
        elems = 1;
        pl_sz = 540 + 16;
    }
    pl = sg_memalign(pl_sz, pg_sz, &free_pl, false);
    if (NULL == pl) {
        pr2serr("%s Not enough user memory\n", __func__);
        return sg_convert_errno(ENOMEM);
    }
    memset(pl, 0, pl_sz);
    if (! rodt_blk_zero) {
        if (flp->del_tkn)       /* only from ddptctl */
            pl[2] = 0x2;        /* DEL_TKN bit */
        else if ((! more_left) && (! flp->no_del_tkn))
            pl[2] = 0x2;        /* last write from ROD which may hold more */
    }
    if (flp->immed)
        pl[2] |= 0x1;           /* IMMED bit */
    if (oir)            /* Offset in ROD field */
        sg_put_unaligned_be64(oir, pl + 8);
    memcpy(pl + 16, tokp, 512);

    sz_bdrd = 16;       /* temporary, stop uninitialized warning */
    if (sglp) {
        lba = sglp->lba + sg0_off;
        num = sglp->num - sg0_off;
        for (k = 0, n = 535; k < elems; ++k, num_blks -= num, ++sglp) {
            if (k > 0) {
                lba = sglp->lba;
                num = sglp->num;
            }
            if (num > num_blks)
                num = num_blks;
            if (vb_a)
                pr2serr("  lba=0x%" PRIx64 ", num=%" PRIu32 ", k=%d\n", lba,
                        num, k);
            sg_put_unaligned_be64(lba, pl + n);
            n += 8;
            sg_put_unaligned_be32(num, pl + n);
            n += 4 + 4;
        }
        sz_bdrd = 16 * k;
        sg_put_unaligned_be16(sz_bdrd, pl + 534);
#if 0           /* should always have sgl. so this shouldn't ... */
    } else {
        sz_bdrd = 16;   /* single element */
        sg_put_unaligned_be16(sz_bdrd, pl + 534);
        lba = op->seek + blk_off;
        if (vb_a)
            pr2serr("  lba=0x%" PRIx64 ", num_blks=%" PRIu32 "\n", lba,
                    num_blks);
        sg_put_unaligned_be64(lba, pl + 536);
        sg_put_unaligned_be32(num_blks, pl + 544);
#endif
    }
    len = 536 +  sz_bdrd;
    n = len - 2;
    sg_put_unaligned_be16(n, pl + 0);
    fd = op->odip->fd;

    tmout = (op->timeout_xcopy < 1) ? DEF_3PC_OUT_TIMEOUT : op->timeout_xcopy;
    res = pt_3party_copy_out(fd, SA_WR_USING_TOK, op->list_id, DEF_GROUP_NUM,
                             tmout, pl, len, true /* noisy */,
                             vb_a - 1, err_vb);
    if ((DDPT_CAT_OP_IN_PROGRESS == res) && walk_list_id) {
        for (j = 0; j < MAX_IN_PROGRESS; ++j) {
            res = pt_3party_copy_out(fd, SA_WR_USING_TOK, ++op->list_id,
                                     DEF_GROUP_NUM, tmout, pl, len, true,
                                     vb_a - 1, err_vb);
            if (DDPT_CAT_OP_IN_PROGRESS != res)
                break;
        }
        if (MAX_IN_PROGRESS == j) {
            if (vb_a)
                pr2serr("%s: too many list_id_s 'in progress'\n", __func__);
        }
    }
    free(free_pl);
    return res;
}

int
process_after_wut(struct opts_t * op, uint64_t * tcp /* transfer count */,
                  int vb_a)
{
    bool changed_pref = false;
    bool cont;
    bool prefer_rcs = op->oflagp->prefer_rcs;
    int res, vb_b, r_count;
    uint32_t delay;
    const char * cmd_name;
    char b[80];
    struct rrti_resp_t r;

    if (op->verbose == vb_a)
        vb_b = op->verbose;
    else
        vb_b = (vb_a > 0) ? (vb_a - 1) : 0;
    do {
resend_cmd:
        if (prefer_rcs) {
            cmd_name = "RCS";
            r_count = rrti_num;
            res = do_rcs(op, DDPT_ARG_OUT, &r, vb_b);
        } else {
            cmd_name = "RRTI";
            r_count = rcs_num;
            res = do_rrti(op, DDPT_ARG_OUT, &r, vb_b);
        }
        if (res) {
            if ((SG_LIB_CAT_ILLEGAL_REQ == res) && ! changed_pref) {
                changed_pref = true;
                prefer_rcs = !prefer_rcs;
                goto resend_cmd;
            } else
                return res;
        }
        if (SA_WR_USING_TOK != r.for_sa) {
            sg_get_opcode_sa_name(THIRD_PARTY_COPY_OUT_CMD, r.for_sa, 0,
                                  sizeof(b), b);
            pr2serr("%s [%d] expected response for Write Using Token\n  but "
                    "got response for %s\n", cmd_name, r_count, b);
        }
        cont = ((r.cstat >= 0x10) && (r.cstat <= 0x12));
        if (cont) {
            delay = r.esu_del;
            if ((delay < 0xfffffffe) && (delay > 0)) {
                if (vb_b > 1)
                    pr2serr("using copy manager recommended delay of %"
                            PRIu32 " milliseconds\n", delay);
            } else {
                delay = DEF_ODX_POLL_DELAY_MS;
                if (vb_b > 1)
                    pr2serr("using default for poll delay\n");
            }
            if (delay)
                sleep_ms(delay);
        }
    } while (cont);

    if ((! ((0x1 == r.cstat) || (0x3 == r.cstat))) || (vb_b > 1))
        pr2serr("%s [%d] for WUT: %s\n", cmd_name, r_count,
                cpy_op_status_str(r.cstat, b, sizeof(b)));
    if (tcp)
        *tcp = r.tc;
    if (vb_a)
        pr2serr("%s [%d] for WUT: Transfer count=%" PRIu64 " [0x%" PRIx64
                "]\n", cmd_name, r_count, r.tc, r.tc);
    return 0;
}

#if 0
static int
odx_check_sgl(struct opts_t * op, uint64_t num_blks, bool in0_out1)
{
    uint32_t allowed_descs;
    uint32_t num_elems = in0_out1 ? op->o_sgli.elems : op->i_sgli.elems;
    struct dev_info_t * dip = in0_out1 ? op->odip : op->idip;
    struct flags_t * flp = in0_out1 ? op->oflagp : op->iflagp;
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
#endif

static int
fetch_read_cap(struct opts_t * op, bool in0_out1, int64_t * num_blks,
               int * blk_sz)
{
    int res;
    int bs = in0_out1 ? op->obs_lb : op->ibs_lb;
    struct dev_info_t * dip = in0_out1 ? op->odip : op->idip;
    struct flags_t * flagp = in0_out1 ? op->oflagp : op->iflagp;
    const char * oip = in0_out1 ? "o" : "i";

    if ((res = pt_read_capacity(op, in0_out1, num_blks, blk_sz))) {
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            pr2serr("Unit attention (readcap(%s)), continuing\n", oip);
            res = pt_read_capacity(op, in0_out1, num_blks, blk_sz);
        }
        if (res)
            return res;
    }
    if (op->verbose) {
        print_blk_sizes(dip->fn, "readcap", *num_blks, *blk_sz, 1);
        if (dip->prot_type > 0)
            pr2serr("    reports Protection_type=%d, p_i_exp=%d\n",
                    dip->prot_type, dip->p_i_exp);
    }
    if ((*num_blks > 0) && (*blk_sz != bs)) {
        pr2serr(">> warning: %s block size confusion: %sbs=%d, device "
                "claims=%d\n", dip->fn, oip, bs, *blk_sz);
        if (0 == flagp->force) {
            pr2serr(">> abort copy, use %sflag=force to override\n", oip);
            return -1;
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
    bool got_count;
    int k, res, out_blk_sz, out_elems, vb, vb3;
    uint64_t out_blk_off, num, tc;
    int64_t out_num_blks, v;
    struct dev_info_t * odip = op->odip;

    vb = op->verbose;
    vb3 = (vb > 1) ? (vb - 2) : 0;
    k = dd_filetype(op->idip->fn, vb);
    got_count = (op->dd_count > 0);
    if (FT_DEV_NULL != k) {
        pr2serr("For single WUT version of ODX write blocks of zeros, "
                "don't give if=IFILE option\n");
        pr2serr("For full copy version of ODX write blocks of zeros, "
                "give if=/dev/null or equivalent\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    res = fetch_read_cap(op, DDPT_ARG_OUT, &out_num_blks, &out_blk_sz);
    if (res)
        return res;
    v = out_num_blks;
    if (op->o_sgli.sglp) {  /* scatter list */
        out_elems = op->o_sgli.elems;
        sgl_sum_scan(&op->o_sgli, "odx full zero",
                     (op->show_sgl_v2 || (vb > 2)), vb > 1);
        out_num_blks = op->o_sgli.sum;
    } else { /* no scatter list */
        out_elems = 1;
        out_num_blks = got_count ? op->dd_count : 0;
    }
    if (0 == op->dd_count) {
        if (vb)
            pr2serr("%s: enough checks, count=0 given so exit\n", __func__);
        return 0;
    }
    if ((op->dd_count < 0) && (0 == out_num_blks)) {
        if (1 == vb)
            pr2serr("%s: zero the lot after scaling for seek=\n", __func__);
        v -= op->o_sgli.lowest_lba;
        if (v < 0) {
            pr2serr("%s: seek exceeds out device size\n", __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        out_num_blks = v;
    }
    out_blk_off = 0;
    op->dd_count = out_num_blks;
    op->stats.dd_count_start = op->dd_count;

    /* Build fixed format ROD Token Block Zero; specified by SBC-3 */
    memset(local_rod_token, 0, sizeof(local_rod_token));
    sg_put_unaligned_be32(RODT_BLK_ZERO, local_rod_token + 0);
    sg_put_unaligned_be16(ODX_ROD_TOK_LEN_FLD, local_rod_token + 6);

    if (vb > 1)
        pr2serr("%s: about to zero %" PRIi64 " blocks\n", __func__,
                out_num_blks);

    for (k = 0; out_num_blks > 0; out_num_blks -= num, ++k) {
        num = out_num_blks;
        if ((op->obpch > 0) && ((uint64_t)op->obpch < num))
            num = op->obpch;    /* in this case BPT refers to OFILE */
        if ((odip->odxp->max_tok_xfer_size > 0) &&
            (num > odip->odxp->max_tok_xfer_size))
            num = odip->odxp->max_tok_xfer_size;
        if (op->o_sgli.sglp)
            num = count_sgl_blocks_from(op->o_sgli.sglp, out_elems,
                                        out_blk_off, num,
                                        odip->odxp->max_range_desc);
        if ((res = do_wut(op, local_rod_token, out_blk_off, num, 0, 0,
                          ! op->list_id_given, vb3)))
            return res;
        if ((res = process_after_wut(op, &tc, vb3)))
            return res;
        if (tc != num) {
            pr2serr("%s: number requested differs from transfer count\n",
                    __func__);
            // ouch, think about this one
        }
        op->stats.out_full += tc;
        out_blk_off += num;
        op->dd_count -= tc;
    }
    return 0;
}

/* This function is designed to be the reading or input ise of a network
 * copy. Returns 0 on success. */
static int
odx_read_into_rods(struct opts_t * op)
{
    bool got_count;
    int k, res, in_blk_sz, in_elems, vb, vb3;
    uint64_t in_blk_off, num, tc_i;
    int64_t in_num_blks, u;
    struct dev_info_t * idip = op->idip;

    vb = op->verbose;
    vb3 = (vb > 1) ? (vb - 2) : 0;
    got_count = (op->dd_count > 0);
    /* need to know block size of input and output */
    res = fetch_read_cap(op, DDPT_ARG_IN, &in_num_blks, &in_blk_sz);
    if (res)
        return res;
    u = in_num_blks;
    if (op->i_sgli.sglp) {   /* gather list */
        in_elems = op->i_sgli.elems;
        sgl_sum_scan(&op->i_sgli, "read into rods",
                     (op->show_sgl_v2 || (vb > 2)), vb > 1);
        in_num_blks = op->i_sgli.sum;
        if (got_count && (in_num_blks != op->dd_count)) {
            pr2serr("%s: count= value not equal to the sum of gather nums\n",
                    __func__);
            return SG_LIB_CAT_OTHER;
        }
    } else {
        in_elems = 1;
        in_num_blks = got_count ? op->dd_count : 0;
    }
    if (0 == op->dd_count) {
        if (vb)
            pr2serr("%s: enough checks, count=0 given so exit\n", __func__);
        return 0;
    }
    if ((op->dd_count < 0) && (0 == in_num_blks)) {
        if (vb > 1)
            pr2serr("%s: read the lot after scaling for skip=\n", __func__);
        u -= op->i_sgli.lowest_lba;
        if (u < 0) {
            pr2serr("%s: skip exceeds input device size\n", __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        in_num_blks = u;
    }

    in_blk_off = 0;
    op->dd_count = in_num_blks;
    op->stats.dd_count_start = op->dd_count;
    if (vb > 1)
        pr2serr("%s: about to read %" PRIi64 " blocks\n", __func__,
                in_num_blks);

    /* read using PT[,PT...] sequence; output separate ROD Token for each */
    for (k = 0; in_num_blks > 0; in_num_blks -= num, ++k) {
        if (k > 0)
            signals_process_delay(op, DELAY_COPY_SEGMENT);
        num = in_num_blks;
        if (op->bpt_given && ((uint64_t)op->bpt_i < num))
            num = op->bpt_i;
        if ((idip->odxp->max_tok_xfer_size > 0) &&
            (num > idip->odxp->max_tok_xfer_size))
            num = idip->odxp->max_tok_xfer_size;
        if (op->i_sgli.sglp)
            num = count_sgl_blocks_from(op->i_sgli.sglp, in_elems,
                                        in_blk_off, num,
                                        idip->odxp->max_range_desc);
        if (vb > 2)
            pr2serr("%s: k=%d, in_blk_off=0x%" PRIx64 ", i_num=%" PRIu64 "\n",
                    __func__, k, in_blk_off, num);

        if ((res = do_pop_tok(op, in_blk_off, num, ! op->list_id_given, vb3)))
            return res;
        if ((res = process_after_poptok(op, &tc_i, vb3)))
            return res;
        if (tc_i != num) {
            pr2serr("%s: number requested (in) differs from transfer "
                    "count\n", __func__);
            // ouch, think about this one
        }
        op->stats.in_full += tc_i;
        in_blk_off += tc_i;
        op->dd_count -= tc_i;
    }
    return 0;
}

/* This function is designed to copy large amounts (terabytes) with
 * potentially different block sizes on input and output. Returns
 * 0 on success. */
static int
odx_write_from_rods(struct opts_t * op)
{
    bool got_count;
    int k, res, n, off, out_blk_sz, out_elems, err, vb, vb3;
    uint64_t out_blk_off, num, o_num, r_o_num, oir, tc_o;
    int64_t out_num_blks, v;
    struct dev_info_t * odip = op->odip;
    uint8_t rt[520];

    vb = op->verbose;
    vb3 = (vb > 1) ? (vb - 2) : 0;
    got_count = (op->dd_count > 0);
    res = fetch_read_cap(op, DDPT_ARG_OUT, &out_num_blks, &out_blk_sz);
    if (res)
        return res;
    v = out_num_blks;
    if (op->o_sgli.sglp) {  /* scatter list */
        out_elems = op->o_sgli.elems;
        sgl_sum_scan(&op->o_sgli, "write from rods",
                     (op->show_sgl_v2 || (vb > 2)), vb > 1);
        out_num_blks = op->o_sgli.sum;
    } else { /* no scatter list */
        out_elems = 1;
        out_num_blks = got_count ? op->dd_count : 0;
    }
    if (0 == op->dd_count) {
        if (vb)
            pr2serr("%s: enough checks, count=0 given so exit\n", __func__);
        return 0;
    }
    if ((op->dd_count < 0) && (0 == out_num_blks)) {
        if (vb > 1)
            pr2serr("%s: write the lot after scaling for seek=\n", __func__);
        v -= op->o_sgli.lowest_lba;
        if (v < 0) {
            pr2serr("%s: seek exceeds out device size\n", __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        out_num_blks = v;
    }

    out_blk_off = 0;
    op->dd_count = out_num_blks;
    op->stats.dd_count_start = op->dd_count;
    if (vb > 1)
        pr2serr("%s: about to write %" PRIi64 " blocks (seen from output)\n",
                    __func__, out_num_blks);

    /* copy using PT, WUT, [WUT, ...], PT, WUT, [WUT, ...] sequence */
    for (k = 0; out_num_blks > 0; out_num_blks -= num, ++k) {
        if (k > 0)
            signals_process_delay(op, DELAY_COPY_SEGMENT);

        memset(rt, 0, sizeof(rt));
        n = op->rtf_len_add ? 520 : 512;
        res = read(op->rtf_fd, rt, n);
        if (res < 0) {
            err = errno;
            pr2serr("%s: could not read '%s': %s\n", __func__, op->rtf,
                    safe_strerror(err));
            return SG_LIB_FILE_ERROR;
        }
        if (0 == res) {
            if (vb)
                pr2serr("%s: there are no more tokens to read from RTF or, \n"
                        "if it is a pipe or socket, the other end closed "
                        " it\n", __func__);
            break;
        }
        if (res < n) {
            pr2serr("%s: unable to read %d bytes from '%s', only got %d "
                    "bytes\n", __func__, (int)sizeof(rt), op->rtf, res);
            pr2serr("    try to continue\n");
        }
        if (op->rtf_len_add)
            off = 512;
        else {
            /* 'number of bytes represented' is a 16 byte integer! It starts
             * at offset 48 and may not be present so its contents might be
             * random. If any of the top 8 bytes are non-zero, give up. */
            for (n = 0; n < 8; ++n) {
                if (0x0 != rt[48 + n])
                    break;
            }
            if (n < 8) {
                pr2serr("%s: wild 'bytes represented' field in ROD Token so "
                        "give up.\n    Try again with conv=rtf_len\n",
                         __func__);
                return SG_LIB_CAT_OTHER;
            }
            off = 56;
        }
        num = sg_get_unaligned_be64(rt + off);
        o_num = num / (unsigned int)op->obs_lb;
        if (o_num > 0xffffffffffLL) {
            pr2serr("%s: ROD size seems too large (%" PRIu64 " blocks "
                    "each %d bytes)\nTry again with conv=rtf_len\n", __func__,
                    o_num, op->obs_lb);
            return SG_LIB_CAT_OTHER;
        }
        if (0 == o_num) {
            pr2serr("%s: ROD size is less than 1 block (%d bytes). Try "
                    "again with conv=rtf_len\n", __func__, op->obs_lb);
            return SG_LIB_CAT_OTHER;
        }
        num = o_num;

        for (oir = 0; o_num > 0; oir += r_o_num, o_num -= r_o_num) {
            /* output dev might be more constrained than input, so multiple
             * WUT calls (latter ones using offset in ROD) may be needed */
            if (k > 0)
                signals_process_delay(op, DELAY_WRITE);
            r_o_num = o_num;
            if (op->bpt_given) {
                /* take either bpt argument since input is a ROD */
                if ((op->obpch > 0) && ((uint64_t)op->obpch < r_o_num))
                    r_o_num = op->obpch;
                else if ((op->bpt_i > 0) && ((uint64_t)op->bpt_i < r_o_num))
                    r_o_num = op->bpt_i;
            }
            if ((odip->odxp->max_tok_xfer_size > 0) &&
                (r_o_num > odip->odxp->max_tok_xfer_size))
                r_o_num = odip->odxp->max_tok_xfer_size;
            if (op->o_sgli.sglp)
                r_o_num = count_sgl_blocks_from(op->o_sgli.sglp, out_elems,
                                                out_blk_off, r_o_num,
                                                odip->odxp->max_range_desc);
            res = do_wut(op, rt, out_blk_off, r_o_num, oir,
                         (r_o_num < o_num), ! op->list_id_given, vb3);
            if (res)
                return res;
            if ((res = process_after_wut(op, &tc_o, vb3)))
                return res;
            if (tc_o != r_o_num) {
                pr2serr("%s: number requested (out) differs from transfer "
                        "count\n", __func__);
                // ouch, could have over-drained ROD
            }
            op->stats.out_full += tc_o;
            out_blk_off += tc_o;
            op->dd_count -= tc_o;
        }
    }
    return 0;
}

/* This function is designed to copy large amounts (terabytes) with
 * potentially different block sizes on input and output. Returns
 * 0 on success. */
static int
odx_full_copy(struct opts_t * op)
{
    bool got_count, ok, oneto1;
    int k, res, in_blk_sz, out_blk_sz, in_mult, out_mult;
    int in_elems, out_elems, vb, vb3;
    uint64_t in_blk_off, out_blk_off, num, o_num, r_o_num, oir, tc_i, tc_o;
    int64_t in_num_blks, out_num_blks, u, uu, v, vv;
    struct dev_info_t * idip = op->idip;
    struct dev_info_t * odip = op->odip;

    vb = op->verbose;
    vb3 = (vb > 1) ? (vb - 2) : 0;
    got_count = (op->dd_count > 0);
    /* need to know block size of input and output */
    res = fetch_read_cap(op, DDPT_ARG_IN, &in_num_blks, &in_blk_sz);
    if (res)
        return res;
    u = in_num_blks;
    res = fetch_read_cap(op, DDPT_ARG_OUT, &out_num_blks, &out_blk_sz);
    if (res)
        return res;
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
    if (op->i_sgli.sglp) {   /* gather list */
        in_elems = op->i_sgli.elems;
        sgl_sum_scan(&op->i_sgli, "odx full[in]",
                     (op->show_sgl_v2 || (vb > 2)), vb > 1);
        in_num_blks = op->i_sgli.sum;
        if (got_count && (in_num_blks != op->dd_count)) {
            pr2serr("%s: count= value not equal to the sum of gather nums\n",
                    __func__);
            return SG_LIB_CAT_OTHER;
        }
    } else {
        in_elems = 1;
        in_num_blks = got_count ? op->dd_count : 0;
    }
    if (op->o_sgli.sglp) {  /* scatter list */
        out_elems = op->o_sgli.elems;
        sgl_sum_scan(&op->o_sgli, "odx full[out]",
                     (op->show_sgl_v2 || (vb > 2)), vb > 1);
        out_num_blks = op->o_sgli.sum;
        if (oneto1) {
            if (got_count && (out_num_blks != op->dd_count)) {
                pr2serr("%s: count= value not equal to the sum of scatter "
                        "nums\n", __func__);
                return SG_LIB_SYNTAX_ERROR;
            }
            if (out_num_blks != in_num_blks) {
                pr2serr("%s: number of blocks in gather list differ from "
                        "scatter list\n", __func__);
                if (op->iflagp->force || op->oflagp->force)
                    pr2serr("... continuing due to force flag\n");
                else {
                    pr2serr("... can be overridden with force flag\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
            }
        } else { /* unequal block size */
            u = out_blk_sz * out_num_blks;
            if (op->i_sgli.sglp && (u != (in_blk_sz * in_num_blks))) {
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
        out_elems = 1;
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
        if (vb)
            pr2serr("%s: enough checks, count=0 given so exit\n", __func__);
        return 0;
    }
    if ((op->dd_count < 0) && (0 == in_num_blks) && (0 == out_num_blks)) {
        if (vb > 1)
            pr2serr("%s: copy the lot after scaling for skip= and seek=\n",
                    __func__);
        u -= op->i_sgli.lowest_lba;
        v -= op->o_sgli.lowest_lba;
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

    in_blk_off = 0;
    out_blk_off = 0;
    op->dd_count = in_num_blks;
    op->stats.dd_count_start = op->dd_count;
    if (vb > 1)
        pr2serr("%s: about to copy %" PRIi64 " blocks (seen from input)\n",
                    __func__, in_num_blks);

    /* copy using PT, WUT, [WUT, ...], PT, WUT, [WUT, ...] sequence */
    for (k = 0; in_num_blks > 0; in_num_blks -= num, ++k) {
        if (k > 0)
            signals_process_delay(op, DELAY_COPY_SEGMENT);
        num = in_num_blks;
        if (op->bpt_given && ((uint64_t)op->bpt_i < num))
            num = op->bpt_i;
        if ((idip->odxp->max_tok_xfer_size > 0) &&
            (num > idip->odxp->max_tok_xfer_size))
            num = idip->odxp->max_tok_xfer_size;
        if (op->i_sgli.sglp)
            num = count_sgl_blocks_from(op->i_sgli.sglp, in_elems,
                                 in_blk_off, num, idip->odxp->max_range_desc);
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
        if (vb > 2)
            pr2serr("%s: k=%d, in_blk_off=0x%" PRIx64 ", i_num=%" PRIu64 ", "
                    "out_blk_off=0x%" PRIx64 ", o_num=%" PRIu64 "\n",
                    __func__, k, in_blk_off, num, out_blk_off, o_num);

        if ((res = do_pop_tok(op, in_blk_off, num, ! op->list_id_given, vb3)))
            return res;
        if ((res = process_after_poptok(op, &tc_i, vb3)))
            return res;
        if (tc_i != num) {
            pr2serr("%s: number requested (in) differs from transfer "
                    "count\n", __func__);
            // ouch, think about this one
        }
        op->stats.in_full += tc_i;
        in_blk_off += tc_i;

        for (oir = 0; o_num > 0; oir += r_o_num, o_num -= r_o_num) {
            /* output dev might be more constrained than input, so multiple
             * WUT calls (latter ones using offset in ROD) may be needed */
            if (k > 0)
                signals_process_delay(op, DELAY_WRITE);
            r_o_num = o_num;
            if ((op->obpch > 0) && ((uint64_t)op->obpch < r_o_num))
                r_o_num = op->obpch;
            if ((odip->odxp->max_tok_xfer_size > 0) &&
                (r_o_num > odip->odxp->max_tok_xfer_size))
                r_o_num = odip->odxp->max_tok_xfer_size;
            if (op->o_sgli.sglp)
                r_o_num = count_sgl_blocks_from(op->o_sgli.sglp, out_elems,
                                                out_blk_off, r_o_num,
                                                odip->odxp->max_range_desc);
            res = do_wut(op, local_rod_token, out_blk_off, r_o_num, oir,
                         (r_o_num < o_num), ! op->list_id_given, vb3);
            if (res)
                return res;
            if ((res = process_after_wut(op, &tc_o, vb3)))
                return res;
            if (tc_o != r_o_num) {
                pr2serr("%s: number requested (out) differs from transfer "
                        "count\n", __func__);
                // ouch, could have over-drained ROD
            }
            op->stats.out_full += tc_o;
            out_blk_off += tc_o;
        }
        op->dd_count -= tc_i;
    }
    return 0;
}

/* *whop<=0 for both in+out, *whop==1 for in, *whop==2 for out. Return 0
 * if okay; else sg3_utils style error code. */
static int
odx_setup_and_run(struct opts_t * op, int * whop)
{
    int fd, res, req;
    uint32_t pg_sz = sg_get_page_size();
    struct dev_info_t * dip;

    if (whop)
        *whop = 0;
    req = op->odx_request;
    if (! op->list_id_given)
        op->list_id = (ODX_WRITE_FROM_RODS == req) ?
                      DEF_LID4_WR_LID : DEF_LID4_LID;
    if ((ODX_READ_INTO_RODS == req) ||
        ((ODX_COPY == req) && (RODT_BLK_ZERO != op->rod_type))) {
        fd = pt_open_if(op, NULL);
        if (fd < 0)
            return -fd;
        dip = op->idip;
        dip->fd = fd;
        dip->odxp = (struct block_rodtok_vpd *)
                        sg_memalign(sizeof(*dip->odxp), pg_sz,
                                    &dip->free_odxp, false);
        if (NULL == dip->odxp) {
            pr2serr("%s: Not enough user memory\n", __func__);
            return sg_convert_errno(ENOMEM);
        }
        memset(dip->odxp, 0, sizeof(*dip->odxp));
        res = get_3pc_vpd_blkdev_lims(op, dip);
        if (res && (op->iflagp->force < 2))
            return res;
    }
    if ((ODX_WRITE_FROM_RODS == req) || (ODX_COPY == req)) {
        fd = pt_open_of(op, NULL);
        if (fd < 0)
            return -fd;
        dip = op->odip;
        dip->fd = fd;
        dip->odxp = (struct block_rodtok_vpd *)
                        sg_memalign(sizeof(*dip->odxp), pg_sz,
                                    &dip->free_odxp, false);
        if (NULL == dip->odxp) {
            pr2serr("Not enough user memory for %s 2\n", __func__);
            return sg_convert_errno(ENOMEM);
        }
        memset(dip->odxp, 0, sizeof(*dip->odxp));
        res = get_3pc_vpd_blkdev_lims(op, dip);
        if (res && (op->oflagp->force < 2))
            return res;
    }

    if (ODX_READ_INTO_RODS == req) {
        if (whop)
            *whop = 1;
        res = odx_read_into_rods(op);
        if (res)
            return res;
    } else if (ODX_WRITE_FROM_RODS == req) {
        if (whop)
            *whop = 2;
        res = odx_write_from_rods(op);
        if (res)
            return res;
    } else if (ODX_COPY == req) {
        if (op->rod_type_given && (RODT_BLK_ZERO == op->rod_type)) {
            if (whop)
                *whop = 2;
            return odx_full_zero_copy(op);
        } else
            return odx_full_copy(op);
    }
    return 0;
}


/* Called from main() in ddpt.c . Returns 0 on success or a positive
 * errno value if problems. This is for ODX which is a subset of
 * xcopy(LID4) for disk->disk, disk->held and held-> disk copies. */
int
do_odx(struct opts_t * op)
{
    int ret, who;

    if (op->iflagp->append || op->oflagp->append)
        op->rtf_append = true;
    if (op->iflagp->rtf_len || op->oflagp->rtf_len)
        op->rtf_len_add = true;
    if (op->rtf[0]) {
        ret = open_rtf(op);
        if (ret)
            goto the_end;
    }
    if (op->do_time)
        calc_duration_init(op);
    ret = odx_setup_and_run(op, &who);
    if (0 == op->status_none)
        print_stats("", op, who, true);
    if (op->do_time)
        calc_duration_throughput("", false /* contin */, op);
    if (op->rtf_fd >= 0) {
        close(op->rtf_fd);
        op->rtf_fd = -1;
    }
the_end:
    if (op->idip && op->idip->free_odxp)
        free(op->idip->free_odxp);
    if (op->idip && op->odip->free_odxp)
        free(op->odip->free_odxp);
    if (op->idip && op->o2dip->free_odxp)
        free(op->o2dip->free_odxp);
    return ret;
}
