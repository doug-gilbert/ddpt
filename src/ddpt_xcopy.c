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


#define LOCAL_ROD_TOKEN_SIZE 2048
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
    return sg_ll_3party_copy_out(fd, SA_XCOPY_LID1, op->list_id,
                                 DEF_GROUP_NUM, tmout, xcopyBuff, desc_offset,
                                 1, verb);
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
    res = sg_ll_receive_copy_results(fd, SA_COPY_OP_PARAMS, 0, rcBuff,
                                     rcBuffLen, 1, verb);
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

static void
decode_designation_descriptor(const unsigned char * ucp, int i_len, int verb)
{
    int m, p_id, piv, c_set, assoc, desig_type, d_id, naa;
    int k;
    const unsigned char * ip;
    uint64_t vsei;
    char b[64];

    ip = ucp + 4;
    p_id = ((ucp[0] >> 4) & 0xf);
    c_set = (ucp[0] & 0xf);
    piv = ((ucp[1] & 0x80) ? 1 : 0);
    assoc = ((ucp[1] >> 4) & 0x3);
    desig_type = (ucp[1] & 0xf);
    pr2serr("    designator type: %d,  code set: %d\n", desig_type, c_set);
    if (piv && ((1 == assoc) || (2 == assoc)))
        pr2serr("     transport: %s\n",
                sg_get_trans_proto_str(p_id, sizeof(b), b));

    switch (desig_type) {
    case 0: /* vendor specific */
        k = 0;
        if ((1 == c_set) || (2 == c_set)) { /* ASCII or UTF-8 */
            for (k = 0; (k < i_len) && isprint(ip[k]); ++k)
                ;
            if (k >= i_len)
                k = 1;
        }
        if (k)
            pr2serr("      vendor specific: %.*s\n", i_len, ip);
        else {
            pr2serr("      vendor specific:\n");
            dStrHexErr((const char *)ip, i_len, 0);
        }
        break;
    case 1: /* T10 vendor identification */
        pr2serr("      vendor id: %.8s\n", ip);
        if (i_len > 8)
            pr2serr("      vendor specific: %.*s\n", i_len - 8, ip + 8);
        break;
    case 2: /* EUI-64 based */
        if ((8 != i_len) && (12 != i_len) && (16 != i_len)) {
            pr2serr("      << expect 8, 12 and 16 byte EUI, got %d>>\n",
                    i_len);
            dStrHexErr((const char *)ip, i_len, 0);
            break;
        }
        pr2serr("      0x");
        for (m = 0; m < i_len; ++m)
            pr2serr("%02x", (unsigned int)ip[m]);
        pr2serr("\n");
        break;
    case 3: /* NAA */
        if (1 != c_set) {
            pr2serr("      << unexpected code set %d for NAA>>\n", c_set);
            dStrHexErr((const char *)ip, i_len, 0);
            break;
        }
        naa = (ip[0] >> 4) & 0xff;
        if (! ((2 == naa) || (5 == naa) || (6 == naa))) {
            pr2serr("      << unexpected NAA [0x%x]>>\n", naa);
            dStrHexErr((const char *)ip, i_len, 0);
            break;
        }
        if ((5 == naa) && (0x10 == i_len)) {
            if (verb > 2)
                pr2serr("      << unexpected NAA 5 len 16, assuming NAA 6 "
                        ">>\n");
            naa = 6;
        }
        if (2 == naa) {
            if (8 != i_len) {
                pr2serr("      << unexpected NAA 2 identifier length: "
                        "0x%x>>\n", i_len);
                dStrHexErr((const char *)ip, i_len, 0);
                break;
            }
            d_id = (((ip[0] & 0xf) << 8) | ip[1]);
            /* c_id = ((ip[2] << 16) | (ip[3] << 8) | ip[4]); */
            /* vsi = ((ip[5] << 16) | (ip[6] << 8) | ip[7]); */
            pr2serr("      0x");
            for (m = 0; m < 8; ++m)
                pr2serr("%02x", (unsigned int)ip[m]);
            pr2serr("\n");
        } else if (5 == naa) {
            if (8 != i_len) {
                pr2serr("      << unexpected NAA 5 identifier length: "
                        "0x%x>>\n", i_len);
                dStrHexErr((const char *)ip, i_len, 0);
                break;
            }
            /* c_id = (((ip[0] & 0xf) << 20) | (ip[1] << 12) | */
                    /* (ip[2] << 4) | ((ip[3] & 0xf0) >> 4)); */
            vsei = ip[3] & 0xf;
            for (m = 1; m < 5; ++m) {
                vsei <<= 8;
                vsei |= ip[3 + m];
            }
            pr2serr("      0x");
            for (m = 0; m < 8; ++m)
                pr2serr("%02x", (unsigned int)ip[m]);
            pr2serr("\n");
        } else if (6 == naa) {
            if (16 != i_len) {
                pr2serr("      << unexpected NAA 6 identifier length: "
                        "0x%x>>\n", i_len);
                dStrHexErr((const char *)ip, i_len, 0);
                break;
            }
            /* c_id = (((ip[0] & 0xf) << 20) | (ip[1] << 12) | */
                    /* (ip[2] << 4) | ((ip[3] & 0xf0) >> 4)); */
            vsei = ip[3] & 0xf;
            for (m = 1; m < 5; ++m) {
                vsei <<= 8;
                vsei |= ip[3 + m];
            }
            pr2serr("      0x");
            for (m = 0; m < 16; ++m)
                pr2serr("%02x", (unsigned int)ip[m]);
            pr2serr("\n");
        }
        break;
    case 4: /* Relative target port */
        if ((1 != c_set) || (1 != assoc) || (4 != i_len)) {
            pr2serr("      << expected binary code_set, target port "
                    "association, length 4>>\n");
            dStrHexErr((const char *)ip, i_len, 0);
            break;
        }
        d_id = ((ip[2] << 8) | ip[3]);
        pr2serr("      Relative target port: 0x%x\n", d_id);
        break;
    case 5: /* (primary) Target port group */
        if ((1 != c_set) || (1 != assoc) || (4 != i_len)) {
            pr2serr("      << expected binary code_set, target port "
                    "association, length 4>>\n");
            dStrHexErr((const char *)ip, i_len, 0);
            break;
        }
        d_id = ((ip[2] << 8) | ip[3]);
        pr2serr("      Target port group: 0x%x\n", d_id);
        break;
    case 6: /* Logical unit group */
        if ((1 != c_set) || (0 != assoc) || (4 != i_len)) {
            pr2serr("      << expected binary code_set, logical unit "
                    "association, length 4>>\n");
            dStrHexErr((const char *)ip, i_len, 0);
            break;
        }
        d_id = ((ip[2] << 8) | ip[3]);
        pr2serr("      Logical unit group: 0x%x\n", d_id);
        break;
    case 7: /* MD5 logical unit identifier */
        if ((1 != c_set) || (0 != assoc)) {
            pr2serr("      << expected binary code_set, logical unit "
                    "association>>\n");
            dStrHexErr((const char *)ip, i_len, 0);
            break;
        }
        pr2serr("      MD5 logical unit identifier:\n");
        dStrHexErr((const char *)ip, i_len, 0);
        break;
    case 8: /* SCSI name string */
        if (3 != c_set) {
            pr2serr("      << expected UTF-8 code_set>>\n");
            dStrHexErr((const char *)ip, i_len, 0);
            break;
        }
        pr2serr("      SCSI name string:\n");
        /* does %s print out UTF-8 ok??
         * Seems to depend on the locale. Looks ok here with my
         * locale setting: en_AU.UTF-8
         */
        pr2serr("      %s\n", (const char *)ip);
        break;
    case 9: /* Protocol specific port identifier */
        /* added in spc4r36, PIV must be set, proto_id indicates */
        /* whether UAS (USB) or SOP (PCIe) or ... */
        if (! piv)
            pr2serr("      >>>> Protocol specific port identifier "
                    "expects protocol\n"
                    "           identifier to be valid and it is not\n");
        if (TPROTO_UAS == p_id) {
            pr2serr("      USB device address: 0x%x\n", 0x7f & ip[0]);
            pr2serr("      USB interface number: 0x%x\n", ip[2]);
        } else if (TPROTO_SOP == p_id) {
            pr2serr("      PCIe routing ID, bus number: 0x%x\n", ip[0]);
            pr2serr("          function number: 0x%x\n", ip[1]);
            pr2serr("          [or device number: 0x%x, function number: "
                    "0x%x]\n", (0x1f & (ip[1] >> 3)), 0x7 & ip[1]);
        } else
            pr2serr("      >>>> unexpected protocol indentifier: 0x%x\n"
                    "           with Protocol specific port "
                    "identifier\n", p_id);
        break;
    default: /* reserved */
        pr2serr("      reserved designator=0x%x\n", desig_type);
        dStrHexErr((const char *)ip, i_len, 0);
        break;
    }
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


static const char *
rod_type_str(uint32_t rt, char * b, int blen)
{
    const char * pitc = "Point in time copy -";

    switch (rt) {
    case 0x0:
        snprintf(b, blen, "Copy Manager internal");
        break;
    case 0x10000:
        snprintf(b, blen, "Access upon reference");
        break;
    case 0x800000:
        snprintf(b, blen, "%s default", pitc);
        break;
    case 0x800001:
        snprintf(b, blen, "%s change vulnerable", pitc);
        break;
    case 0x800002:
        snprintf(b, blen, "%s persistent", pitc);
        break;
    case 0x80ffff:
        snprintf(b, blen, "%s any", pitc);
        break;
    case 0xffff0001:
        snprintf(b, blen, "Block device zero ROD");
        break;
    default:
        if (rt < 0xff000000)
            snprintf(b, blen, "Reserved (any device type)");
        else if (rt < 0xfffffff0)
            snprintf(b, blen, "Reserved (device type specific)");
        else
            snprintf(b, blen, "Vendor specific");
    }
    return b;
}

static const char *
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


static int
odx_rt_info(const struct opts_t * op)
{
    int res, fd, err, m, prot_en, p_type, lbppbe, vendor;
    uint64_t bc;
    uint32_t rod_t, bs;
    uint16_t rtl;
    unsigned char rth[128];
    char b[128];

    if ('\0' == op->rtf[0]) {
        pr2serr("odx_rt_info: expected ROD Token filename (rtf=RTF)\n");
        return SG_LIB_FILE_ERROR;
    }
    if ((fd = open(op->rtf, O_RDONLY)) < 0) {
        err = errno;
        pr2serr("could not open '%s' for reading: %s\n", op->rtf,
                safe_strerror(err));
        return SG_LIB_FILE_ERROR;
    }
    res = read(fd, rth, sizeof(rth));
    if (res < 0) {
        err = errno;
        pr2serr("could not read '%s': %s\n", op->rtf,
                safe_strerror(err));
        close(fd);
        return SG_LIB_FILE_ERROR;
    }
    if (res < (int)sizeof(rth)) {
        pr2serr("unable to read %d bytes from '%s', only got %d bytes\n",
                 (int)sizeof(rth), op->rtf, res);
        pr2serr("... it is unlikely file '%s' contains a ROD Token\n",
                 op->rtf);
        close(fd);
        return SG_LIB_FILE_ERROR;
    }

    if (op->verbose > 3) {
        pr2serr("Hex dump of first %d bytes of ROD Token:\n",
                (int)sizeof(rth));
        dStrHexErr((const char *)rth, (int)sizeof(rth), 1);
    }

    printf("Decoding information from ROD Token header:\n");
    rod_t = (rth[0] << 24) + (rth[1] << 16) + (rth[2] << 8) + rth[3];
    printf("  ROD type: %s [0x%" PRIx32 "]\n",
           rod_type_str(rod_t, b, sizeof(b)), rod_t);
    if (rod_t >= 0xfffffff0) {
        printf("    Since ROD type is vendor specific, the following may "
               "not be relevant\n");
        vendor = 1;
    } else
        vendor = 0;
    rtl = (rth[6] << 8) + rth[7];
    if (rtl < 0x1f8) {
        pr2serr(">>> ROD Token length field is too short, should be at "
                "least\n    504 bytes (0x1f8), got 0x%" PRIx16 "\n", rtl);
        if (! vendor) {
            close(fd);
            return SG_LIB_FILE_ERROR;
        }
    }
    printf("  Copy manager ROD Token identifier=0x");
    for (m = 0; m < 8; ++m)
        printf("%02x", (unsigned int)rth[8 + m]);
    printf("\n");
    printf("  Creator Logical Unit descriptor:\n");
    /* should make smaller version of following that outputs to stdout */
    if (0xe4 != rth[16]) {
        pr2serr(">>> Expected Identification descriptor (0xe4) got 0x%x\n",
                rth[16]);
        if (! vendor) {
            close(fd);
            return SG_LIB_FILE_ERROR;
        }
    }
    printf("    Peripheral Device type: 0x%x\n", rth[17] & 0x1f);
    printf("    Relative initiator port identifier: 0x%x\n",
           (rth[18] << 8) + rth[19]);
    decode_designation_descriptor(rth + 20, 28, op->verbose);
    bc = 0;
    for (m = 0; m < 8; m++) {
        if (m > 0)
            bc <<= 8;
        bc |= rth[48 + m];
    }
    printf("  Number of bytes represented: %" PRIu64 " [0x%" PRIx64 "]\n",
           bc, bc);

    printf("  Assume pdt=0 (e.g. disk) and decode device type specific "
           "data:\n");
    bs = ((rth[96] << 24) + (rth[97] << 16) + (rth[98] << 8) + rth[99]);
    printf("    block size: %" PRIu32 " [0x%" PRIx32 "] bytes\n", bs, bs);
    prot_en = !!(rth[100] & 0x1);
    p_type = ((rth[100] >> 1) & 0x7);
    printf("    Protection: prot_en=%d, p_type=%d, p_i_exponent=%d",
           prot_en, p_type, ((rth[101] >> 4) & 0xf));
    if (prot_en)
        printf(" [type %d protection]\n", p_type + 1);
    else
        printf("\n");
    printf("    Logical block provisioning: lbpme=%d, lbprz=%d\n",
                   !!(rth[102] & 0x80), !!(rth[102] & 0x40));
    lbppbe = rth[102] & 0xf;
    printf("    Logical blocks per physical block exponent=%d", lbppbe);
    if (lbppbe > 0)
        printf(" [so physical block length=%u bytes]\n", bs * (1 << lbppbe));
    else
        printf("\n");
    printf("    Lowest aligned logical block address=%d\n",
           ((rth[102] & 0x3f) << 8) + rth[103]);

    close(fd);
    return 0;
}

static int
fetch_3pc_vpd(struct opts_t * op, struct dev_info_t * dip)
{
    unsigned char rBuff[256];
    unsigned char * rp;
    unsigned char * ucp;
    int res, verb, n, len, bump, desc_type, desc_len, k, j, is_src;
    int found = 0;
    uint32_t max_ito = 0;
    uint64_t ull;

    verb = (op->verbose ? (op->verbose - 1) : 0);
    is_src = (op->idip == dip);
    rp = rBuff;
    n = (int)sizeof(rBuff);
    res = sg_ll_inquiry(dip->fd, 0, 1, VPD_3PARTY_COPY, rp, n, 1, verb);
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
    if (len > n) {
        rp = (unsigned char *)malloc(len);
        if (NULL == rp) {
            pr2serr("Not enough user memory for fetch_3pc_vpd\n");
            return SG_LIB_CAT_OTHER;
        }
        res = sg_ll_inquiry(dip->fd, 0, 1, VPD_3PARTY_COPY, rp, len, 1,
                            verb);
        if (res) {
            pr2serr("3PARTY_COPY VPD inquiry failed with %d\n", res);
            if (rBuff != rp)
                free(rp);
            return res;
        }
    }
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
            dip->odxp->brt_vpd.max_range_desc = (ucp[10] << 8) + ucp[11];
            max_ito = (ucp[12] << 24) | (ucp[13] << 16) | (ucp[14] << 8) |
                      ucp[15];
            dip->odxp->brt_vpd.max_inactivity_to = max_ito;
            dip->odxp->brt_vpd.def_inactivity_to = (ucp[16] << 24) |
                         (ucp[17] << 16) | (ucp[18] << 8) | ucp[19];
            ull = 0;
            for (j = 0; j < 8; j++) {
                if (j > 0)
                    ull <<= 8;
                ull |= ucp[20 + j];
            }
            dip->odxp->brt_vpd.max_tok_xfer_size = ull;
            ull = 0;
            for (j = 0; j < 8; j++) {
                if (j > 0)
                    ull <<= 8;
                ull |= ucp[28 + j];
            }
            dip->odxp->brt_vpd.optimal_xfer_count = ull;
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
        if (is_src && op->iflagp->force)
            ;
        else if ((! is_src) && op->oflagp->force)
            ;
        else {
            pr2serr("... exiting; can override with 'force' flag\n");
            return SG_LIB_CAT_OTHER;
        }
    }
    return 0;
}

static int
do_pop_tok(struct opts_t * op)
{
    int k, n, verb, len, fd, tmout, allowed_descs;
    uint64_t lba, sgl_total;
    uint32_t num;
    unsigned char pl[512 + 32]; /* large enough for biggest sgl */

    verb = (op->verbose > 1) ? (op->verbose - 2) : 0;
    fd = op->idip->fd;
    if (op->in_sgl) {
        for (k = 0, sgl_total = 0; k < op->in_sgl_elems; ++k)
            sgl_total += op->in_sgl[k].num;
        if ((op->dd_count >= 0) && ((uint64_t)op->dd_count != sgl_total)) {
            pr2serr("%s: count= value not equal to the sum of gather "
                    "nums\n", __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        op->dd_count = (int64_t)sgl_total;
        if ((! op->iflagp->force ) && op->idip->odxp) {
            allowed_descs = op->idip->odxp->brt_vpd.max_range_desc;
            if ((allowed_descs > 0) && (op->in_sgl_elems > allowed_descs)) {
                pr2serr("%s: number of gather list elements exceeds what the "
                        "Block Device ROD\nToken Limits descriptor in the 3PC "
                        "VPD page permits (%d).\nCan try 'iflag=force'\n",
                        __func__, allowed_descs);
                return SG_LIB_CAT_OTHER;
            }
        }
    }
    memset(pl, 0, sizeof(pl));
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

    if (op->in_sgl) {
        len = op->in_sgl_elems * 16;
        pl[14] = (unsigned char)((len >> 8) & 0xff);
        pl[15] = (unsigned char)(len & 0xff);
        for (k = 0, n = 15; k < op->in_sgl_elems; ++k) {
            lba = op->in_sgl[k].lba;
            pl[++n] = (unsigned char)((lba >> 56) & 0xff);
            pl[++n] = (unsigned char)((lba >> 48) & 0xff);
            pl[++n] = (unsigned char)((lba >> 40) & 0xff);
            pl[++n] = (unsigned char)((lba >> 32) & 0xff);
            pl[++n] = (unsigned char)((lba >> 24) & 0xff);
            pl[++n] = (unsigned char)((lba >> 16) & 0xff);
            pl[++n] = (unsigned char)((lba >> 8) & 0xff);
            pl[++n] = (unsigned char)(lba & 0xff);
            num = op->in_sgl[k].num;
            pl[++n] = (unsigned char)((num >> 24) & 0xff);
            pl[++n] = (unsigned char)((num >> 16) & 0xff);
            pl[++n] = (unsigned char)((num >> 8) & 0xff);
            pl[++n] = (unsigned char)(num & 0xff);
            n += 4;
        }
        len = n + 1;
    } else {    /* assume count= and possibly skip= given */
        len = 16;       /* single element */
        pl[14] = (unsigned char)((len >> 8) & 0xff);
        pl[15] = (unsigned char)(len & 0xff);
        pl[16] = (unsigned char)((op->skip >> 56) & 0xff);
        pl[17] = (unsigned char)((op->skip >> 48) & 0xff);
        pl[18] = (unsigned char)((op->skip >> 40) & 0xff);
        pl[19] = (unsigned char)((op->skip >> 32) & 0xff);
        pl[20] = (unsigned char)((op->skip >> 24) & 0xff);
        pl[21] = (unsigned char)((op->skip >> 16) & 0xff);
        pl[22] = (unsigned char)((op->skip >> 8) & 0xff);
        pl[23] = (unsigned char)(op->skip & 0xff);
        pl[24] = (unsigned char)((op->dd_count >> 24) & 0xff);
        pl[25] = (unsigned char)((op->dd_count >> 16) & 0xff);
        pl[26] = (unsigned char)((op->dd_count >> 8) & 0xff);
        pl[27] = (unsigned char)(op->dd_count & 0xff);
        len = 32;
    }
    n = len - 2;
    pl[0] = (unsigned char)((n >> 8) & 0xff);
    pl[1] = (unsigned char)(n & 0xff);

    tmout = (op->timeout_xcopy < 1) ? DEF_3PC_OUT_TIMEOUT : op->timeout_xcopy;
    return sg_ll_3party_copy_out(fd, SA_POP_TOK, op->list_id, DEF_GROUP_NUM,
                                 tmout, pl, len, 1, verb);
}

static int
fetch_rt_after_poptok(struct opts_t * op)
{
    int j, res, fd, verb, for_sa, cstat, lsdf, off, err;
    unsigned int len, rtdl;
    uint64_t ull;
    unsigned char rsp[2048];
    char b[400];

    verb = (op->verbose > 1) ? (op->verbose - 2) : 0;
    fd = op->idip->fd;
    res = sg_ll_receive_copy_results(fd, SA_ROD_TOK_INFO, (int)op->list_id,
                                     rsp, sizeof(rsp), 1, verb);
    if (res)
        return res;

    len = ((rsp[0] << 24) | (rsp[1] << 16) | (rsp[2] << 8) | rsp[3]) + 4;
    if (len > sizeof(rsp)) {
        pr2serr("  ROD Token info too long for internal buffer, output "
                "truncated\n");
        len = sizeof(rsp);
    }
    if (verb > 1) {
        pr2serr("\nOutput response in hex:\n");
        dStrHexErr((const char *)rsp, len, 1);
    }
    for_sa = 0x1f & rsp[4];
    if (SA_POP_TOK != for_sa) {
        sg_get_opcode_sa_name(THIRD_PARTY_COPY_OUT_CMD, for_sa, 0, sizeof(b),
                              b);
        pr2serr("Receive ROD Token info expected response for Populate "
                "Token\n  but got response for %s\n", b);
    }
    cstat = 0x7f & rsp[5];
    if ((! ((0x1 == cstat) || (0x3 == cstat))) || verb)
        pr2serr("PT: %s\n", cpy_op_status_str(cstat, b, sizeof(b)));
    ull = 0;
    for (j = 0; j < 8; j++) {
        if (j > 0)
            ull <<= 8;
        ull |= rsp[16 + j];
    }
    op->in_full += ull;
    if (verb > 1)
        pr2serr("PT: Transfer count=%" PRIu64 " [0x%" PRIx64 "]\n", ull, ull);
    lsdf = rsp[13];
    if (lsdf > 0)
        sg_get_sense_str("PT related sense:", rsp + 32, rsp[14], verb,
                         sizeof(b), b);
    off = 32 + lsdf;
    rtdl = (rsp[off] << 24) | (rsp[off + 1] << 16) | (rsp[off + 2] << 8) |
           rsp[off + 3];
    if (rtdl > 2) {
        if (op->rtf[0]) {     /* write ROD Token to RTF */
            fd = open(op->rtf, O_WRONLY | O_CREAT, 0644);
            if (fd < 0) {
                err = errno;
                pr2serr("%s: unable to create file: %s [%s]\n", __func__,
                        op->rtf, safe_strerror(err));
                return SG_LIB_FILE_ERROR;
            }
            res = write(fd, rsp + off + 6, rtdl - 2);
            if (res < 0) {
                err = errno;
                pr2serr("%s: unable to write to file: %s [%s]\n", __func__,
                        op->rtf, safe_strerror(err));
                close(fd);
                return SG_LIB_FILE_ERROR;
            }
            close(fd);
            if (res < (int)(rtdl - 2)) {
                pr2serr("%s: short write to file: %s, wanted %d, got %d\n",
                        __func__, op->rtf, (rtdl - 2), res);
                return SG_LIB_CAT_OTHER;
            }
        } else {        /* write ROD Token to static */
            if ((rtdl - 2) > LOCAL_ROD_TOKEN_SIZE) {
                pr2serr("%s: ROD token too large for static storage, try "
                        "'rtf=RTF'\n", __func__);
                return SG_LIB_CAT_OTHER;
            }
            memcpy(local_rod_token, rsp + off + 6, rtdl - 2);
        }
    }
    return 0;
}

static int
do_wut(struct opts_t * op)
{
    int verb, len, k, n, fd, err, res, tmout, sz_bdrd, allowed_descs;
    struct flags_t * flp;
    uint64_t lba, sgl_total;
    uint32_t num;
    unsigned char pl[1024 + 32]; /* large enough for biggest sgl + ROD tok */
    unsigned char rt[512];

    verb = (op->verbose > 1) ? (op->verbose - 2) : 0;
    flp = op->oflagp;
    memset(pl, 0, sizeof(pl));
    if (op->out_sgl) {
        if (op->dd_count >= 0) {
            pr2serr("%s: count= value and scatter list is confusing\n",
                    __func__);
            return SG_LIB_SYNTAX_ERROR;
        }
        for (k = 0, sgl_total = 0; k < op->out_sgl_elems; ++k)
            sgl_total += op->out_sgl[k].num;
        op->dd_count = (int64_t)sgl_total;      /* >>> wrong is ibs != obs */
        if ((! op->oflagp->force ) && op->odip->odxp) {
            allowed_descs = op->odip->odxp->brt_vpd.max_range_desc;
            if ((allowed_descs > 0) && (op->out_sgl_elems > allowed_descs)) {
                pr2serr("%s: number of scatter list elements exceeds what "
                        "the Block Device ROD\nToken Limits descriptor in "
                        "the 3PC VPD page permits (%d).\nCan try "
                        "'oflag=force'\n", __func__, allowed_descs);
                return SG_LIB_CAT_OTHER;
            }
        }
    }
    if (ODX_REQ_WUT == op->odx_request) {
        if (flp->del_tkn)
            pl[2] = 0x2;        /* DEL_TKN bit */
    } else {
        if (! flp->no_del_tkn)
            pl[2] = 0x2;        /* data->data default DEL_TKN unless */
    }
    if (flp->immed)
        pl[2] |= 0x1;           /* IMMED bit */
    if (op->offset_in_rod) {
        pl[8] = (unsigned char)((op->offset_in_rod >> 56) & 0xff);
        pl[9] = (unsigned char)((op->offset_in_rod >> 48) & 0xff);
        pl[10] = (unsigned char)((op->offset_in_rod >> 40) & 0xff);
        pl[11] = (unsigned char)((op->offset_in_rod >> 32) & 0xff);
        pl[12] = (unsigned char)((op->offset_in_rod >> 24) & 0xff);
        pl[13] = (unsigned char)((op->offset_in_rod >> 16) & 0xff);
        pl[14] = (unsigned char)((op->offset_in_rod >> 8) & 0xff);
        pl[15] = (unsigned char)(op->offset_in_rod & 0xff);
    }
    if (RODT_BLK_ZERO == op->rod_type) {
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

    if (op->out_sgl) {
        sz_bdrd = 16 * op->out_sgl_elems;
        pl[534] = (unsigned char)((sz_bdrd >> 8) & 0xff);
        pl[535] = (unsigned char)(sz_bdrd & 0xff);
        for (k = 0, n = 535; k < op->out_sgl_elems; ++k) {
            lba = op->out_sgl[k].lba;
            pl[++n] = (unsigned char)((lba >> 56) & 0xff);
            pl[++n] = (unsigned char)((lba >> 48) & 0xff);
            pl[++n] = (unsigned char)((lba >> 40) & 0xff);
            pl[++n] = (unsigned char)((lba >> 32) & 0xff);
            pl[++n] = (unsigned char)((lba >> 24) & 0xff);
            pl[++n] = (unsigned char)((lba >> 16) & 0xff);
            pl[++n] = (unsigned char)((lba >> 8) & 0xff);
            pl[++n] = (unsigned char)(lba & 0xff);
            num = op->out_sgl[k].num;
            pl[++n] = (unsigned char)((num >> 24) & 0xff);
            pl[++n] = (unsigned char)((num >> 16) & 0xff);
            pl[++n] = (unsigned char)((num >> 8) & 0xff);
            pl[++n] = (unsigned char)(num & 0xff);
            n += 4;
        }
    } else {    /* assume count= and possibly seek= given */
        sz_bdrd = 16;   /* single element */
        pl[534] = (unsigned char)((sz_bdrd >> 8) & 0xff);
        pl[535] = (unsigned char)(sz_bdrd & 0xff);
        pl[536] = (unsigned char)((op->skip >> 56) & 0xff);
        pl[537] = (unsigned char)((op->skip >> 48) & 0xff);
        pl[538] = (unsigned char)((op->skip >> 40) & 0xff);
        pl[539] = (unsigned char)((op->skip >> 32) & 0xff);
        pl[540] = (unsigned char)((op->skip >> 24) & 0xff);
        pl[541] = (unsigned char)((op->skip >> 16) & 0xff);
        pl[542] = (unsigned char)((op->skip >> 8) & 0xff);
        pl[543] = (unsigned char)(op->skip & 0xff);
        pl[544] = (unsigned char)((op->dd_count >> 24) & 0xff);
        pl[545] = (unsigned char)((op->dd_count >> 16) & 0xff);
        pl[546] = (unsigned char)((op->dd_count >> 8) & 0xff);
        pl[547] = (unsigned char)(op->dd_count & 0xff);
    }
    len = 536 +  sz_bdrd;
    n = len - 2;
    pl[0] = (unsigned char)((n >> 8) & 0xff);
    pl[1] = (unsigned char)(n & 0xff);
    fd = op->odip->fd;

    tmout = (op->timeout_xcopy < 1) ? DEF_3PC_OUT_TIMEOUT : op->timeout_xcopy;
    return sg_ll_3party_copy_out(fd, SA_WR_USING_TOK, op->list_id,
                                 DEF_GROUP_NUM, tmout, pl, len, 1, verb);
}

static int
fetch_rt_after_wut(struct opts_t * op)
{
    int j, res, fd, verb, for_sa, cstat, lsdf, off;
    unsigned int len, rtdl;
    uint64_t ull;
    unsigned char rsp[2048];
    char b[80];

    verb = (op->verbose > 1) ? (op->verbose - 2) : 0;
    fd = op->odip->fd;
    res = sg_ll_receive_copy_results(fd, SA_ROD_TOK_INFO, (int)op->list_id,
                                     rsp, sizeof(rsp), 1, verb);
    if (res)
        return res;

    len = ((rsp[0] << 24) | (rsp[1] << 16) | (rsp[2] << 8) | rsp[3]) + 4;
    if (len > sizeof(rsp)) {
        pr2serr("  %s: response is too long for internal buffer, output "
                "truncated\n", __func__);
        len = sizeof(rsp);
    }
    if (verb > 1) {
        pr2serr("\nOutput response in hex:\n");
        dStrHexErr((const char *)rsp, len, 1);
    }
    for_sa = 0x1f & rsp[4];
    if (SA_WR_USING_TOK != for_sa) {
        sg_get_opcode_sa_name(THIRD_PARTY_COPY_OUT_CMD, for_sa, 0, sizeof(b),
                              b);
        pr2serr("Receive ROD Token info expected response for Write "
                "Using Token\n  but got response for %s\n", b);
    }
    cstat = 0x7f & rsp[5];
    if ((! ((0x1 == cstat) || (0x3 == cstat))) || verb)
        pr2serr("WUT: %s\n", cpy_op_status_str(cstat, b, sizeof(b)));
    ull = 0;
    for (j = 0; j < 8; j++) {
        if (j > 0)
            ull <<= 8;
        ull |= rsp[16 + j];
    }
    op->out_full += ull;
    if (verb)
        pr2serr("WUT: Transfer count=%" PRIu64 " [0x%" PRIx64 "]\n", ull,
                ull);
    lsdf = rsp[13];
    if (lsdf > 0)
        sg_get_sense_str("WUT related sense:", rsp + 32, rsp[14], verb,
                         sizeof(b), b);
    if (len > 38) {
        off = 32 + lsdf;
        rtdl = (rsp[off] << 24) | (rsp[off + 1] << 16) | (rsp[off + 2] << 8) |
               rsp[off + 3];
        if (rtdl > 2)
            pr2serr("%s: Hmmm, got ROD Token returned\n", __func__);
    }
    return 0;
}

static int
report_all_toks(struct opts_t * op, struct dev_info_t * dip)
{
    int res, fd, verb;
    unsigned int len;
    unsigned char rsp[2048];

    verb = (op->verbose > 1) ? (op->verbose - 2) : 0;
    fd = dip->fd;
    res = sg_ll_receive_copy_results(fd, SA_ALL_ROD_TOKS, (int)op->list_id,
                                     rsp, sizeof(rsp), 1, verb);
    if (res)
        return res;

    len = ((rsp[0] << 24) | (rsp[1] << 16) | (rsp[2] << 8) | rsp[3]) + 4;
    if (len > sizeof(rsp)) {
        pr2serr("  ROD Tokens too long for internal buffer, output "
                "truncated\n");
        len = sizeof(rsp);
    }
    if (op->verbose > 2) {
        pr2serr("\nReport all ROD tokens response in hex:\n");
        dStrHexErr((const char *)rsp, len, 1);
    }
    // Hmmm, not supported on HP 3 PAR (and not required for ODX)
    return 0;
}

static int
odx_copy_work(struct opts_t * op)
{
    int fd, res;
    struct dev_info_t * dip;

    if (ODX_REQ_RT_INFO == op->odx_request)
        return odx_rt_info(op);
    else if (ODX_REQ_ALL_TOKS == op->odx_request) {
        if (op->idip->fn[0]) {
            fd = pt_open_if(op);
            if (-1 == fd)
                return SG_LIB_FILE_ERROR;
            else if (fd < -1)
                return -fd;
            dip = op->idip;
            dip->fd = fd;
        } else {
            fd = pt_open_of(op);
            if (-1 == fd)
                return SG_LIB_FILE_ERROR;
            else if (fd < -1)
                return -fd;
            dip = op->odip;
            dip->fd = fd;
        }
        res = report_all_toks(op, dip);
        if (res)
                return res;
    }
    if (! op->list_id_given)
        op->list_id = (ODX_REQ_WUT == op->odx_request) ? 0x102 : 0x101;
    if ((ODX_REQ_PT == op->odx_request) ||
        (ODX_REQ_COPY == op->odx_request)) {
        fd = pt_open_if(op);
        if (-1 == fd)
            return SG_LIB_FILE_ERROR;
        else if (fd < -1)
            return -fd;
        dip = op->idip;
        dip->fd = fd;
        dip->odxp = (struct odx_info_t *)malloc(sizeof(struct odx_info_t));
        if (NULL == dip->odxp) {
            pr2serr("Not enough user memory for do_odx_copy\n");
            return SG_LIB_CAT_OTHER;
        }
        memset(dip->odxp, 0, sizeof(struct odx_info_t));
        res = fetch_3pc_vpd(op, dip);
        if (res)
            return res;
        res = do_pop_tok(op);
        if (res)
            return res;
        res = fetch_rt_after_poptok(op);
        if (res)
            return res;
        if (ODX_REQ_PT == op->odx_request)
            op->dd_count -= op->in_full;
    }
    if ((ODX_REQ_WUT == op->odx_request) ||
        (ODX_REQ_COPY == op->odx_request)) {
        fd = pt_open_of(op);
        if (-1 == fd)
            return SG_LIB_FILE_ERROR;
        else if (fd < -1)
            return -fd;
        dip = op->odip;
        dip->fd = fd;
        dip->odxp = (struct odx_info_t *)malloc(sizeof(struct odx_info_t));
        if (NULL == dip->odxp) {
            pr2serr("Not enough user memory for do_odx_copy\n");
            return SG_LIB_CAT_OTHER;
        }
        res = fetch_3pc_vpd(op, dip);
        if (res)
            return res;
        res = do_wut(op);
        if (res)
            return res;
        res = fetch_rt_after_wut(op);
        if (res)
            return res;
        // careful, following assumes ibs=obs, needs rethink
        op->dd_count -= op->out_full;
    }
    return 0;
}


/* Called from main() in ddpt.c . Returns 0 on success or a positive
 * errno value if problems. This is for ODX which is a subset of
 * xcopy(LID4) for disk->disk, disk->held and held-> disk copies. */
int
do_odx_copy(struct opts_t * op)
{
    int ret;

    if (op->do_time)
        calc_duration_init(op);
    ret = odx_copy_work(op);
    if (0 == op->status_none)
        print_stats("", op);
    if (op->do_time)
        calc_duration_throughput("", 0, op);
    return ret;
}
