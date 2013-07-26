/*
 * Copyright (c) 2008-2013 Douglas Gilbert.
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
                   int dst_desc_len, int seg_desc_type,
                   int64_t num_blk, uint64_t src_lba,
                   uint64_t dst_lba)
{
    unsigned char xcopyBuff[256];
    int desc_offset = 16;
    int seg_desc_len, verb, fd;

    fd = (op->iflagp->xcopy) ? op->infd : op->outfd;
    verb = (op->verbose ? op->verbose - 1: 0);

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
    if (op->verbose > 3) {
        pr2serr("\nParameter list in hex (length %d):\n", desc_offset);
        dStrHex((const char *)xcopyBuff, desc_offset, 1);
    }
    return sg_ll_extended_copy(fd, xcopyBuff, desc_offset, 1, verb);
}

static int
scsi_operating_parameter(struct opts_t * op, int is_target)
{
    int res, fd, ftype, verb;
    unsigned char rcBuff[256];
    unsigned int rcBuffLen = 256, len, n, td_list = 0;
    unsigned long num, max_target_num, max_segment_num, max_segment_len;
    unsigned long max_desc_len, max_inline_data, held_data_limit;
    int valid = 0;

    verb = (op->verbose ? op->verbose - 1: 0);
    fd = (op->iflagp->xcopy) ? op->infd : op->outfd;
    ftype = (op->iflagp->xcopy) ? op->in_type : op->out_type;
    /* In SPC-4 opcode 0x84, service action 0x3 is called RECEIVE COPY
     * OPERATING PARAMETERS */
    res = sg_ll_receive_copy_results(fd, 0x03, 0, rcBuff, rcBuffLen,
                                     1, verb);
    if (0 != res)
        return -res;

    len = (rcBuff[0] << 24) | (rcBuff[1] << 16) | (rcBuff[2] << 8) |
          rcBuff[3];
    if (len > rcBuffLen) {
        pr2serr("  <<report too long for internal buffer,"
                " output truncated\n");
    }
    if (op->verbose > 2) {
        pr2serr("\nOutput response in hex:\n");
        dStrHex((const char *)rcBuff, len, 1);
    }
    max_target_num = rcBuff[8] << 8 | rcBuff[9];
    max_segment_num = rcBuff[10] << 8 | rcBuff[11];
    max_desc_len = rcBuff[12] << 24 | rcBuff[13] << 16 | rcBuff[14] << 8 |
                   rcBuff[15];
    max_segment_len = rcBuff[16] << 24 | rcBuff[17] << 16 |
                      rcBuff[18] << 8 | rcBuff[19];
    op->xc_max_bytes = max_segment_len ? max_segment_len : ULONG_MAX;
    max_inline_data = rcBuff[20] << 24 | rcBuff[21] << 16 | rcBuff[22] << 8 |
                      rcBuff[23];
    if (op->verbose) {
        pr2serr(" >> Receive copy results (report operating "
                  "parameters):\n");
        pr2serr("    Maximum target descriptor count: %lu\n",
                  max_target_num);
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
        pr2serr("    Held data limit: %lu (usage: %d)\n",
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
    op->xc_min_bytes = 1 << rcBuff[37];

    for (n = 0; n < rcBuff[43]; n++) {
        switch(rcBuff[44 + n]) {
        case 0x00: /* copy block to stream device */
            if (!is_target && (ftype & FT_BLOCK))
                valid++;
            if (is_target && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy Block to Stream device\n");
            break;
        case 0x01: /* copy stream to block device */
            if (!is_target && (ftype & FT_TAPE))
                valid++;
            if (is_target && (ftype & FT_BLOCK))
                valid++;
            if (op->verbose)
                pr2serr("        Copy Stream to Block device\n");
            break;
        case 0x02: /* copy block to block device */
            if (!is_target && (ftype & FT_BLOCK))
                valid++;
            if (is_target && (ftype & FT_BLOCK))
                valid++;
            if (op->verbose)
                pr2serr("        Copy Block to Block device\n");
            break;
        case 0x03: /* copy stream to stream device */
            if (!is_target && (ftype & FT_TAPE))
                valid++;
            if (is_target && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy Stream to Stream device\n");
            break;
        case 0x04: /* copy inline data to stream device */
            if (!is_target && (ftype & FT_OTHER))
                valid++;
            if (is_target && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy inline data to Stream device\n");
            break;
        case 0x05: /* copy embedded data to stream device */
            if (!is_target && (ftype & FT_OTHER))
                valid++;
            if (is_target && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy embedded data to Stream device\n");
            break;
        case 0x06: /* Read from stream device and discard */
            if (!is_target && (ftype & FT_TAPE))
                valid++;
            if (is_target && (ftype & FT_DEV_NULL))
                valid++;
            if (op->verbose)
                pr2serr("        Read from stream device and discard\n");
            break;
        case 0x07: /* Verify block or stream device operation */
            if (!is_target && (ftype & (FT_TAPE | FT_BLOCK)))
                valid++;
            if (is_target && (ftype & (FT_TAPE | FT_BLOCK)))
                valid++;
            if (op->verbose)
                pr2serr("        Verify block or stream device operation\n");
            break;
        case 0x08: /* copy block device with offset to stream device */
            if (!is_target && (ftype & FT_BLOCK))
                valid++;
            if (is_target && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy block device with offset to stream "
                       "device\n");
            break;
        case 0x09: /* copy stream device to block device with offset */
            if (!is_target && (ftype & FT_TAPE))
                valid++;
            if (is_target && (ftype & FT_BLOCK))
                valid++;
            if (op->verbose)
                pr2serr("        Copy stream device to block device with "
                       "offset\n");
            break;
        case 0x0a: /* copy block device with offset to block device with
                    * offset */
            if (!is_target && (ftype & FT_BLOCK))
                valid++;
            if (is_target && (ftype & FT_BLOCK))
                valid++;
            if (op->verbose)
                pr2serr("        Copy block device with offset to block "
                       "device with offset\n");
            break;
        case 0x0b: /* copy block device to stream device and hold data */
            if (!is_target && (ftype & FT_BLOCK))
                valid++;
            if (is_target && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy block device to stream device and hold "
                       "data\n");
            break;
        case 0x0c: /* copy stream device to block device and hold data */
            if (!is_target && (ftype & FT_TAPE))
                valid++;
            if (is_target && (ftype & FT_BLOCK))
                valid++;
            if (op->verbose)
                pr2serr("        Copy stream device to block device and hold "
                       "data\n");
            break;
        case 0x0d: /* copy block device to block device and hold data */
            if (!is_target && (ftype & FT_BLOCK))
                valid++;
            if (is_target && (ftype & FT_BLOCK))
                valid++;
            if (op->verbose)
                pr2serr("        Copy block device to block device and hold "
                       "data\n");
            break;
        case 0x0e: /* copy stream device to stream device and hold data */
            if (!is_target && (ftype & FT_TAPE))
                valid++;
            if (is_target && (ftype & FT_TAPE))
                valid++;
            if (op->verbose)
                pr2serr("        Copy block device to block device and hold "
                       "data\n");
            break;
        case 0x0f: /* read from stream device and hold data */
            if (!is_target && (ftype & FT_TAPE))
                valid++;
            if (is_target && (ftype & FT_DEV_NULL))
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
desc_from_vpd_id(struct opts_t * op, int sg_fd, unsigned char *desc,
                 int desc_len, unsigned int block_size, int pad)
{
    int res;
    unsigned char rcBuff[256], *ucp, *best = NULL;
    unsigned int len = 254;
    int off = -1, u, i_len, best_len = 0, assoc, desig, f_desig = 0;

    memset(rcBuff, 0xff, len);
    res = sg_ll_inquiry(sg_fd, 0, 1, 0x83, rcBuff, 4, 1, op->verbose);
    if (0 != res) {
        pr2serr("VPD inquiry failed with %d\n", res);
        return res;
    } else if (rcBuff[1] != 0x83) {
        pr2serr("invalid VPD response\n");
        return SG_LIB_CAT_MALFORMED;
    }
    len = ((rcBuff[2] << 8) + rcBuff[3]) + 4;
    res = sg_ll_inquiry(sg_fd, 0, 1, 0x83, rcBuff, len, 1, op->verbose);
    if (0 != res) {
        pr2serr("VPD inquiry failed with %d\n", res);
        return res;
    } else if (rcBuff[1] != 0x83) {
        pr2serr("invalid VPD response\n");
        return SG_LIB_CAT_MALFORMED;
    }
    if (op->verbose > 2) {
        pr2serr("Output response in hex:\n");
        dStrHex((const char *)rcBuff, len, 1);
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
        if (op->verbose)
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
#if 0
        if (op->verbose)
            decode_designation_descriptor(best, best_len);
#endif
        if (best_len + 4 < desc_len) {
            memset(desc, 0, 32);
            desc[0] = 0xe4;
            memcpy(desc + 4, best, best_len + 4);
            desc[4] &= 0x1f;
            desc[28] = pad << 2;
            desc[29] = (block_size >> 16) & 0xff;
            desc[30] = (block_size >> 8) & 0xff;
            desc[31] = block_size & 0xff;
            if (op->verbose > 3) {
                pr2serr("Descriptor in hex (bs %d):\n", block_size);
                dStrHex((const char *)desc, 32, 1);
            }
            return 32;
        }
        return  best_len + 8;
    }
    return 0;
}
