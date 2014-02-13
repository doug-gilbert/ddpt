/*
 * Copyright (c) 2014 Douglas Gilbert.
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
 * This utility, ddptctl, is an auxiliary to do related tasks for ddpt.
 * That way ddpt can concentrate on copy (or partial copy) operations.
 */

/* Need _GNU_SOURCE for O_DIRECT */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>
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

#include "ddpt.h"

const char * ddptctl_version_str = "0.94 20140212 [svn: r259]";

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

#include "sg_lib.h"


#define THIRD_PARTY_COPY_IN_CMD 0x84

#define SA_COPY_STATUS_LID1     0x0     /* IN, retrieve */
#define SA_COPY_DATA_LID1       0x1     /* IN, retrieve */
#define SA_COPY_OP_PARAMS       0x3     /* IN, retrieve */
#define SA_COPY_FAIL_DETAILS    0x4     /* IN, retrieve */
#define SA_COPY_STATUS_LID4     0x5     /* IN, retrieve */
#define SA_COPY_DATA_LID4       0x6     /* IN, retrieve */
#define SA_ROD_TOK_INFO         0x7     /* IN, retrieve */
#define SA_ALL_ROD_TOKS         0x8     /* IN, retrieve */

#define DEF_3PC_IN_TIMEOUT 60           /* these should be fast */



static struct option long_options[] = {
        {"abort", required_argument, 0, 'A'},
        {"all_toks", no_argument, 0, 'a'},
        {"block", no_argument, 0, 'b'},
        {"help", no_argument, 0, 'h'},
        {"info", no_argument, 0, 'i'},
        {"list_id", required_argument, 0, 'l'},
        {"poll", no_argument, 0, 'p'},
        {"receive", no_argument, 0, 'R'},
        {"rtf", required_argument, 0, 'r'},
        {"size", no_argument, 0, 's'},
        {"verbose", no_argument, 0, 'v'},
        {"version", no_argument, 0, 'V'},
        {0, 0, 0, 0},
};



static void
usage()
{
    pr2serr("Usage: "
            "ddptctl [--abort=LID] [--all_toks] [--block] [--help] [--info] "
            "[--list_id=LID]\n"
            "               [--poll] [--receive] [--rtf=RTF] [--size] "
            "[--verbose]\n"
            "               [--version] [DEVICE]\n"
            "  where:\n"
            "    --abort=LID|-A LID    call COPY OPERATION ABORT on LID\n"
            "    --all_toks|-a         call REPORT ALL ROD TOKENS\n"
            "    --block|-B            treat DEVICE as block device (def: "
            "treat as pt)\n"
            "    --help|-h             print out usage message\n"
            "    --info|-i             provide information on DEVICE or "
            "RTF\n"
            "    --list_id=LID|-l LID    LID is list identifier for --poll "
            "or --receive\n"
            "    --poll|-p             keep calling RRTI until complete\n"
            "    --receive|-R          call RRTI once\n"
            "    --rtf=RTF|-r RTF      ROD Token file for analyse (--info) "
            "or write to\n"
            "                          (with --poll or --receive)\n"
            "    --size|-s             get size of DEVICE (def: pt)\n"
            "    --verbose|-v          increase verbosity\n"
            "    --version|-V          print version string and exit\n\n"
            "ddptctl is a ddpt helper utility, mainly with xcopy(LID1) and "
            "ODX. RRTI\nrefers to the RECEIVE ROD TOKEN INFORMATION "
            "command.\n"
            );
}

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
report_all_toks(struct opts_t * op, struct dev_info_t * dip)
{
    int res, fd, verb;
    unsigned int len;
    unsigned char rsp[2048];

    verb = (op->verbose > 1) ? (op->verbose - 2) : 0;
    fd = dip->fd;
    res = pt_3party_copy_in(fd, SA_ALL_ROD_TOKS, op->list_id,
                            DEF_3PC_IN_TIMEOUT, rsp, sizeof(rsp), 1, verb);
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

static void
state_init(struct opts_t * op, struct flags_t * ifp, struct flags_t * ofp,
           struct dev_info_t * idip, struct dev_info_t * odip,
           struct dev_info_t * o2dip)
{
    memset(op, 0, sizeof(struct opts_t));
    op->dd_count = -1;
    op->highest_unrecovered = -1;
    op->do_time = 1;         /* default was 0 in sg_dd */
    op->id_usage = -1;
    op->list_id = 1;
    op->prio = 1;
    op->max_uas = MAX_UNIT_ATTENTIONS;
    op->max_aborted = MAX_ABORTED_CMDS;
    memset(ifp, 0, sizeof(struct flags_t));
    memset(ofp, 0, sizeof(struct flags_t));
    op->iflagp = ifp;
    op->oflagp = ofp;
    memset(idip, 0, sizeof(struct dev_info_t));
    memset(odip, 0, sizeof(struct dev_info_t));
    memset(o2dip, 0, sizeof(struct dev_info_t));
    idip->d_type = FT_OTHER;
    idip->fd = -1;
    odip->d_type = FT_OTHER;
    odip->fd = -1;
    o2dip->d_type = FT_OTHER;
    o2dip->fd = -1;
    op->idip = idip;
    op->odip = odip;
    op->o2dip = o2dip;
    ifp->cdbsz = DEF_SCSI_CDBSZ;
    ofp->cdbsz = DEF_SCSI_CDBSZ;
#ifdef HAVE_POSIX_FADVISE
    op->lowest_skip = -1;
    op->lowest_seek = -1;
#endif
    op->idip->pdt = -1;
    op->odip->pdt = -1;
}

int
main(int argc, char * argv[])
{
    int c;
    int64_t i64;
    int do_abort = 0;
    int do_all_toks = 0;
    int do_block = 0;
    int do_info = 0;
    int do_poll = 0;
    int do_receive = 0;
    int do_size = 0;
    int ret = 0;
    struct opts_t ops;
    struct flags_t iflag, oflag;
    struct dev_info_t ids, ods, o2ds;
    struct opts_t * op;

    state_init(&ops, &iflag, &oflag, &ids, &ods, &o2ds);
    op = &ops;

    while (1) {
        int option_index = 0;

        c = getopt_long(argc, argv, "A:aBhil:pr:RsvV", long_options,
                        &option_index);
        if (c == -1)
            break;

        switch (c) {
        case 'A':
            ++do_abort;
            i64 = sg_get_llnum(optarg);
            if (-1 == i64) {
                pr2serr("bad argument to 'abort='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (i64 > UINT_MAX) {
                pr2serr("argument to 'abort=' too big for 32 bits\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->list_id = (uint32_t)i64;
            op->list_id_given = 1;
            break;
        case 'a':
            ++do_all_toks;
            break;
        case 'B':
            ++do_block;
            break;
        case 'h':
        case '?':
            usage();
            return 0;
        case 'i':
            ++do_info;
            break;
        case 'l':
            i64 = sg_get_llnum(optarg);
            if (-1 == i64) {
                pr2serr("bad argument to 'list_id='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (i64 > UINT_MAX) {
                pr2serr("argument to 'list_id=' too big for 32 bits\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->list_id = (uint32_t)i64;
            op->list_id_given = 1;
            break;
        case 'p':
            ++do_poll;
            break;
        case 'r':
            if (op->rtf[0]) {
                pr2serr("Can only use --rtf=RTF once for ROD Token "
                        "filename\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            strncpy(op->rtf, optarg, INOUTF_SZ - 1);
        case 'R':
            ++do_receive;
            break;
        case 's':
            ++do_size;
            break;
        case 'v':
            ++op->verbose;
            break;
        case 'V':
            pr2serr("version: %s\n", ddptctl_version_str);
            return 0;
        default:
            pr2serr("unrecognised option code 0x%x ??\n", c);
            usage();
            return SG_LIB_SYNTAX_ERROR;
        }
    }
    if (optind < argc) {
        if ('\0' == op->idip->fn[0]) {
            strncpy(op->idip->fn, argv[optind], INOUTF_SZ - 1);
            ++optind;
        }
        if (optind < argc) {
            for (; optind < argc; ++optind)
                pr2serr("Unexpected extra argument: %s\n", argv[optind]);
            usage();
            return SG_LIB_SYNTAX_ERROR;
        }
    }
    if ('\0' == op->idip->fn[0]) {
        if (op->rtf[0])
            return odx_rt_info(op);
        pr2serr("missing device name!\n");
        usage();
        return SG_LIB_SYNTAX_ERROR;
    }
    if (do_info && op->rtf[0]) {
        pr2serr("Ignore device name [%s] and decode RTF\n", op->idip->fn);
        return odx_rt_info(op);
    }

    op->idip->d_type = do_block ? FT_BLOCK : FT_PT;

    return ret;
}
