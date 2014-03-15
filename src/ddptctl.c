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
#define __STDC_LIMIT_MACROS 1   /* for UINT64_MAX, UINT32_MAX, etc */
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

const char * ddptctl_version_str = "0.94 20140315 [svn: r270]";

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
#include "sg_cmds_basic.h"


#define DEF_3PC_OUT_TIMEOUT (10 * 60)   /* is 10 minutes enough? */
#define DEF_3PC_IN_TIMEOUT 60           /* these should be fast */

#define DEF_ROD_TOK_FILE "ddptctl_rod_tok.bin"


static struct scat_gath_elem fixed_sgl[MAX_FIXED_SGL_ELEMS];

static struct option long_options[] = {
        {"abort", no_argument, 0, 'A'},
        {"all_toks", no_argument, 0, 'a'},
        {"block", no_argument, 0, 'b'},
        {"help", no_argument, 0, 'h'},
        {"info", no_argument, 0, 'i'},
        {"immed", no_argument, 0, 'I'},
        {"list_id", required_argument, 0, 'l'},
        {"oir", required_argument, 0, 'O'},
        {"poll", no_argument, 0, 'p'},
        {"pt", required_argument, 0, 'P'},
        {"receive", no_argument, 0, 'R'},
        {"rtf", required_argument, 0, 'r'},
        {"rtype", required_argument, 0, 't'},
        {"timeout", required_argument, 0, 'T'},
        {"size", no_argument, 0, 's'},
        {"verbose", no_argument, 0, 'v'},
        {"version", no_argument, 0, 'V'},
        {"wut", required_argument, 0, 'w'},
        {0, 0, 0, 0},
};



static void
usage()
{
    pr2serr("Usage: "
            "ddptctl [--abort] [--all_toks] [--block] [--help] [-immed] "
            "[--info]\n"
            "               [--list_id=LID] [--oir=OIR] [--poll] [--pt=GL] "
            "[--receive]\n"
            "               [--rtf=RTF] [rtype=RTYPE] [--size] "
            "[--timeout=ITO[,CMD]]\n"
            "               [--verbose] [--version] [--wut=SL] [DEVICE]\n"
            "  where:\n"
            "    --abort|-A            call COPY OPERATION ABORT command\n"
            "    --all_toks|-a         call REPORT ALL ROD TOKENS command\n"
            "    --block|-B            treat as block DEVICE (def: use "
            "SCSI commands)\n"
            "    --help|-h             print out usage message\n"
            "    --immed|-I            set IMMED bit in PT or WUT, exit "
            "prior to\n"
            "                          data transfer completion (then use "
            "--poll)\n"
            "    --info|-i             provide information on DEVICE or "
            "RTF\n"
            "    --list_id=LID|-l LID    LID is list identifier used with "
            "PT, WUT,\n"
            "                            RTTI or COPY OPERATION ABORT (def: "
            "257)\n"
            "    --oir=OIR|-O OIR      Offset In ROD (def: 0), used by WUT\n"
            "    --poll|-p             call RRTI periodically until "
            "completed\n"
            "    --pt=GL|-P GL         call PT with gather list GL. GL's "
            "format is\n"
            "                          LBA1,NUM1[,LBA2,NUM2...]\n"
            "    --receive|-R          call RRTI once\n"
            "    --rtf=RTF|-r RTF      ROD Token file for analysis (--info); "
            "output by\n"
            "                          -pt=, --poll or --receive; input to "
            "--wut=\n"
            "    --rtype=RTYPE|-t RTYPE    ROD type (def: RTV cleared in "
            "PT command)\n"
            "    --size|-s             get size of DEVICE (def: with SCSI "
            "commands)\n"
            "    --timeout=ITO[,CMD] | -T ITO[,CMD]\n"
            "                          ITO is inactivity timeout (def: 0), "
            "CMD is\n"
            "                          command timeout (def: 600); units: "
            "seconds\n"
            "    --verbose|-v          increase verbosity\n"
            "    --version|-V          print version string and exit\n"
            "    --wut=SL|-w SL        call WUT with scatter list SL. SL's "
            "format same\n"
            "                          as GL\n\n"
            "ddptctl is a ddpt helper utility, mainly for ODX, a subset of "
            "xcopy(LID4).\nPT refers to the POPULATE TOKEN command, WUT to "
            "the WRITE USING TOKEN\ncommand and RRTI to the RECEIVE ROD "
            "TOKEN INFORMATION command.\n"
            );
}

static int
odx_rt_info(const struct opts_t * op)
{
    int res, fd, err, m, prot_en, p_type, lbppbe, vendor, desig_type;
    int all_0, all_1;
    int target_dev_desc = 0;
    uint64_t bc;
    uint32_t rod_t, bs;
    uint16_t rtl;
    unsigned char uc;
    unsigned char rth[256];
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

    printf("Decoding information from ROD Token:\n");
    rod_t = (rth[0] << 24) + (rth[1] << 16) + (rth[2] << 8) + rth[3];
    printf("  ROD type: %s\n", rod_type_str(rod_t, b, sizeof(b)));
    if (rod_t >= 0xfffffff0) {
        printf("    Since ROD type is vendor specific, the following may "
               "not be relevant\n");
        vendor = 1;
    } else {
        vendor = 0;
        target_dev_desc = (RODT_ACCESS_ON_REF == rod_t) ||
                  (RODT_PIT_DEF == rod_t) || (RODT_PIT_VULN == rod_t) ||
                  (RODT_PIT_PERS == rod_t);
    }
    rtl = (rth[6] << 8) + rth[7];
    if (rtl < 0x1f8) {
        pr2serr(">>> ROD Token length field is too short, should be at "
                "least\n    504 bytes (0x1f8), got 0x%" PRIx16 "\n", rtl);
        if (! vendor) {
            close(fd);
            return SG_LIB_FILE_ERROR;
        }
    }
    printf("  Copy manager ROD Token identifier: %s\n",
           rt_cm_id_str(rth, rtl + 8, b, sizeof(b)));
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
    desig_type = rth[20 + 1] & 0xf;
    if ((0x2 == desig_type) || (0x3 == desig_type))
        decode_designation_descriptor(rth + 20, 32 - 4, op->verbose);
    else
        printf("      Expected designator type of EUI-64 or NAA, got 0x%x\n",
               desig_type);

    /* A 16 byte integer worth of bytes! Seems like overkill. */
    /* Look for all 0s or all 1s in the top 8 bytes */
    all_0 = (0x0 == rth[48]);
    if (all_0)
        all_1 = 0;
    else if (0xff == rth[48])
        all_1 = 1;
    else {
        all_1 = 0;
        printf("  Number of bytes represented: strange, bypass\n");
        goto skip_bytes_rep;
    }
    for (m = 1; m < 8; m++) {
        uc = rth[48 + m];
        if (! (((0xff == uc) && all_1) || ((0 == uc) && all_0)))
            break;
    }
    if (m < 8) {
        printf("  Number of bytes represented: strange, bypass\n");
        goto skip_bytes_rep;
    }
    bc = 0;
    for (m = 0; m < 8; m++) {
        if (m > 0)
            bc <<= 8;
        bc |= rth[56 + m];
    }
    if ((UINT64_MAX == bc) && all_1)
        printf("  Number of bytes represented: unknown or too large\n");
    else if (all_0)
        printf("  Number of bytes represented: %" PRIu64 " [0x%" PRIx64 "]\n",
               bc, bc);
    else
        printf("  Number of bytes represented: strange (top 8 bytes 0xff)\n");

skip_bytes_rep:
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
    printf("    Logical blocks per physical block exponent=%d\n", lbppbe);
    if (lbppbe > 0)
        printf("      [so physical block length=%u bytes]\n",
               bs * (1 << lbppbe));
    printf("    Lowest aligned logical block address=%d\n",
           ((rth[102] & 0x3f) << 8) + rth[103]);

    if (target_dev_desc) {
        desig_type = rth[128 + 1] & 0xf;
        if ((0x2 == desig_type) || (0x3 == desig_type) ||
            (0x8 == desig_type) || op->verbose) {
            printf("  Target device descriptor:\n");
            decode_designation_descriptor(rth + 128, 128 - 4, op->verbose);
        } else
            printf("  Target device descriptor: unexpected designator type "
                   "[0x%x]\n", desig_type);
    }
    close(fd);
    return 0;
}

static int
do_copy_abort(struct opts_t * op)
{
    return pt_3party_copy_out(op->idip->fd, SA_COPY_ABORT, op->list_id,
                              DEF_GROUP_NUM, DEF_3PC_OUT_TIMEOUT, NULL, 0, 1,
                              op->verbose);
}

static int
report_all_toks(struct opts_t * op, struct dev_info_t * dip)
{
    int res, fd;
    unsigned int len;
    unsigned char rsp[2048];

    fd = dip->fd;
    res = pt_3party_copy_in(fd, SA_ALL_ROD_TOKS, op->list_id,
                            DEF_3PC_IN_TIMEOUT, rsp, sizeof(rsp), 1,
                            op->verbose);
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
write_to_rtf(const char * rtf, const struct rrti_resp_t * rp)
{
    int fd, res, len, err;
    const char * cp;

    if (rtf[0])
        cp = rtf;
    else {
        cp = DEF_ROD_TOK_FILE;
        pr2serr("no --rtf=RTF given so writing ROD Token to %s\n", cp);
    }
    fd = open(cp, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd < 0) {
        err = errno;
        pr2serr("%s: unable to create file: %s [%s]\n", __func__,
                cp, safe_strerror(err));
        return SG_LIB_FILE_ERROR;
    }
    len = (rp->rt_len > 512) ? 512 : rp->rt_len;
    res = write(fd, rp->rod_tok, len);
    if (res < 0) {
        err = errno;
        pr2serr("%s: unable to write to file: %s [%s]\n", __func__,
                cp, safe_strerror(err));
        close(fd);
        return SG_LIB_FILE_ERROR;
    }
    close(fd);
    if (res < len) {
        pr2serr("%s: short write to file: %s, wanted %d, got %d\n",
                __func__, cp, len, res);
        return SG_LIB_CAT_OTHER;
    }
    return 0;
}

/* Returns the number of times 'ch' is found in string 's' given the
 * string's length. */
static int
num_chs_in_str(const char * s, int slen, int ch)
{
    int res = 0;

    while (--slen >= 0) {
        if (ch == s[slen])
            ++res;
    }
    return res;
}

static int
do_sgl(struct opts_t * op, const char * opt, const char * buf)
{
    int len, res, got;

    len = (int)strlen(buf);
    if ((('-' == buf[0]) && (1 == len)) || ((len > 1) && ('@' == buf[0]))) {
        res = file_to_sgl(((len > 1) ? (buf + 1) : buf), fixed_sgl, &got,
                          MAX_FIXED_SGL_ELEMS);
        if (res) {
            pr2serr("bad argument to '%s'\n", opt);
            return SG_LIB_SYNTAX_ERROR;
        }
    } else if (num_chs_in_str(buf, len, ',') > 0) {
        res = cl_to_sgl(buf, fixed_sgl, &got, MAX_FIXED_SGL_ELEMS);
        if (res) {
            pr2serr("bad argument to '%s'\n", opt);
            return SG_LIB_SYNTAX_ERROR;
        }
    } else {
        pr2serr("bad argument to '%s', need at least one LBA,NUM pair\n",
                 opt);
        return SG_LIB_SYNTAX_ERROR;
    }
    op->in_sgl = fixed_sgl;
    op->in_sgl_elems = got;
    op->out_sgl = fixed_sgl;
    op->out_sgl_elems = got;
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
    int c, k, n, fd, flags, blk_sz, cont, done;
    uint32_t delay;
    int64_t i64, num_blks;
    uint64_t tc;
    int req_abort = 0;
    int req_all_toks = 0;
    int do_block = 0;
    int do_info = 0;
    int do_poll = 0;
    int req_pt = 0;
    int do_receive = 0;
    int do_size = 0;
    int req_wut = 0;
    int ret = 0;
    struct opts_t ops;
    struct flags_t iflag, oflag;
    struct dev_info_t ids, ods, o2ds;
    struct sg_simple_inquiry_resp sir;
    struct rrti_resp_t rrti_rsp;
    struct opts_t * op;
    char * np;
    char b[80];
    char bb[80];

    state_init(&ops, &iflag, &oflag, &ids, &ods, &o2ds);
    op = &ops;
    memset(&sir, 0, sizeof(sir));

    while (1) {
        int option_index = 0;

        c = getopt_long(argc, argv, "AaBhiIl:O:pP:r:Rst:T:vVw:", long_options,
                        &option_index);
        if (c == -1)
            break;

        switch (c) {
        case 'A':
            ++req_abort;
            break;
        case 'a':
            ++req_all_toks;
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
        case 'I':
            ++op->iflagp->immed;
            ++op->oflagp->immed;
            break;
        case 'l':
            i64 = sg_get_llnum(optarg);
            if (-1 == i64) {
                pr2serr("bad argument to 'list_id='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (i64 > UINT32_MAX) {
                pr2serr("argument to 'list_id=' too big for 32 bits\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->list_id = (uint32_t)i64;
            op->list_id_given = 1;
            break;
        case 'O':
            op->offset_in_rod = sg_get_llnum(optarg);
            if (-1LL == op->offset_in_rod) {
                pr2serr("bad argument to '--oir='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            break;
        case 'p':
            ++do_poll;
            break;
        case 'P':       /* takes gather list as argument */
            ++req_pt;
            ret = do_sgl(op, "--pt=", optarg);
            if (ret)
                return ret;
            break;
        case 'r':
            if (op->rtf[0]) {
                pr2serr("Can only use --rtf=RTF once for ROD Token "
                        "filename\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            strncpy(op->rtf, optarg, INOUTF_SZ - 1);
            break;
        case 'R':
            ++do_receive;
            break;
        case 's':
            ++do_size;
            break;
        case 't':
            if (0 == strncmp("pit-def", optarg, 7))
                op->rod_type = RODT_PIT_DEF;
            else if (0 == strncmp("pit-vuln", optarg, 8))
                op->rod_type = RODT_PIT_VULN;
            else if (0 == strncmp("pit-pers", optarg, 8))
                op->rod_type = RODT_PIT_PERS;
            else if (0 == strncmp("pit-any", optarg, 7))
                op->rod_type = RODT_PIT_ANY;
            else if (0 == strncmp("zero", optarg, 4))
                op->rod_type = RODT_BLK_ZERO;
            else {
                i64 = sg_get_llnum(optarg);
                if (-1 == i64) {
                    pr2serr("bad argument to '--rtype='; can give (hex) "
                            "number, 'pit-def', 'pit-vuln',\n");
                    pr2serr("'pit-pers', 'pit-any' or 'zero'\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
                if (i64 > UINT32_MAX) {
                    pr2serr("'rtype=' argument exceeds 32 bits\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
                op->rod_type = (uint32_t)i64;
            }
            ++op->rod_type_given;
            break;
        case 'T':
            n = sg_get_num(optarg);
            if (-1 == n) {
                pr2serr("bad argument to '--timeout='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->inactivity_to = n;
            np = strchr(optarg, ',');
            if (np) {
                op->timeout_xcopy = sg_get_num(++np);
                if (-1 == op->timeout_xcopy) {
                    pr2serr("bad argument to '--timeout=ok,xxx'\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
            }
            break;
        case 'v':
            ++op->verbose;
            break;
        case 'V':
            pr2serr("version: %s\n", ddptctl_version_str);
            return 0;
        case 'w':       /* takes scatter list as argument */
            ++req_wut;
            ret = do_sgl(op, "--wut=", optarg);
            if (ret)
                return ret;
            break;
        default:
            pr2serr("unrecognised option code 0x%x ??\n", c);
            usage();
            return SG_LIB_SYNTAX_ERROR;
        }
    }
    if (optind < argc) {
        if ('\0' == op->idip->fn[0]) {
            strncpy(op->idip->fn, argv[optind], INOUTF_SZ - 1);
            strncpy(op->odip->fn, argv[optind], INOUTF_SZ - 1);
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
        if (! op->rtf[0]) {
            pr2serr("missing device name!\n");
            usage();
            return SG_LIB_SYNTAX_ERROR;
        }
    }

    k = 0;
    if (req_abort)
        ++k;
    if (req_all_toks)
        ++k;
    if (do_poll)
        ++k;
    if (req_pt)
        ++k;
    if (do_receive)
        ++k;
    if (req_wut)
        ++k;
    if (k > 1) {
        pr2serr("Can only have one of --abort, --all_toks, --poll, --pt=, "
                "--receive and --wut=\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if (do_info && (1 == k)) {
        pr2serr("--info cannot be used with an ODX command option (e.g. "
                "--pt=)\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if (do_info && op->rtf[0]) {
        if (op->idip->fn[0])
            pr2serr("Ignore device name [%s] and decode RTF\n", op->idip->fn);
        return odx_rt_info(op);
    }

    op->idip->d_type = do_block ? FT_BLOCK : FT_PT;
    if (op->idip->d_type & FT_PT) {
        fd = pt_open_if(op, &sir);
        if (-1 == fd) {
            ret = SG_LIB_FILE_ERROR;
            goto clean_up;
        } else if (fd < -1) {
            ret = SG_LIB_CAT_OTHER;
            goto clean_up;
        }
        op->idip->fd = fd;
        op->odip->fd = fd;
    } else if (op->idip->d_type & FT_BLOCK) {
        flags = O_RDONLY;
        fd = open(op->idip->fn, flags);
        if (fd < 0) {
            pr2serr("could not open %s for reading: %s\n", op->idip->fn,
                    safe_strerror(errno));
            ret = SG_LIB_FILE_ERROR;
            goto clean_up;
        }
        op->idip->fd = fd;
        op->odip->fd = fd;
    } else {
        pr2serr("expecting to open a file but nothing found\n");
        ret = SG_LIB_CAT_OTHER;
        goto clean_up;
    }

    done = 1;
    if (req_abort) {
        ret = do_copy_abort(op);
        if (ret)
            goto clean_up;
    } else if (req_all_toks) {
        ret = report_all_toks(op, op->idip);
        if (ret)
            goto clean_up;
    } else if (do_poll) {
        if (! op->list_id_given)
            op->list_id = 0x101;
        do {
            ret = fetch_rrti_after_odx(op, DDPT_ARG_IN, &rrti_rsp,
                                       op->verbose);
            if (ret)
                goto clean_up;
            cont = ((rrti_rsp.cstat >= 0x10) && (rrti_rsp.cstat <= 0x12));
            if (cont) {
                delay = rrti_rsp.esu_del;
                if ((delay < 0xfffffffe) && (delay > 0)) {
                    if (op->verbose > 1)
                        pr2serr("using copy manager recommended delay of %"
                                PRIu32 " milliseconds\n", delay);
                } else {
                    delay = DEF_ODX_POLL_DELAY_MS;
                    if (op->verbose > 1)
                        pr2serr("using default for poll delay\n");
                }
                if (delay)
                    sleep_ms(delay);
            }
        } while (cont);
        sg_get_opcode_sa_name(THIRD_PARTY_COPY_OUT_CMD, rrti_rsp.for_sa, 0,
                              (int)sizeof(b), b);
        printf("RRTI for %s: %s\n", b,
               cpy_op_status_str(rrti_rsp.cstat, bb, sizeof(bb)));
        if ((SA_POP_TOK == rrti_rsp.for_sa) && (rrti_rsp.rt_len > 0)) {
            ret = write_to_rtf(op->rtf, &rrti_rsp);
            if (ret)
                goto clean_up;
        }
    } else if (req_pt) {
        if (! op->list_id_given)
            op->list_id = 0x101;
        num_blks = count_sgl_blocks(op->in_sgl, op->in_sgl_elems);
        if ((ret = do_pop_tok(op, 0, num_blks, 0, op->verbose)))
            goto clean_up;
        else if (op->iflagp->immed)
            goto clean_up;
        if ((ret = fetch_rt_after_poptok(op, &tc, op->verbose)))
            goto clean_up;
        printf("PT completes with transfer count %" PRIu64 " [0x%" PRIx64
               "]\n", tc, tc);
        goto clean_up;
    } else if (do_receive) {
        if (! op->list_id_given)
            op->list_id = 0x101;
        ret = fetch_rrti_after_odx(op, DDPT_ARG_IN, &rrti_rsp, op->verbose);
        if (ret)
            goto clean_up;
        sg_get_opcode_sa_name(THIRD_PARTY_COPY_OUT_CMD, rrti_rsp.for_sa, 0,
                              (int)sizeof(b), b);
        printf("RRTI for %s: %s\n", b,
               cpy_op_status_str(rrti_rsp.cstat, bb, sizeof(bb)));
        if ((SA_POP_TOK == rrti_rsp.for_sa) && (rrti_rsp.rt_len > 0)) {
            ret = write_to_rtf(op->rtf, &rrti_rsp);
            if (ret)
                goto clean_up;
        }
    } else if (req_wut) {
        if (! op->list_id_given)
            op->list_id = 0x101;
        num_blks = count_sgl_blocks(op->out_sgl, op->out_sgl_elems);
        if ((ret = do_wut(op, 0, num_blks, 0, 0, 0, op->verbose)))
            goto clean_up;
        else if (op->oflagp->immed)
            goto clean_up;
        if ((ret = fetch_rrti_after_wut(op, &tc, op->verbose)))
            goto clean_up;
        printf("WUT completes with transfer count %" PRIu64 " [0x%" PRIx64
               "]\n", tc, tc);
    } else
        done = 0;
    if (done)
        goto clean_up;

    if (do_info) {
        if (op->idip->d_type & FT_PT) {
            ret = pt_read_capacity(op, DDPT_ARG_IN, &num_blks, &blk_sz);
            if (ret) {
                if (SG_LIB_CAT_UNIT_ATTENTION == ret) {
                    if (op->verbose)
                        pr2serr("Unit attention (readcap), continuing\n");
                    ret = pt_read_capacity(op, DDPT_ARG_IN,
                                           &num_blks, &blk_sz);
                    if (ret) {
                        if (0 == sir.peripheral_type)
                            pr2serr("read capacity failed, perhaps because "
                                    "non-disk device [pdt=%d]\n",
                                    sir.peripheral_type);
                        goto clean_up;
                    }
                }
            }
            print_blk_sizes(op->idip->fn, "readcap", num_blks, blk_sz, 0);
            if (0x8 & sir.byte_5) {
                printf("3PC (third party copy) bit set in standard INQUIRY "
                       "response\n");
                printf("  Print Third Party Copy VPD page:\n");
                print_3pc_vpd(op, 0);
            } else {
                printf("3PC (third party copy) bit clear in standard INQUIRY "
                       "response\n");
                printf("  so %s [pdt=0x%x] does not seem to support XCOPY\n",
                       op->idip->fn, sir.peripheral_type);
            }
        } else if (op->idip->d_type & FT_BLOCK) {
            ret = get_blkdev_capacity(op, DDPT_ARG_IN, &num_blks, &blk_sz);
            if (ret)
                goto clean_up;
            print_blk_sizes(op->idip->fn, "block", num_blks, blk_sz, 0);
        } else {
            num_blks = 0;
            blk_sz = 0;
            printf("unable to print capacity information about device\n");
        }
        done = 1;
    }
    if (do_size && (! do_info)) {
        if (op->idip->d_type & FT_PT) {
            ret = pt_read_capacity(op, DDPT_ARG_IN, &num_blks, &blk_sz);
            if (ret) {
                if (SG_LIB_CAT_UNIT_ATTENTION == ret) {
                    if (op->verbose)
                        pr2serr("Unit attention (readcap), continuing\n");
                    ret = pt_read_capacity(op, DDPT_ARG_IN,
                                           &num_blks, &blk_sz);
                    if (ret) {
                        if (0 == sir.peripheral_type)
                            pr2serr("read capacity failed, perhaps because "
                                    "non-disk device [pdt=%d]\n",
                                    sir.peripheral_type);
                        goto clean_up;
                    }
                }
            }
            print_blk_sizes(op->idip->fn, "readcap", num_blks, blk_sz, 0);
        } else if (op->idip->d_type & FT_BLOCK) {
            ret = get_blkdev_capacity(op, DDPT_ARG_IN, &num_blks, &blk_sz);
            if (ret)
                goto clean_up;
            print_blk_sizes(op->idip->fn, "block", num_blks, blk_sz, 0);
        } else {
            num_blks = 0;
            blk_sz = 0;
            printf("%s: unable to print capacity information\n",
                   op->idip->fn);
        }
        done = 1;
    }

    if (! done)
        pr2serr("not implemented yet\n");

clean_up:
    if (op->idip->fd >= 0) {
        if (op->idip->d_type & FT_PT)
            pt_close(op->idip->fd);
        else if (op->idip->d_type & FT_BLOCK)
            close(op->idip->fd);
    }
    if (ret) {
        if (ret > 0)
            print_exit_status_msg("Exit status", ret, 0);
        else if (ret < 0) {
            pr2serr("Some error occurred\n");
            ret = 1;
        }
    }
    return ret;
}
