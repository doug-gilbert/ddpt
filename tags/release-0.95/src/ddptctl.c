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

const char * ddptctl_version_str = "0.95 20141226 [svn: r307]";

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


#define DEF_3PC_OUT_TIMEOUT (10 * 60)   /* 10 minutes not enough, use IMMED */
#define DEF_3PC_IN_TIMEOUT 60           /* these should be fast */
#define MAX_NUM_MAN_TOKS 32

#define DEF_ROD_TOK_FILE "ddptctl_rod_tok.bin"


static struct scat_gath_elem fixed_sgl[MAX_FIXED_SGL_ELEMS];

static struct option long_options[] = {
        {"abort", no_argument, 0, 'A'},
        {"all_toks", no_argument, 0, 'a'},
        {"block", no_argument, 0, 'b'},
        {"del_tkn", no_argument, 0, 'D'},
        {"help", no_argument, 0, 'h'},
        {"hex", no_argument, 0, 'H'},
        {"info", no_argument, 0, 'i'},
        {"immed", no_argument, 0, 'I'},
        {"list_id", required_argument, 0, 'l'},
        {"oir", required_argument, 0, 'O'},
        {"poll", no_argument, 0, 'p'},
        {"pt", required_argument, 0, 'P'},
        {"readonly", no_argument, 0, 'y'},
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
            "ddptctl [--abort] [--all_toks] [--block] [--del_tkn] [--help] "
            "[--hex]\n"
            "               [-immed] [--info] [--list_id=LID] [--oir=OIR] "
            "[--poll]\n"
            "               [--pt=GL] [--readonly] [--receive] [--rtf=RTF] "
            "[rtype=RTYPE]\n"
            "               [--size] [--timeout=ITO[,CMD]] [--verbose] "
            "[--version]\n"
            "               [--wut=SL] [DEVICE]\n"
            "  where:\n"
            "    --abort|-A            call COPY OPERATION ABORT command\n"
            "    --all_toks|-a         call REPORT ALL ROD TOKENS command\n"
            "    --block|-B            treat as block DEVICE (def: use "
            "SCSI commands)\n"
            "    --del_tkn|-D          set DEL_TKN bit in WUT command\n"
            "    --help|-h             print out usage message\n"
            "    --hex|-H              print response in ASCII hexadecimal\n"
            "    --immed|-I            set IMMED bit in PT or WUT, exit "
            "prior to\n"
            "                          data transfer completion (then use "
            "--poll)\n"
            "    --info|-i             provide information on DEVICE or "
            "RTF\n"
            "    --list_id=LID|-l LID    LID is list identifier used with "
            "PT, WUT,\n"
            "                            RRTI or COPY OPERATION ABORT (def: "
            "257)\n"
            "    --oir=OIR|-O OIR      Offset In ROD (def: 0), used by WUT\n"
            "    --poll|-p             call RRTI periodically until "
            "completed\n"
            "    --pt=GL|-P GL         call PT with gather list GL. GL's "
            "format is\n"
            "                          LBA1,NUM1[,LBA2,NUM2...]\n"
            "    --readonly|-y         open DEVICE read-only (def: "
            "read-write)\n"
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
            "TOKEN INFORMATION command. If\nthe ODX_RTF_LEN environment "
            "variable is present, the ROD's size is\nappended to the ROD "
            "Token placed in the RTF file.\n"
            );
}

/* If len==96 then assume it is a management ROD Token. Returns 0 if okay. */
static int
odx_print_rod_tok(const struct opts_t * op, unsigned char * rth, int len)
{
    int vendor, all_0, all_1, m, prot_en, p_type, desig_type, lbppbe;
    int target_dev_desc = 0;
    uint64_t bc;
    uint32_t rod_t, bs;
    uint16_t rtl;
    char b[128];
    unsigned char uc;

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
    if (rtl < ODX_ROD_TOK_LEN_FLD) {
        pr2serr(">>> ROD Token length field is too short, should be at "
                "least\n    %d bytes (0x%x), got 0x%" PRIx16 "\n",
                ODX_ROD_TOK_LEN_FLD, ODX_ROD_TOK_LEN_FLD, rtl);
        if (! vendor)
            return SG_LIB_CAT_OTHER;
    }
    printf("  Copy manager ROD Token identifier: %s\n",
           rt_cm_id_str(rth, rtl + 8, b, sizeof(b)));
    printf("  Creator Logical Unit descriptor:\n");
    /* should make smaller version of following that outputs to stdout */
    if (0xe4 != rth[16]) {
        pr2serr(">>> Expected Identification descriptor CSCD (0xe4) got "
                "0x%x\n", rth[16]);
        if (! vendor)
            return SG_LIB_CAT_OTHER;
    }
    printf("    Peripheral Device type: 0x%x\n", rth[17] & 0x1f);
    printf("    Relative initiator port identifier: 0x%x\n",
           (rth[18] << 8) + rth[19]);
    desig_type = rth[20 + 1] & 0xf;
    if ((0x2 == desig_type) || (0x3 == desig_type))
        decode_designation_descriptor(rth + 20, rth[23], 0, op->verbose);
    else
        printf("      Expected designator type of EUI-64 or NAA, got "
               "0x%x\n", desig_type);

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
        goto skip_to_bytes_rep;
    }
    for (m = 1; m < 8; m++) {
        uc = rth[48 + m];
        if (! (((0xff == uc) && all_1) || ((0 == uc) && all_0)))
            break;
    }
    if (m < 8) {
        printf("  Number of bytes represented: strange, bypass\n");
        goto skip_to_bytes_rep;
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
        printf("  Number of bytes represented: %" PRIu64 " [0x%" PRIx64
               "]\n", bc, bc);
    else
        printf("  Number of bytes represented: strange (top 8 bytes "
               "0xff)\n");

skip_to_bytes_rep:
    if (len <= 96)
        return 0;
    bs = ((rth[96] << 24) + (rth[97] << 16) + (rth[98] << 8) + rth[99]);
    if (0 == bs) {
        printf("  Device type specific data (for disk) has block size of "
               "0; unlikely so skip\n");
        goto skip_to_target_dev_desc;
    }
    printf("  Assume pdt=0 (e.g. disk) and decode device type specific "
           "data:\n");
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
        printf("      [so physical block length=%" PRIu32 " bytes]\n",
               bs * (1 << lbppbe));
    printf("    Lowest aligned logical block address=%d\n",
           ((rth[102] & 0x3f) << 8) + rth[103]);

skip_to_target_dev_desc:
    if (target_dev_desc) {
        desig_type = rth[128 + 1] & 0xf;
        if ((0x2 == desig_type) || (0x3 == desig_type) ||
            (0x8 == desig_type) || op->verbose) {
            printf("  Target device descriptor:\n");
            decode_designation_descriptor(rth + 128, rth[131], 0,
                                          op->verbose);
        } else
            printf("  Target device descriptor: unexpected designator "
                   "type [0x%x]\n", desig_type);
    }
    return 0;
}

static int
odx_rt_info(const struct opts_t * op)
{
    int res, fd, err, k, m, bp_chunk, num;
    int got_rtf_len = 0;
    int a_err = 0;
    uint64_t bc;
    unsigned char rth[520];
    struct stat st;

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
    if (fstat(fd, &st) < 0) {
        perror("fstat() on rtf");
        return SG_LIB_FILE_ERROR;
    }
    res = st.st_size % 512;
    if (res > 0) {
        res = st.st_size % 520;
        if (res > 0) {
            pr2serr("rtf size is %d bytes, not a multiple of 512 or 520 "
                    "bytes, so exit\n", (int)st.st_size);
            return SG_LIB_FILE_ERROR;
        }
        ++got_rtf_len;
    }
    bp_chunk = got_rtf_len ? 520 : 512;
    num = st.st_size / bp_chunk;
    if (num > 1)
        printf("Decoding file with %d ROD Tokens:\n", num);
    for (k = 0; k < num; ++k) {
        res = read(fd, rth, bp_chunk);
        if (res < 0) {
            err = errno;
            pr2serr("could not read '%s': %s\n", op->rtf, safe_strerror(err));
            close(fd);
            return SG_LIB_FILE_ERROR;
        }
        if (res < bp_chunk) {
            pr2serr("unable to read %d bytes from '%s', only got %d bytes\n",
                     bp_chunk, op->rtf, res);
            pr2serr("... it is unlikely file '%s' contains a ROD Token\n",
                     op->rtf);
            close(fd);
            return SG_LIB_FILE_ERROR;
        }
        if (op->verbose > 3) {
            pr2serr("Hex dump of chunk %d from rtf file:\n", bp_chunk);
            dStrHexErr((const char *)rth, bp_chunk, 1);
        }
        if (num > 1)
            printf("%s Decoding information from ROD Token %d\n",
                   ((k > 0) ? "\n" : ""), k);
        else
            printf("Decoding information from ROD Token:\n");

        res = odx_print_rod_tok(op, rth, 512);
        if (res && (0 == a_err))
            a_err = res;

        if (got_rtf_len) {
            for (m = 0, bc = 0; m < 8; ++m) {
                if (m > 0)
                    bc <<= 8;
                bc += rth[512 + m];
            }
            printf("  Number of bytes represented: %" PRIu64 " [0x%" PRIx64
                   "] (appended to token)\n", bc, bc);
        }
    }
    close(fd);
    return a_err;
}

static int
do_copy_abort(struct opts_t * op)
{
    return pt_3party_copy_out(op->idip->fd, SA_COPY_ABORT, op->list_id,
                              DEF_GROUP_NUM, DEF_3PC_OUT_TIMEOUT, NULL, 0, 1,
                              op->verbose, op->verbose);
}


static int
report_all_toks(struct opts_t * op, struct dev_info_t * dip, int do_hex)
{
    int res, fd, k;
    int a_err = 0;
    unsigned int len, num_mtoks;
    unsigned char rsp[8 + (MAX_NUM_MAN_TOKS * 96)];
    unsigned char * ucp;

    fd = dip->fd;
    res = pt_3party_copy_in(fd, SA_ALL_ROD_TOKS, op->list_id,
                            DEF_3PC_IN_TIMEOUT, rsp, sizeof(rsp), 1,
                            op->verbose, op->verbose);
    if (res)
        return res;

    len = ((rsp[0] << 24) | (rsp[1] << 16) | (rsp[2] << 8) | rsp[3]) + 4;
    if (len <= 8) {
        printf("No management ROD Tokens reported\n");
        if ((len < 8) && op->verbose)
            pr2serr("  somewhat strange available_data=%u\n", len - 4);
        return 0;
    }
    num_mtoks = (len - 8) / 96;
    if ((0 != ((len - 8) % 96)) && op->verbose)
        pr2serr("  available_data=%u implies non-integral number of "
                "management tokens\n", len - 4);
    if (num_mtoks > MAX_NUM_MAN_TOKS) {
        pr2serr("  %u management ROD Tokens available, can only display "
                "first %d\n", num_mtoks, MAX_NUM_MAN_TOKS);
        num_mtoks = MAX_NUM_MAN_TOKS;
    }
    if (do_hex) {
        if (do_hex > 1) {
            dStrHex((const char *)rsp, ((len < 8) ? len : 8), 1);
            for (k = 0, ucp = rsp + 8; k < (int)num_mtoks; ++k, ucp += 96) {
                printf("\n");
                dStrHex((const char *)ucp, 96, 1);
            }
        } else  {
            if (len > sizeof(rsp))
                len = sizeof(rsp);
            dStrHex((const char *)rsp, len , 1);
        }
        return 0;
    }
    printf("Number of managed ROD Token headers returned: %u\n",
           (len - 8) / 96);
    if (num_mtoks < ((len - 8) / 96))
        printf("  Number displayed: %u\n", num_mtoks);
    for (k = 0, ucp = rsp + 8; k < (int)num_mtoks; ++k, ucp += 96) {
        printf("\n ROD Token header %d\n", k + 1);

        res = odx_print_rod_tok(op, ucp, 96);
        if (res && (0 == a_err))
            a_err = res;
    }
    return a_err;
}

static int
write_to_rtf(struct opts_t * op, const struct rrti_resp_t * rp)
{
    int res, len, err;
    const char * cp;

    if (op->rtf_fd < 0) {
        cp = DEF_ROD_TOK_FILE;
        strncpy(op->rtf, cp, INOUTF_SZ - 1);
        pr2serr("no --rtf=RTF given (or RTF broken) so writing ROD Token "
                "to %s\n", cp);
        res = open_rtf(op);
        if (res)
            return SG_LIB_FILE_ERROR;
    } else
        cp = op->rtf;
    len = (rp->rt_len > 512) ? 512 : rp->rt_len;
    res = write(op->rtf_fd, rp->rod_tok, len);
    if (res < 0) {
        err = errno;
        pr2serr("%s: unable to write to file: %s [%s]\n", __func__,
                cp, safe_strerror(err));
        return SG_LIB_FILE_ERROR;
    }
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
    int k, len, res, got;

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
    if (op->verbose > 3) {
        pr2serr("scatter-gather list (%d elements):\n", op->in_sgl_elems);
        for (k = 0; k < op->in_sgl_elems; ++k)
            pr2serr("  lba: 0x%" PRIx64 ", number: 0x%" PRIx32 "\n",
                    op->in_sgl[k].lba, op->in_sgl[k].num);
    }
    return 0;
}


int
main(int argc, char * argv[])
{
    int c, k, n, fd, flags, blk_sz, cont, err;
    uint32_t delay;
    int64_t i64, num_blks;
    uint64_t tc;
    int req_abort = 0;
    int req_all_toks = 0;
    int do_block = 0;
    int do_hex = 0;
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
    const char * sglp = NULL;
    char b[80];
    char bb[80];
    unsigned char rt[512];

    state_init(&ops, &iflag, &oflag, &ids, &ods, &o2ds);
    op = &ops;
    memset(&sir, 0, sizeof(sir));
    memset(&rrti_rsp, 0, sizeof(rrti_rsp));

    while (1) {
        int option_index = 0;

        c = getopt_long(argc, argv, "AaBDhHiIl:O:pP:r:Rst:T:vVw:y",
                        long_options, &option_index);
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
        case 'D':
            ++op->oflagp->del_tkn;
            break;
        case 'h':
        case '?':
            usage();
            return 0;
        case 'H':
            ++do_hex;
            break;
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
            if (req_pt) {
                pr2serr("Using two --pt=GL options is contradictory\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            ++req_pt;
            sglp = optarg;
            break;
        case 'r':
            if (op->rtf[0]) {
                pr2serr("Can only use --rtf=RTF once for ROD Token "
                        "filename\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (optarg && (0 == strlen(optarg))) {
                pr2serr("--rtf= needs a non-blank argument (a filename)\n");
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
            if (req_wut) {
                pr2serr("Using two --wut=SL options is contradictory\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            ++req_wut;
            sglp = optarg;
            break;
        case 'y':
            ++op->o_readonly;
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

    op->odx_request = ODX_REQ_NONE;
    k = 0;
    if (req_abort)
        ++k;
    if (req_all_toks)
        ++k;
    if (do_poll) {
        ++k;
        op->odx_request = ODX_READ_INTO_RODS;
    }
    if (req_pt) {
        ++k;
        op->odx_request = ODX_READ_INTO_RODS;
    }
    if (do_receive) {
        ++k;
        op->odx_request = ODX_READ_INTO_RODS;
    }
    if (req_wut) {
        ++k;
        op->odx_request = ODX_WRITE_FROM_RODS;
    }
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
    if (1 == k) {
        if ('\0' == op->idip->fn[0]) {
            pr2serr("need a DEVICE (e.g. /dev/sg3) to send command to\n");
            return SG_LIB_SYNTAX_ERROR;
        }
        if (! (req_all_toks || op->list_id_given))
            op->list_id = DEF_LID4_LID;
    }
    if (op->rtf[0]) {
        if (do_info) {
            if (op->idip->fn[0])
                pr2serr("Ignore device name [%s] and decode RTF\n",
                        op->idip->fn);
            return odx_rt_info(op);
        } else if ('\0' == op->idip->fn[0])
            return odx_rt_info(op);
    }
    /* If present, this will cause the ROD's size to be appended to the
     * corresponding ROD Token placed in the RTF file (big endian, 8 byte) */
    np = getenv(ODX_RTF_LEN);
    if (np)
        ++op->rtf_len_add;

    if (req_pt || req_wut) {
        ret = do_sgl(op, (req_pt ? "--pt=" : "--wut="), sglp);
        if (ret)
            return ret;
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

    if (op->odx_request != ODX_REQ_NONE) {
        if (('\0' == op->rtf[0]) &&
            (ODX_WRITE_FROM_RODS == op->odx_request) &&
            (RODT_BLK_ZERO != op->rod_type)) {
            pr2serr("--wut= needs ROD token file but no --rtf=RTF\n");
            ret = SG_LIB_FILE_ERROR;
            goto clean_up;
        }
        ret = open_rtf(op);
        if (ret) {
            ret = SG_LIB_FILE_ERROR;
            goto clean_up;
        }
    }

    if (req_abort) {
        ret = do_copy_abort(op);
        if (ret)
            goto clean_up;
    } else if (req_all_toks) {
        ret = report_all_toks(op, op->idip, do_hex);
        if (ret)
            goto clean_up;
    } else if (do_poll) {
        do {
            ret = do_rrti(op, DDPT_ARG_IN, &rrti_rsp, op->verbose);
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
        printf("  transfer count of %" PRIu64 " [0x%" PRIx64 "]\n",
               rrti_rsp.tc, rrti_rsp.tc);
        if ((SA_POP_TOK == rrti_rsp.for_sa) && (rrti_rsp.rt_len > 0)) {
            ret = write_to_rtf(op, &rrti_rsp);
            if (ret)
                goto clean_up;
        }
    } else if (req_pt) {
        op->odx_request = ODX_READ_INTO_RODS;
        num_blks = count_sgl_blocks(op->in_sgl, op->in_sgl_elems);
        if ((ret = do_pop_tok(op, 0, num_blks, 0, op->verbose)))
            goto clean_up;
        else if (op->iflagp->immed)
            goto clean_up;
        if ((ret = process_after_poptok(op, &tc, op->verbose)))
            goto clean_up;
        printf("PT completes with a transfer count of %" PRIu64 " [0x%"
               PRIx64 "]\n", tc, tc);
        if (op->rtf_fd < 0) {   /* ROD Token not sent to file, so ... */
            /* dummy up rrti_rsp object and write to DEF_ROD_TOK_FILE */
            rrti_rsp.rt_len = 512;
            get_local_rod_tok(rrti_rsp.rod_tok, rrti_rsp.rt_len);
            ret = write_to_rtf(op, &rrti_rsp);
            if (ret)
                goto clean_up;
        }
        goto clean_up;
    } else if (do_receive) {
        ret = do_rrti(op, DDPT_ARG_IN, &rrti_rsp, op->verbose);
        if (ret)
            goto clean_up;
        sg_get_opcode_sa_name(THIRD_PARTY_COPY_OUT_CMD, rrti_rsp.for_sa, 0,
                              (int)sizeof(b), b);
        printf("RRTI for %s: %s\n", b,
               cpy_op_status_str(rrti_rsp.cstat, bb, sizeof(bb)));
        printf("  transfer count of %" PRIu64 " [0x%" PRIx64 "]\n",
               rrti_rsp.tc, rrti_rsp.tc);
        if ((SA_POP_TOK == rrti_rsp.for_sa) && (rrti_rsp.rt_len > 0)) {
            ret = write_to_rtf(op, &rrti_rsp);
            if (ret)
                goto clean_up;
        }
    } else if (req_wut) {
        op->odx_request = ODX_WRITE_FROM_RODS;
        memset(rt, 0, sizeof(rt));
        if (RODT_BLK_ZERO == op->rod_type) {
            if (op->verbose > 1)
                pr2serr("  configure for block device zero ROD Token\n");
            rt[0] = (unsigned char)((RODT_BLK_ZERO >> 24) & 0xff);
            rt[1] = (unsigned char)((RODT_BLK_ZERO >> 16) & 0xff);
            rt[2] = (unsigned char)((RODT_BLK_ZERO >> 8) & 0xff);
            rt[3] = (unsigned char)(RODT_BLK_ZERO & 0xff);
            rt[6] = (unsigned char)((ODX_ROD_TOK_LEN_FLD >> 8) & 0xff);
            rt[7] = (unsigned char)(ODX_ROD_TOK_LEN_FLD & 0xff);
        } else {
            ret = read(op->rtf_fd, rt, sizeof(rt));
            if (ret < 0) {
                err = errno;
                pr2serr("could not read '%s': %s\n", op->rtf,
                        safe_strerror(err));
                goto clean_up;
            }
            if (ret < (int)sizeof(rt))
                pr2serr("unable to read %d bytes from '%s', only got %d "
                        "bytes\n", (int)sizeof(rt), op->rtf, ret);
        }
        num_blks = count_sgl_blocks(op->out_sgl, op->out_sgl_elems);
        if ((ret = do_wut(op, rt, 0, num_blks, op->offset_in_rod,
                          1 /* assume more left */, 0, op->verbose)))
            goto clean_up;
        else if (op->oflagp->immed)
            goto clean_up;
        if ((ret = process_after_wut(op, &tc, op->verbose)))
            goto clean_up;
        printf("WUT completes with a transfer count of %" PRIu64 " [0x%"
               PRIx64 "]\n", tc, tc);
    } else if (do_info || do_size) {
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
            if (do_info) {
                if (0x8 & sir.byte_5) {
                    printf("3PC (third party copy) bit set in standard "
                           "INQUIRY response\n");
                    printf(" Third Party Copy VPD page:\n");
                    print_3pc_vpd(op, 0);
                } else {
                    printf("3PC (third party copy) bit clear in standard "
                           "INQUIRY response\n");
                    printf("  so %s [pdt=0x%x] does not seem to support "
                           "XCOPY\n", op->idip->fn, sir.peripheral_type);
                }
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
    } else
        printf("Expecting to see an option; try again with '-h'\n");

clean_up:
    if ((req_pt || req_wut) && op->iflagp->immed && (0 == ret))
        pr2serr("Started ODX %s command in immediate mode.\nUser may need "
                "--list_id=%" PRIu32 " on following invocation with "
                "--receive or\n--poll for completion\n",
                (req_pt ? "Populate Token" : "Write Using Token"),
                op->list_id);
    if (op->idip->fd >= 0) {
        if (op->idip->d_type & FT_PT)
            pt_close(op->idip->fd);
        else if (op->idip->d_type & FT_BLOCK)
            close(op->idip->fd);
    }
    if (op->rtf_fd >= 0)
        close(op->rtf_fd);
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
