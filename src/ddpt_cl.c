/*
 * Copyright (c) 2008-2018, Douglas Gilbert
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
 * This file contains command line (hence '_cl') helper functions for
 * parsing options and arguments for ddpt.
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
#define __STDC_LIMIT_MACROS 1   /* for UINT64_MAX, UINT32_MAX, etc */
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
#include "sg_pr2serr.h"


void
ddpt_usage(int help)
{
    if (help < 2)
        goto primary_help;
    else if (2 == help)
        goto secondary_help;
    else
        goto tertiary_help;

primary_help:
    pr2serr("Usage: "
           "ddpt  [bpt=BPT[,OBPC]] [bs=BS] [cdbsz=6|10|12|16|32] [coe=0|1]\n"
           "             [coe_limit=CL] [conv=CONVS] [count=COUNT] "
           "[delay=MS[,W_MS]]\n"
           "             [ibs=IBS] [id_usage=LIU] if=IFILE [iflag=FLAGS] "
           "[intio=0|1]\n"
           "             [iseek=SKIP] [ito=ITO] [list_id=LID] [obs=OBS] "
           "[of=OFILE]\n"
           "             [of2=OFILE2] [oflag=FLAGS] [oseek=SEEK] "
           "[prio=PRIO]\n"
           "             [protect=RDP[,WRP]] [retries=RETR] [rtf=RTF] "
           "[rtype=RTYPE]\n"
           "             [seek=SEEK] [skip=SKIP] [status=STAT] [to=TO] "
           "[verbose=VERB]\n"
#ifdef SG_LIB_WIN32
           "             [--help] [--odx] [--quiet] [--verbose] [--version] "
           "[--wscan]\n"
           "             [--xcopy]"
#else
           "             [--help] [--odx] [--quiet] [--verbose] [--version] "
           "[--xcopy]\n"
           "             "
#endif
           "[JF]\n"
           "  where the main options are:\n"
           "    bpt         input Blocks Per Transfer (BPT) (def: 128 when "
           "IBS is 512)\n"
           "                Output Blocks Per Check (OBPC) (def: 0 implies "
           "BPT*IBS/OBS)\n"
           "    bs          block size for input and output (overrides "
           "ibs and obs)\n");
    pr2serr(
           "    coe         0->exit on error (def), 1->continue on "
           "error (zero fill)\n"
           "    count       number of input blocks to copy (def: "
           "(remaining)\n"
           "                device/file size)\n"
           "    ibs         input block size (default 512 bytes)\n"
           "    if          file or device to read from (for stdin use "
           "'-')\n"
           "    iflag       input flags, comma separated list from FLAGS "
           "(see below)\n"
           "    obs         output block size (def: 512). When IBS is "
           "not equal to OBS\n"
           "                then (((IBS * BPT) %% OBS) == 0) is required\n"
           "    of          file or device to write to (def: /dev/null)\n");
    pr2serr(
           "    oflag       output flags, comma separated list from FLAGS "
           "(see below)\n"
           "    seek        block position to start writing in OFILE\n"
           "    skip        block position to start reading from IFILE\n"
           "    status      'noxfer' suppresses throughput calculation; "
           "'none'\n"
           "                suppresses all trailing reports (apart from "
           "errors);\n"
           "                'progress' reports every 2 minutes on progress\n"
           "    verbose     0->normal(def), 1->some noise, 2->more noise, "
           "etc\n"
           "                -1->quiet (same as --quiet option)\n"
           "  --dry-run|-d    do preparation, bypass copy\n"
           "  --help|-h     print out this usage message then exit\n"
           "  --job=JF|-j JF    JF is job file containing options\n"
           "  --odx|-o       do ODX copy rather than normal rw copy\n"
           "  --quiet|-q     suppresses messages (by redirecting them "
           "to /dev/null)\n"
           "  --verbose|-v    equivalent to verbose=1\n"
           "  --version|-V    print version information then exit\n"
#ifdef SG_LIB_WIN32
           "  --wscan|-w     windows scan for device names and volumes\n"
#endif
           "  --xcopy|-x    do xcopy(LID1) rather than normal rw copy\n"
           "  JF            job file: a file containing options; can not "
           "start\n"
           "                with '-' or contain '='. Parsed when seen\n"
           "\nCopy all or part of IFILE to OFILE, IBS*BPT bytes at a time. "
           "Similar to\n"
           "dd command. Support for block devices, especially those "
           "accessed via a\nSCSI pass-through. Also supports offloaded "
           "copies: xcopy(LID1) and ODX.\nFor more information use "
           "'-h' multiple times (e.g. '-hh' or '-hhh').\n");
    return;

secondary_help:
    pr2serr("  where the lesser used command line options are:\n"
           "    cdbsz       size of SCSI READ or WRITE cdb (default is "
           "10)\n"
           "    coe_limit   limit consecutive 'bad' blocks on reads to CL "
           "times\n"
           "                when coe=1 (default: 0 which is no limit)\n"
           "    conv        conversions, comma separated list of CONVS "
           "(see '-hhh')\n"
           "    delay       wait MS milliseconds between each copy segment "
           "(def: 0)\n"
           "                wait W_MS milliseconds prior to each write "
           "(def: 0)\n"
           "    id_usage    xcopy: set list_id_usage to hold (0), discard "
           "(2),\n"
           "                disable (3), or the given number (def: 0 or "
           "2)\n"
           "    intio       interrupt during IO; allow signals during reads "
           "and writes\n"
           "                (def: 0 causes signals to be masked during IO)\n"
           "    iseek       block position to start reading from IFILE "
           "(same as skip)\n"
           "    ito         inactivity timeout (def: 0 (from 3PC VPD); "
           "units: seconds)\n"
           "    list_id     xcopy: list_id (def: 1 or 0) [1 byte]\n"
           "                odx: list_id (def: 257 or 258) [4 bytes]\n"
           "    of2         additional output file (def: /dev/null), "
           "OFILE2 should be\n"
           "                regular file or pipe\n"
           "    oseek       block position to start writing in OFILE (same "
           "as seek)\n"
           "    prio        xcopy: set priority field to PRIO (def: 1)\n"
           "    protect     set rdprotect and/or wrprotect fields on "
           "pt commands\n"
           "    retries     retry pass-through errors RETR times "
           "(def: 0)\n"
           "    rtf         ROD Token filename (odx)\n"
           "    rtype       ROD type (odx), can be pit-any, pit-def, "
           "pit-pers,\n"
           "                pit-vuln, zero or number (def: 0 -> cm "
           "decides)\n"
           "    to          xcopy, odx: timeout in seconds (def: 600 "
           "(10 mins))\n\n");
    pr2serr("FLAGS: (arguments to oflag= and oflag=; may be comma "
            "separated)\n"
            "  00 (i)         input will be all 0x0 bytes\n"
            "  append (o)     append (part of) IFILE to end of OFILE\n"
            "  atomic (o,pt)  use WRITE ATOMIC(16) on OFILE\n"
            "  block (pt)     pt opens are non blocking by default\n"
            "  cat (xcopy)    set CAT bit in segment descriptor header\n"
            "  coe            continue on (read) error\n"
            "  dc (xcopy)     set DC bit in segment descriptor header\n"
            "  direct         set O_DIRECT flag in open() of IFILE and/or "
            "OFILE\n"
            "  dpo            set disable page out (DPO) on pt READs and "
            "WRITES\n"
            "  errblk (i,pt)  write errored LBAs to errblk.txt file\n"
            "  excl           set O_EXCL flag in open() of IFILE and/or "
            "OFILE\n"
            "  fdatasync (o)  flushes data to OFILE at the end of copy\n"
            "  ff (i)         input will be all 0xff bytes\n"
            "  flock          use advisory exclusive lock [flock()] on "
            "IFILE/OFILE\n"
            "  force          override inconsistent information that would "
            "stop copy\n"
            "  fsync (o)      like fdatasync but flushes meta-data as well\n"
            "  fua (pt)       force unit access on IFILE or OFILE\n"
            "  fua_nv (pt)    force unit access, non-volatile (obsoleted by "
            "T10)\n"
            "  ignoreew (o)   ignore early warning (end of tape)\n"
            "  immed (odx)    commands poll until complete, report "
            "progress\n"
            "... continued on next page (use '-hhh')\n");
    return;
tertiary_help:
    pr2serr("FLAGS: (continued)\n"
            "  nocache        use posix_fadvise(POSIX_FADV_DONTNEED)\n"
            "  no_del_tkn (odx)  do not set DEL_TKN on last write from "
            "ROD\n"
            "  nofm (o)       no File Mark (FM) on close when writing to "
            "tape\n"
            "  nopad          inhibits tapes blocks less than OBS being "
            "padded\n"
            "  norcap (pt)    do not invoke SCSI READ CAPACITY command\n"
            "  nowrite (o)    bypass all writes to OFILE\n"
            "  null           does nothing, place holder\n"
            "  odx            request xcopy(LID4) based on POPULATE TOKEN "
            "(disk->ROD)\n"
            "                 and/or WRITE USING TOKEN (ROD->disk) "
            "commands\n"
            "  pad (o)        pad blocks shorter than OBS with zeros\n"
            "  pre-alloc (o)  use fallocate() before copy to set OFILE to "
            "its\n"
            "                 expected size\n"
            "  prefer_rts (odx)  prefer RCS over RRTI command (def: RRTI)\n"
            "  pt             instruct pass-through interface to be used\n"
            "  rarc (i,pt)    set RARC (rebuild assist) bit in SCSI READs\n"
            "  resume (o)     attempt to restart an interrupted copy\n"
            "  rtf_len        place ROD size after each ROD token in RTF\n"
            "  self (pt)      used with trim; IFILE=OFILE; trim zero "
            "segments\n"
            "  sparing (o)    read OFILE prior to a write; don't write if "
            "same\n"
            "  sparse (o)     don't write blocks of zeros; move file "
            "pointer\n"
            "                 or if OFILE is pt assume it contains zeros "
            "already\n"
            "  ssync (o,pt)   at end of copy do SCSI SYNCHRONIZE CACHE\n"
            "  strunc (o)     sparse copy using ftruncate to extend OFILE "
            "as needed\n"
            "  sync           set O_SYNC flag in open() of IFILE and/or "
            "OFILE\n"
            "  trim (pt)      use SCSI UNMAP (trim) on zero segments "
            "instead of\n"
            "                 writing them to OFILE\n"
            "  trunc (o)      truncate a regular OFILE prior to copy (def: "
            "overwrite)\n"
            "  unmap (pt)     same as trim flag\n"
            "  xcopy (pt)     invoke SCSI XCOPY; send to IFILE or OFILE.\n\n"
            "CONVS:\n"
            "  fdatasync      same as oflag=fdatasync\n"
            "  fsync          same as oflag=fsync\n"
            "  no_del_tkn     same as oflag=no_del_tkn\n"
            "  noerror        similar to iflag=coe\n"
            "  notrunc        does nothing because this is default action "
            "of ddpt\n"
            "  null           does nothing, place holder\n"
            "  resume         same as oflag=resume\n"
            "  rtf_len        same as oflag=rtf_len\n"
            "  sparing        same as oflag=sparing\n"
            "  sparse         same as oflag=sparse\n"
            "  sync           ignored to allow 'conv=noerror,sync' dd usage "
            "for coe\n"
            "  trunc          same as oflag=trunc\n\n"
            "ENVIRONMENT VARIABLES:\n"
            "  DDPT_DEF_BS    its value replaces the default block size of "
            "512 bytes\n"
            "  ODX_RTF_LEN    append ROD size (8 byte big-endian) to token "
            "in RTF\n"
            "  XCOPY_TO_DST   send XCOPY command to OFILE (destination) "
            "if no other\n"
            "                 indication\n"
            "  XCOPY_TO_SRC   send XCOPY command to IFILE (source)\n");
}

/* Crude attempt to carve a command line into an argv array of pointers
 * to arguments/options and returns argc. No more than max_args elements
 * placed in argv. Modifies given cmd_line! Everything after the first
 * found '#' is ignored. Uses strtok to carve up cmd_line. */
static int
argcargv(char * cmd_line, char ** argv, int max_args)
{
    int argc = 0;
    char * p2;

    if ((p2 = strchr(cmd_line, '#')))
        *p2 = '\0';     /* bad luck if in quotes */
    p2 = strtok(cmd_line, " ");
    while (p2 && (argc < max_args)) {
            argv[argc++] = p2;
        p2 = strtok(0, " ");
    }
    return argc;
}

/* Allocates and then populates a scatter gether list (array) and returns
 * it via *sgl_pp. Return of 0 is okay, else error number (in which case
 * NULL is written to *sgl_pp) . */
static int
skip_seek(struct opts_t * op, const char * key, const char * buf,
          struct scat_gath_elem ** sgl_pp, int * num_elems_p)
{
    bool def_hex = false;
    int len, err;
    int vb = op->verbose;  /* needs to appear before skip/seek= on cl */
    int64_t ll;
    const char * cp;

    len = (int)strlen(buf);
    if ((('-' == buf[0]) && (1 == len)) || ((len > 1) && ('@' == buf[0])) ||
        ((len > 2) && ('H' == toupper(buf[0])) && ('@' == buf[1]))) {
        if ('H' == toupper(buf[0])) {
            cp = buf + 2;
            def_hex = true;
        } else if ('-' == buf[0])
            cp = buf;
        else
            cp = buf + 1;
        *sgl_pp = file2sgl(cp, def_hex, num_elems_p, &err, true);
        if (NULL == *sgl_pp) {
            pr2serr("bad argument to '%s=' [err=%d]\n", key, err);
            return err ? err : SG_LIB_SYNTAX_ERROR;
        }
        if (vb > 1)
            pr2serr("%s: file, %d sgl elements\n", key, *num_elems_p);
    } else if (num_either_ch_in_str(buf, len, ',', ' ') > 0) {
        *sgl_pp = cli2sgl(buf, num_elems_p, vb > 0);
        if (NULL == *sgl_pp) {
            pr2serr("bad cli argument to '%s='\n", key);
            return SG_LIB_SYNTAX_ERROR;
        }
        if (vb > 1)
            pr2serr("%s: cli, %d sgl elements\n", key, *num_elems_p);
    } else {    /* single number on command line (e.g. skip=1234) */
        *sgl_pp = (struct scat_gath_elem *)
                                calloc(1, sizeof(struct scat_gath_elem));
        if (NULL == *sgl_pp) {
            pr2serr("No memory available for '%s='\n", key);
            return sg_convert_errno(ENOMEM);
        }
        ll = sg_get_llnum(buf);
        if (-1LL == ll) {
            pr2serr("bad argument to '%s='\n", key);
            return SG_LIB_SYNTAX_ERROR;
        }
        (*sgl_pp)->lba = (uint64_t)ll;
        *num_elems_p = 1;
        if (vb > 1)
            pr2serr("%s: single, half a sgl element\n", key);
    }
    return 0;
}

static int
do_skip(struct opts_t * op, const char * key, const char * buf)
{
    int ret;

    ret = skip_seek(op, key, buf, &op->i_sgli.sglp, &op->i_sgli.elems);
    if (0 == ret)
        sgl_sum_scan(&op->i_sgli, key, (op->verbose > 1));
    return ret;
}

static int
do_seek(struct opts_t * op, const char * key, const char * buf)
{
    int ret;

    ret = skip_seek(op, key, buf, &op->o_sgli.sglp, &op->o_sgli.elems);
    if (0 == ret)
        sgl_sum_scan(&op->o_sgli, key, (op->verbose > 1));
    return ret;
}

/* Process arguments given to 'conv=" option. Returns 0 on success,
 * 1 on error. */
static int
conv_process(const char * arg, struct flags_t * ifp, struct flags_t * ofp)
{
    char buff[256];
    char * cp;
    char * np;

    strncpy(buff, arg, sizeof(buff));
    buff[sizeof(buff) - 1] = '\0';
    if ('\0' == buff[0]) {
        pr2serr("no conversions found\n");
        return 1;
    }
    cp = buff;
    do {
        np = strchr(cp, ',');
        if (np)
            *np++ = '\0';
        if (0 == strcmp(cp, "fdatasync"))
            ofp->fdatasync = true;
        else if (0 == strcmp(cp, "fsync"))
            ofp->fsync = true;
        else if (0 == strcmp(cp, "no_del_tkn"))
            ofp->no_del_tkn = true;
        else if (0 == strcmp(cp, "noerror"))
            ifp->coe = true;        /* will still fail on write error */
        else if (0 == strcmp(cp, "notrunc"))
            ;         /* this is the default action of ddpt so ignore */
        else if (0 == strcmp(cp, "null"))
            ;
        else if ((0 == strcmp(cp, "prefer_rcs")) ||
                 (0 == strcmp(cp, "prefer-rcs")))
            ofp->prefer_rcs = true;
        else if (0 == strcmp(cp, "resume"))
            ofp->resume = true;
        else if ((0 == strcmp(cp, "rtf_len")) ||
                 (0 == strcmp(cp, "rtf-len")))
            ofp->rtf_len = true;
        else if (0 == strcmp(cp, "sparing"))
            ofp->sparing = true;
        else if (0 == strcmp(cp, "sparse"))
            ++ofp->sparse;
        else if (0 == strcmp(cp, "sync"))
            ;   /* dd(susv4): pad errored block(s) with zeros but ddpt does
                 * that by default. Typical dd use: 'conv=noerror,sync' */
        else if (0 == strcmp(cp, "trunc"))
            ofp->trunc = true;
        else {
            pr2serr("unrecognised flag: %s\n", cp);
            return 1;
        }
        cp = np;
    } while (cp);
    return 0;
}

/* Process arguments given to 'iflag=" and 'oflag=" options. Returns 0
 * on success, 1 on error. */
static int
flags_process(const char * arg, struct flags_t * fp)
{
    char buff[256];
    char * cp;
    char * np;

    strncpy(buff, arg, sizeof(buff));
    buff[sizeof(buff) - 1] = '\0';
    if ('\0' == buff[0]) {
        pr2serr("no flag found\n");
        return 1;
    }
    cp = buff;
    do {
        np = strchr(cp, ',');
        if (np)
            *np++ = '\0';
        if (0 == strcmp(cp, "append"))
            fp->append = true;
        else if (0 == strcmp(cp, "atomic"))
            fp->atomic = true;
        else if (0 == strcmp(cp, "block"))
            fp->block = true;
        else if (0 == strcmp(cp, "bytchk"))
            ++fp->bytchk;
        else if (0 == strcmp(cp, "cat"))
            fp->cat = true;
        else if (0 == strcmp(cp, "coe"))
            fp->coe = true;
        else if (0 == strcmp(cp, "dc"))
            fp->dc = true;
        else if (0 == strcmp(cp, "direct"))
            fp->direct = true;
        else if (0 == strcmp(cp, "dpo"))
            fp->dpo = true;
        else if (0 == strcmp(cp, "errblk"))
            fp->errblk = true;
        else if (0 == strcmp(cp, "excl"))
            fp->excl = true;
        else if (0 == strcmp(cp, "fdatasync"))
            fp->fdatasync = true;
        else if (0 == strcmp(cp, "ff"))
            fp->ff = true;
        else if (0 == strcmp(cp, "flock"))
            fp->flock = true;
        else if (0 == strcmp(cp, "force")) /* 2: try harder on xcopy */
            ++fp->force;
        else if (0 == strcmp(cp, "fsync"))
            fp->fsync = true;
        else if ((0 == strcmp(cp, "fua_nv")) || /* obsolete: sbc3r35l */
                 (0 == strcmp(cp, "fua-nv")))   /* check fua_nv before fua */
            fp->fua_nv = true;
        else if (0 == strcmp(cp, "fua"))
            fp->fua = true;
        else if (0 == strcmp(cp, "ignoreew")) /* tape: ignore early warning */
            fp->ignoreew = true;
        else if (0 == strcmp(cp, "immed"))
            fp->immed = true;
        else if (0 == strcmp(cp, "nocache"))    /* can be (0), 1, 2, or 3 */
            ++fp->nocache;
        else if ((0 == strcmp(cp, "no_del_tkn")) ||
                 (0 == strcmp(cp, "no-del-tkn")))
            fp->no_del_tkn = true;
        else if (0 == strcmp(cp, "nofm"))     /* No filemark on tape close */
            fp->nofm = true;
        else if (0 == strcmp(cp, "nopad"))
            fp->nopad = true;
        else if (0 == strcmp(cp, "norcap"))
            fp->norcap = true;
        else if (0 == strcmp(cp, "nowrite"))
            fp->nowrite = true;
        else if (0 == strcmp(cp, "null"))       /* ignore */
            ;
        else if (0 == strcmp(cp, "odx"))
            fp->odx = true;
        else if (0 == strcmp(cp, "pad"))
            fp->pad = true;
        else if ((0 == strcmp(cp, "pre-alloc")) ||
                 (0 == strcmp(cp, "prealloc")))
            fp->prealloc = true;
        else if ((0 == strcmp(cp, "prefer_rcs")) ||
                 (0 == strcmp(cp, "prefer-rcs")))
            fp->prefer_rcs = true;
        else if (0 == strcmp(cp, "pt"))
            fp->pt = true;
        else if (0 == strcmp(cp, "rarc"))
            fp->rarc = true;
        else if (0 == strcmp(cp, "resume"))
            fp->resume = true;
        else if ((0 == strcmp(cp, "rtf_len")) ||
                 (0 == strcmp(cp, "rtf-len")))
            fp->rtf_len = true;
        else if (0 == strcmp(cp, "self"))
            fp->self = true;
        else if (0 == strcmp(cp, "sparing"))
            fp->sparing = true;
        else if (0 == strcmp(cp, "sparse"))
            ++fp->sparse;
        else if (0 == strcmp(cp, "ssync"))
            fp->ssync = true;
        else if (0 == strcmp(cp, "strunc"))
            fp->strunc = true;
        else if (0 == strcmp(cp, "sync"))
            fp->sync = true;
        else if ((0 == strcmp(cp, "trim")) || (0 == strcmp(cp, "unmap"))) {
            /* treat trim (ATA term) and unmap (SCSI term) as synonyms */
            fp->wsame16 = true;
        } else if (0 == strcmp(cp, "trunc"))
            fp->trunc = true;
        else if (0 == strcmp(cp, "verify"))
            fp->verify = true;
        else if (0 == strcmp(cp, "xcopy"))
            fp->xcopy = true;
        else if (0 == strcmp(cp, "00"))
            fp->zero = true;
        else {
            pr2serr("unrecognised flag: %s\n", cp);
            return 1;
        }
        cp = np;
    } while (cp);
    return 0;
}

/* Defaulting transfer (copy buffer) size depending on IBS. 128*2048 for
 * CD/DVDs is too large for the block layer in lk 2.6 and results in an
 * EIO on the SG_IO ioctl. So reduce it in that case.
 * N.B. FreeBSD may reduce bpt later if pt is used on IFILE or OFILE. */
static int
default_bpt_i(int ibs_lb)
{
    if (ibs_lb < 4)
        return DEF_BPT_LT4;     /* currently 16384 (2**14), may change */
    else if (ibs_lb < 8)
        return DEF_BPT_LT8;
    else if (ibs_lb < 64)
        return DEF_BPT_LT64;
    else if (ibs_lb < 1024)
        return DEF_BPT_LT1024;
    else if (ibs_lb < 8192)
        return DEF_BPT_LT8192;
    else if (ibs_lb < 31768)
        return DEF_BPT_LT32768;
    else
        return DEF_BPT_GE32768;
}

/* Command line processing helper, checks sanity and applies some
 * defaults. Returns 0 on success, > 0 for error. */
static int
cl_sanity_defaults(struct opts_t * op)
{
    int def_bs = DEF_BLOCK_SIZE;
    int vb = op->verbose;
    const char * cp;
    char * csp;
    char * cdp;
    char b[80];
    struct flags_t * ifp = op->iflagp;
    struct flags_t * ofp = op->oflagp;
    struct sgl_info_t * i_sglip = &op->i_sgli;
    struct sgl_info_t * o_sglip = &op->o_sgli;

    cp = getenv(DDPT_DEF_BS);
    if (cp) {
        if (vb)
            pr2serr("  %s=%s environment variable detected, modifying block "
                    "size default\n", DDPT_DEF_BS, cp);
        if ((1 == sscanf(cp, "%d", &def_bs)) && (def_bs > 0))
            ;
        else
            def_bs = DEF_BLOCK_SIZE;
    } else
        def_bs = DEF_BLOCK_SIZE;

    if ((0 == op->ibs_lb) && (0 == op->obs_lb)) {
        op->ibs_lb = def_bs;
        op->ibs_pi = op->ibs_lb;
        op->obs_lb = def_bs;
        op->obs_pi = op->obs_lb;
        if (op->idip->fn[0] && (! op->quiet))
            pr2serr("Assume block size of %d bytes for both input and "
                    "output\n", def_bs);
    } else if (0 == op->obs_lb) {
        op->obs_lb = def_bs;
        op->obs_pi = op->obs_lb;
        if ((op->ibs_lb != def_bs) && op->odip->fn[0] && (! op->quiet))
            pr2serr("Neither obs nor bs given so set obs=%d (default "
                    "block size)\n", op->obs_lb);
    } else if (0 == op->ibs_lb) {
        op->ibs_lb = def_bs;
        op->ibs_pi = op->ibs_lb;
        if ((op->obs_lb != def_bs) && (! op->quiet))
            pr2serr("Neither ibs nor bs given so set ibs=%d (default "
                    "block size)\n", op->ibs_lb);
    }
    op->ibs_hold = op->ibs_lb;
    if (op->bpt_given && (op->bpt_i < 1)) {
        op->bpt_given = false;
        /* want to allow bpt=0,<num> where BPT takes the default, for ODX */
    }
    if (! op->bpt_given)
        op->bpt_i = default_bpt_i(op->ibs_lb);

    if ((op->ibs_lb != op->obs_lb) && (ODX_REQ_NONE == op->odx_request) &&
        (0 != ((op->ibs_lb * op->bpt_i) % op->obs_lb))) {
        pr2serr("when 'ibs' and 'obs' differ, ((ibs*bpt)/obs) must have "
                "no remainder (bpt=%d)\n", op->bpt_i);
        return SG_LIB_SYNTAX_ERROR;
    }
    if ((ofp->append > 0) && (op->o_sgli.elems > 0)) {
        /* seek= has possible sgl, want lowest LBA ... */
        sgl_sum_scan(&op->o_sgli, "oflag=append", vb > 1);
        if (op->o_sgli.lowest_lba > 0) {
            pr2serr("Can't use both append and seek switches\n");
            return SG_LIB_SYNTAX_ERROR;
        }
    }
    if (op->bpt_i < 1) {
        pr2serr("internal BPT value 0, cannot continue\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if (ifp->append)
        pr2serr("append flag ignored on input\n");
    if (ifp->ignoreew)
        pr2serr("ignoreew flag ignored on input\n");
    if (ifp->nofm)
        pr2serr("nofm flag ignored on input\n");
    if (ifp->prealloc)
        pr2serr("pre-alloc flag ignored on input\n");
    if (ifp->sparing)
        pr2serr("sparing flag ignored on input\n");
    if (ifp->ssync)
        pr2serr("ssync flag ignored on input\n");
    if (ofp->trunc) {
        if (ofp->resume) {
            ofp->trunc = false;
            if (vb)
                pr2serr("trunc ignored due to resume flag, "
                        "otherwise open_of() truncates too early\n");
        } else if (ofp->append) {
            ofp->trunc = false;
            pr2serr("trunc ignored due to append flag\n");
        } else if (ofp->sparing) {
            pr2serr("trunc flag conflicts with sparing\n");
            return SG_LIB_SYNTAX_ERROR;
        }
    }
    if (ifp->self || ofp->self) {  /* self trim: move relevant stuff to out */
        if (! ofp->self)
            ofp->self = true;
        if (ifp->wsame16 || ofp->wsame16) {
            if (! ofp->wsame16)
                ofp->wsame16 = true;
            if (! ofp->nowrite)
                ofp->nowrite = true;
        }
        if ((op->ibs_lb > 0) && (op->obs_lb > 0) &&
            (op->ibs_lb != op->obs_lb)) {
            pr2serr("self flag doesn't support unequal ibs= and obs=\n");
            return SG_LIB_SYNTAX_ERROR;
        } else if (op->ibs_lb > 0) {    /* force ibs_lb==obs */
            op->obs_lb = op->ibs_lb;
            op->obs_pi = op->ibs_pi;
        } else if (op->obs_lb > 0) {
            op->ibs_lb = op->obs_lb;
            op->ibs_pi = op->obs_pi;
        }

        if ('\0' == op->odip->fn[0])
            strcpy(op->odip->fn, op->idip->fn);
        if (sgl_empty(o_sglip->sglp, o_sglip->elems) &&
            (! sgl_empty(i_sglip->sglp, i_sglip->elems))) {
            /* move in sgl to out sgl */
            *o_sglip = *i_sglip;        /* structure assignment */
            memset(i_sglip, 0, sizeof(struct sgl_info_t));
            if (vb)
                pr2serr("self: moved in sgl to out sgl\n");
        }
    }
    if (ofp->wsame16)
        ofp->sparse += 2;
    if (ofp->strunc && (0 == ofp->sparse))
        ++ofp->sparse;

    if (ifp->xcopy || ofp->xcopy)
        op->has_xcopy = true;
    if (op->has_xcopy) {
        if ((!! ifp->xcopy) == (!! ofp->xcopy)) {
            csp = getenv(XCOPY_TO_SRC);
            cdp = getenv(XCOPY_TO_DST);
            if ((!! csp) == (!! cdp)) {
#if DEF_XCOPY_SRC0_DST1 == 0
                if (! ifp->xcopy)
                    ifp->xcopy = true;
                ofp->xcopy = 0;
#else
                ifp->xcopy = 0;
                if (! ofp->xcopy)
                    ofp->xcopy = true;
#endif
                if (vb > 1)
                    pr2serr("Default dictates which device to send xcopy "
                            "command to:\n");
            } else {
                if (csp) {
                    if (! ifp->xcopy)
                        ifp->xcopy = true;
                    ofp->xcopy = 0;
                } else {
                    ifp->xcopy = 0;
                    if (! ofp->xcopy)
                        ofp->xcopy = true;
                }
                if (vb > 1)
                    pr2serr("%s dictates which device to send xcopy "
                            "command to:\n",
                            (csp ? XCOPY_TO_SRC : XCOPY_TO_DST));
            }
        }
        if (vb) {
            if (vb > 1)
                pr2serr("  ");
            pr2serr("Will send xcopy command to %s [%s=%s]\n",
                    (ifp->xcopy ? "src" : "dst"),
                    (ifp->xcopy ? "if" : "of"),
                    (ifp->xcopy ? op->idip->fn : op->odip->fn));
        }
        op->xc_dc = (ifp->dc || ofp->dc);
        op->xc_cat = (ifp->cat || ofp->cat);
        if (ifp->xcopy) {
            if (! ifp->pt) {
                ifp->pt = true;
                if (vb > 3)
                    pr2serr("Setting pt (pass-through) on IFILE for "
                            "xcopy\n");

            }
        } else {
            if (! ofp->pt) {
                ofp->pt = true;
                if (vb > 3)
                    pr2serr("Setting pt (pass-through) on OFILE for "
                            "xcopy\n");
            }
        }
    }
    if (ifp->odx || ofp->odx || op->rtf[0] || op->rod_type_given)
        op->has_odx = true;
    if (op->has_odx) {
        if (op->has_xcopy) {
            pr2serr("Can either request xcopy(LID1) or ODX but not "
                    "both\n");
            return SG_LIB_SYNTAX_ERROR;
        }
        cp = "";
        if (op->idip->fn[0] && op->odip->fn[0]) {
            op->odx_request = ODX_COPY;
            if (RODT_BLK_ZERO == op->rod_type) {
                if (vb > 1)
                    cp = "zero output blocks: call WRITE USING TOKEN(s), "
                         "repeatedly";
                else
                    cp = "zero output blocks";

            } else {
                if (vb > 1)
                    cp = "full copy: POPULATE TOKEN then WRITE USING "
                         "TOKEN(s), repeatedly";
                else
                    cp = "full copy";
            }
        } else if (op->idip->fn[0]) {
            op->odx_request = ODX_READ_INTO_RODS;
            if (vb)
                cp = "read to tokens; disk-->ROD";
        } else if (op->odip->fn[0]) {
            op->odx_request = ODX_WRITE_FROM_RODS;
            if (vb)
                cp = "write from tokens; ROD-->disk";
        } else {
            pr2serr("Not enough options given to do ODX (xcopy(LID4))\n");
            return SG_LIB_SYNTAX_ERROR;
        }
        csp = getenv(ODX_RTF_LEN);
        if (csp)
                op->rtf_len_add = true;
        if (vb) {
            pr2serr("ODX: %s%s\n", cp, (csp ? "\n    [ODX_RTF_LEN "
                    "environment variable present]" : ""));
            if ((vb > 1) && op->rod_type_given)
                pr2serr("ODX: ROD type: %s\n",
                        rod_type_str(op->rod_type, b, sizeof(b)));
        }
    }
    if (vb) {      /* report flags used but not supported */
#ifndef SG_LIB_LINUX
        if (ifp->flock || ofp->flock)
            pr2serr("warning: 'flock' flag not supported on this "
                    "platform\n");
#endif

#ifndef HAVE_POSIX_FADVISE
        if (ifp->nocache || ofp->nocache)
            pr2serr("warning: 'nocache' flag not supported on this "
                    "platform\n");
#endif

#if O_SYNC == 0
        if (ifp->sync || ofp->sync)
            pr2serr("warning: 'sync' flag (O_SYNC) not supported on "
                    "this platform\n");
#endif
#if O_DIRECT == 0
        if (ifp->direct || ofp->direct)
            pr2serr("warning: 'direct' flag (O_DIRECT) not supported "
                    "on this platform\n");
#endif
    }
    if (ofp->atomic)
        ofp->cdbsz = 16;        /* only WRITE ATOMIC(16) supported for now */
    if (ofp->ff) {
        pr2serr("oflag=ff disallowed, can only be used for input\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if (ofp->zero) {
        pr2serr("oflag=00 disallowed, can only be used for input\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    op->bs_same = (op->ibs_pi == op->obs_pi);
    return 0;
}

static int
jf_process(struct opts_t * op, const char * jf_name, const char * version_str,
           int jf_depth)
{
    bool first_real;
    int k, len, off, rlen, argc;
    int ret = 0;
    FILE * fp;
    char * cp;
    char * argv[DDPT_MAX_JF_ARGS_PER_LINE];
    struct stat a_stat;
    char b[4096];
    char bb[256];

    ++jf_depth;
    if (jf_depth > DDPT_MAX_JF_DEPTH) {
        pr2serr("error parsing job_file: %s, depth=%d too great\n", jf_name,
                jf_depth);
        return SG_LIB_FILE_ERROR;
    }
    if (stat(jf_name, &a_stat) < 0) {
        pr2serr("unable to stat job_file: %s, depth=%d\n", jf_name, jf_depth);
        return sg_convert_errno(errno);
    }
    if (S_ISBLK(a_stat.st_mode) || S_ISCHR(a_stat.st_mode)) {
        pr2serr("job_file: %s, depth=%d is char or block device, unlikely\n",
                jf_name, jf_depth);
        return SG_LIB_FILE_ERROR;
    }
    if (op->verbose)
        pr2serr("parsing job_file: %s, depth=%d\n", jf_name, jf_depth);

    if (NULL == (fp = fopen(jf_name, "r"))) {
        pr2serr("open of job_file: %s (depth=%d) failed: %s\n", jf_name,
                jf_depth, strerror(errno));
        return SG_LIB_FILE_ERROR;
    }
    rlen = sizeof(b);
    for (off = 0, k = 0, first_real = true;
         (rlen > 0) && ((cp = fgets(b + off, rlen, fp)) &&
         (k < DDPT_MAX_JF_LINES));
         rlen -= len, off += len, ++k) {
        len = strlen(b + off);
        if (0 == len)
            continue;
        if (first_real) {
            first_real = false;
            if ((0 == op->iflagp->force) && (0 == op->oflagp->force)) {
                /* in absence of iflag=force or oflag-force, check if job
                 * file seems to be binary */
                if ((b[0] > 0x7e) || ((len > 1) && (b[1] > 0x7e)) ||
                    ((len > 2) && (b[2] > 0x7e)) ||
                    ((len > 3) && (b[3] > 0x7e))) {
                        pr2serr("job_file: %s might be binary so exit; use\n"
                                "iflag=force or oflag=force prior to job "
                                "file to override\n", jf_name);
                        return SG_LIB_FILE_ERROR;
                }
            }
        }
        if ('\n' == b[off + len - 1]) {
            b[off + len - 1] = '\0';
            --len;
            if (0 == len)
                continue;
            else if ('\r' == b[off + len - 1]) {
                b[off + len - 1] = '\0';
                --len;
                if (0 == len)
                    continue;
            }
        }
        snprintf(bb, sizeof(bb), "%s (depth=%d) line %d", jf_name, jf_depth,
                 k + 1);
        argv[0] = bb;
        argc = argcargv(b + off, argv + 1, DDPT_MAX_JF_ARGS_PER_LINE - 1);
        ++argc;
        ret = cl_process(op, argc, argv, version_str, jf_depth);
        if (ret) {
            pr2serr("failed parsing job file %s (depth=%d) at line %d\n",
                    jf_name, jf_depth, k + 1);
            break;
        }
    }
    if (rlen < 1) {
        pr2serr("job file %s (depth=%d) too large\n", jf_name, jf_depth);
        ret = SG_LIB_FILE_ERROR;
    } else if (ferror(fp)) {
        pr2serr("job file %s (depth=%d) read error\n", jf_name, jf_depth);
        ret = SG_LIB_FILE_ERROR;
    } else if (k >= DDPT_MAX_JF_LINES) {
        pr2serr("job file %s (depth=%d) more than %d lines\n", jf_name,
                jf_depth, DDPT_MAX_JF_LINES);
        ret = SG_LIB_FILE_ERROR;
    }
    fclose(fp);
    return ret;
}

/* Process options on the command line. Returns 0 if successful, > 0 for
 * (syntax) error and -1 for early exit (e.g. after '--help') */
int
cl_process(struct opts_t * op, int argc, char * argv[],
           const char * version_str, int jf_depth)
{
    bool seek_seen = false;
    bool skip_seen = false;
    char str[STR_SZ];
    char * key;
    char * buf;
    char * cp;
    int k, n, keylen, res, orig_strlen;
    int64_t i64;
    struct flags_t * ifp = op->iflagp;
    struct flags_t * ofp = op->oflagp;

    for (k = 1; k < argc; ++k) {
        if (argv[k]) {
            strncpy(str, argv[k], STR_SZ);
            str[STR_SZ - 1] = '\0';
        } else
            continue;
        orig_strlen = strlen(str);
        // replace '=' with null and set buf pointer to following char
        for (key = str, buf = key; *buf && ('=' != *buf); )
            ++buf;
        if (*buf)
            *buf++ = '\0';
        // If ('\0' == *buf) then there is no '=' in the option/argument
        //   or '=' is the trailing character.
        keylen = (int)strlen(key);
        // check for option names, in alphabetical order
        // check important ones for duplication
        if (0 == strcmp(key, "bpt")) {
            if (op->bpt_given) {
                pr2serr("Only one bpt= argument please\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            cp = strchr(buf, ',');
            if (cp)
                *cp = '\0';
            if ((n = sg_get_num(buf)) < 0) {
                pr2serr("bad BPT argument to 'bpt='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (n > 0) {
                op->bpt_i = n;
                op->bpt_given = true;
            }
            if (cp) {
                n = sg_get_num(cp + 1);
                if (n < 0) {
                    pr2serr("bad OBPC argument to 'bpt='\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
                op->obpch = n;
            }
        } else if (0 == strcmp(key, "bs")) {
            n = sg_get_num(buf);
            if (n < 0) {
                pr2serr("bad argument to 'bs='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (op->bs_given) {
                pr2serr("second 'bs=' option given, dangerous\n");
                return SG_LIB_SYNTAX_ERROR;
            } else
                op->bs_given = true;
            if ((op->ibs_given) || (op->obs_given)) {
                pr2serr("'bs=' option cannot be combined with "
                        "'ibs=' or 'obs='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->ibs_lb = n;
            op->ibs_pi = n;
            op->obs_lb = n;
            op->obs_pi = n;
        } else if (0 == strcmp(key, "cbs"))
            pr2serr("the cbs= option is ignored\n");
        else if (0 == strcmp(key, "cdbsz")) {
            ifp->cdbsz = sg_get_num(buf);
            ofp->cdbsz = ifp->cdbsz;
            op->cdbsz_given = true;
        } else if (0 == strcmp(key, "coe")) {
            ifp->coe = sg_get_num(buf);
            ofp->coe = ifp->coe;
        } else if ((0 == strcmp(key, "coe_limit")) ||
                   (0 == strcmp(key, "coe-limit"))) {
            op->coe_limit = sg_get_num(buf);
            if (-1 == op->coe_limit) {
                pr2serr("bad argument to 'coe_limit='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "conv")) {
            if (conv_process(buf, ifp, ofp)) {
                pr2serr("bad argument to 'conv='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "count")) {
            if (op->count_given) {
                pr2serr("second 'count=' argument detected, only one "
                        "please\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (0 != strcmp("-1", buf)) {
                op->dd_count = sg_get_llnum(buf);
                if (-1LL == op->dd_count) {
                    pr2serr("bad argument to 'count='\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
            }   /* 'count=-1' is accepted, means calculate count */
            op->count_given = true;
        } else if (0 == strcmp(key, "delay")) {
            cp = strchr(buf, ',');
            if (cp)
                *cp = '\0';
            n = sg_get_num(buf);
            if (n < 0) {
                pr2serr("bad MS argument to 'delay='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->delay = n;
            if (cp) {
                n = sg_get_num(cp + 1);
                if (n < 0) {
                    pr2serr("bad W_MS argument to 'delay='\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
                op->wdelay = n;
            }
        } else if (0 == strcmp(key, "ibs")) {
            n = sg_get_num(buf);
            if (n < 0) {
                pr2serr("bad argument to 'ibs='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (op->bs_given) {
                pr2serr("'ibs=' option cannot be combined with "
                        "'bs='; try 'obs=' instead\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->ibs_given = true;
            op->ibs_lb = n;
            op->ibs_pi = n;
        } else if ((0 == strcmp(key, "id_usage")) ||
                   (0 == strcmp(key, "id-usage"))) {
            if (isdigit(buf[0])) {
                n = sg_get_num(buf);
                if (n < 0) {
                    pr2serr("bad numeric argument to 'id_usage='\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
                op->id_usage = n;
            } else if (! strncmp(buf, "hold", 4))
                op->id_usage = 0;
            else if (! strncmp(buf, "discard", 7))
                op->id_usage = 2;
            else if (! strncmp(buf, "disable", 7))
                op->id_usage = 3;
            else {
                pr2serr("bad argument to 'id_usage='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (strcmp(key, "if") == 0) {
            if ('\0' != op->idip->fn[0]) {
                pr2serr("Second IFILE argument??\n");
                return SG_LIB_SYNTAX_ERROR;
            } else if (0 == strlen(buf)) {
                pr2serr("expected if=IFILE but no IFILE argument\n");
                return SG_LIB_SYNTAX_ERROR;
            } else
                strncpy(op->idip->fn, buf, INOUTF_SZ - 1);
        } else if (0 == strcmp(key, "iflag")) {
            if (flags_process(buf, ifp)) {
                pr2serr("bad argument to 'iflag='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "intio"))
            op->interrupt_io = sg_get_num(buf);
        else if (0 == strcmp(key, "iseek")) {
            if (skip_seen) {
                pr2serr("Only one skip= (or iseek=) argument please\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            res = do_skip(op, key, buf);
            if (res)
                return res;
            skip_seen = true;
        } else if (0 == strcmp(key, "ito")) {
            n = sg_get_num(buf);
            if (-1 == n) {
                pr2serr("bad argument to 'ito='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->inactivity_to = n;
        } else if ((0 == strcmp(key, "list_id")) ||
                   (0 == strcmp(key, "list-id"))) {
            i64 = sg_get_llnum(buf);
            if (-1 == i64) {
                pr2serr("bad argument to 'list_id='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (i64 > UINT32_MAX) {
                pr2serr("argument to 'list_id=' too big for 32 bits\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->list_id = (uint32_t)i64;
            op->list_id_given = true;
        } else if (0 == strcmp(key, "obs")) {
            n = sg_get_num(buf);
            if (n < 0) {
                pr2serr("bad argument to 'obs='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (op->bs_given) {
                pr2serr("'obs=' option cannot be combined with "
                        "'bs='; try 'ibs=' instead\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->obs_given = true;
            op->obs_lb = n;
            op->obs_pi = n;
        } else if (strcmp(key, "of") == 0) {
            if ('\0' != op->odip->fn[0]) {
                pr2serr("Second OFILE argument??\n");
                return SG_LIB_SYNTAX_ERROR;
            } else if (0 == strlen(buf)) {
                pr2serr("expected of=OFILE but no OFILE argument\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            strncpy(op->odip->fn, buf, INOUTF_SZ - 1);
            op->outf_given = true;
        } else if (strcmp(key, "of2") == 0) {
            if ('\0' != op->o2dip->fn[0]) {
                pr2serr("Second OFILE2 argument??\n");
                return SG_LIB_SYNTAX_ERROR;
            } else
                strncpy(op->o2dip->fn, buf, INOUTF_SZ - 1);
        } else if (0 == strcmp(key, "oflag")) {
            if (flags_process(buf, ofp)) {
                pr2serr("bad argument to 'oflag='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "oseek")) {
            if (seek_seen) {
                pr2serr("Only one seek= (or oseek=) argument please\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            res = do_seek(op, key, buf);
            if (res)
                return res;
            seek_seen = true;
        } else if (0 == strcmp(key, "prio")) {
            n = sg_get_num(buf);
            if ((n < 0) || (n > 7)) {
                pr2serr("bad argument to 'prio=' (max: 7)\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->prio = n;
        } else if (0 == strcmp(key, "protect")) {
            cp = strchr(buf, ',');
            if (cp)
                *cp = '\0';
            n = sg_get_num(buf);
            if ((n < 0) || (n > 7)) {
                pr2serr("bad RDP argument to 'protect='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->rdprotect = n;
            if (cp) {
                n = sg_get_num(cp + 1);
                if ((n < 0) || (n > 7)) {
                    pr2serr("bad WRP argument to 'protect='\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
                op->wrprotect = n;
            }
        } else if (0 == strcmp(key, "retries")) {
            ifp->retries = sg_get_num(buf);
            ofp->retries = ifp->retries;
            if (-1 == ifp->retries) {
                pr2serr("bad argument to 'retries='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "rtf")) {
            if (op->rtf[0]) {
                pr2serr("Can only use rtf=RTF once for ROD Token filename\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if ((NULL == buf) || (strlen(buf) < 1)) {
                pr2serr("rtf=RTF requires an non-empty filename for RTF\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            strncpy(op->rtf, buf, INOUTF_SZ - 1);
        } else if (0 == strcmp(key, "rtype")) {
            if ((0 == strncmp("pit-def", buf, 7)) ||
                (0 == strncmp("pit_def", buf, 7)))
                op->rod_type = RODT_PIT_DEF;
            else if ((0 == strncmp("pit-vuln", buf, 8)) ||
                     (0 == strncmp("pit_vuln", buf, 8)))
                op->rod_type = RODT_PIT_VULN;
            else if ((0 == strncmp("pit-pers", buf, 8)) ||
                     (0 == strncmp("pit_pers", buf, 8)))
                op->rod_type = RODT_PIT_PERS;
            else if ((0 == strncmp("pit-any", buf, 7)) ||
                     (0 == strncmp("pit_any", buf, 7)))
                op->rod_type = RODT_PIT_ANY;
            else if (0 == strncmp("zero", buf, 4))
                op->rod_type = RODT_BLK_ZERO;
            else {
                i64 = sg_get_llnum(buf);
                if (-1 == i64) {
                    pr2serr("bad argument to 'rtype='; can give (hex) "
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
            op->rod_type_given = true;
        } else if (0 == strcmp(key, "seek")) {
            if (seek_seen) {
                pr2serr("Only one seek= (or oseek=) argument please\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            res = do_seek(op, key, buf);
            if (res)
                return res;
            seek_seen = true;
        } else if (0 == strcmp(key, "skip")) {
            if (skip_seen) {
                pr2serr("Only one skip= (or iseek=) argument please\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            res = do_skip(op, key, buf);
            if (res)
                return res;
            skip_seen = true;
        } else if (0 == strcmp(key, "status")) {
            if (0 == strncmp(buf, "null", 4))
                ;
            else if (0 == strncmp(buf, "noxfer", 6))
                op->do_time = false;
            else if (0 == strncmp(buf, "none", 4)) {
                op->status_none = true;
                op->do_time = false;
            } else if (0 == strncmp(buf, "progress", 8)) {
                ++op->progress;
                if (0 == strncmp(buf, "progress,progress", 17)) {
                    ++op->progress;
                    if (0 == strncmp(buf, "progress,progress,progress", 25))
                        ++op->progress;
                }
            } else {
                pr2serr("'status=' expects 'none', 'noxfer' or 'null'\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "to")) {
            op->timeout_xcopy = sg_get_num(buf);
            if (-1 == op->timeout_xcopy) {
                pr2serr("bad argument to 'to='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strncmp(key, "verb", 4)) {
            op->verbose = sg_get_num(buf);
            if ((-1 == op->verbose) && ('-' != buf[0])) {
                pr2serr("bad argument to 'verbose='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (op->verbose < 0) {
                op->quiet = true;
                op->verbose = 0;
            }
        }
        /* look for long options that start with '--' */
        else if (0 == strncmp(key, "--dry-run", 9))
            ++op->dry_run;
        else if (0 == strncmp(key, "--dry_run", 9))
            ++op->dry_run;
        else if (0 == strncmp(key, "--help", 6))
            ++op->do_help;
        else if (0 == strncmp(key, "--job", 5)) {
            if (strlen(buf) > 0) {
                res = jf_process(op, buf, version_str, jf_depth);
                if (res)
                    return res;
            } else {
                pr2serr("'job=' expects a file name\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strncmp(key, "--odx", 5))
            op->has_odx = true;
        else if (0 == strncmp(key, "--quiet", 7))
            op->quiet = true;
        else if (0 == strncmp(key, "--verb", 6))
            ++op->verbose;
        else if (0 == strncmp(key, "--vers", 6)) {
            pr2serr("%s\n", version_str);
            return -1;          /* want early exit */
        }
#ifdef SG_LIB_WIN32
        else if (0 == strncmp(key, "--wscan", 7))
            ++op->wscan;
#endif
        else if (0 == strncmp(key, "--xcopy", 7))
            op->has_xcopy = true;
        /* look for short options that start with a single '-', they can be
         * concaternated (e.g. '-vvvx') */
        else if ((keylen > 1) && ('-' == key[0]) && ('-' != key[1])) {
            res = 0;
            n = num_chs_in_str(key + 1, keylen - 1, 'd');
            op->dry_run += n;
            res += n;
            n = num_chs_in_str(key + 1, keylen - 1, 'h');
            op->do_help += n;
            res += n;
            n = num_chs_in_str(key + 1, keylen - 1, 'o');
            op->has_odx = (n > 0);
            res += n;
            n = num_chs_in_str(key + 1, keylen - 1, 'q');
            op->quiet = (n > 0);
            res += n;
            n = num_chs_in_str(key + 1, keylen - 1, 'v');
            op->verbose += n;
            res += n;
            if (num_chs_in_str(key + 1, keylen - 1, 'V')) {
                pr2serr("%s\n", version_str);
                return -1;
            }
#ifdef SG_LIB_WIN32
            n = num_chs_in_str(key + 1, keylen - 1, 'w');
            op->wscan += n;
            res += n;
#endif
            n = num_chs_in_str(key + 1, keylen - 1, 'x');
            op->has_xcopy = (n > 0);
            res += n;
            if (res < (keylen - 1)) {
                pr2serr("Unrecognised short option in '%s', try '--help'\n",
                        key);
                if (0 == op->do_help)
                    return -1;          /* early exit */
            }
        } else if (('\0' == *buf) && (orig_strlen == (int)strlen(str))) {
            res = jf_process(op, str, version_str, jf_depth);
            if (res)
                return res;
        } else {
            pr2serr("Unrecognized option '%s'\n", key);
            if (0 == jf_depth)
                pr2serr("For more information use '--help'\n");
            return SG_LIB_SYNTAX_ERROR;
        }
    }
    return cl_sanity_defaults(op);
}

