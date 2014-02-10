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

const char * ddptctl_version_str = "0.94 20140209 [svn: r257]";

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



static struct option long_options[] = {
        {"help", no_argument, 0, 'h'},
        // {"lba", required_argument, 0, 'l'},
        {"verbose", no_argument, 0, 'v'},
        {"version", no_argument, 0, 'V'},
        {0, 0, 0, 0},
};



static void
usage()
{
    pr2serr("Usage: "
          "sg_unmap [--anchor] [--grpnum=GN] [--help] [--in=FILE]\n"
          "                [--lba=LBA,LBA...] [--num=NUM,NUM...] "
          "[--timeout=TO]\n"
          "                [--verbose] [--version] DEVICE\n"
          "  where:\n"
          "    --anchor|-a          set anchor field in cdb\n"
          "    --grpnum=GN|-g GN    GN is group number field (def: 0)\n"
          "    --help|-h            print out usage message\n"
          "    --in=FILE|-I FILE    read LBA, NUM pairs from FILE (if "
          "FILE is '-'\n"
          "                         then stdin is read)\n"
          "    --lba=LBA,LBA...|-l LBA,LBA...    LBA is the logical block "
          "address\n"
          "                                      to start NUM unmaps\n"
          "    --num=NUM,NUM...|-n NUM,NUM...    NUM is number of logical "
          "blocks to\n"
          "                                      unmap starting at "
          "corresponding LBA\n"
          "    --timeout=TO|-t TO    command timeout (unit: seconds) "
          "(def: 60)\n"
          "    --verbose|-v         increase verbosity\n"
          "    --version|-V         print version string and exit\n\n"
          "Perform a SCSI UNMAP command. LBA, NUM and the values in FILE "
          "are assumed\n"
          "to be decimal. Use '0x' prefix or 'h' suffix for hex values.\n"
          "Example to unmap LBA 0x12345:\n"
          "    sg_unmap --lba=0x12345 --num=1 /dev/sdb\n"
          );
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
    int res, c, num, k, j;
    const char * device_name = NULL;
    int ret = 0;
    struct opts_t ops;
    struct flags_t iflag, oflag;
    struct dev_info_t ids, ods, o2ds;
    struct opts_t * op;

    state_init(&ops, &iflag, &oflag, &ids, &ods, &o2ds);
    op = &ops;

    while (1) {
        int option_index = 0;

        c = getopt_long(argc, argv, "hvV", long_options,
                        &option_index);
        if (c == -1)
            break;

        switch (c) {
        case 'h':
        case '?':
            usage();
            return 0;
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
        if (NULL == device_name) {
            device_name = argv[optind];
            ++optind;
        }
        if (optind < argc) {
            for (; optind < argc; ++optind)
                pr2serr("Unexpected extra argument: %s\n", argv[optind]);
            usage();
            return SG_LIB_SYNTAX_ERROR;
        }
    }
    if (NULL == device_name) {
        pr2serr("missing device name!\n");
        usage();
        return SG_LIB_SYNTAX_ERROR;
    }

    return ret;
}
