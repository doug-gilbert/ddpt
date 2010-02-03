#ifndef DDPT_H
#define DDPT_H

/* This is a C header file for the ddpt utility. See ddpt.c and ddpt.8
 * for more information.
 */

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef SG_LIB_FREEBSD
#ifndef SIGINFO
/* hack to undo hiding by _XOPEN_SOURCE and _GNU_SOURCE */
#define SIGINFO         29
#endif
#endif

#ifdef SG_LIB_WIN32
#ifdef SG_LIB_MINGW
#define SIGPIPE 13
#define SIGQUIT 3
#define SIGUSR1 25
#endif
#endif

#define ME "ddpt: "


#define STR_SZ 1024
#define INOUTF_SZ 512
#define EBUFF_SZ 512

#define DEF_BLOCK_SIZE 512
#define DEF_BLOCKS_PER_TRANSFER 128     /* of input */
#define DEF_BLOCKS_PER_2048TRANSFER 32
#define DEF_SCSI_CDBSZ 10
#define MAX_SCSI_CDBSZ 16

#define DEF_MODE_CDB_SZ 10
#define DEF_MODE_RESP_LEN 252
#define RW_ERR_RECOVERY_MP 1
#define CACHING_MP 8
#define CONTROL_MP 0xa

#define SENSE_BUFF_LEN 32       /* Arbitrary, could be larger */
#define READ_CAP_REPLY_LEN 8
#define RCAP16_REPLY_LEN 32

#define DEF_TIMEOUT 60000       /* 60,000 millisecs == 60 seconds */

#ifdef SG_LIB_LINUX
#ifndef RAW_MAJOR
#define RAW_MAJOR 255   /*unlikey value */
#endif
#define DEV_NULL_MINOR_NUM 3
#endif

#define SG_LIB_FLOCK_ERR 90

#define FT_OTHER 1              /* filetype is unknown (unexpected) */
#define FT_PT 2                 /* filetype is a device that SCSI */
                                /* commands can be sent via a pass-through */
#define FT_REG 4                /* a normal (regular) file */
#define FT_DEV_NULL 8           /* either "/dev/null" or "." as filename */
#define FT_TAPE 16              /* filetype is tape style device */
#define FT_BLOCK 32             /* filetype is block device */
#define FT_FIFO 64              /* filetype is a fifo (name pipe) */
#define FT_ERROR 128            /* couldn't "stat" file */

/* If O_DIRECT or O_SYNC not supported then define harmlessly */
#ifndef O_DIRECT
#define O_DIRECT 0
#endif
#ifndef O_SYNC
#define O_SYNC 0
#endif
#ifndef O_NONBLOCK
#define O_NONBLOCK 0
#endif

#define MIN_RESERVED_SIZE 8192

#define MAX_UNIT_ATTENTIONS 10
#define MAX_ABORTED_CMDS 256


struct flags_t {
    int append;
    int cdbsz;
    int coe;
    int direct;
    int dpo;
    int excl;
    int flock;
    int fua;
    int fua_nv;
    int pdt;
    int nocache;
    int pt;
    int retries;
    int sparing;
    int sparse;
    int ssync;
    int sync;
};

struct opts_t {
    int64_t skip;
    int64_t seek;
    int64_t out2_off;
    int ibs;
    int obs;
    int bpt_i;          /* blocks (of input) per transfer */
    int bpt_given;
    char inf[INOUTF_SZ];
    int in_type;
    char outf[INOUTF_SZ];
    char out2f[INOUTF_SZ];
    int out_type;
    int out2_type;
    int cdbsz_given;
    struct flags_t * iflagp;
    struct flags_t * oflagp;
};


extern int dd_filetype(const char * fn);
extern void win32_adjust_fns(struct opts_t * optsp);

#endif
