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

#ifdef SG_LIB_WIN32
#include <windows.h>
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
#define DEF_BPT_LT8 8192     /* BPT when IBS < 8 */
#define DEF_BPT_LT64 1024    /* BPT when IBS < 64 */
#define DEF_BPT_LT1024 128   /* BPT when IBS < 1024 */
#define DEF_BPT_LT8192 16    /* BPT when IBS < 8192 */
#define DEF_BPT_LT32768 4    /* BPT when IBS < 32768 */
#define DEF_BPT_GE32768 1    /* BPT when IBS >= 32768 */
#define DEF_SCSI_CDBSZ 10
#define MAX_SCSI_CDBSZ 16

#define SENSE_BUFF_LEN 32       /* Arbitrary, could be larger */
#define READ_CAP_REPLY_LEN 8
#define RCAP16_REPLY_LEN 32

#define DEF_TIMEOUT 60000       /* 60,000 millisecs == 60 seconds */
#define WRITE_SAME16_TIMEOUT 180000  /* 3 minutes */

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

#define DDPT_ARG_IN 0
#define DDPT_ARG_OUT 1
#define DDPT_ARG_OUT2 2

#define MIN_RESERVED_SIZE 8192

#define MAX_UNIT_ATTENTIONS 10
#define MAX_ABORTED_CMDS 256

#define ERRBLK_SUPPORTED 1


struct flags_t {
    int append;
    int cdbsz;
    int coe;
    int direct;
    int dpo;
#ifdef ERRBLK_SUPPORTED
    int errblk;
#endif
    int excl;
    int flock;
    int force;
    int fua;
    int fua_nv;
    int pdt;
    int nocache;
    int norcap;
    int nowrite;
    int pt;
    int resume;
    int retries;
    int self;
    int sparing;
    int sparse;
    int ssync;
    int strunc;
    int sync;
    int trunc;
    int wsame16;
};

struct opts_t {
    int64_t skip;
    int64_t seek;
    int bs_given;
    int ibs;
    int ibs_given;
    int obs;
    int obs_given;
    int bpt_i;          /* blocks (of input) per transfer */
    int bpt_given;
    int obpc;
    char inf[INOUTF_SZ];
    int in_type;
    int infd;
    char outf[INOUTF_SZ];
    char out2f[INOUTF_SZ];
    int out_type;
    int outfd;
    int out2_type;
    int out2fd;
    int cdbsz_given;
    struct flags_t * iflagp;
    struct flags_t * oflagp;
#ifdef SG_LIB_WIN32
    HANDLE ib_fh;
    HANDLE ob_fh;
#endif
};

struct cp_state_t {
    int64_t if_filepos;
    int64_t of_filepos;
    int icbpt;
    int ocbpt;
    int bytes_read;
    int bytes_of;
    int bytes_of2;
};


#ifdef SG_LIB_WIN32
extern int dd_filetype(const char * fn);
extern int get_blkdev_capacity(struct opts_t * optsp, int which_arg,
                               int64_t * num_sect, int * sect_sz,
                               int verbose);
extern void win32_adjust_fns(struct opts_t * optsp);
extern int win32_open_if(struct opts_t * optsp, int verbose);
extern int win32_open_of(struct opts_t * optsp, int verbose);
extern int win32_set_file_pos(struct opts_t * optsp, int if0_of1,
                              int64_t pos, int verbose);
extern int win32_block_read(struct opts_t * optsp, unsigned char * bp,
                            int num_bytes, int verbose);
extern int win32_block_read_from_of(struct opts_t * optsp, unsigned char * bp,
                                    int num_bytes, int verbose);
extern int win32_block_write(struct opts_t * optsp, const unsigned char * bp,
                             int num_bytes, int verbose);

#ifdef SG_LIB_MINGW
/* Without this gives a warning about implicit declaration.
 * This stop the warning but if getpagesize() appears may need to
 * remove this declaration. */
extern int getpagesize(void);
#endif

#endif

#endif
