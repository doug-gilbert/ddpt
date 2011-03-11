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

/* File type categorizations */
#define FT_OTHER 1              /* unknown (unable to identify) */
#define FT_PT 2                 /* SCSI commands can be sent via a
                                   pass-through */
#define FT_REG 4                /* a normal (regular) file */
#define FT_DEV_NULL 8           /* either "/dev/null" or "." as filename */
#define FT_TAPE 16              /* tape style device */
#define FT_BLOCK 32             /* block device */
#define FT_FIFO 64              /* fifo (named or unnamed pipe (stdout)) */
#define FT_CHAR 128             /* char dev, doesn't fit another category */
#define FT_ERROR 256            /* couldn't "stat" file */

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
#define MAX_ABORTED_CMDS 16

#define ERRBLK_SUPPORTED 1

#define REASON_TAPE_SHORT_READ 1024     /* leave_reason indication */


/* One instance for arguments to iflag= , another instance for oflag= */
/* conv= arguments are mapped to flag arguments */
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
    int fdatasync;
    int flock;
    int force;
    int fsync;
    int fua;
    int fua_nv;
    int pdt;
    int nocache;
    int nopad;
    int norcap;
    int nowrite;
    int pad;
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

/* command line options */
/* The _given fields indicate whether option was given or is a default */
struct opts_t {
    int64_t skip;
    int64_t seek;
    int bs_given;       /* 1 implies bs= option given on command line */
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
    int outf_given;
    int out_type;
    int outfd;
    char out2f[INOUTF_SZ];
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

/* state of working variables within do_copy() */
/* permits do_copy() to be broken up into lots of helpers */
struct cp_state_t {
    int64_t if_filepos;
    int64_t of_filepos;
    int icbpt;
    int ocbpt;
    int bytes_read;
    int bytes_of;
    int bytes_of2;
    int leave_after_write;
    int leave_reason;   /* ==0 for no error (e.g. EOF) */
    int partial_write_bytes;
};

struct signum_name_t {
    int num;
    char * name;
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
extern int win32_cp_read_block(struct opts_t * optsp, struct cp_state_t * csp,
                               unsigned char * wrkPos, int * ifull_extrap,
                               int verbose);
extern int coe_process_eio(int64_t skip);
extern void zero_coe_limit_count(void);

extern int sg_do_wscan(char letter, int do_scan, int verb);

#ifdef SG_LIB_MINGW
/* Without this gives a warning about implicit declaration.
 * This stop the warning but if getpagesize() appears may need to
 * remove this declaration. */
extern int getpagesize(void);
#endif

#endif

#endif
