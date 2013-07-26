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

/* Borrow signal handling from dd (src/dd.c in coreutils-8.13) */
/* Use SA_NOCLDSTOP as a proxy for whether the sigaction machinery is
   present.  */
#ifndef SA_NOCLDSTOP
# define SA_NOCLDSTOP 0
# define sigprocmask(How, Set, Oset) /* empty */
# define sigset_t int
# if ! HAVE_SIGINTERRUPT
#  define siginterrupt(sig, flag) /* empty */
# endif
#endif

/* NonStop circa 2011 lacks SA_RESETHAND; see Bug#9076.  */
#ifndef SA_RESETHAND
# define SA_RESETHAND 0
#endif

#ifndef SIGINFO
# define SIGINFO SIGUSR1
#endif
/* end of borrow from dd signal handling defines */


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
#define MAX_SCSI_CDBSZ 32

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

#define REASON_TAPE_SHORT_READ 1024     /* leave_reason indication */

#ifndef SG_LIB_CAT_PROTECTION
#define SG_LIB_CAT_PROTECTION 0x40
#define SG_LIB_CAT_PROTECTION_WITH_INFO 0x41
#endif


/* One instance for arguments to iflag= , another instance for oflag= */
/* conv= arguments are mapped to flag arguments */
struct flags_t {
    int append;
    int cat;            /* xcopy related */
    int cdbsz;
    int coe;
    int dc;             /* xcopy related */
    int direct;
    int dpo;
    int errblk;
    int excl;
    int fdatasync;
    int flock;
    int force;
    int fsync;
    int fua;
    int fua_nv;
    int ignoreew;
    int pdt;
    int nocache;
    int nofm;
    int nopad;
    int norcap;
    int nowrite;
    int pad;
    int prealloc;
    int pt;     /* use pass-through to inject SCSI commands */
    int resume;
    int rarc;
    int retries;
    int self;
    int sparing;
    int sparse;
    int ssync;
    int strunc;
    int sync;
    int trunc;
    int wsame16;
    int xcopy;
};

/* command line options, statistics and other semi-static data */
/* The _given fields indicate whether option was given or is a default */
struct opts_t {
    /* command line related variables */
    int64_t skip;
    int64_t seek;
    int bs_given;       /* 1 implies bs= option given on command line */
    int ibs;
    int ibs_pi;    /* if (protect) ibs_pi = ibs+pi_len else ibs_pi=ibs */
    int ibs_given;
    int obs;
    int obs_pi;    /* if (protect) obs_pi = obs+pi_len else obs_pi=obs */
    int obs_given;
    int bpt_i;          /* blocks (of input) per transfer */
    int bpt_given;
    int obpc;
    int id_usage;       /* xcopy related, init to -1 */
    char inf[INOUTF_SZ];
    int in_type;
    int interrupt_io;   /* [intio=0|1] if 0, mask SIGINFO++ during IO */
    int list_id;        /* xcopy related */
    int list_id_given;  /* xcopy related */
    char outf[INOUTF_SZ];
    int outf_given;
    int out_type;
    char out2f[INOUTF_SZ];
    int out2_type;
    int out2fd;
    int prio;           /* xcopy related */
    int rdprotect;
    int rdprot_typ;     /* from RCAP(16) */
    int rdp_i_exp;      /* from RCAP(16) */
    int wrprotect;
    int wrprot_typ;     /* from RCAP(16) */
    int wrp_i_exp;      /* from RCAP(16) */
    int cdbsz_given;
    int coe_limit;
    int coe_count;
    int verbose;
    int quiet;
    struct flags_t * iflagp;
    struct flags_t * oflagp;
    /* working variables and statistics */
    int64_t dd_count;
    int64_t in_full;
    int64_t out_full;
    int64_t out_sparse;  /* used for sparse, sparing + trim */
    int64_t lowest_unrecovered;         /* on reads */
    int64_t highest_unrecovered;        /* on reads */
    int in_partial;
    int max_aborted;
    int max_uas;
    int out_partial;
    int out_sparse_active;
    int out_sparing_active;
    int out_sparse_partial;
    int out_trim_active;
    int recovered_errs;          /* on reads */
    int unrecovered_errs;        /* on reads */
    int wr_recovered_errs;
    int wr_unrecovered_errs;
    int do_help;
    int do_time;
    int has_xcopy;
    int xc_cat;
    int xc_dc;
    unsigned long xc_min_bytes;
    unsigned long xc_max_bytes;
    int status_none;
    int trim_errs;
    int read_tape_numbytes;
    int last_tape_read_len;  /* Length of previous tape read */
    unsigned int consec_same_len_reads;
    int num_retries;
    int sum_of_resids;
    int interrupted_retries;
    int err_to_report;
    int reading_fifo;
    int read1_or_transfer; /* 1 when of=/dev/null or similar */
    int ibs_hold;
    int out_type_hold;
    int infd;
    int outfd;
    struct sg_pt_base * if_ptvp;
    struct sg_pt_base * of_ptvp;
    unsigned char * zeros_buff;
#ifdef HAVE_POSIX_FADVISE
    off_t lowest_skip;
    off_t lowest_seek;
#endif
#if SA_NOCLDSTOP
    sigset_t caught_signals;
    sigset_t orig_mask;
#endif
#ifdef SG_LIB_WIN32
    int wscan;
    HANDLE ib_fh;
    HANDLE ob_fh;
#endif
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    int start_tm_valid;
    struct timespec start_tm;
#elif defined(HAVE_GETTIMEOFDAY)
    int start_tm_valid;
    struct timeval start_tm;
#endif
    FILE * errblk_fp;
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
    const char * name;
};

extern int pr2serr(const char * fmt, ...);

extern void * pt_construct_obj(void);
extern void pt_destruct_obj(void * vp);
extern int pt_open_if(struct opts_t * op);
extern int pt_open_of(struct opts_t * op);
extern void pt_close(int fd);
extern int pt_read_capacity(struct opts_t * op, int in0_out1,
                            int64_t * num_sect, int * sect_sz);
extern int pt_read(struct opts_t * op, int in0_out1, unsigned char * buff,
                   int blocks, int * blks_readp);
extern int pt_write(struct opts_t * op, unsigned char * buff, int blocks,
                    int64_t to_block);
extern int pt_write_same16(struct opts_t * op, unsigned char * buff, int bs,
                           int blocks, int64_t start_block);
extern void pt_sync_cache(int fd);

extern void put_errblk(uint64_t lba, struct opts_t * op);
extern void put_range_errblk(uint64_t lba, int num, struct opts_t * op);
extern void zero_coe_limit_count(struct opts_t * op);


#ifdef SG_LIB_WIN32
extern int dd_filetype(const char * fn, int verbose);
extern int get_blkdev_capacity(struct opts_t * optsp, int which_arg,
                               int64_t * num_sect, int * sect_sz);
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
extern int coe_process_eio(struct opts_t * op, int64_t skip);

extern int sg_do_wscan(char letter, int do_scan, int verb);

#ifdef SG_LIB_MINGW
/* Without this gives a warning about implicit declaration.
 * This stop the warning but if getpagesize() appears may need to
 * remove this declaration. */
extern int getpagesize(void);
#endif

#endif

#endif
