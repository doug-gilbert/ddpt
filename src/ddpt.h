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

#ifndef DDPT_H
#define DDPT_H

/* This is a C header file for the ddpt utility. See ddpt.c and ddpt.8
 * for more information.
 */

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <signal.h>
#include <sys/time.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef __cplusplus
extern "C" {
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
#define MAX_XC_BPT 65535     /* BPT maximum for xcopy(LID1) */
#define MAX_XC_BPT_POW2 32768  /* BPT maximum that is power of 2 */
#define DEF_SCSI_CDBSZ 10
#define MAX_SCSI_CDBSZ 32
#define DDPT_MAX_JF_DEPTH 4
#define DDPT_MAX_JF_LINES 1000
#define DDPT_MAX_JF_ARGS_PER_LINE 16
#define DDPT_COUNT_INDEFINITE (-1)

#define VPD_DEVICE_ID 0x83
#define VPD_3PARTY_COPY 0x8f

#define SENSE_BUFF_LEN 32       /* Arbitrary, could be larger */
#define READ_CAP_REPLY_LEN 8
#define RCAP16_REPLY_LEN 32

#define DEF_RW_TIMEOUT 60       /* 60 seconds for READ and WRITE */
#define WRITE_SAME16_TIMEOUT 180  /* 3 minutes */

#define DEF_GROUP_NUM 0
#define DEF_LID4_LID 257        /* just above the LID1 highest of 255 */
#define DEF_LID4_WR_LID 258

/* status=progress or status=progress,progress */
#define PROGRESS_TRIGGER_MS 120000      /* milliseconds: 2 minutes */
#define PROGRESS2_TRIGGER_MS 60000      /* milliseconds: 1 minute */

#ifdef SG_LIB_LINUX
#ifndef RAW_MAJOR
#define RAW_MAJOR 255   /*unlikey value */
#endif
#define DEV_NULL_MINOR_NUM 3
#endif

#define SG_LIB_FLOCK_ERR 90

/* File type categories */
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
#define FT_ALL_FF 512           /* iflag=ff so input will be 0xff bytes */

/* ODX type requested */
#define ODX_REQ_NONE 0          /* some other type of copy */
#define ODX_READ_INTO_RODS 1    /* POPULATE TOKENs (PTs): disk->rods */
#define ODX_WRITE_FROM_RODS 2   /* WRITE USING TOKENs (WUTs): rods->disk */
#define ODX_COPY 3              /* odx disk->disk or zero->disk */

/* ROD Types used by ODX */
#define RODT_CM_INTERNAL 0x0
#define RODT_ACCESS_ON_REF 0x10000
#define RODT_PIT_DEF 0x800000
#define RODT_PIT_VULN 0x800001
#define RODT_PIT_PERS 0x800002
#define RODT_PIT_ANY 0x80ffff
#define RODT_BLK_ZERO 0xffff0001

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

#define DELAY_COPY_SEGMENT 0
#define DELAY_WRITE 1

#define REASON_TAPE_SHORT_READ 1024     /* leave_reason indication */

/* Following used for sense_key=aborted_command, asc=0x10, ascq=* which
 * contains errors associated with protection fields */
#ifndef SG_LIB_CAT_PROTECTION
#define SG_LIB_CAT_PROTECTION 40
#define SG_LIB_CAT_PROTECTION_WITH_INFO 41
#endif

#define DDPT_CAT_PARAM_LST_LEN_ERR 50
#define DDPT_CAT_INVALID_FLD_IN_PARAM 51
#define DDPT_CAT_TOO_MANY_SEGS_IN_PARAM 52
#define DDPT_CAT_TARGET_UNDERRUN 53
#define DDPT_CAT_TARGET_OVERRUN 54
#define DDPT_CAT_OP_IN_PROGRESS 55
#define DDPT_CAT_INSUFF_RES_CREATE_ROD 56
#define DDPT_CAT_INSUFF_RES_CREATE_RODTOK 57
#define DDPT_CAT_CMDS_CLEARED_BY_DEV_SVR 58
#define DDPT_CAT_TOKOP_BASE 70        /* assume less than 20 above this */

#define XCOPY_TO_SRC "XCOPY_TO_SRC"
#define XCOPY_TO_DST "XCOPY_TO_DST"
#define DEF_XCOPY_SRC0_DST1 1
#define ODX_RTF_LEN "ODX_RTF_LEN"     /* append 8 byte ROD size to token */
#define DDPT_DEF_BS "DDPT_DEF_BS" /* default default block size: 512 bytes */

/* ODX: length field inside ROD Token constant, implies 512 byte ROD Token */
#define ODX_ROD_TOK_LEN_FLD 504       /* 0x1f8 */

#define DEF_ODX_POLL_DELAY_MS 500

/* In SPC-4 the cdb opcodes have more generic names */
#define THIRD_PARTY_COPY_OUT_CMD 0x83
#define THIRD_PARTY_COPY_IN_CMD 0x84

/* Third party copy IN (opcode 0x84) and OUT (opcode 0x83) command service
 * actions */
#define SA_XCOPY_LID1           0x0     /* OUT, originate */
#define SA_XCOPY_LID4           0x1     /* OUT, originate */
#define SA_POP_TOK              0x10    /* OUT, originate [PT] */
#define SA_WR_USING_TOK         0x11    /* OUT, originate [WUT] */
#define SA_COPY_ABORT           0x1C    /* OUT, abort */

#define SA_COPY_STATUS_LID1     0x0     /* IN, retrieve */
#define SA_COPY_DATA_LID1       0x1     /* IN, retrieve */
#define SA_COPY_OP_PARAMS       0x3     /* IN, retrieve */
#define SA_COPY_FAIL_DETAILS    0x4     /* IN, retrieve */
#define SA_COPY_STATUS_LID4     0x5     /* IN, retrieve [RCS] */
#define SA_COPY_DATA_LID4       0x6     /* IN, retrieve */
#define SA_ROD_TOK_INFO         0x7     /* IN, retrieve [RRTI] */
#define SA_ALL_ROD_TOKS         0x8     /* IN, retrieve */

#define MAX_FIXED_SGL_ELEMS 128         /* same for gl and sl; MS max is 64 */


struct scat_gath_elem {
    uint64_t lba;       /* of first block */
    uint32_t num;       /* of blocks */
};

struct block_rodtok_vpd {
    uint16_t max_range_desc;
    uint32_t max_inactivity_to;
    uint32_t def_inactivity_to;
    uint32_t max_tok_xfer_size;
    uint32_t optimal_xfer_count;
};

/* One instance for arguments to iflag= , another instance for oflag=
 * conv= arguments are mapped to flag arguments.
 * flags for classic dd on disks or files unless otherwise noted. */
struct flags_t {
    bool append;        /* open non-pt OF with O_APPEND flag */
    bool atomic;        /* for pt OF use WRITE ATOMIC instead of WRITE */
    bool block;         /* only for pt, non blocking open is default */
    bool cat;           /* xcopy(lid1) tape: strategy for inexact fit */
    bool coe;           /* continue on (read) error, supply zeros */
    bool dc;            /* xcopy(lid1): destination count */
    bool del_tkn;       /* xcopy(odx): delete token after operation */
    bool direct;        /* set O_DIRECT on non-pt open() */
    bool dpo;           /* 'disable page out' bits on READ and WRITE cdbs */
    bool errblk;        /* write unreadable LB addresses to errblk.txt */
    bool excl;          /* opens IF and OF with O_EXCL flag */
    bool fdatasync;     /* use fdatasync() system call on OF after xfer */
    bool ff;            /* iflag=ff makes input all 0xff bytes */
    bool flock;         /* linux: use flock(LOCK_EX | LOCK_NB) before xfer */
    bool fsync;         /* use fsync() system call on OF after xfer */
    bool fua;           /* force unit access on pt calls */
    bool fua_nv;        /* obsolete (sbc3r35l): fua_non_volatile on pt */
    bool ignoreew;      /* tape: ignore early warning */
    bool immed;         /* xcopy(odx): returns immediately from POPULATE
                         * TOKEN and WRITE USING TOKEN then poll */
    bool no_del_tkn;    /* xcopy(odx): don't delete token after xfer */
    bool nofm;          /* tape: no filemark on close */
    bool nopad;         /* tape: no pad on partial writes */
    bool norcap;        /* no READ CAPACITY calls on pt */
    bool nowrite;       /* don't write to OF */
    bool odx;           /* xcopy(LID4), sbc-3's POPULATE TOKEN++ */
    bool pad;           /* pad with zeros partial (trailing) pt writes; also
                         * xcopy(lid1) tape: strategy for inexact fit */
    bool prealloc;      /* use (posix_)fallocate() on output file before
                         *  transfer */
    bool prefer_rcs;    /* prefer Receive Copy Status command over RRTI */
    bool pt;            /* use pass-through to inject SCSI commands */
    bool rarc;          /* Set Rebuild Assist Recovery Control bit on READs */
    bool resume;        /* try to restart previously interrupted copy */
    bool rtf_len;       /* odx: append number of bytes ROD represents to end
                         * of RTF */
    bool self;          /* trim (zero segments) out of a file in place (i.e.
                         * with no copy) */
    bool sparing;       /* saves on writes by reading OF (and/or OF2) and if
                         * same as segment read from IF, move on (i.e. don't
                         * overwrite OF (and/or OF2) with same data */
    bool ssync;         /* for pt OF (or OF2) do a SCSI SYNCHRONIZE CACHE
                         * at end of transfer before close */
    bool strunc;        /* perform sparse copy on non-pt OF using the
                         * ftruncate() call to extend/truncate OF */
    bool sync;          /* open non-pt file with O_SYNC flag */
    bool trunc;         /* truncate non-pt OF to SEEK (typically 0 length)
                         * before start of copy */
    bool verify;        /* oflag with pt, turns WRITE into WRITE AND VERIFY */
    bool wsame16;       /* given trim or unmap then wsame16 is set. Trim/unmap
                         * done on pt using SCSI WRITE SAME(16) command */
    bool xcopy;         /* xcopy(LID1) requested */

    int bytchk;         /* set field (2 bit) in WRITE AND VERIFY */
    int cdbsz;          /* 6, 10, 12, 16 or 32 */
    int force;          /* overrides errors, 2: force harder */
    int nocache;        /* (1 & nocache): IF and/or OF; (2 & nocache): OF .
                         * OF, OF2: use posix_fadvise(POSIX_FADV_DONTNEED)
                         * IF: use posix_fadvise(POSIX_FADV_SEQUENTIAL) */
    int retries;        /* retry each failed READ or WRITE this number of
                         * times (def: 0 (for no retries)) */
    int sparse;         /* when > 0 skips writes on segments that are all
                         * zeros. When 1 writes last segment even if all
                         * zeros, if > 1 then will skip last segment as well
                         * if all zeros */
};

/* one instance per file/device: if, of and of2 */
struct dev_info_t {
    int d_type;         /* one of FT_* values */
    int d_type_hold;
    int fd;
#ifdef SG_LIB_WIN32
    HANDLE fh;
#endif
    int pdt;
    int prot_type;      /* from RCAP(16) or 0 */
    int p_i_exp;        /* protection intervals exponent */
    uint32_t xc_min_bytes;
    uint32_t xc_max_bytes;
    char fn[INOUTF_SZ];
    struct block_rodtok_vpd * odxp;
    uint8_t * free_odxp;
    struct sg_pt_base * ptvp;
};

/* command line options plus most other state variables */
/* The _given fields indicate whether option was given or is a default */
struct opts_t {
    bool bpt_given;     /* true implies bpt= option given on command line */
    bool bs_given;
    bool cdbsz_given;
    bool count_given;
    bool do_time;       /* default true, set false by --status=none */
    bool has_odx;       /* --odx: equivalent to iflag=odx or oflag=odx */
    bool has_xcopy;     /* --xcopy (LID1): iflag=xcopy or oflag=xcopy */
    bool ibs_given;
    bool interrupt_io;  /* [intio=0|1] if false, mask SIGINFO++ during IO */
    bool list_id_given;
    bool obs_given;
    bool o_readonly;
    bool out_sparing_active;
    bool out_sparse_active;
    bool out_trim_active;
    bool outf_given;
    bool quiet;         /* set true when verbose=-1 (or any negative int) */
    bool reading_fifo;
    bool read1_or_transfer;     /* true when of=/dev/null or similar */
    bool rod_type_given;
    bool rtf_append;            /* if rtf is regular file: open(O_APPEND) */
    bool rtf_len_add;           /* append 64 bit ROD byte size to token */
    bool status_none;           /* status=none given */
    bool subsequent_wdelay;     /* so no delay before first write */
    bool xc_cat;
    bool xc_dc;
    /* command line related variables */
    int delay;          /* intra copy segment delay in milliseconds */
    int wdelay;         /* delay prior to each write in copy segment */
    int ibs;
    int ibs_pi;    /* if (protect) ibs_pi = ibs+pi_len else ibs_pi=ibs */
    int obs;
    int obs_pi;    /* if (protect) obs_pi = obs+pi_len else obs_pi=obs */
    int bpt_i;          /* blocks (of input) per transfer */
    int obpch;          /* output blocks per check, granularity of sparse,
                         * sparing and trim checks for zeros */
    int id_usage;       /* xcopy(LID1) List identifier usage, init to -1 */
    int prio;           /* xcopy(LID1) related */
    int rdprotect;
    int wrprotect;
    int coe_limit;
    int coe_count;
    int progress;       /* status=progress */
    int verbose;
    int do_help;
    int odx_request;    /* ODX_REQ_NONE==0 for no ODX */
    int timeout_xcopy;          /* xcopy(LID1) and ODX */
    int in_sgl_elems;           /* xcopy, odx */
    int out_sgl_elems;          /* xcopy, odx */
    int rtf_fd;                 /* ODX: rtf's file descriptor (init: -1) */
    uint32_t inactivity_to;     /* ODX: timeout in seconds */
    uint32_t list_id;           /* xcopy(LID1) and odx related */
    uint32_t rod_type;          /* ODX: ROD type */
    int64_t offset_in_rod;      /* ODX: units are obs bytes */
    int64_t skip;
    int64_t seek;
    /* working variables and statistics */
    int64_t dd_count;   /* -1 for not specified, 0 for no blocks to copy */
                        /* after copy/read starts, decrements to 0 */
    int64_t dd_count_start;     /* dd_count prior to start of copy/read */
    int64_t in_full;    /* full blocks read from IFILE so far */
    int64_t out_full;   /* full blocks written to OFILE so far */
    int64_t out_sparse; /* used for sparse, sparing + trim */
    int64_t lowest_unrecovered;         /* on reads */
    int64_t highest_unrecovered;        /* on reads */
    int64_t num_xcopy;                  /* xcopy(LID1) */
    int in_partial;
    int max_aborted;
    int max_uas;
    int out_partial;
    int out_sparse_partial;
    int recovered_errs;          /* on reads */
    int unrecovered_errs;        /* on reads */
    int wr_recovered_errs;
    int wr_unrecovered_errs;
    int trim_errs;
    int read_tape_numbytes;
    int last_tape_read_len;  /* Length of previous tape read */
    unsigned int consec_same_len_reads;
    int num_retries;
    int sum_of_resids;
    int interrupted_retries;
    int io_eagains;
    int err_to_report;
    int ibs_hold;
    FILE * errblk_fp;
    struct scat_gath_elem * in_sgl;     /* xcopy, odx: alternative to skip=
                                         * and count= */
    struct scat_gath_elem * out_sgl;    /* xcopy, odx: alternative to seek=
                                         * and count= */
    struct flags_t * iflagp;
    struct dev_info_t * idip;
    struct flags_t * oflagp;
    struct dev_info_t * odip;
    struct dev_info_t * o2dip;
    uint8_t * wrkPos;
    uint8_t * free_wrkPos;
    uint8_t * wrkPos2;
    uint8_t * free_wrkPos2;
    uint8_t * zeros_buff;
    uint8_t * free_zeros_buff;
    char rtf[INOUTF_SZ];        /* ODX: ROD token filename */
#ifdef SG_LIB_WIN32
    int wscan;          /* only used on Windows, for scanning devices */
#endif
#ifdef HAVE_POSIX_FADVISE
    off_t lowest_skip;
    off_t lowest_seek;
#endif
#if SA_NOCLDSTOP
    sigset_t caught_signals;
    sigset_t orig_mask;
#endif
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    bool start_tm_valid;
    struct timespec start_tm;
#elif defined(HAVE_GETTIMEOFDAY)
    bool start_tm_valid;
    struct timeval start_tm;
#endif
};

/* state of working variables within do_copy() */
/* permits do_copy() to be broken up into lots of helpers */
struct cp_state_t {
    bool leave_after_write;
    int icbpt;
    int ocbpt;
    int bytes_read;
    int bytes_of;
    int bytes_of2;
    int leave_reason;   /* ==0 for no error (e.g. EOF) */
    int partial_write_bytes;
    int64_t if_filepos;
    int64_t of_filepos;
};

struct val_str_t {
    int num;
    const char * name;
};

/* This data is extracted from the response of the Receive ROD Token
 * Information (RRTI) command and the Receive Copy Status (RCS) command. */
struct rrti_resp_t {
    uint8_t for_sa;     /* response to service action */
    uint8_t cstat;      /* copy operation status */
    uint8_t xc_cstatus; /* extended copy completion status */
    uint8_t sense_len;  /* (parameter data, actual) sense data length */
    uint32_t esu_del;   /* estimated status update delay (ms) */
    uint64_t tc;        /* transfer count (blocks) */
    /* Prior to this point response is in common with the RCS command */
    uint32_t rt_len;    /* might differ from 512, 0 if no ROD token */
    uint8_t rod_tok[512]; /* (perhaps truncate to) ODX ROD Token */
};

struct sg_simple_inquiry_resp;


/* Functions declared below are shared by different compilation units */

/* defined in ddpt.c */
/* No global function defined in ddpt.c apart from main() */

/* defined in ddpt_com.c */
void sleep_ms(int millisecs);
void state_init(struct opts_t * op, struct flags_t * ifp,
                struct flags_t * ofp, struct dev_info_t * idip,
                struct dev_info_t * odip, struct dev_info_t * o2dip);
void print_stats(const char * str, struct opts_t * op, int who);
int dd_filetype(const char * filename, int verbose);
char * dd_filetype_str(int ft, char * buff, int max_bufflen,
                       const char * fname);
void calc_duration_init(struct opts_t * op);
void calc_duration_throughput(const char * leadin, bool contin,
                              struct opts_t * op);
void print_blk_sizes(const char * fname, const char * access_typ,
                     int64_t num_blks, int blk_sz, bool to_stderr);
void zero_coe_limit_count(struct opts_t * op);
int get_blkdev_capacity(struct opts_t * op, int which_arg,
                        int64_t * num_blks, int * blk_sz);
void errblk_open(struct opts_t * op);
void errblk_put(uint64_t lba, struct opts_t * op);
void errblk_put_range(uint64_t lba, int num, struct opts_t * op);
void errblk_close(struct opts_t * op);
#ifdef SG_LIB_LINUX
void print_tape_summary(struct opts_t * op, int res, const char * str);
void print_tape_pos(const char * prefix, const char * postfix,
                    struct opts_t * op);
#endif
void install_signal_handlers(struct opts_t * op);
void signals_process_delay(struct opts_t * op, int delay_type);
void decode_designation_descriptor(const uint8_t * ucp, int len_less_4,
                                   bool to_stderr, int verb);
int coe_process_eio(struct opts_t * op, int64_t skip);
char * rod_type_str(uint32_t rt, char * b, int b_mlen);
char * rt_cm_id_str(const uint8_t * rtp, int rt_len, char * b,
                    int b_mlen);
void print_exit_status_msg(const char * prefix, int exit_stat,
                           bool to_stderr);
int cl_to_sgl(const char * inp, struct scat_gath_elem * sgl_arr,
              int * arr_len, int max_arr_len);
int file_to_sgl(const char * file_name, struct scat_gath_elem * sgl_arr,
                int * arr_len, int max_arr_len);

/* defined in ddpt_pt.c */
void * pt_construct_obj(void);
void pt_destruct_obj(void * vp);
int pt_open_if(struct opts_t * op, struct sg_simple_inquiry_resp * sirp);
int pt_open_of(struct opts_t * op, struct sg_simple_inquiry_resp * sirp);
void pt_close(int fd);
int pt_read_capacity(struct opts_t * op, bool in0_out1, int64_t * num_blks,
                     int * blk_sz);
int pt_read(struct opts_t * op, bool in0_out1, uint8_t * buff,
            int blocks, int * blks_readp);
int pt_write(struct opts_t * op, const uint8_t * buff, int blocks,
             int64_t to_block);
int pt_write_same16(struct opts_t * op, const uint8_t * buff, int bs,
                    int blocks, int64_t start_block);
void pt_sync_cache(int fd);
int pt_3party_copy_out(int sg_fd, int sa, uint32_t list_id, int group_num,
                       int timeout_secs, void * paramp, int param_len,
                       bool noisy, int verbose, int err_vb);
int pt_3party_copy_in(int sg_fd, int sa, uint32_t list_id, int timeout_secs,
                      void * resp, int mx_resp_len, bool noisy, int verbose,
                      int err_vb);

/* defined in ddpt_xcopy.c */
int open_rtf(struct opts_t * op);
const char * cpy_op_status_str(int cos, char * b, int blen);
uint64_t count_sgl_blocks(const struct scat_gath_elem * sglp, int elems);
int print_3pc_vpd(struct opts_t * op, bool to_stderr);
int do_xcopy_lid1(struct opts_t * op);
int do_pop_tok(struct opts_t * op, uint64_t blk_off, uint32_t num_blks,
               bool walk_list_id, int vb_a);
int do_rrti(struct opts_t * op, bool in0_out1, struct rrti_resp_t * rrp,
            int verb);
int do_rcs(struct opts_t * op, bool in0_out1, struct rrti_resp_t * rrp,
           int verb);
void get_local_rod_tok(uint8_t * tokp, int max_tok_len);
int process_after_poptok(struct opts_t * op, uint64_t * tcp, int vb_a);
int do_wut(struct opts_t * op, uint8_t * tokp, uint64_t blk_off,
           uint32_t num_blks, uint64_t oir, bool more_left, bool walk_list_id,
           int vb_a);
int process_after_wut(struct opts_t * op, uint64_t * tcp, int vb_a);
int do_odx(struct opts_t * op);

/* defined in ddpt_cl.c */
int cl_process(struct opts_t * op, int argc, char * argv[],
               const char * version_str, int jf_depth);
void ddpt_usage(int help);


#ifdef SG_LIB_WIN32
/* defined in ddpt_win32.c */
int win32_dd_filetype(const char * fn, int verbose);
int win32_get_blkdev_capacity(struct opts_t * optsp, int which_arg,
                              int64_t * num_blks, int * blk_sz);
void win32_adjust_fns_pt(struct opts_t * optsp);
int win32_open_if(struct opts_t * optsp, int flags, int verbose);
int win32_open_of(struct opts_t * optsp, int flags, int verbose);
int win32_set_file_pos(struct opts_t * optsp, int if0_of1, int64_t pos,
                       int verbose);
int win32_block_read(struct opts_t * optsp, uint8_t * bp, int num_bytes,
                     int verbose);
int win32_block_read_from_of(struct opts_t * optsp, uint8_t * bp,
                             int num_bytes, int verbose);
int win32_block_write(struct opts_t * optsp, const uint8_t * bp,
                      int num_bytes, int verbose);
int win32_cp_read_block(struct opts_t * optsp, struct cp_state_t * csp,
                        uint8_t * wrkPos, int * ifull_extrap, int verbose);
void win32_sleep_ms(int millisecs);

int sg_do_wscan(char letter, int do_scan, int verb);
#ifndef HAVE_SYSCONF
size_t win32_pagesize(void);
#endif
#endif          /* SG_LIB_WIN32 */

#ifdef __cplusplus
}
#endif

#endif  /* DDPT_H guard against multiple includes */


