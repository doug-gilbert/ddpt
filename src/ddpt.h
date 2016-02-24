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
#define SA_POP_TOK              0x10    /* OUT, originate */
#define SA_WR_USING_TOK         0x11    /* OUT, originate */
#define SA_COPY_ABORT           0x1C    /* OUT, abort */
#define SA_COPY_STATUS_LID1     0x0     /* IN, retrieve */
#define SA_COPY_DATA_LID1       0x1     /* IN, retrieve */
#define SA_COPY_OP_PARAMS       0x3     /* IN, retrieve */
#define SA_COPY_FAIL_DETAILS    0x4     /* IN, retrieve */
#define SA_COPY_STATUS_LID4     0x5     /* IN, retrieve */
#define SA_COPY_DATA_LID4       0x6     /* IN, retrieve */
#define SA_ROD_TOK_INFO         0x7     /* IN, retrieve */
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
 * General or for disk unless otherwise marked. */
struct flags_t {
    int append;
    int atomic;
    int block;          /* only for pt, non blocking default */
    int bytchk;         /* set field in WRITE AND VERIFY */
    int cat;            /* xcopy(lid1) related */
    int cdbsz;
    int coe;
    int dc;             /* xcopy(lid1) related */
    int del_tkn;        /* xcopy(odx) related */
    int direct;
    int dpo;
    int errblk;
    int excl;
    int fdatasync;
    int ff;             /* iflag=ff makes input all 0xff bytes */
    int flock;
    int force;
    int fsync;
    int fua;
    int fua_nv;
    int ignoreew;       /* tape */
    int immed;          /* xcopy(odx) related */
    int nocache;
    int no_del_tkn;     /* xcopy(odx) related */
    int nofm;           /* tape */
    int nopad;
    int norcap;
    int nowrite;
    int odx;            /* xcopy(LID4), sbc-3's POPULATE TOKEN++ */
    int pad;            /* used for xcopy(lid1) or tape */
    int prealloc;
    int pt;             /* use pass-through to inject SCSI commands */
    int resume;
    int rarc;
    int retries;
    int rtf_len;
    int self;
    int sparing;
    int sparse;
    int ssync;
    int strunc;
    int sync;
    int trunc;
    int verify;         /* oflag with pt, turns WRITE into WRITE AND VERIFY */
    int wsame16;
    int xcopy;          /* xcopy(LID1) */
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
    unsigned long xc_min_bytes;
    unsigned long xc_max_bytes;
    char fn[INOUTF_SZ];
    struct block_rodtok_vpd * odxp;
    struct sg_pt_base * ptvp;
};

/* command line options and most other state */
/* The _given fields indicate whether option was given or is a default */
struct opts_t {
    /* command line related variables */
    int64_t skip;
    int64_t seek;
    int count_given;
    int bs_given;       /* 1 implies bs= option given on command line */
    int delay;          /* intra copy segment delay in milliseconds */
    int wdelay;         /* delay prior to each write in copy segment */
    int subsequent_wdelay;      /* so no delay before first write */
    int ibs;
    int ibs_pi;    /* if (protect) ibs_pi = ibs+pi_len else ibs_pi=ibs */
    int ibs_given;
    int obs;
    int obs_pi;    /* if (protect) obs_pi = obs+pi_len else obs_pi=obs */
    int obs_given;
    int bpt_i;          /* blocks (of input) per transfer */
    int bpt_given;
    int obpch;          /* output blocks per check, granularity of sparse,
                         * sparing and trim checks for zeros */
    int id_usage;       /* xcopy(LID1) List identifier usage, init to -1 */
    int interrupt_io;   /* [intio=0|1] if 0, mask SIGINFO++ during IO */
    uint32_t list_id;   /* xcopy(LID1) and odx related */
    int list_id_given;
    int outf_given;
    int prio;           /* xcopy(LID1) related */
    int rdprotect;
    int wrprotect;
    int cdbsz_given;
    int coe_limit;
    int coe_count;
    int verbose;
    int quiet;
    int do_help;
    int do_time;
    int has_xcopy;      /* --xcopy (LID1): iflag=xcopy or oflag=xcopy */
    int odx_request;    /* ODX_REQ_NONE==0 for no ODX */
    int has_odx;        /* --odx: equivalent to iflag=odx or oflag=odx */
    uint32_t inactivity_to;     /* ODX: timeout in seconds */
    uint32_t rod_type;          /* ODX: ROD type */
    int rod_type_given;
    int64_t offset_in_rod;      /* ODX: units are obs bytes */
    int timeout_xcopy;          /* xcopy(LID1) and ODX */
    int in_sgl_elems;           /* xcopy, odx */
    int out_sgl_elems;          /* xcopy, odx */
    int rtf_fd;                 /* ODX: rtf's file descriptor (init: -1) */
    int rtf_len_add;            /* append 64 bit ROD byte size to token */
    int rtf_append;             /* if rtf is regular file: open(O_APPEND) */
    char rtf[INOUTF_SZ];        /* ODX: ROD token filename */
    struct scat_gath_elem * in_sgl;     /* xcopy, odx: alternative to skip=
                                         * and count= */
    struct scat_gath_elem * out_sgl;    /* xcopy, odx: alternative to seek=
                                         * and count= */
    struct flags_t * iflagp;
    struct dev_info_t * idip;
    struct flags_t * oflagp;
    struct dev_info_t * odip;
    struct dev_info_t * o2dip;
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
    int out_sparse_active;
    int out_sparing_active;
    int out_sparse_partial;
    int out_trim_active;
    int recovered_errs;          /* on reads */
    int unrecovered_errs;        /* on reads */
    int wr_recovered_errs;
    int wr_unrecovered_errs;
    int xc_cat;
    int xc_dc;
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
    int o_readonly;
    unsigned char * wrkBuff;
    unsigned char * wrkPos;
    unsigned char * wrkBuff2;
    unsigned char * wrkPos2;
    unsigned char * zeros_buff;
#ifdef HAVE_POSIX_FADVISE
    off_t lowest_skip;
    off_t lowest_seek;
#endif
#if SA_NOCLDSTOP
    sigset_t caught_signals;
    sigset_t orig_mask;
#endif
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    int start_tm_valid;
    struct timespec start_tm;
#elif defined(HAVE_GETTIMEOFDAY)
    int start_tm_valid;
    struct timeval start_tm;
#endif
    FILE * errblk_fp;
    int wscan;  /* only used on Windows, for scanning devices */
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

struct val_str_t {
    int num;
    const char * name;
};

/* This data is from the parameter data found in data-in of RRTI command */
struct rrti_resp_t {
    uint8_t for_sa;     /* response to service action */
    uint8_t cstat;      /* copy operation status */
    uint8_t xc_cstatus; /* extended copy completion status */
    uint8_t sense_len;  /* (parameter data, actual) sense data length */
    uint32_t esu_del;   /* estimated status update delay (ms) */
    uint64_t tc;        /* transfer count (blocks) */
    uint32_t rt_len;    /* might differ from 512, 0 if no ROD token */
    unsigned char rod_tok[512]; /* (perhaps truncate to) ODX ROD Token */
};

struct sg_simple_inquiry_resp;


/* Functions declared below are shared by different compilation units */

/* defined in ddpt.c */
/* No global function defined in ddpt.c apart from main() */

/* defined in ddpt_com.c */
#ifdef __GNUC__
int pr2serr(const char * fmt, ...) __attribute__ ((format (printf, 1, 2)));
#else
int pr2serr(const char * fmt, ...);
#endif
void sleep_ms(int millisecs);
void state_init(struct opts_t * op, struct flags_t * ifp,
                struct flags_t * ofp, struct dev_info_t * idip,
                struct dev_info_t * odip, struct dev_info_t * o2dip);
void print_stats(const char * str, struct opts_t * op, int who);
int dd_filetype(const char * filename, int verbose);
char * dd_filetype_str(int ft, char * buff, int max_bufflen,
                       const char * fname);
void calc_duration_init(struct opts_t * op);
void calc_duration_throughput(const char * leadin, int contin,
                              struct opts_t * op);
void print_blk_sizes(const char * fname, const char * access_typ,
                     int64_t num_blks, int blk_sz, int to_stderr);
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
void decode_designation_descriptor(const unsigned char * ucp, int len_less_4,
                                   int to_stderr, int verb);
int coe_process_eio(struct opts_t * op, int64_t skip);
char * rod_type_str(uint32_t rt, char * b, int b_mlen);
char * rt_cm_id_str(const unsigned char * rtp, int rt_len, char * b,
                    int b_mlen);
void print_exit_status_msg(const char * prefix, int exit_stat, int to_stderr);
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
int pt_read_capacity(struct opts_t * op, int in0_out1, int64_t * num_blks,
                     int * blk_sz);
int pt_read(struct opts_t * op, int in0_out1, unsigned char * buff,
            int blocks, int * blks_readp);
int pt_write(struct opts_t * op, const unsigned char * buff, int blocks,
             int64_t to_block);
int pt_write_same16(struct opts_t * op, const unsigned char * buff, int bs,
                    int blocks, int64_t start_block);
void pt_sync_cache(int fd);
int pt_3party_copy_out(int sg_fd, int sa, uint32_t list_id, int group_num,
                       int timeout_secs, void * paramp, int param_len,
                       int noisy, int verbose, int err_vb);
int pt_3party_copy_in(int sg_fd, int sa, uint32_t list_id, int timeout_secs,
                      void * resp, int mx_resp_len, int noisy, int verbose,
                      int err_vb);

/* defined in ddpt_xcopy.c */
int open_rtf(struct opts_t * op);
const char * cpy_op_status_str(int cos, char * b, int blen);
uint64_t count_sgl_blocks(const struct scat_gath_elem * sglp, int elems);
int print_3pc_vpd(struct opts_t * op, int to_stderr);
int do_xcopy_lid1(struct opts_t * op);
int do_pop_tok(struct opts_t * op, uint64_t blk_off, uint32_t num_blks,
               int walk_list_id, int vb_a);
int do_rrti(struct opts_t * op, int in0_out1, struct rrti_resp_t * rrp,
            int verb);
void get_local_rod_tok(unsigned char * tokp, int max_tok_len);
int process_after_poptok(struct opts_t * op, uint64_t * tcp, int vb_a);
int do_wut(struct opts_t * op, unsigned char * tokp, uint64_t blk_off,
           uint32_t num_blks, uint64_t oir, int more_left, int walk_list_id,
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
int win32_block_read(struct opts_t * optsp, unsigned char * bp, int num_bytes,
                     int verbose);
int win32_block_read_from_of(struct opts_t * optsp, unsigned char * bp,
                             int num_bytes, int verbose);
int win32_block_write(struct opts_t * optsp, const unsigned char * bp,
                      int num_bytes, int verbose);
int win32_cp_read_block(struct opts_t * optsp, struct cp_state_t * csp,
                        unsigned char * wrkPos, int * ifull_extrap,
                        int verbose);
void win32_sleep_ms(int millisecs);

int sg_do_wscan(char letter, int do_scan, int verb);
#ifndef HAVE_SYSCONF
size_t win32_pagesize(void);
#endif
#endif          /* SG_LIB_WIN32 */

#ifdef __cplusplus
}
#endif

#endif
