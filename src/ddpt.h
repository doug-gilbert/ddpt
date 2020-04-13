/*
 * Copyright (c) 2008-2020, Douglas Gilbert
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
#define DEF_BPT_LT4 16384    /* BPT when IBS < 4 */
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
#define DDPT_LBA_INVALID DDPT_COUNT_INDEFINITE

#define VPD_DEVICE_ID 0x83
#define VPD_3PARTY_COPY 0x8f

#define SENSE_BUFF_LEN 64       /* Arbitrary, could be larger */
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
#define PROGRESS3_TRIGGER_MS 30000      /* milliseconds: 30 seconds */

#ifdef SG_LIB_LINUX
#define DEV_NULL_MINOR_NUM 3
#define DEV_ZERO_MINOR_NUM 5
#endif

#define SG_LIB_FLOCK_ERR 47

/* File/device type groups, (N.B. powers of 2), so file can be multiple */
#define FT_OTHER 1              /* unknown (unable to identify), default */
#define FT_PT 2                 /* SCSI commands can be sent via a
                                   pass-through */
#define FT_REG 4                /* a normal (regular) file */
#define FT_DEV_NULL 8           /* either "/dev/null" or "." as filename */
                                /* as input equivalent to /dev/zero */
#define FT_TAPE 16              /* tape style device */
#define FT_BLOCK 32             /* block device */
#define FT_FIFO 64              /* fifo (named or unnamed pipe (stdout)) */
#define FT_CHAR 128             /* char dev, doesn't fit another category */
#define FT_NVME 256             /* char or blk dev, NVMe device/SSD */
#define FT_ALL_FF 512           /* iflag=ff so input will be 0xff bytes */
#define FT_ERROR 0x800000       /* couldn't "stat" file */

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
#define RODT_PIT_COW 0x800003		/* added spc5r20 */
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

/* So ddpt does not hog all the CPU and IO resources, allow delays defined
 * by delay=MS[,W_MS] . Used in signals_process_delay(). */
#define DELAY_COPY_SEGMENT 0    /* after each copy segment (prior to next) */
#define DELAY_WRITE 1   /* prior to each write, may be muliple per segment */

/* cp_state_t::leave_reason indication */
#define DDPT_REASON_UNKNOWN -1
#define DDPT_REASON_EOF_ON_READ 0
#define DDPT_REASON_TAPE_SHORT_READ 1024


#define DDPT_CAT_PARAM_LST_LEN_ERR 100
#define DDPT_CAT_INVALID_FLD_IN_PARAM 101
#define DDPT_CAT_TOO_MANY_SEGS_IN_PARAM 102
#define DDPT_CAT_TARGET_UNDERRUN 103
#define DDPT_CAT_TARGET_OVERRUN 104
#define DDPT_CAT_OP_IN_PROGRESS 105
#define DDPT_CAT_INSUFF_RES_CREATE_ROD 106
#define DDPT_CAT_INSUFF_RES_CREATE_RODTOK 107
#define DDPT_CAT_CMDS_CLEARED_BY_DEV_SVR 108
#define DDPT_CAT_SEE_LEAVE_REASON 109   /* see cp_state_t::leave_reason */
#define DDPT_CAT_TOKOP_BASE 110   /* + ascq; Invalid token operation (0x23) */

#define XCOPY_TO_SRC "XCOPY_TO_SRC"
#define XCOPY_TO_DST "XCOPY_TO_DST"
#define DEF_XCOPY_SRC0_DST1 1
#define ODX_RTF_LEN "ODX_RTF_LEN"     /* append 8 byte ROD size to token */
#define DDPT_DEF_BS "DDPT_DEF_BS" /* default logical block size: 512 bytes */

#define DDPT_BIGGEST_CONTINUAL (1024LL * 1024 * 1024 * 64)

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

struct sg_pt_base;


struct val_str_t {
    int num;
    const char * name;
};

/* Sizing matches largest SCSI READ and WRITE commands plus those of Unix
 * read(2)s and write(2)s. User can give larger than 31 bit 'num's but they
 * are split into several consecutive elements. */
struct scat_gath_elem {
    uint64_t lba;       /* of start block */
    uint32_t num;       /* number of blocks from and including start block */
};

/* Old cylinder/head/sector addressing than can be manipulated by ddpt_sgl.
 * Assume 28 bit format as used by early ATA standards (EIDE + ATA-2) */
struct chs_t {
    uint16_t cyls;     /* 0 based; 16 bits, all bits used */
    uint8_t heads;     /* 0 based; 4 bits used, to 4 bits zero */
    uint8_t sects;     /* 1 based; all 8 bits used except 0 */
};

/* Used by ddpt_sgl */
struct sgl_stats {
    bool last_degen;
    bool not_mono_asc;  /* negated so zeroed default is mono_asc */
    bool not_mono_desc; /* LBAn+1 < (LBAn + NUMn - 1) while (NUMn > 0) */
    bool fragmented;    /* false if monotonic and all LBAs between highest
                         * and lowest are transferred once (i.e no holes) */
    int num_degen;
    int elems;  /* because of degen_mask may have < sge_p array elements */
    int64_t sum;        /* sum of number_of_blocks (NUM) fields in sgl */
    uint64_t lowest_lba;
    uint64_t highest_lba;
};

/* Used by ddpt_sgl */
struct cl_sgl_stats {
    struct sgl_stats a_stats;
    struct sgl_stats b_stats;
};


/* Iterator on a scatter gather list (which are arrays). IFILE and OFILE have
 * iterators, not OFILE2. The iterator is a "post increment" type starting
 * [0,0]. If the sgl is accessed in a linear fashion, after the last IO the
 * iterator will be at [<elems>, 0]. */
struct sgl_iter_t {
    bool extend_last;   /* hack for extending (sglp + elems - 1)->num */
    int elems;          /* elements in sglp array */
    int it_e_ind;       /* iterator element index into sglp array */
    int it_bk_off;      /* iterator: 0 <= it_blk_off < (sglp + i_e_ind)->num */
    /* underlying (what iterator points to) and filepos, if needed */
    struct scat_gath_elem * sglp;  /* start of scatter gather list array */
    int64_t filepos;    /* file 'pos' is a byte offset in reg/block file,
                         * origin 0, next byte to read */
};

/* Holds one scatter gather list and its associated metadata */
struct sgl_info_t {
    bool fragmented;    /* only valid if 'monotonic' is true. Thence true
                         * if gaps between first and last LBA, else false */
    bool monotonic;     /* LBAs grow larger in [0..elems). Allow LBAn ==
                         * LBAn+1 only if NUMn is zero */
    bool overlapping;   /* only valid if 'monotonic' is true. Means at least
                         * one LBA is in two sgl elements. Makes sgl
                         * sensitive to order its elements are processed */
    bool sum_hard;      /* 'num' in last element of 'sgl' is > 0 */
    int elems;          /* number of elements; when 0 'sgl' must be NULL */
    int64_t high_lba_p1;  /* highest LBA plus 1, next write from and above */
    int64_t lowest_lba; /* initialized to 0 */
    int64_t sum;        /* of all 'num' elements in 'sgl' */
    struct scat_gath_elem * sglp;  /* an array on heap [0..elems), owner */
};

/* Info from SCSI Third Party Copy VPD page (0x8f), descriptor 0 (Block
 * device ROD Limits). */
struct block_rodtok_vpd {
    uint16_t max_range_desc;
    uint32_t max_inactivity_to;
    uint32_t def_inactivity_to;
    uint32_t max_tok_xfer_size;
    uint32_t optimal_xfer_count;
};

/* one instance per file/device: if, of and of2 */
struct dev_info_t {
    bool limits_xfer;   /* size of this object is xfer limit */
    int d_type;         /* one of FT_* values */
    int d_type_hold;
    int fd;             /* Unix file descriptor corresponding to fn[] */
#ifdef SG_LIB_WIN32
    HANDLE fh;
#endif
    int pdt;
    int prot_type;      /* from RCAP(16) or 0 */
    int p_i_exp;        /* protection intervals (PIs) exponent */
    int bs_pi;          /* logical block size plus PI, if any */
    int ddpt_arg;       /* 1 of DDPT_ARG_IN, DDPT_ARG_OUT or DDPT_ARG_OUT2 */
    uint32_t xc_min_bytes;
    uint32_t xc_max_bytes;
    int64_t reg_sz;     /* regular file size in bytes, -1 --> no info */
    char fn[INOUTF_SZ]; /* file/device name */
    struct block_rodtok_vpd * odxp;
    uint8_t * free_odxp;
    struct sg_pt_base * ptvp;
    const char * dir_n; /* points to "in", "out" or "out2" */
};

struct cp_statistics_t {
    bool copied_from_working;
    bool prev_valid;    /* if delta throughput available */
    int64_t in_full;    /* full blocks read from IFILE so far */
    int64_t out_full;   /* full blocks written to OFILE so far */
    int64_t prev_count; /* full blocks written to OFILE so far */
    int64_t dd_count_start;     /* dd_count prior to start of copy/read */
    int64_t out_sparse; /* counter for sparse, sparing + trim */
    int in_partial;
    int out_partial;
    int out_sparse_partial;
    int recovered_errs;          /* on reads */
    int unrecovered_errs;        /* on reads */
    int wr_recovered_errs;
    int wr_unrecovered_errs;
    int trim_errs;
    int num_retries;
    int sum_of_resids;
    int interrupted_retries;
    int io_eagains;
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    struct timespec prev_tm;
#elif defined(HAVE_GETTIMEOFDAY)
    struct timeval prev_tm;
#endif
};

/* state of working variables within do_copy(). Note that logical block size
 * from the copy buffer's perspective is dip->bs_pi bytes. */
struct cp_state_t {
    bool leave_after_write;     /* partial read then EOF or error */
    bool in_soft;       /* keep going at end of in sgl */
    bool out_soft;      /* keep going at end of out sgl */
    bool reading;
    bool last_segment;
    int icbpt;          /* input, current blocks_per_transfer */
    int ocbpt;          /* output, current blocks_per_transfer */
    int bytes_read;     /* previous IO: into the working buffer */
    int bytes_of;       /* previous IO: bytes written to of */
    int bytes_of2;      /* previous IO: bytes written to of2 */
    int blks_xfer;  /* prev IO: blocks transferred (e.g. bytes_read/ibs) */
    int bytes_xfer; /* prev IO: bytes transferred, -1 -> block device */
    int leave_reason;   /* def: -1 (unknown); 0: EOF on read; else error */
    int rem_seg_bytes;  /* remaining valid bytes in buffer (after short) */
    int partial_write_bytes;
    int last_seg_wbytes; /* when ofile sz is limit; 0: no reduction */
    uint32_t cur_in_num;   /* current in number of blocks */
    uint32_t cur_out_num;  /* current out number of blocks */
    uint64_t cur_in_lba;   /* current in starts at this logical block */
    uint64_t prev_in_lba;  /* previous in leaves file "pointer" here */
    uint64_t cur_out_lba;  /* current out starts at this logical block */
    uint64_t prev_out_lba; /* previous out leaves file "pointer" here */
    struct cp_statistics_t stats;  /* running totals kept here */
    struct sgl_iter_t in_iter;
    struct sgl_iter_t out_iter;
    uint8_t * low_bp;      /* byte pointer to start of work segment */
    uint8_t * subseg_bp;   /* byte pointer to start of sub-segment */
    uint8_t * cur_bp;    /* pointer in sub-segment for start of current IO */
    int64_t * cur_countp;  /* points to in_full, out_full, or is NULL for
                            * don't count */
    const char * buf_name; /* optional buffer name for debugging */
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
    /* Prior to this point response * is in common with the RCS command */
    uint32_t rt_len;    /* might differ from 512, 0 if no ROD token */
    uint8_t rod_tok[512]; /* (perhaps truncate to) ODX ROD Token */
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
    bool v_verify;      /* oflag with pt, hidden flag, set by --verify */
    bool wsame16;       /* given trim or unmap then wsame16 is set. Trim/unmap
                         * done on pt using SCSI WRITE SAME(16) command */
    bool wstream;       /* oflag with pt, send WRITE STREAM(16) with list_id
                         * as Stream ID (valid range: 0x1 to 0xffff) */
    bool wverify;       /* oflag with pt, turns WRITE into WRITE AND VERIFY */
    bool xcopy;         /* xcopy(LID1) requested */
    bool zero;          /* iflag=00 makes input all 0x0 bytes */

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

/* Command line options plus some other state variables.
 * The _given fields indicate whether option was given or if true, the
 * corresponding value takes its default value when false. */
struct opts_t {
    bool bpt_given;     /* true when bpt= given, BPT --> bpt_i */
    bool bs_given;      /* bs=BS given, check if ibs= or obs= also given */
    bool bs_same;       /* true when ibs[_lb|_pi] and obs[_lb|_pi] the same */
    bool cdbsz_given;
    bool count_given;   /* count=COUNT value placed in dd_count variable */
    bool do_time;       /* default true, set false by --status=none */
    bool flexible;      /* if 'HEX' in sgl file, parse as hex without
                         * 'H' prior to @ in skip= or seek= argument */
    bool has_odx;       /* --odx: equivalent to iflag=odx or oflag=odx */
    bool has_xcopy;     /* --xcopy (LID1): iflag=xcopy or oflag=xcopy */
    bool ibs_given;
    bool interrupt_io;  /* [intio=0|1] if false, mask SIGINFO++ during IO */
    bool jf_given;      /* at least 1 level of job file given */
    bool list_id_given;
    bool obs_given;
    bool o_readonly;
    bool out_sparing_active;
    bool out_sparse_active;
    bool out_trim_active;
    bool outf_given;
    bool prefetch_given;/* only active with --verify */
    bool primary_ddpt;  /* true if ddpt, false if helper utility */
    bool quiet;         /* set true when verbose=-1 (or any negative int) */
    bool reading_fifo;  /* true when if=- (read stdin) or if=PIPE */
    bool read1_or_transfer;     /* true when of=/dev/null or similar */
    bool rod_type_given;
    bool rtf_append;    /* if rtf is regular file: open(O_APPEND) */
    bool rtf_len_add;   /* append 64 bit ROD byte size to token */
    bool show_sgl_v2;   /* show built sgls if -vv or higher also given */
    bool status_none;   /* status=none given */
    bool subsequent_wdelay;     /* so no delay before first write */
    bool verbose_given;
    bool verify_given;
    bool version_given;
    bool xc_cat;
    bool xc_dc;
    /* command line related variables */
    int ddpt_strs;      /* number of times 'ddpt' appears in job_file(s) */
    int delay;          /* intra copy segment delay in milliseconds */
    int wdelay;         /* delay prior to each write in copy segment */
    int dry_run;        /* do preparation, bypass copy; >1 go deeper */
    int ibs_lb;         /* ibs= value, stress its logical block size */
    int ibs_pi;    /* if (PI) ibs_pi = ibs_lb+pi_len else ibs_pi=ibs_lb */
    int ibs_hold;       /* not sure why we need this hold */
    int obs_lb;         /* obs= value, stress its logical block size */
    int obs_pi;    /* if (PI) obs_pi = obs_lb+pi_len else obs_pi=obs_lb */
    int bpt_i;          /* Blocks Per Transfer, input sized blocks */
    int obpch;          /* output blocks per check, granularity of sparse,
                         * sparing and trim checks for zeros (def: 0) */
    int id_usage;       /* xcopy(LID1) List identifier usage, init to -1 */
    int prio;           /* xcopy(LID1) related */
    int rdprotect;
    int wrprotect;
    int coe_limit;
    int coe_count;
    int progress;       /* status=progress, report every 2 minutes or less */
    int verbose;
    int do_help;
    int odx_request;    /* ODX_REQ_NONE==0 for no ODX */
    int timeout_xcopy;          /* xcopy(LID1) and ODX */
    int rtf_fd;                 /* ODX: rtf's file descriptor (init: -1) */
    uint32_t inactivity_to;     /* ODX: timeout in seconds */
    uint32_t list_id;           /* xcopy(LID1), odx + wstream related */
    uint32_t rod_type;          /* ODX: ROD type */
    int64_t offset_in_rod;      /* ODX: units are obs bytes */
    /* working variables and statistics */
    int64_t dd_count;   /* -1 for not specified, 0 for no blocks to copy */
                        /* after copy/read starts, decrements to 0 */
                        /* unit is ibs (input logical block size) */
    int64_t lowest_unrecovered;         /* on reads */
    int64_t highest_unrecovered;        /* on reads */
    int64_t resume_iblks;       /* nz when this indicates restart point */
    int64_t num_xcopy;                  /* xcopy(LID1) */
    int max_aborted;
    int max_uas;
    int err_to_report;
    int read_tape_numbytes;
    int last_tape_read_len;  /* Length of previous tape read */
    unsigned int consec_same_len_reads;
    FILE * errblk_fp;
    struct flags_t * iflagp;
    struct dev_info_t * idip;
    struct flags_t * oflagp;
    struct dev_info_t * odip;
    struct dev_info_t * o2dip;  /* of2=OFILE2  reg or pipe file, no sgl */
    uint8_t * wrkPos;
    uint8_t * free_wrkPos;
    uint8_t * wrkPos2;
    uint8_t * free_wrkPos2;
    uint8_t * zeros_buff;
    uint8_t * free_zeros_buff;
    struct cp_statistics_t * stp;  /* NULL or points to cp_state_t's copy */
    struct cp_statistics_t stats;  /* copied here after internal copy done */
    struct sgl_info_t i_sgli; /* in scatter gather list info including list */
    struct sgl_info_t o_sgli; /* out scatter gather list info */
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


struct sg_simple_inquiry_resp;

typedef int (*ddpt_rw_f)(struct dev_info_t * dip, struct cp_state_t * csp,
                         struct opts_t * op);

typedef int (*process_sge_f)(FILE * fp, const struct scat_gath_elem * sge_r,
                             int hex, int verbose);


extern const char * ddpt_arg_strs[];

/* Some inline functions */

/* Make sure multiplication doesn't overflow int */
static inline int x_mult_div(int x, int mult, int div)
{
    return ((int64_t)x * mult) / div;
}

/* Make sure multiplication doesn't overflow int */
static inline int x_mult_rem(int x, int mult, int rem)
{
    return ((int64_t)x * mult) % rem;
}

/* Functions declared below are shared by different compilation units */

/* defined in ddpt.c */
/* No global function defined in ddpt.c apart from main() */

/* defined in ddpt_com.c */
const char * get_ddpt_arg_str(int ddpt_arg);
void sleep_ms(int millisecs);
void state_init(struct opts_t * op, struct flags_t * ifp,
                struct flags_t * ofp, struct dev_info_t * idip,
                struct dev_info_t * odip, struct dev_info_t * o2dip);
void print_stats(const char * str, struct opts_t * op, int who,
                 bool estimate);
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
struct scat_gath_elem * cl2sgl(const char * inp, int * arr_elems, bool b_vb);
struct scat_gath_elem * file2sgl(const char * file_name, bool def_hex,
                                 bool flexible, int * arr_elems, int * errp,
                                 bool b_vb);
/* Assumes sgli_p->elems and sgli_p->slp are setup and the other fields
 * in struct sgl_info_t are zeroed. This function will populate the other
 * fields in that structure. Does one pass through the scatter gather list
 * (array). Sets these fields in struct sgl_info_t: fragmented, lowest_lba,
 * high_lba_p1, monotonic, overlapping, sum and sum_hard. Degenerate
 * elements (i.e. those with 0 blocks) are ignored apart from when one is
 * last which makes sum_hard false and its LBA becomes high_lba_p1 if it
 * is the highest in the list. An empty sgl is equivalent to a 1 element
 * list with [0, 0], so sum_hard==false, monit==true, fragmented==false
 * overlapping ==false . id_str may be NULL, present to enhance verbose
 * output. */
void sgl_sum_scan(struct sgl_info_t * sgli_p, const char * id_str,
                  bool show_sgl, bool b_verbose);

/* Prints sgl to stderr or stdout. */
void sgl_print(struct sgl_info_t * sgli_p, bool skip_meta,
               const char * id_str, bool to_stdout, bool show_sg);

/* Prints a single sge (scatter gather list element) to stderr or stdout. */
void sge_print(const struct scat_gath_elem * sgep, const char * id_str,
               bool to_stdout);

/* Return minimum(num_blks, <blocks_from_sgl-post-blk_off>). First it
 * starts skipping blk_off blocks and if elems is exceeded then it
 * returns 0. Then it sums up the number of blocks from each subsequent
 * sg element checking that elems and max_descriptors are not exceeded. It
 * also stops counting if that sum exceeds num_blks. If max_descriptors is
 * 0 then it is not constraining. Note that elems and blk_off are relative
 * to the start of the sgl; while num_blks and max_descriptors are relative
 * to the sgl+blk_off . */
uint64_t count_sgl_blocks_from(const struct scat_gath_elem * sglp, int elems,
                               uint64_t blk_off, uint32_t num_blks,
                               uint32_t max_descriptors /* from blk_off */);

/* Points to start of sgl after init, sets extend_last bit */
void sgl_iter_init(struct sgl_iter_t * iter_p, struct scat_gath_elem * sglp,
                   int elems);
/* Given a blk_count, the iterator (*iter_p) is moved toward the EOL. If
 * relative is true the move is from the current position of the iterator.
 * If relative is false then the move is from the start of the sgl. The
 * sgl_iter_add(itp, 0, false) call sets the iterator to the start of the
 * sgl. Returns true unless blk_count takes iterator two or more past the
 * last element. So if blk_count takes the iterator to the EOL, this
 * function returns true. */
bool sgl_iter_add(struct sgl_iter_t * iter_p, uint64_t blk_count,
                  bool relative);
/* Move the iterator from its current position (which may be to EOL) towards
 * the start of the sgl (i.e. backwards) for blk_count blocks. Returns true
 * if iterator is valid after the move, else returns false. N.B. if false is
 * returned, then the iterator is invalid and may need to set it to a valid
 * value. */
bool sgl_iter_sub(struct sgl_iter_t * iter_p, uint64_t blk_count);

/* Calculates difference between iterators, logically: res <-- lhs - rhs
 * Checks that lhsp and rhsp have same underlying sgl, if not returns
 * INT_MIN. Assumes iterators close enough for result to lie in range
 * from (-INT_MAX) to INT_MAX (inclusive). */
int sgl_iter_diff(const struct sgl_iter_t * lhsp,
                  const struct sgl_iter_t * rhsp);

/* For each segment from and including iter_p, for length blk_count call
 * *a_fp with fp and my_op as arguments. Move iter_p forward by blk_count
 * (if valid). Returns 0 for good, else error value. */
int iter_add_process(struct sgl_iter_t * iter_p, uint64_t blk_count,
                     process_sge_f a_fp, FILE * fp, int hex, int rblks,
                     int verbose);

/* Returns the number of times 'ch' is found in string 's' given the
 * string's length. */
int num_chs_in_str(const char * s, int slen, int ch);
/* Returns the number of times either 'ch1' or 'ch2' is found in
 * string 's' given the string's length. */
int num_either_ch_in_str(const char * s, int slen, int ch1, int ch2);
/* Copies abs(add_blks) blocks for current position of file/device (referred
 * to by dip) to or from a segment of the computer's ram. The copy is into
 * ram when ddpt_arg is 0 (i.e. a "read"), otherwise it is from ram (i.e. a
 * "write"). IO is performed by the fp (callback function) and the iterator
 * (or file pointer) is moved forward (i.e. toward the end) when add_blks > 0.
 * When add_blks < 0, the iterator is moved backward (i.e. toward the
 * beginning). Returns 0 for okay else an error number. -9999 is returned
 * for an unexpected error with the iterator. */
int cp_via_sgl_iter(struct dev_info_t * dip, struct cp_state_t * csp,
                    int add_blks, ddpt_rw_f fp, struct opts_t * op);
/* Returns number elements in scatter gather list (array) whose pointer
 * is written to *sge_pp. On error returns negated error number and
 * NULL is written to *sge_pp . The caller is responsible for freeing
 * memory associated with *sge_pp . */
int build_sgl(struct scat_gath_elem ** sge_pp, int64_t count, int64_t offs);
/* Builds single element sgl with (*sge_pp)->num==0 . */
int build_degen_sgl(struct scat_gath_elem ** sge_pp, int64_t start_lba);
/* Similar to build_sgl but appends to existing sgl whose length is cur_elems.
 * Note that *sge_pp will probably change (in which case the previous *sge_pp
 * is freed). */
int append2sgl(struct scat_gath_elem ** sge_pp, int cur_elems,
               int64_t extra_blks, int64_t start_lba);

/* Returns true if associated iterator is monotonic (increasing) and not
 * fragmented. Empty sgl and single element degenerate considered linear.
 * Assumes sgl_sum_scan() has been called on sgl. */
bool is_iter_linear(const struct sgl_info_t * sglip);
/* Returns LBA referred to by iterator if valid or returns DDPT_LBA_INVALID
 * (-1) if at end or invalid. */
int64_t sgl_iter_lba(const struct sgl_iter_t * itp);
/* Returns true of no sgl or sgl is at the end [<elems>, 0], otherwise it
 * returns false. */
bool sgl_iter_at_end(const struct sgl_iter_t * itp);
/* Print data held in struct sgl_iter_t to fp. If fp is NULL then to stderr */
void sgl_iter_print(const struct sgl_iter_t * itp, const char * leadin,
                    bool index_only, bool in_hex, FILE * fp);

/* Returns true if either argument is NULL/0 or a 1 element list with both
 * lba and num 0; otherwise returns false. */
bool sgl_empty(struct scat_gath_elem * sglp, int elems);
/* Returns >= 0 if sgl can be simplified to a single LBA. So an empty sgl
 * will return 0; a one element sgl will return its LBA. A multiple element
 * sgl only returns the first element's LBA (that is not degenerate) if the
 * sgl is monotonic and not fragmented. In the extreme case takes last
 * element's LBA if all prior elements are degenerate. Else returns -1 .
 * Assumes sgl_sum_scan() has been called. */
int64_t get_low_lba_from_linear(const struct sgl_info_t * sglip);
/* If bad arguments returns -1, otherwise returns the lowest LBA in *sglp .
 * If no elements considered returns 0. If ignore_degen is true than
 * ignores all elements with num_blks zero unless always_last is also
 * true in which case the last element is always considered. */
int64_t get_lowest_lba(const struct scat_gath_elem * sglp, int num_elems,
                       bool ignore_degen, bool always_last);
void cp_state_init(struct cp_state_t * csp, struct opts_t * op);

/* Writes the LBA and number_of_blocks in sge_r to a line at the current
 * file position (typically the end) of fp, with a trailing \n character.
 * Returns 0 on success, else SG_LIB_FILE_ERROR . */
int output_sge_f(FILE * fp, const struct scat_gath_elem * sgep, int hex,
                 int verbose);

/* Compares from lsgep, offset by l_bk_off against rsgep, offset by r_bk_off.
 * While lbas compare equal lsgep is advanced by up to lelems, while rsgep
 * is advanced by up to relems. Returns false on the first inequality;
 * otherwise if both list are exhausted at the same point, then returns true.
 * If no inequality and one list is exhausted before the other, then returns
 * allow_partial. */
bool sgl_eq_f(const struct scat_gath_elem * lsgep, int lelems, int l_bk_off,
            const struct scat_gath_elem * rsgep, int relems, int r_bk_off,
            bool allow_partial);

/* Compares from the current iterator positions of lhsp and rhsp until
 * the shorter list is exhausted. Returns false on the first inequality.
 * If no inequality and both remaining lists are same length then returns
 * true. If no inequality but remaining lists differ in length then returns
 * allow_partial. */
bool sgl_iter_eq(const struct sgl_iter_t * lhsp,
                 const struct sgl_iter_t * rhsp, bool allow_partial);


/* defined in ddpt_pt.c */
void * pt_construct_obj(void);
void pt_destruct_obj(void * vp);
int pt_open_if(struct opts_t * op, struct sg_simple_inquiry_resp * sirp);
int pt_open_of(struct opts_t * op, struct sg_simple_inquiry_resp * sirp);
void pt_close(int fd);
int pt_read_capacity(struct opts_t * op, bool in0_out1, int64_t * num_blks,
                     int * blk_sz);
int pt_read(struct opts_t * op, bool in0_out1, uint8_t * buff,
            int blocks, int64_t from_block, int * blks_readp);
int pt_write(struct opts_t * op, const uint8_t * buff, int blocks,
             int64_t to_block);
/* Sets UNMAP bit and no other flags, sends 1 block (which should be
 * ignored by device). */
int pt_write_same16(struct opts_t * op, const uint8_t * buff, int bs,
                    int blocks, int64_t start_block);
void pt_sync_cache(int fd);
int pt_3party_copy_out(int sg_fd, int sa, uint32_t list_id, int group_num,
                       int timeout_secs, void * paramp, int param_len,
                       bool noisy, int verbose, int err_vb);
int pt_3party_copy_in(int sg_fd, int sa, uint32_t list_id, int timeout_secs,
                      void * resp, int mx_resp_len, bool noisy, int verbose,
                      int err_vb);
int pt_pre_fetch(struct opts_t * op, int blocks, int64_t start_block);

/* defined in ddpt_xcopy.c */
int open_rtf(struct opts_t * op);
const char * cpy_op_status_str(int cos, char * b, int blen);
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
int cl_parse(struct opts_t * op, int argc, char * argv[],
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
int win32_set_file_pos(struct dev_info_t * dip, int64_t pos, int verbose);
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
}       /* trailing brace for 'extern "C" { ' at top of this file */

/* Following only compiled to C++, bypassed for C */
struct split_fn_fp {
    // constructor
    split_fn_fp(const char * fn, FILE * a_fp) : out_fn(fn), fp(a_fp) {}
public:
    std::string out_fn;
    FILE * fp;
};

struct sgl_opts_t {
    bool append2iaf;
    bool append2out_f;
    bool chs_given;
    bool div_lba_only;
    bool div_num_only;
    bool elem_given;
    bool flexible;
    bool iaf2stdout;
    bool index_given;
    bool out2stdout;
    bool non_overlap_chk;
    bool pr_stats;
    bool quiet;
    int act_val;
    int degen_mask;
    int div_scale_n;
    int document;       /* Add comment(s) to O_SGL(s), >1 add cmdline */
    int do_hex;
    int help;
    int interleave;     /* when splitting a sgl, max number of blocks before
                         * moving to next sgl; def=0 --> no interleave */
    int last_elem;      /* init to -1 which makes start_elem a singleton */
    int round_blks;
    int sort_cmp_val;
    int split_n;
    int start_elem;     /* init to -1 which means write out whole O_SGL */
    int verbose;
    const char * b_sgl_arg;
    const char * iaf;   /* index array filename */
    const char * out_fn;
    const char * fne;
    struct chs_t chs;
    struct cl_sgl_stats ab_sgl_stats;
    std::string cmd_line;
    std::vector<int> index_arr; /* input from --index=IA */
    std::vector<struct split_fn_fp> split_out_fns;
    std::vector<struct split_fn_fp> b_split_out_fns; /* 'b' side for tsplit */
};

#endif  /* end of __cplusplus block */

#endif  /* DDPT_H guard against multiple includes */


