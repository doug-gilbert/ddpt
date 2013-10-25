/*
 * Copyright (c) 2008-2013 Douglas Gilbert.
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
 * The ddpt utility is a rewritten and extended version of the sg_dd utility
 * found in the sg3_utils package. sg_dd has a GPL (version 2) which has been
 * changed to a somewhat freer FreeBSD style license in ddpt.
 * Both licenses are considered "open source".
 *
 * Windows "block" devices, when _not_ accessed via the pass-through, don't
 * seem to work when POSIX/Unix like IO calls are used (e.g. write()).
 * So may need CreateFile, ReadFile, WriteFile, SetFilePointer and friends.
 */

static const char * version_str = "0.93 20131024 [svn: r237]";

/* Was needed for posix_fadvise() */
/* #define _XOPEN_SOURCE 600 */

/* Need _GNU_SOURCE for O_DIRECT */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#include <errno.h>
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

#ifdef HAVE_NANOSLEEP
#include <time.h>
#endif

#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
#include <time.h>
#elif defined(HAVE_GETTIMEOFDAY)
#include <time.h>
#include <sys/time.h>
#endif

#include "ddpt.h"

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

#ifndef HAVE_SYSCONF
#include <windows.h>

static size_t
my_pagesize(void)
{
    SYSTEM_INFO sys_info;

    GetSystemInfo(&sys_info);
    return sys_info.dwPageSize;
}
#endif

#endif  /* end SG_LIB_WIN32 */

#include "sg_lib.h"

#ifndef EREMOTEIO
#define EREMOTEIO EIO
#endif

/* Used for outputting diagnostic messages for oflag=pre-alloc */
#define PREALLOC_DEBUG 1


/* The use of signals is borrowed from GNU's dd source code which is
 * found in their coreutils package. If SA_NOCLDSTOP is non-zero then
 * a modern Posix compliant version of signals is assumed. Still
 * thinking about SIGHUP which will be delivered if the controlling
 * process/terminal is terminated or receives SIGHUP. */

/* If nonzero, the value of the pending fatal signal.  */
static sig_atomic_t volatile interrupt_signal;

/* A count of pending info(usr1) signals, decremented as processed */
static sig_atomic_t volatile info_signals_pending;

static const char * errblk_file = "errblk.txt";

static struct signum_name_t signum_name_arr[] = {
    {SIGINT, "SIGINT"},
    {SIGQUIT, "SIGQUIT"},
    {SIGPIPE, "SIGPIPE"},
#if SIGINFO == SIGUSR1
    {SIGUSR1, "SIGUSR1"},
#else
    {SIGINFO, "SIGINFO"},
#endif
#ifndef SG_LIB_WIN32
    {SIGHUP, "SIGHUP"},
#endif
    {0, NULL},
};

static void calc_duration_throughput(const char * leadin, int contin,
                                     struct opts_t * op);
#ifdef SG_LIB_LINUX
static void print_tape_summary(struct opts_t * op, int res, const char * str);
static void print_tape_pos(const char * prefix, const char * postfix,
                           struct opts_t * op);
#endif


static void
usage(int help)
{
    if (help < 2)
        goto primary_help;
    else
        goto secondary_help;

primary_help:
    pr2serr("Usage: "
           "ddpt  [bpt=BPT[,OBPC]] [bs=BS] [cdbsz=6|10|12|16|32] [coe=0|1]\n"
           "             [coe_limit=CL] [conv=CONVS] [count=COUNT] "
           "[delay=MS[,W_MS]]\n"
           "             [ibs=IBS] [id_usage=LIU] if=IFILE [iflag=FLAGS] "
           "[intio=0|1]\n"
           "             [iseek=SKIP] [list_id=LID] [obs=OBS] [of=OFILE] "
           "[of2=OFILE2]\n"
           "             [oflag=FLAGS] [oseek=SEEK] [prio=PRIO] "
           "[protect=RDP[,WRP]]\n"
           "             [retries=RETR] [seek=SEEK] [skip=SKIP] "
           "[status=STAT]\n"
           "             [verbose=VERB]\n"
#ifdef SG_LIB_WIN32
           "             [--help] [--verbose] [--version] [--wscan]\n"
#else
           "             [--help] [--verbose] [--version]\n"
#endif
           "  where:\n"
           "    bpt         input Blocks Per Transfer (BPT) (def: 128 when "
           "IBS is 512)\n"
           "                Output Blocks Per Check (OBPC) (def: 0 implies "
           "BPT*IBS/OBS)\n"
           "    bs          block size for input and output (overrides "
           "ibs and obs)\n");
    pr2serr(
           "    cdbsz       size of SCSI READ or WRITE cdb (default is "
           "10)\n"
           "    coe         0->exit on error (def), 1->continue on "
           "error (zero fill)\n"
           "    coe_limit   limit consecutive 'bad' blocks on reads to CL "
           "times\n"
           "                when coe=1 (default: 0 which is no limit)\n"
           "    conv        conversions, comma separated list of CONVS "
           "(see below)\n"
           "    count       number of input blocks to copy (def: "
           "(remaining)\n"
           "                device/file size)\n"
           "    delay       wait MS milliseconds between each copy segment "
           "(def: 0)\n"
           "                wait W_MS milliseconds prior to each write "
           "(def: 0)\n"
           "    ibs         input block size (default 512 bytes)\n"
           "    id_usage    xcopy: set list_id_usage to hold (0), discard "
           "(2),\n"
           "                disable (3), or the given number (def: 0 or "
           "2)\n"
           "    if          file or device to read from (for stdin use "
           "'-')\n"
           "    iflag       input flags, comma separated list from FLAGS "
           "(see below)\n"
           "    intio       interrupt during IO; allow signals during reads "
           "and writes\n"
           "                (def: 0 causes signals to be masked during IO)\n"
           "    iseek       block position to start reading from IFILE\n"
           "    list_id     xcopy: list_id (def: 1 or 0)\n"
           "    obs         output block size (def: 512). When IBS is "
           "not equal to OBS\n"
           "                then (((IBS * BPT) %% OBS) == 0) is required\n"
           "    of          file or device to write to (def: /dev/null)\n");
    pr2serr(
           "    of2         additional output file (def: /dev/null), "
           "OFILE2 should be\n"
           "                regular file or pipe\n"
           "    oflag       output flags, comma separated list from FLAGS "
           "(see below)\n"
           "    oseek       block position to start writing to OFILE\n"
           "    prio        xcopy: set priority field to PRIO (def: 1)\n"
           "    protect     set rdprotect and/or wrprotect fields on "
           "pt commands\n"
           "    retries     retry pass-through errors RETR times "
           "(def: 0)\n"
           "    seek        block position to start writing to OFILE\n"
           "    skip        block position to start reading from IFILE\n"
           "    status      'noxfer' suppresses throughput calculation; "
           "'none'\n"
           "                suppresses all trailing reports (apart from "
           "errors)\n"
           "    verbose     0->normal(def), 1->some noise, 2->more noise, "
           "etc\n"
           "                -1->quiet (stderr->/dev/null)\n"
           "    --help      print out this usage message then exit\n"
           "    --verbose   equivalent to verbose=1\n"
           "    --version   print version information then exit\n"
#ifdef SG_LIB_WIN32
           "    --wscan     windows scan for device names and volumes\n"
#endif
           "    --xcopy     do xcopy rather than normal rw copy\n"
           "\nCopy all or part of IFILE to OFILE, IBS*BPT bytes at a time. "
           "Similar to\n"
           "dd command. Support for block devices, especially those "
           "accessed via\na SCSI pass-through. For list of FLAGS and CONVS "
           "use '-hh' .\n");
    return;

secondary_help:
    pr2serr("FLAGS:\n"
            "  append (o)     append (part of) IFILE to end of OFILE\n"
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
            "  flock          use advisory exclusive lock [flock()] on "
            "IFILE/OFILE\n"
            "  force          override inconsistent information that would "
            "stop copy\n"
            "  fsync (o)      like fdatasync but flushes meta-data as well\n"
            "  fua (pt)       force unit access on IFILE or OFILE\n"
            "  fua_nv (pt)    force unit access, non-volatile (obsoleted by "
            "T10)\n"
            "  ignoreew (o)   ignore early warning (end of tape)\n"
            "  nocache        use posix_fadvise(POSIX_FADV_DONTNEED)\n"
            "  nofm (o)       no File Mark (FM) on close when writing to "
            "tape\n"
            "  nopad          inhibits tapes blocks less than OBS being "
            "padded\n"
            "  norcap (pt)    do not invoke SCSI READ CAPACITY command\n"
            "  nowrite (o)    bypass all writes to OFILE\n"
            "  null           does nothing, place holder\n"
            "  pad (o)        pad blocks shorter than OBS with zeroes\n"
            "  pre-alloc (o)  use fallocate() before copy to set OFILE to "
            "its\n"
            "                 expected size\n"
            "  pt             instruct pass-through interface to be used\n"
            "  rarc (i,pt)    set RARC (rebuild assist) bit in SCSI READs\n"
            "  resume (o)     attempt to restart an interrupted copy\n"
            "  self (pt)      used with trim; IFILE=OFILE; trim zero "
            "segments\n"
            "  sparing (o)    read OFILE prior to a write; don't write if "
            "same\n"
            "  sparse (o)     don't write blocks of zeroes; move file "
            "pointer\n"
            "                 or if OFILE is pt assume it contains zeroes "
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
            "  noerror        same as iflag=coe\n"
            "  notrunc        does nothing because this is default action "
            "of ddpt\n"
            "  null           does nothing, place holder\n"
            "  resume         same as oflag=resume\n"
            "  sparing        same as oflag=sparing\n"
            "  sparse         same as oflag=sparse\n"
            "  sync           ignored to allow 'conv=noerror,sync' dd usage "
            "for coe\n"
            "  trunc          same as oflag=trunc\n");
}

/* Want safe, 'n += snprintf(b + n, blen - n, ...)' style sequence of
 * functions. Returns number number of chars placed in cp excluding the
 * trailing null char. So for cp_max_len > 0 the return value is always
 * < cp_max_len; for cp_max_len <= 1 the return value is 0 and no chars
 * are written to cp. Note this means that when cp_max_len = 1, this
 * function assumes that cp[0] is the null character and does nothing
 * (and returns 0).  */
static int
my_snprintf(char * cp, int cp_max_len, const char * fmt, ...)
{
    va_list args;
    int n;

    if (cp_max_len < 2)
        return 0;
    va_start(args, fmt);
    n = vsnprintf(cp, cp_max_len, fmt, args);
    va_end(args);
    return (n < cp_max_len) ? n : (cp_max_len - 1);
}

/* Abbreviation of fprintf(stderr, ...) */
int     /* Global function */
pr2serr(const char * fmt, ...)
{
    va_list args;
    int n;

    va_start(args, fmt);
    n = vfprintf(stderr, fmt, args);
    va_end(args);
    return n;
}

static void
print_stats(const char * str, struct opts_t * op)
{
#ifdef SG_LIB_LINUX
    /* Print tape read summary if necessary . */
    print_tape_summary(op, 0, str);
#endif

    if ((0 != op->dd_count) && (! op->reading_fifo))
        pr2serr("  remaining block count=%" PRId64 "\n", op->dd_count);
    pr2serr("%s%" PRId64 "+%d records in\n", str, op->in_full,
            op->in_partial);
    pr2serr("%s%" PRId64 "+%d records out\n", str, op->out_full,
            op->out_partial);
    if (op->out_sparse_active || op->out_sparing_active) {
        if (op->out_trim_active) {
            const char * cp;

            cp = op->trim_errs ? "attempted trim" : "trimmed";
            if (op->out_sparse_partial > 0)
                pr2serr("%s%" PRId64 "+%d %s records out\n", str,
                        op->out_sparse, op->out_sparse_partial, cp);
            else
                pr2serr("%s%" PRId64 " %s records out\n", str,
                        op->out_sparse, cp);
        } else if (op->out_sparse_partial > 0)
            pr2serr("%s%" PRId64 "+%d bypassed records out\n", str,
                    op->out_sparse, op->out_sparse_partial);
        else
            pr2serr("%s%" PRId64 " bypassed records out\n", str,
                    op->out_sparse);
    }
    if (op->recovered_errs > 0)
        pr2serr("%s%d recovered read errors\n", str, op->recovered_errs);
    if (op->num_retries > 0)
        pr2serr("%s%d retries attempted\n", str, op->num_retries);
    if (op->unrecovered_errs > 0)
        pr2serr("%s%d unrecovered read error%s\n", str, op->unrecovered_errs,
                ((1 == op->unrecovered_errs) ? "" : "s"));
    if (op->unrecovered_errs && (op->highest_unrecovered >= 0))
        pr2serr("lowest unrecovered read lba=%" PRId64 ", highest "
                "unrecovered lba=%" PRId64 "\n", op->lowest_unrecovered,
                op->highest_unrecovered);
    if (op->wr_recovered_errs > 0)
        pr2serr("%s%d recovered write errors\n", str, op->wr_recovered_errs);
    if (op->wr_unrecovered_errs > 0)
        pr2serr("%s%d unrecovered write error%s\n", str,
                op->wr_unrecovered_errs,
                ((1 == op->wr_unrecovered_errs) ? "" : "s"));
    if (op->trim_errs)
        pr2serr("%s%d trim errors\n", str, op->trim_errs);
    if (op->interrupted_retries > 0)
        pr2serr("%s%d %s after interrupted system call(s)\n",
                str, op->interrupted_retries,
                ((1 == op->interrupted_retries) ? "retry" : "retries"));
    if (op->has_xcopy)
        pr2serr("%s%" PRId64 " xcopy command%s done\n", str, op->num_xcopy,
                ((1 == op->num_xcopy) ? "" : "s"));
}

/* Return signal name for signum if known, else return signum as a string. */
static const char *
get_signal_name(int signum, char * b, int blen)
{
    const struct signum_name_t * sp;

    for (sp = signum_name_arr; sp->num; ++sp) {
        if (signum == sp->num)
            break;
    }
    if (blen < 1)
        return b;
    b[blen - 1] = '\0';
    if (sp->num)
        strncpy(b, sp->name, blen - 1);
    else
        snprintf(b, blen, "%d", signum);
    return b;
}

/* An ordinary signal was received; arrange for the program to exit.  */
static void
interrupt_handler(int sig)
{
    if (! SA_RESETHAND)
        signal(sig, SIG_DFL);
    interrupt_signal = sig;
}

/* An info signal was received; arrange for the program to print status.  */
static void
siginfo_handler(int sig)
{
    if (! SA_NOCLDSTOP)
        signal(sig, siginfo_handler);
    ++info_signals_pending;
}

/* Install the signal handlers. We try to cope gracefully with signals whose
 * disposition is 'ignored'. SUSv3 recommends that a process should start
 * with no blocked signals; if needed unblock SIGINFO, SIGINT or SIGPIPE.  */
static void
install_signal_handlers(struct opts_t * op)
{
#if SIGINFO == SIGUSR1
    const char * sname = "SIGUSR1";
#else
    const char * sname = "SIGINFO";
#endif

    if (op->verbose > 2)
        pr2serr(" >> %s signal implementation assumed "
                "[SA_NOCLDSTOP=%d], %smasking during IO\n",
                (SA_NOCLDSTOP ? "modern" : "old"), SA_NOCLDSTOP,
                (op->interrupt_io ? "not " : ""));
#if SA_NOCLDSTOP
    struct sigaction act;
    sigset_t starting_mask;
    int num_members = 0;
    int unblock_starting_mask = 0;

    sigemptyset(&op->caught_signals);
    sigemptyset(&op->orig_mask);
    sigaction(SIGINFO, NULL, &act);
    if (act.sa_handler != SIG_IGN)
        sigaddset(&op->caught_signals, SIGINFO);
    else if (op->verbose)
        pr2serr("%s ignored, progress reports not available\n", sname);
    sigaction(SIGINT, NULL, &act);
    if (act.sa_handler != SIG_IGN)
        sigaddset(&op->caught_signals, SIGINT);
    else if (op->verbose)
        pr2serr("SIGINT ignored\n");
    sigaction(SIGPIPE, NULL, &act);
    if (act.sa_handler != SIG_IGN)
        sigaddset(&op->caught_signals, SIGPIPE);
    else if (op->verbose)
        pr2serr("SIGPIPE ignored\n");

    sigprocmask(SIG_UNBLOCK /* ignored */, NULL, &starting_mask);
    if (sigismember(&starting_mask, SIGINFO)) {
        if (op->verbose)
            pr2serr("%s blocked on entry, unblock\n", sname);
        ++unblock_starting_mask;
    }
    if (sigismember(&starting_mask, SIGINT)) {
        if (op->verbose)
            pr2serr("SIGINT blocked on entry, unblock\n");
        ++unblock_starting_mask;
    }
    if (sigismember(&starting_mask, SIGPIPE)) {
        if (op->verbose)
            pr2serr("SIGPIPE blocked on entry, unblock\n");
        ++unblock_starting_mask;
    }
    act.sa_mask = op->caught_signals;

    if (sigismember(&op->caught_signals, SIGINFO)) {
        act.sa_handler = siginfo_handler;
        act.sa_flags = 0;
        sigaction(SIGINFO, &act, NULL);
        ++num_members;
    }

    if (sigismember(&op->caught_signals, SIGINT)) {
        act.sa_handler = interrupt_handler;
        act.sa_flags = SA_NODEFER | SA_RESETHAND;
        sigaction(SIGINT, &act, NULL);
        ++num_members;
    }

    if (sigismember(&op->caught_signals, SIGPIPE)) {
        act.sa_handler = interrupt_handler;
        act.sa_flags = SA_NODEFER | SA_RESETHAND;
        sigaction(SIGPIPE, &act, NULL);
        ++num_members;
    }
    if (unblock_starting_mask)
        sigprocmask(SIG_UNBLOCK, &op->caught_signals, NULL);

    if ((0 == op->interrupt_io) && (num_members > 0))
        sigprocmask(SIG_BLOCK, &op->caught_signals, &op->orig_mask);
#else   /* not SA_NOCLDSTOP */
    if (op) { ; }    /* suppress warning */
    if (signal(SIGINFO, SIG_IGN) != SIG_IGN) {
        signal(SIGINFO, siginfo_handler);
        siginterrupt(SIGINFO, 1);
    } else if (op->verbose)
        pr2serr("old %s ignored, progress report not available\n", sname);
    if (signal(SIGINT, SIG_IGN) != SIG_IGN) {
        signal(SIGINT, interrupt_handler);
        siginterrupt(SIGINT, 1);
    } else if (op->verbose)
        pr2serr("old SIGINT ignored\n");
#endif  /* SA_NOCLDSTOP */
}

static void
sleep_ms(int millisecs)
{
#ifdef SG_LIB_WIN32
    win32_sleep_ms(millisecs);
#elif defined(HAVE_NANOSLEEP)
    struct timespec request;

    if (millisecs > 0) {
        request.tv_sec = millisecs / 1000;
        request.tv_nsec = (millisecs % 1000) * 1000000;
        if ((nanosleep(&request, NULL) < 0) && (EINTR != errno))
            perror("nanosleep");
    }
#endif
}

/* Process any pending signals and perhaps do delay. If signals are caught,
 * this function should be called periodically.  Ideally there should never
 * be an unbounded amount of time when signals are not being processed. */
void    /* Global function, used by ddpt_xcopy.c */
signals_process_delay(struct opts_t * op, int delay_type)
{
    char b[32];
    int got_something = 0;
    int delay = 0;

#if SA_NOCLDSTOP
    int found_pending = 0;

    if ((0 == op->interrupt_io) &&
        (sigismember(&op->caught_signals, SIGINT) ||
         sigismember(&op->caught_signals, SIGPIPE) ||
         sigismember(&op->caught_signals, SIGINFO))) {
        sigset_t pending_set;

        sigpending(&pending_set);
        if (sigismember(&pending_set, SIGINT) ||
            sigismember(&pending_set, SIGPIPE) ||
            sigismember(&pending_set, SIGINFO)) {
            /* Signal handler for a pending signal run during suspend */
            sigsuspend(&op->orig_mask);
            found_pending = 1;
        } else {        /* nothing pending so perhaps delay */
            if ((op->delay > 0) && (DELAY_COPY_SEGMENT == delay_type))
                delay = op->delay;
            else if ((op->wdelay > 0) && (DELAY_WRITE == delay_type))
                delay = op->wdelay;
            if (delay) {
                sigprocmask(SIG_SETMASK, &op->orig_mask, NULL);
                sleep_ms(delay);
                sigprocmask(SIG_BLOCK, &op->caught_signals, NULL);
            }
            if (! (interrupt_signal || info_signals_pending))
                return;
        }
    }
#endif

    while (interrupt_signal || info_signals_pending) {
        int interrupt;
        int infos;

        got_something = 1;
#if SA_NOCLDSTOP
        if (! found_pending)
            sigprocmask(SIG_BLOCK, &op->caught_signals, NULL);
#endif

        /* Reload interrupt_signal and info_signals_pending, in case a new
           signal was handled before sigprocmask took effect.  */
        interrupt = interrupt_signal;
        infos = info_signals_pending;

        if (infos)
            info_signals_pending = infos - 1;

#if SA_NOCLDSTOP
        if (! found_pending)
            sigprocmask(SIG_SETMASK, &op->orig_mask, NULL);
#endif

        if (interrupt) {
            pr2serr("Interrupted by signal %s\n",
                    get_signal_name(interrupt, b, sizeof(b)));
            print_stats("", op);
            /* Don't show next message if using oflag=pre-alloc and we didn't
             * use FALLOC_FL_KEEP_SIZE */
            if ((0 == op->reading_fifo) && (FT_REG & op->odip->d_type_hold)
                 && (0 == op->oflagp->prealloc))
                pr2serr("To resume, invoke with same arguments plus "
                        "oflag=resume\n");
            ; // >>>>>>>>>>>>> cleanup ();
        } else {
            pr2serr("Progress report:\n");
            print_stats("  ", op);
            if (op->do_time)
                calc_duration_throughput("  ", 1, op);
            pr2serr("  continuing ...\n");
        }
        if (interrupt) {
#if SA_NOCLDSTOP
            if (found_pending) {
                sigset_t int_set;

                sigemptyset(&int_set);
                sigaddset(&int_set, interrupt);
                sigprocmask(SIG_UNBLOCK, &int_set, NULL);
            }
#endif
            raise(interrupt);
        }
    }

    if (! got_something) {
        if ((op->delay > 0) && (DELAY_COPY_SEGMENT == delay_type))
            delay = op->delay;
        else if ((op->wdelay > 0) && (DELAY_WRITE == delay_type))
            delay = op->wdelay;
        if (delay)
            sleep_ms(op->delay);
    }
}


/* Create errblk file (see iflag=errblk) and if we have gettimeofday
 * puts are start timestampl on the first line. */
static void
errblk_open(struct opts_t * op)
{
    op->errblk_fp = fopen(errblk_file, "a");        /* append */
    if (NULL == op->errblk_fp)
        pr2serr("unable to open or create %s\n", errblk_file);
    else {
#ifdef HAVE_GETTIMEOFDAY
        {
            time_t t;
            char b[64];

            t = time(NULL);
            strftime(b, sizeof(b), "# start: %Y-%m-%d %H:%M:%S\n",
                     localtime(&t));
            fputs(b, op->errblk_fp);
        }
#else
        fputs("# start\n", op->errblk_fp);
#endif
    }
}

void    /* Global function, used by ddpt_pt.c */
errblk_put(uint64_t lba, struct opts_t * op)
{
    if (op->errblk_fp)
        fprintf(op->errblk_fp, "0x%" PRIx64 "\n", lba);
}

void    /* Global function, used by ddpt_pt.c */
errblk_put_range(uint64_t lba, int num, struct opts_t * op)
{
    if (op->errblk_fp) {
        if (1 == num)
            errblk_put(lba, op);
        else if (num > 1)
            fprintf(op->errblk_fp, "0x%" PRIx64 "-0x%" PRIx64 "\n", lba,
                    lba + (num - 1));
    }
}

static void
errblk_close(struct opts_t * op)
{
    if (op->errblk_fp) {
#ifdef HAVE_GETTIMEOFDAY
        {
            time_t t;
            char b[64];

            t = time(NULL);
            strftime(b, sizeof(b), "# stop: %Y-%m-%d %H:%M:%S\n",
                     localtime(&t));
            fputs(b, op->errblk_fp);
        }
#else
        fputs("# stop\n", op->errblk_fp);
#endif
        fclose(op->errblk_fp);
        op->errblk_fp = NULL;
    }
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
            ++ofp->fdatasync;
        else if (0 == strcmp(cp, "fsync"))
            ++ofp->fsync;
        else if (0 == strcmp(cp, "noerror"))
            ++ifp->coe;         /* will still fail on write error */
        else if (0 == strcmp(cp, "notrunc"))
            ;         /* this is the default action of ddpt so ignore */
        else if (0 == strcmp(cp, "null"))
            ;
        else if (0 == strcmp(cp, "resume"))
            ++ofp->resume;
        else if (0 == strcmp(cp, "sparing"))
            ++ofp->sparing;
        else if (0 == strcmp(cp, "sparse"))
            ++ofp->sparse;
        else if (0 == strcmp(cp, "sync"))
            ;   /* dd(susv4): pad errored block(s) with zeros but ddpt does
                 * that by default. Typical dd use: 'conv=noerror,sync' */
        else if (0 == strcmp(cp, "trunc"))
            ++ofp->trunc;
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
            ++fp->append;
        else if (0 == strcmp(cp, "block"))
            ++fp->block;
        else if (0 == strcmp(cp, "cat"))
            ++fp->cat;
        else if (0 == strcmp(cp, "coe"))
            ++fp->coe;
        else if (0 == strcmp(cp, "dc"))
            ++fp->dc;
        else if (0 == strcmp(cp, "direct"))
            ++fp->direct;
        else if (0 == strcmp(cp, "dpo"))
            ++fp->dpo;
        else if (0 == strcmp(cp, "errblk"))
            ++fp->errblk;
        else if (0 == strcmp(cp, "excl"))
            ++fp->excl;
        else if (0 == strcmp(cp, "fdatasync"))
            ++fp->fdatasync;
        else if (0 == strcmp(cp, "flock"))
            ++fp->flock;
        else if (0 == strcmp(cp, "force"))
            ++fp->force;
        else if (0 == strcmp(cp, "fsync"))
            ++fp->fsync;
        else if (0 == strcmp(cp, "fua_nv"))   /* check fua_nv before fua */
            ++fp->fua_nv;
        else if (0 == strcmp(cp, "fua"))
            ++fp->fua;
        else if (0 == strcmp(cp, "ignoreew")) /* tape: ignore early warning */
            ++fp->ignoreew;
        else if (0 == strcmp(cp, "nocache"))
            ++fp->nocache;
        else if (0 == strcmp(cp, "nofm"))     /* No filemark on tape close */
            ++fp->nofm;
        else if (0 == strcmp(cp, "nopad"))
            ++fp->nopad;
        else if (0 == strcmp(cp, "norcap"))
            ++fp->norcap;
        else if (0 == strcmp(cp, "nowrite"))
            ++fp->nowrite;
        else if (0 == strcmp(cp, "null"))
            ;
        else if (0 == strcmp(cp, "pad"))
            ++fp->pad;
        else if (0 == strcmp(cp, "pre-alloc") || 0 == strcmp(cp, "prealloc"))
            ++fp->prealloc;
        else if (0 == strcmp(cp, "pt"))
            ++fp->pt;
        else if (0 == strcmp(cp, "rarc"))
            ++fp->rarc;
        else if (0 == strcmp(cp, "resume"))
            ++fp->resume;
        else if (0 == strcmp(cp, "self"))
            ++fp->self;
        else if (0 == strcmp(cp, "sparing"))
            ++fp->sparing;
        else if (0 == strcmp(cp, "sparse"))
            ++fp->sparse;
        else if (0 == strcmp(cp, "ssync"))
            ++fp->ssync;
        else if (0 == strcmp(cp, "strunc"))
            ++fp->strunc;
        else if (0 == strcmp(cp, "sync"))
            ++fp->sync;
        else if ((0 == strcmp(cp, "trim")) || (0 == strcmp(cp, "unmap"))) {
            /* treat trim (ATA term) and unmap (SCSI term) as synonyms */
            ++fp->wsame16;
        } else if (0 == strcmp(cp, "trunc"))
            ++fp->trunc;
        else if (0 == strcmp(cp, "xcopy"))
            ++fp->xcopy;
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
default_bpt_i(int ibs)
{
    if (ibs < 8)
        return DEF_BPT_LT8;
    else if (ibs < 64)
        return DEF_BPT_LT64;
    else if (ibs < 1024)
        return DEF_BPT_LT1024;
    else if (ibs < 8192)
        return DEF_BPT_LT8192;
    else if (ibs < 31768)
        return DEF_BPT_LT32768;
    else
        return DEF_BPT_GE32768;
}

/* Command line processing helper, checks sanity and applies some
 * defaults. Returns 0 on success, > 0 for error. */
static int
cl_sanity_defaults(struct opts_t * op)
{
    if ((0 == op->ibs) && (0 == op->obs)) {
        op->ibs = DEF_BLOCK_SIZE;
        op->obs = DEF_BLOCK_SIZE;
        if (op->idip->fn[0])
            pr2serr("Assume block size of %d bytes for both input and "
                    "output\n", DEF_BLOCK_SIZE);
    } else if (0 == op->obs) {
        op->obs = DEF_BLOCK_SIZE;
        if ((op->ibs != DEF_BLOCK_SIZE) && op->odip->fn[0])
            pr2serr("Neither obs nor bs given so set obs=%d (default "
                    "block size)\n", op->obs);
    } else if (0 == op->ibs) {
        op->ibs = DEF_BLOCK_SIZE;
        if (op->obs != DEF_BLOCK_SIZE)
            pr2serr("Neither ibs nor bs given so set ibs=%d (default "
                    "block size)\n", op->ibs);
    }
    op->ibs_hold = op->ibs;
    if (0 == op->bpt_given)
        op->bpt_i = default_bpt_i(op->ibs);

    if ((op->ibs != op->obs) &&
        (0 != ((op->ibs * op->bpt_i) % op->obs))) {
        pr2serr("when 'ibs' and 'obs' differ, ((ibs*bpt)/obs) must have "
                "no remainder (bpt=%d)\n", op->bpt_i);
        return SG_LIB_SYNTAX_ERROR;
    }
    if ((op->skip < 0) || (op->seek < 0)) {
        pr2serr("neither skip nor seek can be negative\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if ((op->oflagp->append > 0) && (op->seek > 0)) {
        pr2serr("Can't use both append and seek switches\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if (op->bpt_i < 1) {
        pr2serr("bpt must be greater than 0\n");
        return SG_LIB_SYNTAX_ERROR;
    }
    if (op->iflagp->append)
        pr2serr("append flag ignored on input\n");
    if (op->iflagp->ignoreew)
        pr2serr("ignoreew flag ignored on input\n");
    if (op->iflagp->nofm)
        pr2serr("nofm flag ignored on input\n");
    if (op->iflagp->prealloc)
        pr2serr("pre-alloc flag ignored on input\n");
    if (op->iflagp->sparing)
        pr2serr("sparing flag ignored on input\n");
    if (op->iflagp->ssync)
        pr2serr("ssync flag ignored on input\n");
    if (op->oflagp->trunc) {
        if (op->oflagp->resume) {
            op->oflagp->trunc = 0;
            if (op->verbose)
                pr2serr("trunc ignored due to resume flag, "
                        "otherwise open_of() truncates too early\n");
        } else if (op->oflagp->append) {
            op->oflagp->trunc = 0;
            pr2serr("trunc ignored due to append flag\n");
        } else if (op->oflagp->sparing) {
            pr2serr("trunc flag conflicts with sparing\n");
            return SG_LIB_SYNTAX_ERROR;
        }
    }
    if (op->iflagp->self || op->oflagp->self) {
        if (! op->oflagp->self)
            ++op->oflagp->self;
        if (op->iflagp->wsame16 || op->oflagp->wsame16) {
            if (! op->oflagp->wsame16)
                ++op->oflagp->wsame16;
            if (! op->oflagp->nowrite)
                ++op->oflagp->nowrite;
        }
        if ('\0' == op->odip->fn[0])
            strcpy(op->odip->fn, op->idip->fn);
        if ((0 == op->seek) && (op->skip > 0)) {
            if (op->ibs == op->obs)
                op->seek = op->skip;
            else if (op->obs > 0) {
                int64_t l;

                l = op->skip * op->ibs;
                op->seek = l / op->obs;
                if ((op->seek * op->obs) != l) {
                    pr2serr("self cannot translate skip to seek "
                            "properly, try different skip value\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
            }
            if (op->verbose)
                pr2serr("self: set seek=%" PRId64 "\n", op->seek);
        }
    }
    if (op->oflagp->wsame16)
        op->oflagp->sparse += 2;
    if (op->oflagp->strunc && (0 == op->oflagp->sparse))
        ++op->oflagp->sparse;

    if (op->has_xcopy) {
        if (! (op->iflagp->xcopy || op->oflagp->xcopy))
            op->iflagp->xcopy = 1;
    }
    if (op->iflagp->xcopy || op->oflagp->xcopy) {
        if (op->iflagp->xcopy && op->oflagp->xcopy) {
            pr2serr("Since xcopy set in both iflag= and oflags"
                    "will send xcopy to if=%s\n", op->idip->fn);
            op->oflagp->xcopy = 0;
        }
        op->has_xcopy = 1;
        op->xc_dc = (op->iflagp->dc || op->oflagp->dc);
        op->xc_cat = (op->iflagp->cat || op->oflagp->cat);
        if (op->iflagp->xcopy) {
            if (! op->iflagp->pt) {
                op->iflagp->pt = 1;
                if (op->verbose)
                    pr2serr("setting pt (pass-through) on IFILE for "
                            "xcopy\n");

            }
        } else {
            if (! op->oflagp->pt) {
                op->oflagp->pt = 1;
                if (op->verbose)
                    pr2serr("setting pt (pass-through) on OFILE for "
                            "xcopy\n");
            }
        }
    }

    if (op->verbose) {      /* report flags used but not supported */
#ifndef SG_LIB_LINUX
        if (op->iflagp->flock || op->oflagp->flock)
            pr2serr("warning: 'flock' flag not supported on this "
                    "platform\n");
#endif

#ifndef HAVE_POSIX_FADVISE
        if (op->iflagp->nocache || op->oflagp->nocache)
            pr2serr("warning: 'nocache' flag not supported on this "
                    "platform\n");
#endif

#if O_SYNC == 0
        if (op->iflagp->sync || op->oflagp->sync)
            pr2serr("warning: 'sync' flag (O_SYNC) not supported on "
                    "this platform\n");
#endif
#if O_DIRECT == 0
        if (op->iflagp->direct || op->oflagp->direct)
            pr2serr("warning: 'direct' flag (O_DIRECT) not supported "
                    "on this platform\n");
#endif
    }
    return 0;
}

/* Process options on the command line. Returns 0 if successful, > 0 for
 * (syntax) error and -1 for early exit (e.g. after '--help') */
static int
cl_process(struct opts_t * op, int argc, char * argv[])
{
    char str[STR_SZ];
    char * key;
    char * buf;
    char * cp;
    int k, n;

    for (k = 1; k < argc; ++k) {
        if (argv[k]) {
            strncpy(str, argv[k], STR_SZ);
            str[STR_SZ - 1] = '\0';
        } else
            continue;
        // replace '=' with null and set buf pointer to following char
        for (key = str, buf = key; *buf && *buf != '=';)
            ++buf;
        if (*buf)
            *buf++ = '\0';
        // check for option names, in alphabetical order
        if (0 == strcmp(key, "bpt")) {
            cp = strchr(buf, ',');
            if (cp)
                *cp = '\0';
            if ((n = sg_get_num(buf)) < 0) {
                pr2serr("bad BPT argument to 'bpt='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (n > 0) {
                op->bpt_i = n;
                op->bpt_given = 1;
            }
            if (cp) {
                n = sg_get_num(cp + 1);
                if (n < 0) {
                    pr2serr("bad OBPC argument to 'bpt='\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
                op->obpc = n;
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
                op->bs_given = 1;
            if ((op->ibs_given) || (op->obs_given)) {
                pr2serr("'bs=' option cannot be combined with "
                        "'ibs=' or 'obs='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->ibs = n;
            op->obs = n;
        } else if (0 == strcmp(key, "cbs"))
            pr2serr("the cbs= option is ignored\n");
        else if (0 == strcmp(key, "cdbsz")) {
            op->iflagp->cdbsz = sg_get_num(buf);
            op->oflagp->cdbsz = op->iflagp->cdbsz;
            op->cdbsz_given = 1;
        } else if (0 == strcmp(key, "coe")) {
            op->iflagp->coe = sg_get_num(buf);
            op->oflagp->coe = op->iflagp->coe;
        } else if (0 == strcmp(key, "coe_limit")) {
            op->coe_limit = sg_get_num(buf);
            if (-1 == op->coe_limit) {
                pr2serr("bad argument to 'coe_limit='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "conv")) {
            if (conv_process(buf, op->iflagp, op->oflagp)) {
                pr2serr("bad argument to 'conv='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "count")) {
            if (0 != strcmp("-1", buf)) {
                op->dd_count = sg_get_llnum(buf);
                if (-1LL == op->dd_count) {
                    pr2serr("bad argument to 'count='\n");
                    return SG_LIB_SYNTAX_ERROR;
                }
            }   /* 'count=-1' is accepted, means calculate count */
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
            ++op->ibs_given;
            op->ibs = n;
        } else if (0 == strcmp(key, "id_usage")) {
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
                pr2serr("bad argument to 'list_id='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (strcmp(key, "if") == 0) {
            if ('\0' != op->idip->fn[0]) {
                pr2serr("Second IFILE argument??\n");
                return SG_LIB_SYNTAX_ERROR;
            } else
                strncpy(op->idip->fn, buf, INOUTF_SZ);
        } else if (0 == strcmp(key, "iflag")) {
            if (flags_process(buf, op->iflagp)) {
                pr2serr("bad argument to 'iflag='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "intio")) {
            op->interrupt_io = sg_get_num(buf);
        } else if (0 == strcmp(key, "iseek")) {
            op->skip = sg_get_llnum(buf);
            if (-1LL == op->skip) {
                pr2serr("bad argument to 'iseek='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "list_id")) {
            n = sg_get_num(buf);
            if (-1 == n || n > 0xff) {
                pr2serr("bad argument to 'list_id='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            op->list_id = (n & 0xff);
            op->list_id_given = 1;
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
            ++op->obs_given;
            op->obs = n;
        } else if (strcmp(key, "of") == 0) {
            if ('\0' != op->odip->fn[0]) {
                pr2serr("Second OFILE argument??\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            strncpy(op->odip->fn, buf, INOUTF_SZ);
            ++op->outf_given;
        } else if (strcmp(key, "of2") == 0) {
            if ('\0' != op->o2dip->fn[0]) {
                pr2serr("Second OFILE2 argument??\n");
                return SG_LIB_SYNTAX_ERROR;
            } else
                strncpy(op->o2dip->fn, buf, INOUTF_SZ);
        } else if (0 == strcmp(key, "oflag")) {
            if (flags_process(buf, op->oflagp)) {
                pr2serr("bad argument to 'oflag='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "oseek")) {
            op->seek = sg_get_llnum(buf);
            if (-1LL == op->seek) {
                pr2serr("bad argument to 'oseek='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
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
            op->iflagp->retries = sg_get_num(buf);
            op->oflagp->retries = op->iflagp->retries;
            if (-1 == op->iflagp->retries) {
                pr2serr("bad argument to 'retries='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "seek")) {
            op->seek = sg_get_llnum(buf);
            if (-1LL == op->seek) {
                pr2serr("bad argument to 'seek='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "skip")) {
            op->skip = sg_get_llnum(buf);
            if (-1LL == op->skip) {
                pr2serr("bad argument to 'skip='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strcmp(key, "status")) {
            if (0 == strncmp(buf, "null", 4))
                ;
            else if (0 == strncmp(buf, "noxfer", 6))
                op->do_time = 0;
            else if (0 == strncmp(buf, "none", 4)) {
                ++op->status_none;
                op->do_time = 0;
            } else {
                pr2serr("'status=' expects 'none', 'noxfer' or 'null'\n");
                return SG_LIB_SYNTAX_ERROR;
            }
        } else if (0 == strncmp(key, "verb", 4)) {
            op->verbose = sg_get_num(buf);
            if ((-1 == op->verbose) && ('-' != buf[0])) {
                pr2serr("bad argument to 'verbose='\n");
                return SG_LIB_SYNTAX_ERROR;
            }
            if (op->verbose < 0) {
                ++op->quiet;
                op->verbose = 0;
            }
        } else if (0 == strncmp(key, "--verb", 6))
            ++op->verbose;
        else if (0 == strncmp(key, "-vvvv", 5))
            op->verbose += 4;
        else if (0 == strncmp(key, "-vvv", 4))
            op->verbose += 3;
        else if (0 == strncmp(key, "-vv", 3))
            op->verbose += 2;
        else if (0 == strncmp(key, "-v", 2))
            ++op->verbose;
        else if (0 == strncmp(key, "-hh", 3))
            op->do_help += 2;
        else if ((0 == strncmp(key, "--help", 7)) ||
                 (0 == strncmp(key, "-h", 2)) ||
                 (0 == strcmp(key, "-?")))
            ++op->do_help;
        else if ((0 == strncmp(key, "--vers", 6)) ||
                 (0 == strncmp(key, "-V", 2))) {
            pr2serr("%s\n", version_str);
            return -1;
        }
#ifdef SG_LIB_WIN32
        else if (0 == strncmp(key, "--wscan", 7))
            ++op->wscan;
        else if (0 == strncmp(key, "-wwww", 5))
            op->wscan += 4;
        else if (0 == strncmp(key, "-www", 4))
            op->wscan += 3;
        else if (0 == strncmp(key, "-ww", 3))
            op->wscan += 2;
        else if (0 == strncmp(key, "-w", 2))
            ++op->wscan;
#endif
        else if ((0 == strncmp(key, "--xcopy", 7)) ||
                 (0 == strncmp(key, "-x", 2)))
            ++op->has_xcopy;
        else {
            pr2serr("Unrecognized option '%s'\n", key);
            pr2serr("For more information use '--help'\n");
            return SG_LIB_SYNTAX_ERROR;
        }
    }
    return cl_sanity_defaults(op);
}

/* Attempt to categorize the file type from the given filename.
 * Separate version for Windows and Unix. Windows version does some
 * file name processing. */
#ifndef SG_LIB_WIN32

#ifdef SG_LIB_LINUX
static int bsg_major_checked = 0;
static int bsg_major = 0;

/* In Linux search /proc/devices for bsg character driver in order to
 * find its major device number since it is allocated dynamically.  */
static void
find_bsg_major(int verbose)
{
    const char * proc_devices = "/proc/devices";
    FILE *fp;
    char a[128];
    char b[128];
    char * cp;
    int n;

    if (NULL == (fp = fopen(proc_devices, "r"))) {
        if (verbose)
            pr2serr("fopen %s failed: %s\n", proc_devices,
                    strerror(errno));
        return;
    }
    while ((cp = fgets(b, sizeof(b), fp))) {
        if ((1 == sscanf(b, "%s", a)) &&
            (0 == memcmp(a, "Character", 9)))
            break;
    }
    while (cp && (cp = fgets(b, sizeof(b), fp))) {
        if (2 == sscanf(b, "%d %s", &n, a)) {
            if (0 == strcmp("bsg", a)) {
                bsg_major = n;
                break;
            }
        } else
            break;
    }
    if (verbose > 5) {
        if (cp)
            pr2serr("found bsg_major=%d\n", bsg_major);
        else
            pr2serr("found no bsg char device in %s\n", proc_devices);
    }
    fclose(fp);
}
#endif

/* Categorize file by using the stat() system call on its filename.
 * If not found FT_ERROR returned. The FT_* constants are a bit mask
 * and later logic can combine them (e.g. FT_BLOCK | FT_PT).
 */
static int
dd_filetype(const char * filename, int verbose)
{
    struct stat st;
    size_t len = strlen(filename);

    if (verbose) { ; }    /* suppress warning */
    if ((1 == len) && ('.' == filename[0]))
        return FT_DEV_NULL;
    if (stat(filename, &st) < 0)
        return FT_ERROR;
    if (S_ISREG(st.st_mode)) {
        // pr2serr("dd_filetype: regular file, st_size=%" PRId64 "\n",
        //         st.st_size);
        return FT_REG;
    } else if (S_ISCHR(st.st_mode)) {
#ifdef SG_LIB_LINUX
        /* major() and minor() defined in sys/sysmacros.h */
        if ((MEM_MAJOR == major(st.st_rdev)) &&
            (DEV_NULL_MINOR_NUM == minor(st.st_rdev)))
            return FT_DEV_NULL;
        if (SCSI_GENERIC_MAJOR == major(st.st_rdev))
            return FT_PT;
        if (SCSI_TAPE_MAJOR == major(st.st_rdev))
            return FT_TAPE;
        if (! bsg_major_checked) {
            bsg_major_checked = 1;
            find_bsg_major(verbose);
        }
        if (bsg_major == (int)major(st.st_rdev))
            return FT_PT;
        return FT_CHAR; /* assume something like /dev/zero */
#elif SG_LIB_FREEBSD
        {
            /* int d_flags;  for FIOFTYPE ioctl see sys/filio.h */
            char s[STR_SZ];
            char * bname;

            strcpy(s, filename);
            bname = basename(s);
            if (0 == strcmp("null", bname))
                return FT_DEV_NULL;
            else if (0 == memcmp("pass", bname, 4))
                return FT_PT;
            else if (0 == memcmp("sa", bname, 2))
                return FT_TAPE;
            else
                return FT_BLOCK;  /* freebsd doesn't have block devices! */
        }
#elif SG_LIB_SOLARIS
        /* might be /dev/rdsk or /dev/scsi , require pt override */
        return FT_BLOCK;
#else
        return FT_PT;
#endif
    } else if (S_ISBLK(st.st_mode))
        return FT_BLOCK;
    else if (S_ISFIFO(st.st_mode))
        return FT_FIFO;
    return FT_OTHER;
}
#endif

static char *
dd_filetype_str(int ft, char * buff, int max_bufflen, const char * fname)
{
    int off = 0;

    if (FT_DEV_NULL & ft)
        off += my_snprintf(buff + off, max_bufflen - off, "null device ");
    if (FT_PT & ft)
        off += my_snprintf(buff + off, max_bufflen - off,
                           "pass-through [pt] device ");
    if (FT_TAPE & ft)
        off += my_snprintf(buff + off, max_bufflen - off, "SCSI tape device ");
    if (FT_BLOCK & ft)
        off += my_snprintf(buff + off, max_bufflen - off, "block device ");
    if (FT_FIFO & ft)
        off += my_snprintf(buff + off, max_bufflen - off,
                           "fifo [stdin, stdout, named pipe] ");
    if (FT_REG & ft)
        off += my_snprintf(buff + off, max_bufflen - off, "regular file ");
    if (FT_CHAR & ft)
        off += my_snprintf(buff + off, max_bufflen - off, "char device ");
    if (FT_OTHER & ft)
        off += my_snprintf(buff + off, max_bufflen - off, "other file type ");
    if (FT_ERROR & ft)
        off += my_snprintf(buff + off, max_bufflen - off,
                           "unable to 'stat' %s ", (fname ? fname : "file"));
    return buff;
}

/* get_blkdev_capacity() returns 0 -> success or -1 -> failure.
 * which_arg should either be DDPT_ARG_IN, DDPT_ARG_OUT or DDPT_ARG_OUT2.
 * If successful writes back sector size (logical block
 * size) using the sect_sz * pointer. Also writes back the number of
 * sectors (logical blocks) on the block device using num_sect pointer. */

#ifdef SG_LIB_LINUX
static int
get_blkdev_capacity(struct opts_t * op, int which_arg, int64_t * num_sect,
                    int * sect_sz)
{
    int blk_fd;
    const char * fname;

    blk_fd = (DDPT_ARG_IN == which_arg) ? op->idip->fd : op->odip->fd;
    fname = (DDPT_ARG_IN == which_arg) ? op->idip->fn : op->odip->fn;
    if (op->verbose > 2)
        pr2serr("get_blkdev_capacity: for %s\n", fname);
    /* BLKGETSIZE64, BLKGETSIZE and BLKSSZGET macros problematic (from
     *  <linux/fs.h> or <sys/mount.h>). */
#ifdef BLKSSZGET
    if ((ioctl(blk_fd, BLKSSZGET, sect_sz) < 0) && (*sect_sz > 0)) {
        perror("BLKSSZGET ioctl error");
        return -1;
    } else {
 #ifdef BLKGETSIZE64
        uint64_t ull;

        if (ioctl(blk_fd, BLKGETSIZE64, &ull) < 0) {

            perror("BLKGETSIZE64 ioctl error");
            return -1;
        }
        *num_sect = ((int64_t)ull / (int64_t)*sect_sz);
        if (op->verbose > 5)
            pr2serr("Used Linux BLKGETSIZE64 ioctl\n");
 #else
        unsigned long ul;

        if (ioctl(blk_fd, BLKGETSIZE, &ul) < 0) {
            perror("BLKGETSIZE ioctl error");
            return -1;
        }
        *num_sect = (int64_t)ul;
        if (op->verbose > 5)
            pr2serr("Used Linux BLKGETSIZE ioctl\n");
 #endif
    }
    return 0;
#else   /* not BLKSSZGET */
    blk_fd = blk_fd;
    if (op->verbose)
        pr2serr("      BLKSSZGET+BLKGETSIZE ioctl not available\n");
    *num_sect = 0;
    *sect_sz = 0;
    return -1;
#endif
}
#endif  /* BLKSSZGET */

#ifdef SG_LIB_FREEBSD
static int
get_blkdev_capacity(struct opts_t * op, int which_arg, int64_t * num_sect,
                    int * sect_sz)
{
// Why do kernels invent their own typedefs and not use C standards?
#define u_int unsigned int
    off_t mediasize;
    unsigned int sectorsize;
    int blk_fd;
    const char * fname;

    blk_fd = (DDPT_ARG_IN == which_arg) ? op->idip->fd : op->odip->fd;
    fname = (DDPT_ARG_IN == which_arg) ? op->idip->fn : op->odip->fn;
    if (op->verbose > 2)
        pr2serr("get_blkdev_capacity: for %s\n", fname);

    /* For FreeBSD post suggests that /usr/sbin/diskinfo uses
     * ioctl(fd, DIOCGMEDIASIZE, &mediasize), where mediasize is an off_t.
     * also: ioctl(fd, DIOCGSECTORSIZE, &sectorsize) */
    if (ioctl(blk_fd, DIOCGSECTORSIZE, &sectorsize) < 0) {
        perror("DIOCGSECTORSIZE ioctl error");
        return -1;
    }
    *sect_sz = sectorsize;
    if (ioctl(blk_fd, DIOCGMEDIASIZE, &mediasize) < 0) {
        perror("DIOCGMEDIASIZE ioctl error");
        return -1;
    }
    if (sectorsize)
        *num_sect = mediasize / sectorsize;
    else
        *num_sect = 0;
    return 0;
}
#endif

#ifdef SG_LIB_SOLARIS
static int
get_blkdev_capacity(struct opts_t * op, int which_arg, int64_t * num_sect,
                    int * sect_sz)
{
    struct dk_minfo info;
    int blk_fd;
    const char * fname;

    blk_fd = (DDPT_ARG_IN == which_arg) ? op->idip->fd : op->odip->fd;
    fname = (DDPT_ARG_IN == which_arg) ? op->idip->fn : op->odip->fn;
    if (op->verbose > 2)
        pr2serr("get_blkdev_capacity: for %s\n", fname);

    /* this works on "char" block devs (e.g. in /dev/rdsk) but not /dev/dsk */
    if (ioctl(blk_fd, DKIOCGMEDIAINFO , &info) < 0) {
        perror("DKIOCGMEDIAINFO ioctl error");
        *num_sect = 0;
        *sect_sz = 0;
        return -1;
    }
    *num_sect = info.dki_capacity;
    *sect_sz = info.dki_lbsize;
    return 0;
}
#endif

void
zero_coe_limit_count(struct opts_t * op)
{
    if (op->coe_limit > 0)
        op->coe_count = 0;
}

/* Print number of blocks, block size. If over 1 MB print size in MB
 * (10**6 bytes), GB (10**9 bytes) or TB (10**12 bytes) to stderr. */
static void
print_blk_sizes(const char * fname, const char * access_typ, int64_t num_sect,
                int sect_sz)
{
    int mb, gb, tb;
    size_t len;
    int64_t n = 0;
    char b[32];
    char dec[4];

    if (num_sect <= 0) {
        pr2serr("  %s [%s]: blocks=%" PRId64 ", _bs=%d\n", fname, access_typ,
                num_sect, sect_sz);
        return;
    }
    gb = 0;
    if ((num_sect > 0) && (sect_sz > 0)) {
        n = num_sect * sect_sz;
        gb = n / 1000000000;
    }
    if (gb > 999999) {
        tb = gb / 1000;
        snprintf(b, sizeof(b), "%d", tb);
        len = strlen(b); // len must be >= 4
        dec[0] = b[len - 3];
        dec[1] = b[len - 2];
        dec[2] = '\0';
        b[len - 3] = '\0';
        pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                "_bs=%d, %s.%s PB\n", fname, access_typ, num_sect,
                num_sect, sect_sz, b, dec);
    } else if (gb > 99999) {
        tb = gb / 1000;
        pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                "_bs=%d, %d TB\n", fname, access_typ, num_sect,
                num_sect, sect_sz, tb);
    } else {
        mb = n / 1000000;
        if (mb > 999999) {
            gb = mb / 1000;
            snprintf(b, sizeof(b), "%d", gb);
            len = strlen(b); // len must be >= 4
            dec[0] = b[len - 3];
            dec[1] = b[len - 2];
            dec[2] = '\0';
            b[len - 3] = '\0';
            pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "_bs=%d, %s.%s TB\n", fname, access_typ, num_sect,
                    num_sect, sect_sz, b, dec);
        } else if (mb > 99999) {
            gb = mb / 1000;
            pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "_bs=%d, %d GB\n", fname, access_typ, num_sect,
                    num_sect, sect_sz, gb);
        } else if (mb > 999) {
            snprintf(b, sizeof(b), "%d", mb);
            len = strlen(b); // len must be >= 4
            dec[0] = b[len - 3];
            dec[1] = b[len - 2];
            dec[2] = '\0';
            b[len - 3] = '\0';
            pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "_bs=%d, %s.%s GB\n", fname, access_typ, num_sect,
                    num_sect, sect_sz, b, dec);
        } else if (mb > 0) {
            pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "_bs=%d, %d MB%s\n", fname, access_typ, num_sect,
                    num_sect, sect_sz, mb, ((mb < 10) ? " approx" : ""));
        } else
            pr2serr("  %s [%s]: blocks=%" PRId64 " [0x%" PRIx64 "], "
                    "_bs=%d\n", fname, access_typ, num_sect, num_sect,
                    sect_sz);
    }
}

static void
calc_duration_init(struct opts_t * op)
{
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    if (op->do_time) {
        op->start_tm.tv_sec = 0;
        op->start_tm.tv_nsec = 0;
        if (0 == clock_gettime(CLOCK_MONOTONIC, &op->start_tm))
            op->start_tm_valid = 1;
    }
#elif defined(HAVE_GETTIMEOFDAY)
    if (op->do_time) {
        op->start_tm.tv_sec = 0;
        op->start_tm.tv_usec = 0;
        gettimeofday(&op->start_tm, NULL);
        op->start_tm_valid = 1;
    }
#else
    if (op) { ; }
#endif
}

/* Calculates transfer throughput, typically in Megabytes per second.
 * A megabyte in this context is 1000000 bytes (gives bigger numbers so
 * is preferred by industry). The clock_gettime() interface is preferred
 * since time is guaranteed to advance; gettimeofday() is impacted if the
 * user (or something like ntpd) changes the time.
 * Also if the transfer is large enough and isn't about to finish, it
 * makes an estimate of the time remaining. */
static void
calc_duration_throughput(const char * leadin, int contin, struct opts_t * op)
{
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
    struct timespec end_tm, res_tm;
    double a, b, r;
    int secs, h, m;
    int64_t blks;

    if (op->start_tm_valid && (op->start_tm.tv_sec || op->start_tm.tv_nsec)) {
        blks = op->in_full;
        clock_gettime(CLOCK_MONOTONIC, &end_tm);
        res_tm.tv_sec = end_tm.tv_sec - op->start_tm.tv_sec;
        res_tm.tv_nsec = end_tm.tv_nsec - op->start_tm.tv_nsec;
        if (res_tm.tv_nsec < 0) {
            --res_tm.tv_sec;
            res_tm.tv_nsec += 1000000000;
        }
        a = res_tm.tv_sec;
        a += (0.000001 * (res_tm.tv_nsec / 1000));
        b = (double)op->ibs_hold * blks;
        pr2serr("%stime to %s data%s: %d.%06d secs", leadin,
                (op->read1_or_transfer ? "read" : "transfer"),
                (contin ? " so far" : ""), (int)res_tm.tv_sec,
                (int)(res_tm.tv_nsec / 1000));
        r = 0.0;
        if ((a > 0.00001) && (b > 511)) {
            r = b / (a * 1000000.0);
            if (r < 1.0)
                pr2serr(" at %.1f KB/sec\n", r * 1000);
            else
                pr2serr(" at %.2f MB/sec\n", r);
        } else
            pr2serr("\n");
        if (contin && (! op->reading_fifo) && (r > 0.01) &&
            (op->dd_count > 100)) {
            secs = (int)(((double)op->ibs_hold * op->dd_count) /
                         (r * 1000000));
            if (secs > 10) {
                h = secs / 3600;
                secs = secs - (h * 3600);
                m = secs / 60;
                secs = secs - (m * 60);
                if (h > 0)
                    pr2serr("%sestimated time remaining: %d:%02d:%02d\n",
                            leadin, h, m, secs);
                else
                    pr2serr("%sestimated time remaining: %d:%02d\n",
                            leadin, m, secs);
            }
        }
    }
#elif defined(HAVE_GETTIMEOFDAY)
    struct timeval end_tm, res_tm;
    double a, b, r;
    int secs, h, m;
    int64_t blks;

    if (op->start_tm_valid && (op->start_tm.tv_sec || op->start_tm.tv_usec)) {
        blks = op->in_full;
        gettimeofday(&end_tm, NULL);
        res_tm.tv_sec = end_tm.tv_sec - op->start_tm.tv_sec;
        res_tm.tv_usec = end_tm.tv_usec - op->start_tm.tv_usec;
        if (res_tm.tv_usec < 0) {
            --res_tm.tv_sec;
            res_tm.tv_usec += 1000000;
        }
        a = res_tm.tv_sec;
        a += (0.000001 * res_tm.tv_usec);
        b = (double)op->ibs_hold * blks;
        pr2serr("%stime to %s data%s: %d.%06d secs", leadin,
                (op->read1_or_transfer ? "read" : "transfer"),
                (contin ? " so far" : ""), (int)res_tm.tv_sec,
                (int)res_tm.tv_usec);
        r = 0.0;
        if ((a > 0.00001) && (b > 511)) {
            r = b / (a * 1000000.0);
            if (r < 1.0)
                pr2serr(" at %.1f KB/sec\n", r * 1000);
            else
                pr2serr(" at %.2f MB/sec\n", r);
        } else
            pr2serr("\n");
        if (contin && (! op->reading_fifo) && (r > 0.01) &&
            (op->dd_count > 100)) {
            secs = (int)(((double)op->ibs_hold * op->dd_count) /
                         (r * 1000000));
            if (secs > 10) {
                h = secs / 3600;
                secs = secs - (h * 3600);
                m = secs / 60;
                secs = secs - (m * 60);
                if (h > 0)
                    pr2serr("%sestimated time remaining: "
                            "%d:%02d:%02d\n", leadin, h, m, secs);
                else
                    pr2serr("%sestimated time remaining: "
                            "%d:%02d\n", leadin, m, secs);
            }
        }
    }
#else   /* no clock reading functions available */
    if (leadin) { ; }    // suppress warning
    if (contin) { ; }    // suppress warning
#endif
}

/* Returns open input file descriptor (>= 0) or a negative value
 * (-SG_LIB_FILE_ERROR or -SG_LIB_CAT_OTHER) if error.
 */
static int
open_if(struct opts_t * op)
{
    int flags;
    int fd = -SG_LIB_FILE_ERROR;
    char ebuff[EBUFF_SZ];
    struct flags_t * ifp = op->iflagp;
    struct dev_info_t * idip = op->idip;
    const char * ifn = idip->fn;

    idip->d_type = dd_filetype(ifn, op->verbose);
    if (FT_ERROR & idip->d_type) {
        pr2serr("unable to access %s\n", ifn);
        goto file_err;
    } else if (((FT_BLOCK | FT_TAPE | FT_OTHER) & idip->d_type) && ifp->pt)
        idip->d_type |= FT_PT;
    if (op->verbose)
        pr2serr(" >> Input file type: %s\n",
                dd_filetype_str(idip->d_type, ebuff, EBUFF_SZ, ifn));
    if (!(FT_PT & idip->d_type) && op->rdprotect)
        pr2serr("rdprotect ignored on non-pt device\n");
    if ((FT_FIFO | FT_CHAR | FT_TAPE) & idip->d_type)
        ++op->reading_fifo;

    if ((FT_TAPE & idip->d_type) && (FT_PT & idip->d_type)) {
        pr2serr("SCSI tape device %s not supported via pt\n", ifn);
        goto file_err;
    }
    if (FT_PT & idip->d_type) {
        fd = pt_open_if(op);
        if (-1 == fd)
            goto file_err;
        else if (fd < -1)
            goto other_err;
    }
#ifdef SG_LIB_WIN32
    else if (FT_BLOCK & idip->d_type) {
        if (win32_open_if(op, (ifp->excl ? O_EXCL : 0), op->verbose))
            goto file_err;
        fd = 0;
    }
#endif
    else {
        flags = O_RDONLY;
        if (ifp->direct)
            flags |= O_DIRECT;
        if (ifp->excl)
            flags |= O_EXCL;
        if (ifp->sync)
            flags |= O_SYNC;
        fd = open(ifn, flags);
        if (fd < 0) {
            pr2serr("could not open %s for reading: %s\n", ifn,
                    safe_strerror(errno));
            goto file_err;
        } else {
            if (sg_set_binary_mode(fd) < 0)
                perror("sg_set_binary_mode");
            if (op->verbose)
                pr2serr("        open %s, flags=0x%x\n", ifn, flags);
#ifdef HAVE_POSIX_FADVISE
            if (ifp->nocache) {
                int rt;

                rt = posix_fadvise(fd, 0, 0, POSIX_FADV_SEQUENTIAL);
                if (rt)
                    pr2serr("open_if: posix_fadvise(SEQUENTIAL), err=%d\n",
                            rt);
            }
#endif
        }
    }
#ifdef SG_LIB_LINUX
    if (ifp->flock) {
        int res;

        res = flock(fd, LOCK_EX | LOCK_NB);
        if (res < 0) {
            close(fd);
            pr2serr("flock(LOCK_EX | LOCK_NB) on %s failed: %s\n",
                    ifn, safe_strerror(errno));
            return -SG_LIB_FLOCK_ERR;
        }
    }
#endif
    return fd;

file_err:
    return -SG_LIB_FILE_ERROR;
other_err:
    return -SG_LIB_CAT_OTHER;
}

/* Returns open output file descriptor (>= 0), -1 for don't
 * bother opening (e.g. /dev/null), or a more negative value
 * (-SG_LIB_FILE_ERROR or -SG_LIB_CAT_OTHER) if error.
 */
static int
open_of(struct opts_t * op)
{
    int flags;
    int fd = -SG_LIB_FILE_ERROR;
    int outf_exists = 0;
    char ebuff[EBUFF_SZ];
    struct stat st;
    struct flags_t * ofp = op->oflagp;
    struct dev_info_t * odip = op->odip;
    const char * ofn = odip->fn;

    odip->d_type = dd_filetype(ofn, op->verbose);
    if (((FT_BLOCK | FT_TAPE | FT_OTHER) & odip->d_type) && ofp->pt)
        odip->d_type |= FT_PT;
    odip->d_type_hold = odip->d_type;
    if (op->verbose)
        pr2serr(" >> Output file type: %s\n",
                dd_filetype_str(odip->d_type, ebuff, EBUFF_SZ, ofn));
    if (!(FT_PT & odip->d_type) && op->wrprotect)
        pr2serr("wrprotect ignored on non-pt device\n");

    if ((FT_TAPE & odip->d_type) && (FT_PT & odip->d_type)) {
        pr2serr("SCSI tape device %s not supported via pt\n", ofn);
        goto file_err;
    }
    if (FT_PT & odip->d_type) {
        fd = pt_open_of(op);
        if (-1 == fd)
            goto file_err;
        else if (fd < -1)
            goto other_err;
    } else if (FT_DEV_NULL & odip->d_type)
        fd = -1; /* don't bother opening */
#ifdef SG_LIB_WIN32
    else if (FT_BLOCK & odip->d_type) {
        if (win32_open_of(op, (ofp->excl ? O_EXCL : 0), op->verbose))
            goto file_err;
        fd = 0;
    }
#endif
    else {      /* typically regular file or block device node */
        int needs_ftruncate = 0;
        int64_t offset = 0;

        memset(&st, 0, sizeof(st));
        if (0 == stat(ofn, &st))
            outf_exists = 1;
        else if (ofp->pt) {
            /* if oflag=pt, then creating a regular file is unhelpful */
            pr2serr("Cannot create a regular file called %s as a pt\n", ofn);
            goto other_err;
        }
        flags = ofp->sparing ? O_RDWR : O_WRONLY;
        if (0 == outf_exists)
            flags |= O_CREAT;
        if (ofp->direct)
            flags |= O_DIRECT;
        if (ofp->excl)
            flags |= O_EXCL;
        if (ofp->sync)
            flags |= O_SYNC;
        if (ofp->append)
            flags |= O_APPEND;
        if ((FT_REG & odip->d_type) && outf_exists && ofp->trunc &&
            (! ofp->nowrite)) {
            if (op->seek > 0) {
                offset = op->seek * op->obs;
                if (st.st_size > offset)
                    ++needs_ftruncate;  // only truncate to shorten
            } else
                flags |= O_TRUNC;
        }
        if ((fd = open(ofn, flags, 0666)) < 0) {
            pr2serr("could not open %s for writing: %s\n", ofn,
                    safe_strerror(errno));
            goto file_err;
        }
        if (needs_ftruncate && (offset > 0)) {
            if (ftruncate(fd, offset) < 0) {
                pr2serr("could not ftruncate %s after open (seek): %s\n",
                        ofn, safe_strerror(errno));
                goto file_err;
            }
            /* N.B. file offset (pointer) not changed by ftruncate */
        }
        if ((! outf_exists) && (FT_ERROR & odip->d_type)) {
            odip->d_type = FT_REG;   /* exists now */
            odip->d_type_hold = odip->d_type;
        }
        if (sg_set_binary_mode(fd) < 0)
            perror("sg_set_binary_mode");
        if (op->verbose) {
            pr2serr("        %s %s, flags=0x%x\n",
                    (outf_exists ? "open" : "create"), ofn, flags);
            if (needs_ftruncate && (offset > 0))
                pr2serr("        truncated file at byte offset "
                        "%" PRId64 " \n", offset);
        }
    }
#ifdef SG_LIB_LINUX
    if (ofp->flock) {
        int res;

        res = flock(fd, LOCK_EX | LOCK_NB);
        if (res < 0) {
            close(fd);
            pr2serr("flock(LOCK_EX | LOCK_NB) on %s failed: %s\n",
                    ofn, safe_strerror(errno));
            return -SG_LIB_FLOCK_ERR;
        }
    }
#endif
    return fd;

file_err:
    return -SG_LIB_FILE_ERROR;
other_err:
    return -SG_LIB_CAT_OTHER;
}

/* Helper for calc_count(). Attempts to size IFILE. Returns 0 if no error
 * detected. */
static int
calc_count_in(struct opts_t * op, int64_t * in_num_sectp)
{
    int res;
    struct stat st;
    int64_t num_sect, t;
    int in_sect_sz, sect_sz, in_type;
    const char * ifn = op->idip->fn;

    *in_num_sectp = -1;
    in_type = op->idip->d_type;
    if (FT_PT & in_type) {
        if (op->iflagp->norcap) {
            if ((FT_BLOCK & in_type) && (0 == op->iflagp->force)) {
                pr2serr(">> warning: norcap on input block device "
                        "accessed via pt is risky.\n");
                pr2serr(">> Abort copy, use iflag=force to override.\n");
                return -1;
            }
            return 0;
        }
        res = pt_read_capacity(op, 0, in_num_sectp, &in_sect_sz);
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            pr2serr("Unit attention (readcap in), continuing\n");
            res = pt_read_capacity(op, 0, in_num_sectp, &in_sect_sz);
        } else if (SG_LIB_CAT_ABORTED_COMMAND == res) {
            pr2serr("Aborted command (readcap in), continuing\n");
            res = pt_read_capacity(op, 0, in_num_sectp, &in_sect_sz);
        }
        if (0 != res) {
            if (res == SG_LIB_CAT_INVALID_OP)
                pr2serr("read capacity not supported on %s\n", ifn);
            else if (res == SG_LIB_CAT_NOT_READY)
                pr2serr("read capacity failed on %s - not ready\n", ifn);
            else
                pr2serr("Unable to read capacity on %s\n", ifn);
            *in_num_sectp = -1;
            return res;
        } else {
            if (op->verbose) {
                print_blk_sizes(ifn, "pt", *in_num_sectp, in_sect_sz);
                if (op->idip->prot_type > 0)
                    pr2serr("    reports Protection_type=%d, p_i_exp=%d\n",
                            op->idip->prot_type, op->idip->p_i_exp);
            }
            if ((*in_num_sectp > 0) && (in_sect_sz != op->ibs)) {
                pr2serr(">> warning: %s block size confusion: ibs=%d, "
                        "device claims=%d\n", ifn, op->ibs, in_sect_sz);
                if (0 == op->iflagp->force) {
                    pr2serr(">> abort copy, use iflag=force to override\n");
                    return -1;
                }
            }
        }
        if ((FT_BLOCK & in_type) && (0 == op->iflagp->force) &&
            (0 == get_blkdev_capacity(op, DDPT_ARG_IN, &num_sect,
                                      &sect_sz))) {
            t = (*in_num_sectp) * in_sect_sz;
            if (t != (num_sect * sect_sz)) {
                pr2serr(">> warning: Size of input block device is "
                        "different from pt size.\n>> Pass-through on block "
                        "partition can give unexpected offsets.\n");
                pr2serr(">> Abort copy, use iflag=force to override.\n");
                return -1;
            }
        }
    } else if ((op->dd_count > 0) && (0 == op->oflagp->resume))
        return 0;
    else if (FT_BLOCK & in_type) {
        if (0 != get_blkdev_capacity(op, DDPT_ARG_IN, in_num_sectp,
                                     &in_sect_sz)) {
            pr2serr("Unable to read block capacity on %s\n", ifn);
            *in_num_sectp = -1;
        }
        if (op->verbose)
            print_blk_sizes(ifn, "blk", *in_num_sectp, in_sect_sz);
        if ((*in_num_sectp > 0) && (op->ibs != in_sect_sz)) {
            pr2serr(">> warning: %s block size confusion: bs=%d, "
                    "device claims=%d\n", ifn, op->ibs, in_sect_sz);
            *in_num_sectp = -1;
        }
    } else if (FT_REG & in_type) {
        if (fstat(op->idip->fd, &st) < 0) {
            perror("fstat(idip->fd) error");
            *in_num_sectp = -1;
        } else {
            *in_num_sectp = st.st_size / op->ibs;
            res = st.st_size % op->ibs;
            if (op->verbose) {
                print_blk_sizes(ifn, "reg", *in_num_sectp, op->ibs);
                if (res)
                    pr2serr("    residual_bytes=%d\n", res);
            }
            if (res)
                ++*in_num_sectp;
        }
    }
    return 0;
}

/* Helper for calc_count(). Attempts to size OFILE. Returns 0 if no error
 * detected. */
static int
calc_count_out(struct opts_t * op, int64_t * out_num_sectp)
{
    int res;
    struct stat st;
    int64_t num_sect, t;
    int out_sect_sz, sect_sz, out_type;
    const char * ofn = op->odip->fn;

    *out_num_sectp = -1;
    out_type = op->odip->d_type;
    if (FT_PT & out_type) {
        if (op->oflagp->norcap) {
            if ((FT_BLOCK & out_type) && (0 == op->oflagp->force)) {
                pr2serr(">> warning: norcap on output block device "
                        "accessed via pt is risky.\n");
                pr2serr(">> Abort copy, use oflag=force to override.\n");
                return -1;
            }
            return 0;
        }
        res = pt_read_capacity(op, 1, out_num_sectp, &out_sect_sz);
        if (SG_LIB_CAT_UNIT_ATTENTION == res) {
            pr2serr("Unit attention (readcap out), continuing\n");
            res = pt_read_capacity(op, 1, out_num_sectp, &out_sect_sz);
        } else if (SG_LIB_CAT_ABORTED_COMMAND == res) {
            pr2serr("Aborted command (readcap out), continuing\n");
            res = pt_read_capacity(op, 1, out_num_sectp, &out_sect_sz);
        }
        if (0 != res) {
            if (res == SG_LIB_CAT_INVALID_OP)
                pr2serr("read capacity not supported on %s\n", ofn);
            else
                pr2serr("Unable to read capacity on %s\n", ofn);
            *out_num_sectp = -1;
            return res;
        } else {
            if (op->verbose) {
                print_blk_sizes(ofn, "pt", *out_num_sectp, out_sect_sz);
                if (op->odip->prot_type > 0)
                    pr2serr("    reports Protection_type=%d, p_i_exp=%d\n",
                            op->odip->prot_type, op->odip->p_i_exp);
            }
            if ((*out_num_sectp > 0) && (op->obs != out_sect_sz)) {
                pr2serr(">> warning: %s block size confusion: "
                        "obs=%d, device claims=%d\n", ofn, op->obs,
                        out_sect_sz);
                if (0 == op->oflagp->force) {
                    pr2serr(">> abort copy, use oflag=force to override\n");
                    return -1;
                }
            }
        }
        if ((FT_BLOCK & out_type) && (0 == op->oflagp->force) &&
             (0 == get_blkdev_capacity(op, DDPT_ARG_OUT, &num_sect,
                                       &sect_sz))) {
            t = (*out_num_sectp) * out_sect_sz;
            if (t != (num_sect * sect_sz)) {
                pr2serr(">> warning: size of output block device is "
                        "different from pt size.\n>> Pass-through on block "
                        "partition can give unexpected results.\n");
                pr2serr(">> abort copy, use oflag=force to override\n");
                return -1;
            }
        }
    } else if ((op->dd_count > 0) && (0 == op->oflagp->resume))
        return 0;
    if (FT_BLOCK & out_type) {
        if (0 != get_blkdev_capacity(op, DDPT_ARG_OUT, out_num_sectp,
                                     &out_sect_sz)) {
            pr2serr("Unable to read block capacity on %s\n", ofn);
            *out_num_sectp = -1;
        } else {
            if (op->verbose)
                print_blk_sizes(ofn, "blk", *out_num_sectp, out_sect_sz);
            if ((*out_num_sectp > 0) && (op->obs != out_sect_sz)) {
                pr2serr(">> warning: %s block size confusion: obs=%d, "
                        "device claims=%d\n", ofn, op->obs, out_sect_sz);
                *out_num_sectp = -1;
            }
        }
    } else if (FT_REG & out_type) {
        if (fstat(op->odip->fd, &st) < 0) {
            perror("fstat(odip->fd) error");
            *out_num_sectp = -1;
        } else {
            *out_num_sectp = st.st_size / op->obs;
            res = st.st_size % op->obs;
            if (op->verbose) {
                print_blk_sizes(ofn, "reg", *out_num_sectp, op->obs);
                if (res)
                    pr2serr("    residual_bytes=%d\n", res);
            }
            if (res)
                ++*out_num_sectp;
        }
    }
    return 0;
}


/* Calculates the number of blocks associated with the in and out files.
 * May also yield the block size in bytes of devices. For regular files
 * uses ibs or obs as the block (sector) size. Returns 0 for continue,
 * otherwise bypass copy and exit. */
static int
calc_count(struct opts_t * op, int64_t * in_num_sectp,
           int64_t * out_num_sectp)
{
    int res;

    res = calc_count_in(op, in_num_sectp);
    if (res) {
        *out_num_sectp = -1;
        return res;
    }
    return calc_count_out(op, out_num_sectp);
}

#ifdef HAVE_POSIX_FADVISE
/* Used by iflag=nocache and oflag=nocache to suggest (via posix_fadvise()
 * system call) that the OS doesn't cache data it has just read or written
 * since it is unlikely to be used again in the short term. iflag=nocache
 * additionally increases the read-ahead. Errors ignored. */
static void
do_fadvise(struct opts_t * op, int bytes_if, int bytes_of, int bytes_of2)
{
    int rt, in_valid, out2_valid, out_valid, id_type, od_type, o2d_type;

    id_type = op->idip->d_type;
    od_type = op->odip->d_type;
    o2d_type = op->o2dip->d_type;
    in_valid = ((FT_REG == id_type) || (FT_BLOCK == id_type));
    out2_valid = ((FT_REG == o2d_type) || (FT_BLOCK == o2d_type));
    out_valid = ((FT_REG == od_type) || (FT_BLOCK == od_type));
    if (op->iflagp->nocache && (bytes_if > 0) && in_valid) {
        if ((op->lowest_skip < 0) || (op->skip > op->lowest_skip))
            op->lowest_skip = op->skip;
        rt = posix_fadvise(op->idip->fd, (op->lowest_skip * op->ibs),
                           ((op->skip - op->lowest_skip) * op->ibs) + bytes_if,
                           POSIX_FADV_DONTNEED);
        if (rt)         /* returns error as result */
            pr2serr("posix_fadvise on read, skip=%" PRId64 " ,err=%d\n",
                    op->skip, rt);
    }
    if ((op->oflagp->nocache & 2) && (bytes_of2 > 0) && out2_valid) {
        rt = posix_fadvise(op->o2dip->fd, 0, 0, POSIX_FADV_DONTNEED);
        if (rt)
            pr2serr("posix_fadvise on of2, seek=%" PRId64 " ,err=%d\n",
                    op->seek, rt);
    }
    if ((op->oflagp->nocache & 1) && (bytes_of > 0) && out_valid) {
        if ((op->lowest_seek < 0) || (op->seek > op->lowest_seek))
            op->lowest_seek = op->seek;
        rt = posix_fadvise(op->odip->fd, (op->lowest_seek * op->obs),
                   ((op->seek - op->lowest_seek) * op->obs) + bytes_of,
                           POSIX_FADV_DONTNEED);
        if (rt)
            pr2serr("posix_fadvise on output, seek=%" PRId64 " , err=%d\n",
                    op->seek, rt);
    }
}
#endif

/* Main copy loop's read (input) via pt. Returns 0 on success, else see
 * pt_read()'s return values. */
static int
cp_read_pt(struct opts_t * op, struct cp_state_t * csp, unsigned char * bp)
{
    int res;
    int blks_read = 0;

    res = pt_read(op, 0, bp, csp->icbpt, &blks_read);
    if (res) {
        if (0 == blks_read) {
            pr2serr("pt_read failed,%s at or after lba=%" PRId64 " "
                    "[0x%" PRIx64 "]\n",
                    ((-2 == res) ?  " try reducing bpt," : ""),
                    op->skip, op->skip);
            return res;
        }
        /* limp on if data, should stop after write; hold err number */
        op->err_to_report = res;
    }
    if (blks_read < csp->icbpt) {
        /* assume close to end, or some data prior to read error */
        if (op->verbose > 1)
            pr2serr("short read, requested %d blocks, got %d blocks\n",
                    csp->icbpt, blks_read);
        ++csp->leave_after_write;
        /* csp->leave_reason = 0; assume at end rather than error */
        csp->icbpt = blks_read;
        /* round down since don't do partial writes from pt reads */
        csp->ocbpt = (blks_read * op->ibs) / op->obs;
    }
    op->in_full += csp->icbpt;
    return 0;
}

/* Helper for case when EIO or EREMOTE errno suggests the equivalent
 * of a medium error. Returns 0 unless coe_limit exceeded. */
int             /* Global function, used by ddpt_win32.c */
coe_process_eio(struct opts_t * op, int64_t skip)
{
    if ((op->coe_limit > 0) && (++op->coe_count > op->coe_limit)) {
        pr2serr(">> coe_limit on consecutive reads "
                "exceeded\n");
        return SG_LIB_CAT_MEDIUM_HARD;
    }
    if (op->highest_unrecovered < 0) {
        op->highest_unrecovered = skip;
        op->lowest_unrecovered = skip;
    } else {
        if (skip < op->lowest_unrecovered)
            op->lowest_unrecovered = skip;
        if (skip > op->highest_unrecovered)
            op->highest_unrecovered = skip;
    }
    ++op->unrecovered_errs;
    ++op->in_partial;
    --op->in_full;
    pr2serr(">> unrecovered read error at blk=%" PRId64 ", "
            "substitute zeros\n", skip);
    return 0;
}

/* Error occurred on block/regular read. coe active so assume all full
 * blocks prior to error are good (if any) and start to read from the
 * block containing the error, one block at a time, until ibpt. Supply
 * zeros for unreadable blocks. Return 0 if successful, SG_LIB_CAT_OTHER
 * if error other than EIO or EREMOTEIO, SG_LIB_FILE_ERROR if lseek fails,
 * and SG_LIB_CAT_MEDIUM_HARD if the coe_limit is exceeded. */
static int
coe_cp_read_block_reg(struct opts_t * op, struct cp_state_t * csp,
                      unsigned char * bp, int numread_errno)
{
    int res, res2, k, total_read, num_read;
    int ibs = op->ibs_pi;
    int64_t offset, off_res, my_skip;

    if (0 == numread_errno) {
        csp->icbpt = 0;
        csp->ocbpt = 0;
        ++csp->leave_after_write;
        csp->leave_reason = 0;
        return 0;       /* EOF */
    } else if (numread_errno < 0) {
        if ((-EIO == numread_errno) || (-EREMOTEIO == numread_errno)) {
            num_read = 0;
            if (1 == csp->icbpt) {
                // Don't read again, this must be bad block
                memset(bp, 0, ibs);
                if ((res2 = coe_process_eio(op, op->skip)))
                    return res2;
                ++op->in_full;
                csp->bytes_read += ibs;
                return 0;
            }
        } else
            return SG_LIB_CAT_OTHER;
    } else
        num_read = (numread_errno / ibs) * ibs;

    k = num_read / ibs;
    if (k > 0) {
        op->in_full += k;
        zero_coe_limit_count(op);
    }
    csp->bytes_read = num_read;
    my_skip = op->skip + k;
    offset = my_skip * ibs;
    bp += num_read;
    for ( ; k < csp->icbpt; ++k, ++my_skip, bp += ibs, offset += ibs) {
        if (offset != csp->if_filepos) {
            if (op->verbose > 2)
                pr2serr("moving if filepos: new_pos=%" PRId64 "\n",
                        (int64_t)offset);
            off_res = lseek(op->idip->fd, offset, SEEK_SET);
            if (off_res < 0) {
                pr2serr("failed moving if filepos: new_pos="
                        "%" PRId64 "\nlseek on input: %s\n", (int64_t)offset,
                        safe_strerror(errno));
                return SG_LIB_FILE_ERROR;
            }
            csp->if_filepos = offset;
        }
        memset(bp, 0, ibs);
        while (((res = read(op->idip->fd, bp, ibs)) < 0) &&
               (EINTR == errno))
            ++op->interrupted_retries;
        if (0 == res) {
            csp->leave_reason = 0;
            goto short_read;
        } else if (res < 0) {
            if ((EIO == errno) || (EREMOTEIO == errno)) {
                if ((res2 = coe_process_eio(op, my_skip)))
                    return res2;
            } else {
                pr2serr("reading 1 block, skip=%" PRId64 " : %s\n", my_skip,
                        safe_strerror(errno));
                csp->leave_reason = SG_LIB_CAT_OTHER;
                goto short_read;
            }
        } else if (res < ibs) {
            if (op->verbose)
                pr2serr("short read at skip=%" PRId64 " , wanted=%d, "
                        "got=%d bytes\n", my_skip, ibs, res);
            csp->leave_reason = 0;  /* assume EOF */
            goto short_read;
        } else { /* if (res == ibs) */
            zero_coe_limit_count(op);
            csp->if_filepos += ibs;
            if (op->verbose > 2)
                pr2serr("reading 1 block, skip=%" PRId64 " : okay\n",
                        my_skip);
        }
        ++op->in_full;
        csp->bytes_read += ibs;
    }
    return 0;

short_read:
    total_read = (ibs * k) + ((res > 0) ? res : 0);
    csp->icbpt = total_read / ibs;
    if ((total_read % ibs) > 0) {
        ++csp->icbpt;
        ++op->in_partial;
    }
    csp->ocbpt = total_read / op->obs;
    ++csp->leave_after_write;
    if (0 == csp->leave_reason) {
        csp->partial_write_bytes = total_read % op->obs;
    } else {
        /* if short read (not EOF) implies partial writes, bump obpt */
        if ((total_read % op->obs) > 0)
            ++csp->ocbpt;
    }
    return 0;
}

/* Main copy loop's read (input) for block device or regular file.
 * Returns 0 on success, else SG_LIB_FILE_ERROR, SG_LIB_CAT_MEDIUM_HARD,
 * SG_LIB_CAT_OTHER or -1 . */
static int
cp_read_block_reg(struct opts_t * op, struct cp_state_t * csp,
                  unsigned char * bp)
{
    int res, res2, in_type;
    int64_t offset = op->skip * op->ibs_pi;
    int numbytes = csp->icbpt * op->ibs_pi;
    int ibs = op->ibs_pi;

    in_type = op->idip->d_type;
#ifdef SG_LIB_WIN32
    if (FT_BLOCK & in_type) {
        int ifull_extra;

        if ((res = win32_cp_read_block(op, csp, bp, &ifull_extra,
                                       op->verbose)))
            return res;
        op->in_full += ifull_extra;
        return 0;
    }
#endif
    if (offset != csp->if_filepos) {
        int64_t off_res;

        if (op->verbose > 2)
            pr2serr("moving if filepos: new_pos=%" PRId64 "\n",
                    (int64_t)offset);
        off_res = lseek(op->idip->fd, offset, SEEK_SET);
        if (off_res < 0) {
            pr2serr("failed moving if filepos: new_pos="
                    "%" PRId64 "\nlseek on input: %s\n", (int64_t)offset,
                    safe_strerror(errno));
            return SG_LIB_FILE_ERROR;
        }
        csp->if_filepos = offset;
    }
    while (((res = read(op->idip->fd, bp, numbytes)) < 0) &&
           (EINTR == errno))
        ++op->interrupted_retries;

    if (op->verbose > 2)
        pr2serr("read(unix): requested bytes=%d, res=%d\n", numbytes, res);
    if ((op->iflagp->coe) && (res < numbytes)) {
        res2 = (res >= 0) ? res : -errno;
        if ((res < 0) && op->verbose) {
            pr2serr("reading, skip=%" PRId64 " : %s, go to coe\n",
                    op->skip, safe_strerror(errno));
        } else if (op->verbose)
            pr2serr("reading, skip=%" PRId64 " : short read, go to coe\n",
                    op->skip);
        if (res2 > 0)
            csp->if_filepos += res2;
        return coe_cp_read_block_reg(op, csp, bp, res2);
    }
    if (res < 0) {
        pr2serr("reading, skip=%" PRId64 " : %s\n", op->skip,
                safe_strerror(errno));
        if ((EIO == errno) || (EREMOTEIO == errno))
            return SG_LIB_CAT_MEDIUM_HARD;
        else
            return SG_LIB_CAT_OTHER;
    } else if (res < numbytes) {
        csp->icbpt = res / ibs;
        if ((res % ibs) > 0) {
            ++csp->icbpt;
            ++op->in_partial;
            --op->in_full;
        }
        csp->ocbpt = res / op->obs;
        ++csp->leave_after_write;
        csp->leave_reason = 0;  /* fall through is assumed EOF */
        if (op->verbose > 1) {
            if (FT_BLOCK & in_type)
                pr2serr("short read at skip=%" PRId64 ", requested "
                        "%d blocks, got %d blocks\n", op->skip,
                        numbytes / ibs, csp->icbpt);
            else
                pr2serr("short read, requested %d bytes, got %d bytes\n",
                        numbytes, res);
        }
        res2 = 0;
        if ((res >= ibs) && (res <= (numbytes - ibs))) {
            /* Want to check for a EIO lurking */
            while (((res2 = read(op->idip->fd, bp + res, ibs)) < 0) &&
                   (EINTR == errno))
                ++op->interrupted_retries;
            if (res2 < 0) {
                if ((EIO == errno) || (EREMOTEIO == errno)) {
                    csp->leave_reason = SG_LIB_CAT_MEDIUM_HARD;
                    ++op->unrecovered_errs;
                } else
                    csp->leave_reason = SG_LIB_CAT_OTHER;
                if (op->verbose)
                    pr2serr("after short read, read at skip=%" PRId64
                            ": %s\n", op->skip + csp->icbpt,
                            safe_strerror(errno));
            } else {    /* actually expect 0==res2 indicating EOF */
                csp->if_filepos += res2;   /* could have moved filepos */
                if (op->verbose > 1)
                    pr2serr("extra read after short read, res=%d\n", res2);
            }
        }
        if (0 == csp->leave_reason)    /* if EOF, allow for partial write */
            csp->partial_write_bytes = (res + res2) % op->obs;
        else if ((res % op->obs) > 0) /* else if extra bytes bump obpt */
            ++csp->ocbpt;
    }
    csp->if_filepos += res;
    csp->bytes_read = res;
    op->in_full += csp->icbpt;
    return 0;
}

#ifdef SG_LIB_LINUX

/* Summarise previous consecutive same-length reads. Do that when:
 * - read length (res) differs from the previous read length, and
 * - there were more than one consecutive reads of the same length
 * The str argument is a prefix string, typically one or two spaces, used
 * to e.g. make output line up when printing on kill -USR1. */
static void
print_tape_summary(struct opts_t * op, int res, const char * str)
{
    int len = op->last_tape_read_len;
    int num = op->read_tape_numbytes;

    if ((op->verbose > 1) && (res != len) && (op->consec_same_len_reads >= 1))
        pr2serr("%s(%d%s read%s of %d byte%s)\n", str,
                op->consec_same_len_reads, (len < num) ? " short" : "",
                (op->consec_same_len_reads != 1) ? "s" : "", len,
                (len != 1) ? "s" : "");
}

/* Main copy loop's read (input) for tape device. Returns 0 on success,
 * else SG_LIB_CAT_MEDIUM_HARD, SG_LIB_CAT_OTHER or -1 . */
static int
cp_read_tape(struct opts_t * op, struct cp_state_t * csp, unsigned char * bp)
{
    int res, err, num;

    num = csp->icbpt * op->ibs;
    op->read_tape_numbytes = num;
    while (((res = read(op->idip->fd, bp, num)) < 0) && (EINTR == errno))
        ++op->interrupted_retries;

    err = errno;

    /* Summarise previous consecutive same-length reads. */
    print_tape_summary(op, res, "");

    if (op->verbose > 2)
        pr2serr("read(tape%s): requested bytes=%d, res=%d\n",
                ((res >= num) || (res < 0)) ? "" : ", short", num, res);

    if (op->verbose > 3)
        print_tape_pos("", "", op);

    if (res < 0) {
        /* If a tape block larger than the requested read length is
         * encountered, the Linux st driver returns ENOMEM. Handle that case
         * otherwise we would print a confusing/incorrect message
         * "Cannot allocate memory". */
        pr2serr("reading, skip=%" PRId64 " : %s\n", op->skip,
                (ENOMEM == err) ? "Tape block larger than requested read"
                " length" : safe_strerror(err));

        /* So print_stats() doesn't print summary. */
        op->last_tape_read_len = 0;

        if ((EIO == err) || (EREMOTEIO == err))
            return SG_LIB_CAT_MEDIUM_HARD;
        else
            return SG_LIB_CAT_OTHER;
    } else {
        if (op->verbose > 1) {
            if (res == op->last_tape_read_len)
                op->consec_same_len_reads++;
            else {
                op->last_tape_read_len = res;
                op->consec_same_len_reads = 1;
            }
        }
        if (res < num) {
            csp->icbpt = res / op->ibs;
            if ((res % op->ibs) > 0) {
                ++csp->icbpt;
                ++op->in_partial;
                --op->in_full;
            }
            csp->ocbpt = res / op->obs;
            ++csp->leave_after_write;
            csp->leave_reason = REASON_TAPE_SHORT_READ;
            csp->partial_write_bytes = res % op->obs;
            if ((op->verbose == 2) && (op->consec_same_len_reads == 1))
                pr2serr("short read: requested %d bytes, got %d\n",
                        op->read_tape_numbytes, res);
        }
    }
    csp->if_filepos += res;
    csp->bytes_read = res;
    op->in_full += csp->icbpt;
    return 0;
}

#endif /* SG_LIB_LINUX */

/* Main copy loop's read (input) for a fifo. Returns 0 on success, else
 * SG_LIB_CAT_OTHER or -1 . */
static int
cp_read_fifo(struct opts_t * op, struct cp_state_t * csp, unsigned char * bp)
{
    int res, k, err;
    int64_t offset = op->skip * op->ibs;
    int numbytes = csp->icbpt * op->ibs;

    if (offset != csp->if_filepos) {
        if (op->verbose > 2)
            pr2serr("fifo: _not_ moving IFILE filepos to %" PRId64 "\n",
                    (int64_t)offset);
        csp->if_filepos = offset;
    }

    for (k = 0; k < numbytes; k += res) {
        while (((res = read(op->idip->fd, bp + k, numbytes - k)) < 0) &&
               (EINTR == errno))
            ++op->interrupted_retries;

        err = errno;
        if (op->verbose > 2)
            pr2serr("read(fifo): requested bytes=%d, res=%d\n",
                    numbytes, res);
        if (res < 0) {
            pr2serr("read(fifo), skip=%" PRId64 " : %s\n", op->skip,
                    safe_strerror(err));
            return SG_LIB_CAT_OTHER;
        } else if (0 == res) {
            csp->icbpt = k / op->ibs;
            if ((k % op->ibs) > 0) {
                ++csp->icbpt;
                ++op->in_partial;
                --op->in_full;
            }
            csp->ocbpt = k / op->obs;
            ++csp->leave_after_write;
            csp->leave_reason = 0;  /* EOF */
            csp->partial_write_bytes = k % op->obs;
            break;
        }
    }
    csp->if_filepos += k;
    csp->bytes_read = k;
    op->in_full += csp->icbpt;
    return 0;
}

/* Main copy loop's write (to of2) for regular file. Returns 0 if success,
 * else -1 on error. */
static int
cp_write_of2(struct opts_t * op, struct cp_state_t * csp,
             const unsigned char * bp)
{
    int res, off, part, err;
    int numbytes = (csp->ocbpt * op->obs) + csp->partial_write_bytes;

    // write to fifo (reg file ?) is non-atomic so loop if making progress
    off = 0;
    part = 0;
    do {
        while (((res = write(op->o2dip->fd, bp + off, numbytes - off)) < 0) &&
               (EINTR == errno))
            ++op->interrupted_retries;
        err = errno;
        if ((res > 0) && (res < (numbytes - off)))
            ++part;
    } while ((FT_FIFO & op->o2dip->d_type) && (res > 0) &&
             ((off += res) < numbytes));
    if (off >= numbytes) {
        res = numbytes;
        if (part && op->verbose)
            pr2serr("write to of2 splintered\n");
    } else if (off > 0)
        pr2serr("write to of2 fifo problem: count=%d, off=%d, res=%d\n",
                numbytes, off, res);
    if ((op->verbose > 2) && (0 == off))
        pr2serr("write to of2: count=%d, res=%d\n", numbytes, res);
    if (res < 0) {
        pr2serr("writing to of2, seek=%" PRId64 " : %s\n", op->seek,
                safe_strerror(err));
        return -1;
    }
    csp->bytes_of2 = res;
    return 0;
}

/* Main copy loop's read (output (of)) via pt. Returns 0 on success, else
 * see pt_read()'s return values. */
static int
cp_read_of_pt(struct opts_t * op, struct cp_state_t * csp, unsigned char * bp)
{
    int res, blks_read;

    res = pt_read(op, 1, bp, csp->ocbpt, &blks_read);
    if (res) {
        pr2serr("pt_read(sparing) failed, at or after "
                "lba=%" PRId64 " [0x%" PRIx64 "]\n", op->seek,
                op->seek);
        return res;
    } else if (blks_read != csp->ocbpt)
        return 1;
    return 0;
}

/* Main copy loop's read (output (of)) for block device or regular file.
 * Returns 0 on success, else SG_LIB_FILE_ERROR, SG_LIB_CAT_MEDIUM_HARD
 * or -1 . */
static int
cp_read_of_block_reg(struct opts_t * op, struct cp_state_t * csp,
                     unsigned char * bp)
{
    int res, err;
    int64_t offset = op->seek * op->obs;
    int numbytes = csp->ocbpt * op->obs;

#ifdef SG_LIB_WIN32
    if (FT_BLOCK & op->odip->d_type) {
        if (offset != csp->of_filepos) {
            if (op->verbose > 2)
                pr2serr("moving of filepos: new_pos=%" PRId64 "\n",
                        (int64_t)offset);
            if (win32_set_file_pos(op, DDPT_ARG_OUT, offset, op->verbose))
                return SG_LIB_FILE_ERROR;
            csp->of_filepos = offset;
        }
        res = win32_block_read_from_of(op, bp, numbytes, op->verbose);
        if (op->verbose > 2)
            pr2serr("read(sparing): requested bytes=%d, res=%d\n", numbytes,
                    res);
        if (res < 0) {
            pr2serr("read(sparing), seek=%" PRId64 "\n", op->seek);
            return (-SG_LIB_CAT_MEDIUM_HARD == res) ? -res : -1;
        } else if (res == numbytes) {
            csp->of_filepos += numbytes;
            return 0;
        } else {
            if (op->verbose > 2)
                pr2serr("short read\n");
            return -1;
        }
    } else
#endif
    {
        if (offset != csp->of_filepos) {
            int64_t off_res;

            if (op->verbose > 2)
                pr2serr("moving of filepos: new_pos=%" PRId64 "\n",
                        (int64_t)offset);
            off_res = lseek(op->odip->fd, offset, SEEK_SET);
            if (off_res < 0) {
                pr2serr("failed moving of filepos: new_pos="
                        "%" PRId64 "\nlseek on output: %s\n", (int64_t)offset,
                        safe_strerror(errno));
                return SG_LIB_FILE_ERROR;
            }
            csp->of_filepos = offset;
        }
        if (csp->partial_write_bytes > 0) {
            numbytes += csp->partial_write_bytes;
            if (op->verbose)
                pr2serr("read(sparing): %d bytes extra to fetch "
                        "due to partial read\n", csp->partial_write_bytes);
        }
        while (((res = read(op->odip->fd, bp, numbytes)) < 0) &&
               (EINTR == errno))
            ++op->interrupted_retries;

        err = errno;
        if (op->verbose > 2)
            pr2serr("read(sparing): requested bytes=%d, res=%d\n", numbytes,
                    res);
        if (res < 0) {
            pr2serr("read(sparing), seek=%" PRId64 " : %s\n", op->seek,
                    safe_strerror(err));
            return -1;
        } else if (res == numbytes) {
            csp->of_filepos += numbytes;
            return 0;
        } else {
            if (op->verbose > 2)
                pr2serr("short read\n");
            return 1;
        }
    }
}


/* Main copy loop's write (output (of)) via pt. Returns 0 on success, else
 * see pt_write()'s return values. */
static int
cp_write_pt(struct opts_t * op, struct cp_state_t * csp, int seek_delta,
            int blks, const unsigned char * bp)
{
    int res;
    int numbytes;
    int64_t aseek = op->seek + seek_delta;

    if (op->oflagp->nowrite)
        return 0;
    if (csp->partial_write_bytes > 0) {
        if (op->oflagp->pad) {
            unsigned char * ncbp = (unsigned char *)bp;

            numbytes = blks * op->obs;
            numbytes += csp->partial_write_bytes;
            ++csp->ocbpt;
            ++blks;
            res = blks * op->obs;
            if (res > numbytes)
                memset(ncbp + numbytes, 0, res - numbytes);
            if (op->verbose > 1)
                pr2serr("pt_write: padding probable final write at "
                        "seek=%" PRId64 "\n", aseek);
        } else
            pr2serr(">>> ignore partial write of %d bytes to pt "
                    "(unless oflag=pad given)\n", csp->partial_write_bytes);
    }
    res = pt_write(op, bp, blks, aseek);
    if (0 != res) {
        pr2serr("pt_write failed,%s seek=%" PRId64 "\n",
                ((-2 == res) ? " try reducing bpt," : ""), aseek);
        return res;
    } else
        op->out_full += blks;
    return 0;
}

#ifdef SG_LIB_LINUX

/* Main copy loop's write (output (of)) for a tape device.
 * Returns 0 on success, else SG_LIB_CAT_OTHER, SG_LIB_CAT_MEDIUM_HARD
 * or -1 . */
static int
cp_write_tape(struct opts_t * op, struct cp_state_t * csp,
              const unsigned char * bp, int could_be_last)
{
    int res, err;
    int numbytes;
    int partial = 0;
    int blks = csp->ocbpt;
    int64_t aseek = op->seek;
    int got_early_warning = 0;
/* Only print early warning message once when verbose=2 */
    static int printed_ew_message = 0;

    numbytes = blks * op->obs;
    if (op->oflagp->nowrite)
        return 0;
    if (csp->partial_write_bytes > 0) {
        ++partial;
        numbytes += csp->partial_write_bytes;
        if (op->oflagp->nopad)
            ++op->out_partial;
        else {
            unsigned char * ncbp = (unsigned char *)bp;

            ++csp->ocbpt;
            ++blks;
            res = blks * op->obs;
            if (res > numbytes)
                memset(ncbp + numbytes, 0, res - numbytes);
            numbytes = res;
        }
    }

ew_retry:
    while (((res = write(op->odip->fd, bp, numbytes)) < 0) &&
           (EINTR == errno))
        ++op->interrupted_retries;

    err = errno;
    if ((op->verbose > 2) || ((op->verbose > 0) && could_be_last)) {
        const char * cp;

        cp = ((! op->oflagp->nopad) && partial) ? ", padded" : "";
        pr2serr("write(tape%s%s): requested bytes=%d, res=%d\n",
                (partial ? ", partial" : ""), cp, numbytes, res);
    }

/* Handle EOM early warning. */
/* The Linux st driver returns -1 and ENOSPC to indicate the drive has reached
 * end of medium early warning. It is still possible to write a significant
 * amount of data before reaching end of tape (e.g. over 200MB for LTO 1). If
 * the user specified oflag=ignoreew (ignore early warning) retry the write.
 * The st driver should allow it; writes alternate until EOM, i.e. write okay,
 * ENOSPC, write okay, ENOSPC, etc. Exit if more than one ENOSPC in a row. */
    if ((op->oflagp->ignoreew) && (-1 == res) && (ENOSPC == err) &&
        (0 == got_early_warning)) {
        got_early_warning = 1;
        if (0 == printed_ew_message) {
            if (op->verbose > 1)
                pr2serr("writing, seek=%" PRId64 " : EOM early "
                        "warning, continuing...\n", aseek);
             if (2 == op->verbose) {
                pr2serr("(suppressing further early warning messages)\n");
                printed_ew_message = 1;
            }
        }
        goto ew_retry;
    }

    if (op->verbose > 3)
        print_tape_pos("", "", op);

    if (res < 0) {
        pr2serr("writing, seek=%" PRId64 " : %s\n", aseek,
                safe_strerror(err));
        if ((EIO == err) || (EREMOTEIO == err))
            return SG_LIB_CAT_MEDIUM_HARD;
        else
            return SG_LIB_CAT_OTHER;
    } else if (res < numbytes) {
        pr2serr("write(tape): wrote less than requested, exit\n");
        csp->of_filepos += res;
        csp->bytes_of = res;
        op->out_full += res / op->obs;
        /* can get a partial write due to a short write */
        if ((res % op->obs) > 0) {
            ++op->out_partial;
            ++op->out_full;
        }
        return -1;
    } else {    /* successful write */
        csp->of_filepos += numbytes;
        csp->bytes_of = numbytes;
        op->out_full += blks;
    }
    return 0;
}

#endif /* SG_LIB_LINUX */

/* Main copy loop's write (output (of)) for block device fifo or regular
 * file. Returns 0 on success, else SG_LIB_FILE_ERROR,
 * SG_LIB_CAT_MEDIUM_HARD or -1 . */
static int
cp_write_block_reg(struct opts_t * op, struct cp_state_t * csp,
                   int seek_delta, int blks, const unsigned char * bp)
{
    int64_t offset;
    int64_t aseek = op->seek + seek_delta;
    int res, off, part, out_type, err;
    int numbytes = blks * op->obs_pi;
    int obs = op->obs_pi;

    if (op->oflagp->nowrite)
        return 0;
    out_type = op->odip->d_type;
    offset = aseek * obs;
#ifdef SG_LIB_WIN32
    if (FT_BLOCK & out_type) {
        if (csp->partial_write_bytes > 0) {
            if (op->oflagp->pad) {
                numbytes += csp->partial_write_bytes;
                ++csp->ocbpt;
                ++blks;
                res = blks * obs;
                if (res > numbytes)
                    memset((unsigned char *)bp + numbytes, 0,
                           res - numbytes);
                numbytes = res;
                if (op->verbose > 1)
                    pr2serr("write(win32_block): padding probable "
                            "final write at seek=%" PRId64 "\n", aseek);
            } else
                pr2serr(">>> ignore partial write of %d bytes to "
                        "block device\n", csp->partial_write_bytes);
        }
        if (offset != csp->of_filepos) {
            if (op->verbose > 2)
                pr2serr("moving of filepos: new_pos=%" PRId64 "\n",
                        (int64_t)offset);
            if (win32_set_file_pos(op, DDPT_ARG_OUT, offset, op->verbose))
                return SG_LIB_FILE_ERROR;
            csp->of_filepos = offset;
        }
        res = win32_block_write(op, bp, numbytes, op->verbose);
        if (res < 0) {
            pr2serr("write(win32_block), seek=%" PRId64 " ", aseek);
            return (-SG_LIB_CAT_MEDIUM_HARD == res) ? -res : -1;
        } else if (res < numbytes) {
            pr2serr("output file probably full, seek=%" PRId64 " ",
                    aseek);
            csp->of_filepos += res;
            csp->bytes_of = res;
            op->out_full += res / obs;
            /* can get a partial write due to a short write */
            if ((res % obs) > 0) {
                ++op->out_partial;
                ++op->out_full;
            }
            return -1;
        } else {
            csp->of_filepos += numbytes;
            csp->bytes_of = numbytes;
            op->out_full += blks;
        }
        return 0;
    } else
#endif
    {
        if (csp->partial_write_bytes > 0) {
            if (op->oflagp->pad) {
                unsigned char * ncbp = (unsigned char *)bp;

                numbytes += csp->partial_write_bytes;
                ++csp->ocbpt;
                ++blks;
                res = blks * obs;
                if (res > numbytes)
                    memset(ncbp + numbytes, 0, res - numbytes);
                numbytes = res;
                if (op->verbose > 1)
                    pr2serr("write(unix): padding probable final "
                            "write at seek=%" PRId64 "\n", aseek);
            } else {
                if (FT_BLOCK & out_type)
                    pr2serr(">>> ignore partial write of %d bytes to block "
                            "device\n", csp->partial_write_bytes);
                else {
                    numbytes += csp->partial_write_bytes;
                    ++op->out_partial;
                }
            }
        }
        if ((offset != csp->of_filepos) &&
            (! (REASON_TAPE_SHORT_READ == csp->leave_reason))) {
            int64_t off_res;

            if (op->verbose > 2)
                pr2serr("moving of filepos: new_pos=%" PRId64 "\n",
                        (int64_t)offset);
            off_res = lseek(op->odip->fd, offset, SEEK_SET);
            if (off_res < 0) {
                pr2serr("failed moving of filepos: new_pos="
                        "%" PRId64 "\nlseek on output: %s\n", (int64_t)offset,
                        safe_strerror(errno));
                return SG_LIB_FILE_ERROR;
            }
            csp->of_filepos = offset;
        }
        // write to fifo (reg file ?) is non-atomic so loop if making progress
        off = 0;
        part = 0;
        do {
            while (((res = write(op->odip->fd, bp + off,
                                 numbytes - off)) < 0) && (EINTR == errno))
                ++op->interrupted_retries;
            err = errno;
            if ((res > 0) && (res < (numbytes - off)))
                ++part;
        } while ((FT_FIFO & out_type) && (res > 0) &&
                 ((off += res) < numbytes));
        if (off >= numbytes) {
            res = numbytes;
            if (part && op->verbose)
                pr2serr("write to output file splintered\n");
        } else if (off > 0)
            pr2serr("write to of fifo problem: count=%d, off=%d, "
                    "res=%d\n", numbytes, off, res);
        if ((op->verbose > 2) && (0 == off))
            pr2serr("write(unix): requested bytes=%d, res=%d\n", numbytes,
                    res);
        if (res < 0) {
            pr2serr("writing, seek=%" PRId64 " : %s\n", aseek,
                    safe_strerror(err));
            if ((EIO == err) || (EREMOTEIO == err))
                return SG_LIB_CAT_MEDIUM_HARD;
            else
                return SG_LIB_CAT_OTHER;
        } else if (res < numbytes) {
            pr2serr("output file probably full, seek=%" PRId64 "\n", aseek);
            csp->of_filepos += res;
            csp->bytes_of = res;
            op->out_full += res / obs;
            /* can get a partial write due to a short write */
            if ((res % obs) > 0) {
                ++op->out_partial;
                ++op->out_full;
            }
            return -1;
        } else {    /* successful write */
            csp->of_filepos += numbytes;
            csp->bytes_of = numbytes;
            op->out_full += blks;
        }
        return 0;
    }
}

/* Only for regular OFILE. Check what to do if last blocks where
 * not written, may require OFILE length adjustment */
static void
cp_sparse_cleanup(struct opts_t * op, struct cp_state_t * csp)
{
    int64_t offset = op->seek * op->obs;
    struct stat a_st;

    if (offset > csp->of_filepos) {
        if ((0 == op->oflagp->strunc) && (op->oflagp->sparse > 1)) {
            if (op->verbose > 1)
                pr2serr("asked to bypass writing sparse last block zeros\n");
            return;
        }
        if (fstat(op->odip->fd, &a_st) < 0) {
            pr2serr("cp_sparse_cleanup: fstat: %s\n", safe_strerror(errno));
            return;
        }
        if (offset == a_st.st_size) {
            if (op->verbose > 1)
                pr2serr("cp_sparse_cleanup: OFILE already correct length\n");
            return;
        }
        if (offset < a_st.st_size) {
            if (op->verbose > 1)
                pr2serr("cp_sparse_cleanup: OFILE longer than required, do "
                        "nothing\n");
            return;
        }
        if (op->oflagp->strunc) {
            if (op->verbose > 1)
                pr2serr("About to truncate %s to byte offset "
                        "%" PRId64 "\n", op->odip->fn, offset);
            if (ftruncate(op->odip->fd, offset) < 0) {
                pr2serr("could not ftruncate after copy: %s\n",
                        safe_strerror(errno));
                return;
            }
            /* N.B. file offset (pointer) not changed by ftruncate */
        } else if (1 == op->oflagp->sparse) {
            if (op->verbose > 1)
                pr2serr("writing sparse last block zeros\n");
            if (cp_write_block_reg(op, csp, -1, 1, op->zeros_buff) < 0)
                pr2serr("writing sparse last block zeros "
                        "error, seek=%" PRId64 "\n", op->seek - 1);
            else
                --op->out_sparse;
        }
    }
}

/* Main copy loop's finer grain comparison and possible write (to output
 * (of)) for all file types. Returns 0 on success. */
static int
cp_finer_comp_wr(struct opts_t * op, struct cp_state_t * csp,
                 const unsigned char * b1p, const unsigned char * b2p)
{
    int res, k, n, oblks, numbytes, chunk, need_wr, wr_len, wr_k, obs;
    int trim_check, need_tr, tr_len, tr_k, out_type;

    oblks = csp->ocbpt;
    obs = op->obs;
    out_type = op->odip->d_type;
    if (op->obpc >= oblks) {
        if (FT_DEV_NULL & out_type)
            ;
        else if (FT_PT & out_type) {
            if ((res = cp_write_pt(op, csp, 0, oblks, b1p)))
                return res;
        } else if ((res = cp_write_block_reg(op, csp, 0, oblks, b1p)))
            return res;
        return 0;
    }
    numbytes = oblks * obs;
    if ((FT_REG & out_type) && (csp->partial_write_bytes > 0))
        numbytes += csp->partial_write_bytes;
    chunk = op->obpc * obs;
    trim_check = (op->oflagp->sparse && op->oflagp->wsame16 &&
                  (FT_PT & out_type));
    need_tr = 0;
    tr_len = 0;
    tr_k = 0;
    for (k = 0, need_wr = 0, wr_len = 0, wr_k = 0; k < numbytes; k += chunk) {
        n = ((k + chunk) < numbytes) ? chunk : (numbytes - k);
        if (0 == memcmp(b1p + k, b2p + k, n)) {
            if (need_wr) {
                if (FT_DEV_NULL & out_type)
                    ;
                else if (FT_PT & out_type) {
                    if ((res = cp_write_pt(op, csp, wr_k / obs,
                                           wr_len / obs, b1p + wr_k)))
                        return res;
                } else if ((res = cp_write_block_reg(op, csp,
                                wr_k / obs, wr_len / obs, b1p + wr_k)))
                    return res;
                need_wr = 0;
            }
            if (need_tr)
                tr_len += n;
            else if (trim_check) {
                need_tr = 1;
                tr_len = n;
                tr_k = k;
            }
            op->out_sparse += (n / obs);
        } else {   /* look for a sequence of unequals */
            if (need_wr)
                wr_len += n;
            else {
                need_wr = 1;
                wr_len = n;
                wr_k = k;
            }
            if (need_tr) {
                res = pt_write_same16(op, b2p, obs, tr_len / obs,
                                      op->seek + (tr_k / obs));
                if (res)
                    ++op->trim_errs;
                /* continue past trim errors */
                need_tr = 0;
            }
        }
    }
    if (need_wr) {
        if (FT_DEV_NULL & out_type)
            ;
        else if (FT_PT & out_type) {
            if ((res = cp_write_pt(op, csp, wr_k / obs, wr_len / obs,
                                   b1p + wr_k)))
                return res;
        } else if ((res = cp_write_block_reg(op, csp, wr_k / obs,
                                             wr_len / obs, b1p + wr_k)))
            return res;
    }
    if (need_tr) {
        res = pt_write_same16(op, b2p, obs, tr_len / obs,
                              op->seek + (tr_k / obs));
        if (res)
            ++op->trim_errs;
        /* continue past trim errors */
    }
    return 0;
}

static int
cp_construct_pt_zero_buff(struct opts_t * op, int obpt)
{
    if ((FT_PT & op->idip->d_type) && (NULL == op->idip->ptvp)) {
        op->idip->ptvp = (struct sg_pt_base *)pt_construct_obj();
        if (NULL == op->idip->ptvp)
            return -1;
    }
    if ((FT_PT & op->odip->d_type) && (NULL == op->odip->ptvp)) {
        op->odip->ptvp = (struct sg_pt_base *)pt_construct_obj();
        if (NULL == op->odip->ptvp)
            return -1;
    }
    if ((op->oflagp->sparse) && (NULL == op->zeros_buff)) {
        op->zeros_buff = (unsigned char *)calloc(obpt * op->obs, 1);
        if (NULL == op->zeros_buff) {
            pr2serr("zeros_buff calloc failed\n");
            return -1;
        }
    }
    return 0;
}

/* Look at IFILE and OFILE lengths and blocks sizes. If dd_count
 * not given, try to deduce a value for it. If oflag=resume do skip,
 * seek, dd_count adjustments. Returns 0 to start copy, otherwise
 * bypass copy and exit */
static int
count_calculate(struct opts_t * op)
{
    int64_t in_num_sect = -1;
    int64_t out_num_sect = -1;
    int64_t ibytes, obytes, ibk;
    int valid_resume = 0;
    int res;

    if ((res = calc_count(op, &in_num_sect, &out_num_sect)))
        return res;
    if ((0 == op->oflagp->resume) && (op->dd_count > 0))
        return 0;
    if (op->verbose > 1)
        pr2serr("calc_count: in_num_sect=%" PRId64 ", out_num_sect"
                "=%" PRId64 "\n", in_num_sect, out_num_sect);
    if (op->skip && (FT_REG == op->idip->d_type) &&
        (op->skip > in_num_sect)) {
        pr2serr("cannot skip to specified offset on %s\n", op->idip->fn);
        op->dd_count = 0;
        return -1;
    }
    if (op->oflagp->resume) {
        if (FT_REG == op->odip->d_type) {
            if (out_num_sect < 0)
                pr2serr("resume cannot determine size of OFILE, ignore\n");
            else
                valid_resume = 1;
        } else
            pr2serr("resume expects OFILE to be regular, ignore\n");
    }
    if ((op->dd_count < 0) && (! valid_resume)) {
        /* Scale back in_num_sect by value of skip */
        if (op->skip && (in_num_sect > op->skip))
            in_num_sect -= op->skip;
        /* Scale back out_num_sect by value of seek */
        if (op->seek && (out_num_sect > op->seek))
            out_num_sect -= op->seek;

        if ((out_num_sect < 0) && (in_num_sect > 0))
            op->dd_count = in_num_sect;
        else if ((op->reading_fifo) && (out_num_sect < 0))
            ;
        else if ((out_num_sect < 0) && (in_num_sect <= 0))
            ;
        else {
            ibytes = (in_num_sect > 0) ? (op->ibs * in_num_sect) : 0;
            obytes = op->obs * out_num_sect;
            if (0 == ibytes)
                op->dd_count = obytes / op->ibs;
            else if ((ibytes > obytes) && (FT_REG != op->odip->d_type)) {
                op->dd_count = obytes / op->ibs;
            } else
                op->dd_count = in_num_sect;
        }
    }
    if (valid_resume) {
        if (op->dd_count < 0)
            op->dd_count = in_num_sect - op->skip;
        if (out_num_sect <= op->seek)
            pr2serr("resume finds no previous copy, restarting\n");
        else {
            obytes = op->obs * (out_num_sect - op->seek);
            ibk = obytes / op->ibs;
            if (ibk >= op->dd_count) {
                pr2serr("resume finds copy complete, exiting\n");
                op->dd_count = 0;
                return -1;
            }
            /* align to bpt multiple */
            ibk = (ibk / op->bpt_i) * op->bpt_i;
            op->skip += ibk;
            op->seek += (ibk * op->ibs) / op->obs;
            op->dd_count -= ibk;
            pr2serr("resume adjusting skip=%" PRId64 ", seek=%"
                    PRId64 ", and count=%" PRId64 "\n", op->skip, op->seek,
                    op->dd_count);
        }
    }
    return 0;
}

/* This is the main copy loop. Attempts to copy 'dd_count' (a static)
 * blocks (size given by bs or ibs) in chunks of op->bpt_i blocks.
 * Returns 0 if successful.  */
static int
do_rw_copy(struct opts_t * op)
{
    int ibpt, obpt, res, n, sparse_skip, sparing_skip, continual_read;
    int ret = 0;
    int first_time = 1;
    int could_be_last = 0;
    int id_type = op->idip->d_type;
    int od_type = op->odip->d_type;
    struct cp_state_t cp_st;
    struct cp_state_t * csp;
    unsigned char * wPos = op->wrkPos;

    continual_read = op->reading_fifo && (op->dd_count < 0);
    if (op->verbose > 3) {
        if (continual_read)
            pr2serr("do_rw_copy: reading fifo continually\n");
        else
            pr2serr("do_rw_copy: dd_count=%" PRId64 "\n", op->dd_count);
    }
    if ((op->dd_count <= 0) && (! op->reading_fifo))
        return 0;
    csp = &cp_st;
    memset(csp, 0, sizeof(struct cp_state_t));
    ibpt = op->bpt_i;
    obpt = (op->ibs * op->bpt_i) / op->obs;
    if ((ret = cp_construct_pt_zero_buff(op, obpt)))
        goto copy_end;
    /* Both csp->if_filepos and csp->of_filepos are 0 */

    /* <<< main loop that does the copy >>> */
    while ((op->dd_count > 0) || continual_read) {
        if (first_time)
            first_time = 0;
        else
            signals_process_delay(op, DELAY_COPY_SEGMENT);
        csp->bytes_read = 0;
        csp->bytes_of = 0;
        csp->bytes_of2 = 0;
        sparing_skip = 0;
        sparse_skip = 0;
        if ((op->dd_count >= ibpt) || continual_read) {
            csp->icbpt = ibpt;
            csp->ocbpt = obpt;
        } else {
            csp->icbpt = op->dd_count;
            res = op->dd_count;
            n = res * op->ibs;
            csp->ocbpt = n / op->obs;
            if (n % op->obs) {
                ++csp->ocbpt;
                memset(wPos, 0, op->ibs * ibpt);
            }
        }

        /* Start of reading section */
        if (FT_PT & id_type) {
            if ((ret = cp_read_pt(op, csp, wPos)))
                break;
        } else if (FT_FIFO & id_type) {
             if ((ret = cp_read_fifo(op, csp, wPos)))
                break;
        } else if (FT_TAPE & id_type) {
#ifdef SG_LIB_LINUX
             if ((ret = cp_read_tape(op, csp, wPos)))
                break;
#else
            pr2serr("reading from tape not supported in this OS\n");
            ret = SG_LIB_CAT_OTHER;
            break;
#endif
        } else {
             if ((ret = cp_read_block_reg(op, csp, wPos)))
                break;
        }
        if (0 == csp->icbpt)
            break;      /* nothing read so leave loop */

        if ((op->o2dip->fd >= 0) &&
            ((ret = cp_write_of2(op, csp, wPos))))
            break;

        if (op->oflagp->sparse) {
            n = (csp->ocbpt * op->obs) + csp->partial_write_bytes;
            signals_process_delay(op, DELAY_WRITE);
            if (0 == memcmp(wPos, op->zeros_buff, n)) {
                sparse_skip = 1;
                if (op->oflagp->wsame16 && (FT_PT & od_type)) {
                    res = pt_write_same16(op, op->zeros_buff, op->obs,
                                          csp->ocbpt, op->seek);
                    if (res)
                        ++op->trim_errs;
                }
            } else if (op->obpc) {
                ret = cp_finer_comp_wr(op, csp, wPos, op->zeros_buff);
                if (ret)
                    break;
                goto bypass_write;
            }
        }
        if (op->oflagp->sparing && (! sparse_skip)) {
            /* In write sparing, we read from the output */
            signals_process_delay(op, DELAY_WRITE);
            if (FT_PT & od_type)
                res = cp_read_of_pt(op, csp, op->wrkPos2);
            else
                res = cp_read_of_block_reg(op, csp, op->wrkPos2);
            if (0 == res) {
                n = (csp->ocbpt * op->obs) + csp->partial_write_bytes;
                if (0 == memcmp(wPos, op->wrkPos2, n))
                    sparing_skip = 1;
                else if (op->obpc) {
                    ret = cp_finer_comp_wr(op, csp, wPos, op->wrkPos2);
                    if (ret)
                        break;
                    goto bypass_write;
                }
            } else {
                ret = res;
                break;
            }
        }

        /* Start of writing section */
        if ((! continual_read) && (csp->icbpt >= op->dd_count))
            could_be_last = 1;
        if (sparing_skip || sparse_skip) {
            op->out_sparse += csp->ocbpt;
            if (csp->partial_write_bytes > 0)
                ++op->out_sparse_partial;
        } else {
            if (FT_DEV_NULL & od_type)
                ;  /* don't bump out_full (earlier revs did) */
            else {
                signals_process_delay(op, DELAY_WRITE);
                if (FT_PT & od_type) {
                    if ((ret = cp_write_pt(op, csp, 0, csp->ocbpt, wPos)))
                        break;
                } else if (FT_TAPE & od_type) {
#ifdef SG_LIB_LINUX
                    if ((ret = cp_write_tape(op, csp, wPos, could_be_last)))
                        break;
#else
                    pr2serr("writing to tape not supported in this OS\n");
                    ret = SG_LIB_CAT_OTHER;
                    break;
#endif
                } else if ((ret = cp_write_block_reg(op, csp, 0, csp->ocbpt,
                                                     wPos))) /* plus fifo */
                    break;
            }
        }
bypass_write:
#ifdef HAVE_POSIX_FADVISE
        do_fadvise(op, csp->bytes_read, csp->bytes_of, csp->bytes_of2);
#endif
        if (op->dd_count > 0)
            op->dd_count -= csp->icbpt;
        op->skip += csp->icbpt;
        op->seek += csp->ocbpt;
        if (csp->leave_after_write) {
            if (REASON_TAPE_SHORT_READ == csp->leave_reason) {
                /* allow multiple partial writes for tape */
                csp->partial_write_bytes = 0;
                csp->leave_after_write = 0;
            } else {
                /* other cases: stop copy after partial write */
                ret = csp->leave_reason;
                break;
            }
        }
    } /* end of main loop that does the copy ... */

    /* sparse: clean up ofile length when last block(s) were not written */
    if ((FT_REG & od_type) && (0 == op->oflagp->nowrite) &&
        op->oflagp->sparse)
        cp_sparse_cleanup(op, csp);

#ifdef HAVE_FDATASYNC
    else if (op->oflagp->fdatasync) {
        if (fdatasync(op->odip->fd) < 0)
            perror("fdatasync() error");
        if (op->verbose)
            pr2serr("Called fdatasync() on %s successfully\n", op->odip->fn);
    }
#endif
#ifdef HAVE_FSYNC
    else if (op->oflagp->fsync) {
        if (fsync(op->odip->fd) < 0)
            perror("fsync() error");
        if (op->verbose)
            pr2serr("Called fsync() on %s successfully\n", op->odip->fn);
    }
#endif

copy_end:
    if (op->idip->ptvp) {
        pt_destruct_obj(op->idip->ptvp);
        op->idip->ptvp = NULL;
    }
    if (op->odip->ptvp) {
        pt_destruct_obj(op->odip->ptvp);
        op->odip->ptvp = NULL;
    }
    return ret;
}

#ifdef SG_LIB_LINUX

static void
show_tape_pos_error(const char * postfix)
{
    pr2serr("Could not get tape position%s: %s\n", postfix,
            safe_strerror(errno));
}

/* Print tape position(s) if verbose > 1. If both reading from and writing to
 * tape, make clear in output which is which. Also only print the position if
 * necessary, i.e. not already printed.
 * Prefix argument is e.g. "Initial " or "Final ". */
static void
print_tape_pos(const char * prefix, const char * postfix,
               struct opts_t * op)
{
    static int lastreadpos, lastwritepos;
    static char lastreadposvalid = 0;
    static char lastwriteposvalid = 0;
    int res;
    struct mtpos pos;

    if (op->verbose > 1) {
        if (FT_TAPE & op->idip->d_type) {
            res = ioctl(op->idip->fd, MTIOCPOS, &pos);
            if (0 == res) {
                if ((pos.mt_blkno != lastreadpos) ||
                    (0 == lastreadposvalid)) {
                    lastreadpos = pos.mt_blkno;
                    lastreadposvalid = 1;
                    pr2serr("%stape position%s: %u%s\n", prefix,
                            (FT_TAPE & op->odip->d_type) ? " (reading)" : "",
                            lastreadpos, postfix);
                }
            } else {
                lastreadposvalid = 0;
                show_tape_pos_error((FT_TAPE & op->odip->d_type) ?
                                    " (reading)" : "");
            }
        }

        if (FT_TAPE & op->odip->d_type) {
            res = ioctl(op->odip->fd, MTIOCPOS, &pos);
            if (0 == res) {
                if ((pos.mt_blkno != lastwritepos) ||
                    (0 == lastwriteposvalid)) {
                    lastwritepos = pos.mt_blkno;
                    lastwriteposvalid = 1;
                    pr2serr("%stape position%s: %u%s\n", prefix,
                            (FT_TAPE & op->idip->d_type) ? " (writing)" : "",
                            lastwritepos, postfix);
                }
            } else {
                lastwriteposvalid = 0;
                show_tape_pos_error((FT_TAPE & op->idip->d_type) ?
                                    " (writing)" : "");
            }
        }

    }
}

#endif  /* SG_LIB_LINUX */

static int
prepare_pi(struct opts_t * op)
{
#define PI_WORK 1       /* Protection Information */
#ifdef PI_WORK
    int res;

    op->ibs_pi = op->ibs;
    op->obs_pi = op->obs;
    if (op->rdprotect) {
        if ((0 == op->idip->prot_type) || (! (FT_PT & op->idip->d_type))) {
            pr2serr("IFILE is not a pt device or doesn't have "
                    "protection information\n");
            return SG_LIB_CAT_OTHER;
        }
        if (op->ibs != op->obs) {
            pr2serr("protect: don't support IFILE and OFILE "
                    "with different block sizes\n");
            return SG_LIB_CAT_OTHER;
        }
        if (op->wrprotect) {
            if (op->idip->p_i_exp != op->odip->p_i_exp) {
                pr2serr("Don't support IFILE and OFILE with "
                        "different P_I_EXP fields\n");
                return SG_LIB_CAT_OTHER;
            }
        }
        res = (op->idip->p_i_exp ? (1 << op->idip->p_i_exp) : 1) * 8;
        op->ibs_pi += res;
        op->obs_pi += res;
    }
    if (op->wrprotect) {
        if ((0 == op->odip->prot_type) || (! (FT_PT & op->odip->d_type))) {
            pr2serr("OFILE is not a pt device or doesn't have "
                    "protection information\n");
            return SG_LIB_CAT_OTHER;
        }
        if (op->ibs != op->obs) {
            pr2serr("protect: don't support IFILE and OFILE "
                    "with different block sizes\n");
            return SG_LIB_CAT_OTHER;
        }
        res = (op->odip->p_i_exp ? (1 << op->odip->p_i_exp) : 1) * 8;
        op->ibs_pi += res;
        op->obs_pi += res;
    }
#else
    if (op) { ; }       /* suppress warning */
#endif  /* PI_WORK */
    return 0;
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

static int
open_files_devices(struct opts_t * op)
{
    int fd, ret;
    struct dev_info_t * idip = op->idip;
    struct dev_info_t * odip = op->odip;
    struct dev_info_t * o2dip = op->o2dip;

#ifdef SG_LIB_WIN32
    win32_adjust_fns_pt(op);
#endif
    if (idip->fn[0]) {
        if (('-' == idip->fn[0]) && ('\0' == idip->fn[1])) {
            fd = STDIN_FILENO;
            idip->d_type = FT_FIFO;
            ++op->reading_fifo;
            if (op->verbose)
                pr2serr(" >> Input file type: fifo [stdin, stdout, named "
                        "pipe]\n");
        } else {
            fd = open_if(op);
            if (fd < 0)
                return -fd;
        }
        idip->fd = fd;
    } else {
        pr2serr("'if=IFILE' option must be given. For stdin as input use "
                "'if=-'\n");
        pr2serr("For more information use '--help'\n");
        return SG_LIB_SYNTAX_ERROR;
    }

    if ('\0' == odip->fn[0])
        strcpy(odip->fn, "."); /* treat no 'of=OFILE' option as /dev/null */
    if (('-' == odip->fn[0]) && ('\0' == odip->fn[1])) {
        fd = STDOUT_FILENO;
        odip->d_type = FT_FIFO;
        odip->d_type_hold = odip->d_type;
        if (op->verbose)
            pr2serr(" >> Output file type: fifo [stdin, stdout, named "
                    "pipe]\n");
    } else {
        fd = open_of(op);
        if (fd < -1)
            return -fd;
    }
    odip->fd = fd;

    if (o2dip->fn[0]) {
        if (('-' == o2dip->fn[0]) && ('\0' == o2dip->fn[1])) {
            fd = STDOUT_FILENO;
            o2dip->d_type = FT_FIFO;
            if (op->verbose)
                pr2serr(" >> Output 2 file type: fifo  [stdin, stdout, "
                        "named pipe]\n");
        } else {
            o2dip->d_type = dd_filetype(o2dip->fn, op->verbose);
            if (FT_DEV_NULL & o2dip->d_type)
                fd = -1;
            else if (! ((FT_REG | FT_FIFO) & o2dip->d_type)) {
                pr2serr("Error: output 2 file type must be regular "
                        "file or fifo\n");
                return SG_LIB_FILE_ERROR;
            } else {
                if ((fd = open(o2dip->fn, O_WRONLY | O_CREAT, 0666)) < 0) {
                    ret = errno;
                    pr2serr("could not open %s for writing: %s\n", o2dip->fn,
                            safe_strerror(errno));
                    return ret;
                }
                if (sg_set_binary_mode(fd) < 0)
                    perror("sg_set_binary_mode");
                if (op->verbose)
                    pr2serr(" >> Output 2 file type: regular\n");
            }
        }
    } else
        fd = -1;
    o2dip->fd = fd;
    return 0;
}

static void
block_size_bpt_check(struct opts_t * op)
{
    if (0 == op->bpt_given) {
/* If reading from or writing to tape, use default bpt 1 if user did not
 * specify. Avoids inadvertent/accidental use of wrong tape block size. */
        if ((FT_TAPE & op->idip->d_type) || (FT_TAPE & op->odip->d_type)) {
            op->bpt_i = 1;
        }
#ifdef SG_LIB_FREEBSD
        else {
     /* FreeBSD (7+8 [DFLTPHYS]) doesn't like buffers larger than 64 KB being
     * sent to its pt interface (CAM), so take that into account when choosing
     * the default bpt value. There is overhead in the pt interface so reduce
     * default bpt value so bpt*ibs <= 32 KB .*/
        if (((FT_PT & op->idip->d_type) || (FT_PT & op->odip->d_type)) &&
            ((op->ibs <= 32768) && (op->bpt_i * op->ibs) > 32768))
            op->bpt_i = 32768 / op->ibs;
        }
#endif
    }
}

static void
sparse_sparing_check(struct opts_t * op)
{
    if (op->iflagp->sparse && (! op->oflagp->sparse)) {
        if (FT_DEV_NULL & op->odip->d_type) {
            pr2serr("sparse flag usually ignored on input; set it "
                    "on output in this case\n");
            ++op->oflagp->sparse;
        } else
            pr2serr("sparse flag ignored on input\n");
    }
    if (op->oflagp->sparse) {
        if ((FT_FIFO | FT_TAPE) & op->odip->d_type) {
            pr2serr("oflag=sparse needs seekable output file, ignore\n");
            op->oflagp->sparse = 0;
        } else {
            op->out_sparse_active = 1;
            if (op->oflagp->wsame16)
                op->out_trim_active = 1;
        }
    }
    if (op->oflagp->sparing) {
        if ((FT_DEV_NULL | FT_FIFO | FT_TAPE) & op->odip->d_type) {
            pr2serr("oflag=sparing needs a readable and seekable "
                    "output file, ignore\n");
            op->oflagp->sparing = 0;
        } else
            op->out_sparing_active = 1;
    }
}

static void
cdb_size_prealloc(struct opts_t * op)
{
    if (op->oflagp->prealloc) {
        if ((FT_DEV_NULL | FT_FIFO | FT_TAPE | FT_PT) & op->odip->d_type) {
            pr2serr("oflag=pre-alloc needs a normal output file, ignore\n");
            op->oflagp->prealloc = 0;
        }
    }
    if (! op->cdbsz_given) {
        if ((FT_PT & op->idip->d_type) && (op->iflagp->cdbsz < 16) &&
            (((op->dd_count + op->skip) > UINT_MAX) ||
             (op->bpt_i > USHRT_MAX))) {
            if (op->verbose > 0)
                pr2serr("SCSI command size increased from 10 to 16 "
                        "bytes on %s\n", op->idip->fn);
            op->iflagp->cdbsz = 16;
        }
        if ((FT_PT & op->odip->d_type) && (op->oflagp->cdbsz < 16) &&
            (((op->dd_count + op->seek) > UINT_MAX) ||
             (((op->ibs * op->bpt_i) / op->obs) > USHRT_MAX))) {
            if (op->verbose)
                pr2serr("SCSI command size increased from 10 to 16 "
                        "bytes on %s\n", op->odip->fn);
            op->oflagp->cdbsz = 16;
        }
    }
}

#ifdef SG_LIB_LINUX

static void
tape_cleanup_of(struct opts_t * op)
{
    /* Before closing OFILE, if writing to tape handle suppressing the
     * writing of a filemark and/or flushing the drive buffer which the
     * Linux st driver normally does when tape file is closed after writing.
     * Possibilities depend on oflags:
     * nofm:         MTWEOFI 0 if possible (kernel 2.6.37+), else MTBSR 0
     * nofm & fsync: MTWEOF 0
     * fsync:        Do nothing; st writes filemark & flushes buffer on close.
     * neither:      MTWEOFI 1 if possible (2.6.37+), else nothing (drive
     *               buffer will be flushed if MTWEOFI not possible). */
    struct mtop mt_cmd;
    int res;

    if (op->oflagp->nofm || !op->oflagp->fsync) {
        mt_cmd.mt_op = (op->oflagp->fsync) ? MTWEOF : MTWEOFI;
        mt_cmd.mt_count = (op->oflagp->nofm) ? 0 : 1;
        res = ioctl(op->odip->fd, MTIOCTOP, &mt_cmd);
        if (res != 0) {
            if (op->verbose > 0)
                pr2serr("MTWEOF%s %d failed: %s\n",
                        (op->oflagp->fsync) ? "" : "I", mt_cmd.mt_count,
                        safe_strerror(errno));
            if (op->oflagp->nofm && !op->oflagp->fsync) {
                if (op->verbose > 0)
                    pr2serr("Trying MTBSR 0 instead\n");
                mt_cmd.mt_op = MTBSR; /* mt_cmd.mt_count = 0 from above */
                res = ioctl(op->odip->fd, MTIOCTOP, &mt_cmd);
                if (res != 0)
                    pr2serr("MTBSR 0 failed: %s\n(Filemark will be written "
                            "when tape file is closed)\n",
                            safe_strerror(errno));
            }
        }
    }
}

#endif  /* SG_LIB_LINUX */

static int
do_falloc(struct opts_t * op)
{
#ifdef SG_LIB_LINUX
#ifdef HAVE_FALLOCATE
    /* Try to pre-allocate space in the output file.
     *
     * If fallocate() does not succeed, exit with an error message. The user
     * can then either free up some disk space or invoke ddpt without
     * oflag=pre-alloc (at the risk of running out of disk space).
     *
     * TODO/DISCUSSION: Some filesystems (e.g. FAT32) don't support
     * fallocate(). In that case we should probably have a way to continue if
     * fallocate() fails, rather than exiting; useful for use in scripts
     * where the user would like to pre-allocate space when possible.
     *
     * On Linux, try fallocate() with the FALLOC_FL_KEEP_SIZE flag, which
     * allocates space but doesn't change the apparent file size (useful
     * since oflag=resume can be used).
     *
     * If fallocate() with FALLOC_FL_KEEP_SIZE returns ENOTTY, EINVAL or
     * EOPNOTSUPP, retry without that flag (since the flag is only supported
     * in recent Linux kernels). */
    int res;

#ifdef PREALLOC_DEBUG
    pr2serr("About to call fallocate() with FALLOC_FL_KEEP_SIZE\n");
#endif
    res = fallocate(op->odip->fd, FALLOC_FL_KEEP_SIZE, op->obs*op->seek,
                    op->obs*op->dd_count);
#ifdef PREALLOC_DEBUG
    pr2serr("fallocate() returned %d\n", res);
#endif
    /* fallocate() fails if the kernel does not support
     * FALLOC_FL_KEEP_SIZE, so retry without that flag. */
    if (-1 == res) {
        if ((ENOTTY == errno) || (EINVAL == errno)
             || (EOPNOTSUPP == errno)) {
            if (op->verbose)
                pr2serr("Could not pre-allocate with "
                        "FALLOC_FL_KEEP_SIZE (%s), retrying without "
                        "...\n", safe_strerror(errno));
            res = fallocate(op->odip->fd, 0, op->obs*op->seek,
                            op->obs*op->dd_count);
#ifdef PREALLOC_DEBUG
            pr2serr("fallocate() without FALLOC_FL_KEEP_SIZE "
                    " returned %d\n", res);
#endif
        }
    } else {
        /* fallocate() with FALLOC_FL_KEEP_SIZE succeeded. Set
         * op->oflagp->prealloc to 0 so the possible message about using
         * oflag=resume is not suppressed later. */
        op->oflagp->prealloc = 0;
    }
    if (-1 == res) {
            pr2serr("Unable to pre-allocate space: %s\n",
                    safe_strerror(errno));
            return SG_LIB_CAT_OTHER;
    }
    if (op->verbose > 1)
        pr2serr("Pre-allocated %" PRId64 " bytes at offset %"
                PRId64 "\n", op->obs*op->dd_count, op->obs*op->seek);

#endif  /* HAVE_FALLOCATE */
#else   /* other than SG_LIB_LINUX */
#ifdef HAVE_POSIX_FALLOCATE
    int res;

    /* If not on Linux, use posix_fallocate(). (That sets the file size to its
     * full length, so re-invoking ddpt with oflag=resume will do nothing.) */
    res = posix_fallocate(op->odip->fd, op->obs*op->seek,
                          op->obs*op->dd_count);
    if (-1 == res) {
            pr2serr("Unable to pre-allocate space: %s\n",
                    safe_strerror(errno));
            return SG_LIB_CAT_OTHER;
    }
    if (op->verbose > 1)
        pr2serr("Pre-allocated %" PRId64 " bytes at offset %" PRId64 "\n",
                op->obs*op->dd_count, op->obs*op->seek);
#else   /* do not HAVE_POSIX_FALLOCATE */
    if (op) { ; }
#endif  /* HAVE_POSIX_FALLOCATE else */
#endif  /* SG_LIB_LINUX else */
    return 0;
}

static void
details_pre_copy_print(struct opts_t * op)
{
    pr2serr("skip=%" PRId64 " (blocks on input), seek=%" PRId64
            " (blocks on output)\n", op->skip, op->seek);
    if (op->verbose > 1) {
        pr2serr("  ibs=%d bytes, obs=%d bytes, OBPC=%d\n",
                op->ibs, op->obs, op->obpc);
        if (op->ibs != op->ibs_pi)
            pr2serr("  due to protect ibs_pi=%d bytes, "
                    "obs_pi=%d bytes\n", op->ibs_pi, op->obs_pi);
    }
    if (op->reading_fifo && (op->dd_count < 0))
        pr2serr("  reading fifo, blocks_per_transfer=%d\n", op->bpt_i);
    else
        pr2serr("  initial count=%" PRId64 " (blocks of input), "
                "blocks_per_transfer=%d\n", op->dd_count, op->bpt_i);
}

static int
wrk_buffers_init(struct opts_t * op)
{
    int len = op->ibs_pi * op->bpt_i;

    if (op->has_xcopy)
        return 0;
    if (op->iflagp->direct || op->oflagp->direct) {
        size_t psz;

#if defined(HAVE_SYSCONF) && defined(_SC_PAGESIZE)
        psz = sysconf(_SC_PAGESIZE); /* POSIX.1 (was getpagesize()) */
#elif defined(SG_LIB_WIN32)
        psz = my_pagesize();
#else
        psz = 4096;     /* give up, pick likely figure */
#endif

#ifdef HAVE_POSIX_MEMALIGN
        {
            int err;
            void * wp;

            wp = op->wrkBuff;
            err = posix_memalign(&wp, psz, len);
            if (err) {
                pr2serr("posix_memalign: error [%d] out of memory?\n", err);
                return SG_LIB_CAT_OTHER;
            }
            op->wrkBuff = (unsigned char *)wp;
            memset(op->wrkBuff, 0, len);
            op->wrkPos = op->wrkBuff;
            if (op->oflagp->sparing) {
                wp = op->wrkBuff2;
                err = posix_memalign(&wp, psz, len);
                if (err) {
                    pr2serr("posix_memalign(2): error [%d] out of memory?\n",
                             err);
                    return SG_LIB_CAT_OTHER;
                }
                op->wrkBuff2 = (unsigned char *)wp;
                memset(op->wrkBuff2, 0, len);
                op->wrkPos2 = op->wrkBuff2;
            }
        }
#else   /* do not HAVE_POSIX_MEMALIGN */
        op->wrkBuff = (unsigned char*)calloc(len + psz, 1);
        if (0 == op->wrkBuff) {
            pr2serr("Not enough user memory for aligned usage\n");
            return SG_LIB_CAT_OTHER;
        }
        op->wrkPos = (unsigned char *)(((unsigned long)op->wrkBuff + psz - 1) &
                                       (~(psz - 1)));
        if (op->oflagp->sparing) {
            op->wrkBuff2 = (unsigned char*)calloc(len + psz, 1);
            if (0 == op->wrkBuff2) {
                pr2serr("Not enough user memory for aligned usage(2)\n");
                return SG_LIB_CAT_OTHER;
            }
            op->wrkPos2 = (unsigned char *)
                    (((unsigned long)op->wrkBuff2 + psz - 1) & (~(psz - 1)));
        }
#endif  /* HAVE_POSIX_MEMALIGN */
    } else {
        op->wrkBuff = (unsigned char*)calloc(op->ibs_pi * op->bpt_i, 1);
        if (0 == op->wrkBuff) {
            pr2serr("Not enough user memory\n");
            return SG_LIB_CAT_OTHER;
        }
        op->wrkPos = op->wrkBuff;
        if (op->oflagp->sparing) {
            op->wrkBuff2 = (unsigned char*)calloc(op->ibs_pi * op->bpt_i, 1);
            if (0 == op->wrkBuff2) {
                pr2serr("Not enough user memory(2)\n");
                return SG_LIB_CAT_OTHER;
            }
            op->wrkPos2 = op->wrkBuff2;
        }
    }
    return 0;
}

static void
cleanup_resources(struct opts_t * op)
{
#ifdef SG_LIB_LINUX
    if ((FT_TAPE & op->idip->d_type) || (FT_TAPE & op->odip->d_type)) {
        /* For writing, the st driver writes a filemark on closing the file
         * (unless user specified oflag=nofm), so make clear that the
         * position shown is prior to closing. */
        print_tape_pos("Final ", " (before closing file)", op);
        if ((FT_TAPE & op->odip->d_type) && (op->verbose > 1) &&
            op->oflagp->nofm)
            pr2serr("(suppressing writing of filemark on close)\n");
    }
#endif

    if (op->iflagp->errblk)
        errblk_close(op);

    if (op->wrkBuff)
        free(op->wrkBuff);
    if (op->wrkBuff2)
        free(op->wrkBuff2);
    if (op->zeros_buff)
        free(op->zeros_buff);
    if (FT_PT & op->idip->d_type)
        pt_close(op->idip->fd);
    else if ((op->idip->fd >= 0) && (STDIN_FILENO != op->idip->fd))
        close(op->idip->fd);
    if (FT_PT & op->odip->d_type)
        pt_close(op->odip->fd);
    if ((op->odip->fd >= 0) && (STDOUT_FILENO != op->odip->fd) &&
        !(FT_DEV_NULL & op->odip->d_type)) {
#ifdef SG_LIB_LINUX
        if (FT_TAPE & op->odip->d_type)
            tape_cleanup_of(op);
#endif
        close(op->odip->fd);
    }
    if ((op->o2dip->fd >= 0) && (STDOUT_FILENO != op->o2dip->fd))
        close(op->o2dip->fd);
}


/* The main() function: much of the its complex logic is spawned off to
 * helper functions shown directly above. */
int
main(int argc, char * argv[])
{
    int ret = 0;
    int started_copy = 0;
    struct opts_t ops;
    struct flags_t iflag, oflag;
    struct dev_info_t ids, ods, o2ds;
    struct opts_t * op;

    state_init(&ops, &iflag, &oflag, &ids, &ods, &o2ds);
    op = &ops;
    ret = cl_process(op, argc, argv);
    if (op->do_help > 0) {
        usage(op->do_help);
        return 0;
    } else if (ret)
        return (ret < 0) ? 0 : ret;

    if (op->quiet) {
        if (NULL == freopen("/dev/null", "w", stderr))
            pr2serr("freopen: failed to redirect stderr to /dev/null : %s\n",
                    safe_strerror(errno));
    }

#ifdef SG_LIB_WIN32
    if (op->wscan)
        return sg_do_wscan('\0', op->wscan, op->verbose);
#endif

    install_signal_handlers(op);

    if ((ret = open_files_devices(op)))
        return ret;

    block_size_bpt_check(op);
    sparse_sparing_check(op);

    if ((ret = count_calculate(op))) {
        if (op->verbose)
            pr2serr("count_calculate() returned %d, exit\n", ret);
        goto cleanup;
    }

    if ((ret = prepare_pi(op)))
        goto cleanup;

    if ((op->dd_count < 0) && (! op->reading_fifo)) {
        pr2serr("Couldn't calculate count, please give one\n");
        ret = SG_LIB_CAT_OTHER;
        goto cleanup;
    }

    cdb_size_prealloc(op);

    if ((ret = wrk_buffers_init(op)))
        goto cleanup;

    if (ops.verbose)
        details_pre_copy_print(op);

    op->read1_or_transfer = !! (FT_DEV_NULL & op->odip->d_type);
    if (op->read1_or_transfer && (! op->outf_given) &&
        ((op->dd_count > 0) || op->reading_fifo))
        pr2serr("Output file not specified so no copy, just reading input\n");

    if (op->do_time)
        calc_duration_init(op);

    if (op->iflagp->errblk)
        errblk_open(op);

#ifdef SG_LIB_LINUX
    if ((FT_TAPE & op->idip->d_type) || (FT_TAPE & op->odip->d_type))
        print_tape_pos("Initial ", "", op);
#endif

    if (op->oflagp->prealloc) {
        if ((ret = do_falloc(op)))
            goto cleanup;
    }

    ++started_copy;
    if (op->has_xcopy)
        ret = do_xcopy(op);
    else
        ret = do_rw_copy(op);

    if (0 == op->status_none)
        print_stats("", op);

    if ((op->oflagp->ssync) && (FT_PT & op->odip->d_type)) {
        if (0 == op->status_none)
            pr2serr(">> SCSI synchronizing cache on %s\n", op->odip->fn);
        pt_sync_cache(op->odip->fd);
    }
    if (op->do_time)
        calc_duration_throughput("", 0, op);

    if (op->sum_of_resids)
        pr2serr(">> Non-zero sum of residual counts=%d\n", op->sum_of_resids);

cleanup:
    cleanup_resources(op);
    if ((0 == ret) && op->err_to_report)
        ret = op->err_to_report;
    if (started_copy && (0 != op->dd_count) && (! op->reading_fifo)) {
        if (0 == ret)
            pr2serr("Early termination, EOF on input?\n");
        else if (SG_LIB_CAT_MEDIUM_HARD == ret)
            pr2serr("Early termination, medium error occurred\n");
        else if ((SG_LIB_CAT_PROTECTION == ret) ||
                 (SG_LIB_CAT_PROTECTION_WITH_INFO == ret))
            pr2serr("Early termination, protection information "
                    "error occurred\n");
        else
            pr2serr("Early termination, some error occurred\n");
    }
    return (ret >= 0) ? ret : SG_LIB_CAT_OTHER;
}
