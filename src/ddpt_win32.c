/*
 * Copyright (c) 2010-2019, Douglas Gilbert
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

/* This is a C source file contains Windows specific code for the ddpt
 * utility.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef SG_LIB_WIN32

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
#include <sys/time.h>

#include "ddpt.h"       /* includes <signal.h> */

#include <windows.h>
#include <winioctl.h>

#ifndef SG_LIB_MINGW
/* cygwin */
#include <sys/ioctl.h>
#endif

#include "sg_lib.h"
#include "sg_cmds_basic.h"
#include "sg_cmds_extra.h"
#include "sg_pr2serr.h"
#include "sg_pt.h"

#ifdef HAVE_NANOSLEEP
#include <time.h>
#elif defined(MSC_VER) || defined(__MINGW32__)
#define HAVE_MS_SLEEP
#endif


#ifndef HAVE_SYSCONF
size_t
win32_pagesize(void)
{
    SYSTEM_INFO sys_info;

    GetSystemInfo(&sys_info);
    return sys_info.dwPageSize;
}
#endif

void
win32_sleep_ms(int millisecs)
{
    if (millisecs > 0) {
#ifdef HAVE_NANOSLEEP
        struct timespec request;

        request.tv_sec = millisecs / 1000;
        request.tv_nsec = (millisecs % 1000) * 1000000;
        if ((nanosleep(&request, NULL) < 0) && (EINTR != errno))
            pr2serr("nanosleep: failed, errno=%d\n", errno);
#elif defined(HAVE_MS_SLEEP)
        Sleep(millisecs);
#endif
    }
}

/* Fetches system error message corresponding to errnum,
 * placing string in b not exceeding blen bytes. Returns
 * bytes placed in b (excluding trailing NULL) or -1 for
 * error. MS refers to them as "System Error Codes". */
static const char *
win32_errmsg(int errnum, char * b, int blen)
{
    LPTSTR err_txt = 0;
    DWORD errn = errnum;
    int len = 0;

    if (FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                      FORMAT_MESSAGE_FROM_SYSTEM,
                      NULL,
                      errn,
                      0,
                      (LPTSTR)&err_txt,
                      0,
                      NULL) == 0) {
        snprintf(b, blen, "FormatMessage(errnum=%d) failed", errnum);
        return b;
    } else {
        len = strlen(err_txt);
        if (len) {
            if ('\n' == err_txt[len - 1]) {
                err_txt[len - 1] = '\0';
                if ((len > 1) && ('\r' == err_txt[len - 2]))
                    err_txt[len - 2] = '\0';
                len = strlen(err_txt);
            }
        }
        if (len < 1)
            b[0] = '\0';
        else if (len < blen)
            strcpy(b, err_txt);
        else {
            strncpy(b, err_txt, blen);
            if (blen > 0)
                b[blen - 1] = '\0';
        }
    }
    if (err_txt)
        LocalFree(err_txt);
    len = strlen(b);
    snprintf(b + len, blen - len, " [%d]", errnum);
    return b;
}

/* Return 1 for filenames starting with '\', or of the form '<letter>:'
 * or of the form PD<n>, PHYSICALDRIVE<n>, CDROM<n> or TAPE<n>. The <n>
 * is one or two digits with no following characters. Otherwise return 0. */
static int
is_win_blk_dev(const char * fn)
{
    int len, off;

    len = strlen(fn);
    if ((2 == len) && isalpha((int)fn[0]) && (':' == fn[1]))
        return 1;
    if (len < 3)
        return 0;
    if ('\\' == fn[0])
        return 1;
    if (0 == strncmp(fn, "PD", 2))
        off = 2;
    else if (0 == strncmp(fn, "CDROM", 5))
        off = 5;
    else if (0 == strncmp(fn, "PHYSICALDRIVE", 13))
        off = 13;
    else if (0 == strncmp(fn, "TAPE", 4))
        off = 4;
    else
        return 0;

    if (len <= off)
        return 0;
    if (! isdigit((int)fn[off]))
        return 0;
    if (len == (off + 1))
        return 1;
    if ((len != off + 2) || (! isdigit((int)fn[off + 1])))
        return 0;
    else
        return 1;
}

int
win32_dd_filetype(const char * fn, int verbose)
{
    size_t len = strlen(fn);

    if (verbose) { ; }          /* suppress warning */
    if ((1 == len) && ('.' == fn[0]))
        return FT_DEV_NULL;
    else if ((3 == len) && (
             (0 == strcmp("NUL", fn)) || (0 == strcmp("nul", fn))))
        return FT_DEV_NULL;
    else if ((len > 8) && (0 == strncmp("\\\\.\\TAPE", fn, 8)))
        return FT_TAPE;
    else if ((len > 4) && (0 == strncmp("\\\\.\\", fn, 4)))
        return FT_BLOCK;
    else
        return FT_REG;
}

/* Adjust device file name for Windows; pass-through setup */
void
win32_adjust_fns_pt(struct opts_t * op)
{
    char b[INOUTF_SZ];
    char * fn_arr[2];
    char * cp;
    int k, j, len;

    memset(fn_arr, 0 , sizeof(fn_arr));
    fn_arr[0] = op->idip->fn;
    fn_arr[1] = op->odip->fn;
    for (k = 0; k < 2; ++k) {
        cp = fn_arr[k];
        if (NULL == cp)
            continue;
        len = strlen(cp);
        if (len < 2)
            continue;
        if ('\\' == cp[0])
            continue;
        for (j = 0; j < len; ++j)
            b[j] = toupper((int)cp[j]);
        b[len] = '\0';
        if (is_win_blk_dev(b)) {
            if (0 == strncmp(b, "PD", 2)) {
                strcpy(cp, "\\\\.\\PHYSICALDRIVE");
                if (b[2])
                    strncat(cp, b + 2, len - 2);
            } else {
                strcpy(cp, "\\\\.\\");
                strncat(cp, b, len);
            }
        }
    }
#ifdef SG_LIB_WIN32_DIRECT
    if (op->verbose > 4)
        pr2serr("Initial win32 SPT interface state: %s\n",
                scsi_pt_win32_spt_state() ? "direct" : "indirect");
    scsi_pt_win32_direct(SG_LIB_WIN32_DIRECT /* SPT pt interface */);
#endif
}

/* Main copy loop's read (input) for win32 block device. Returns 0 on
 * success, else SG_LIB_FILE_ERROR, SG_LIB_CAT_MEDIUM_HARD or -1 . */
int
win32_cp_read_block(struct opts_t * op, struct cp_state_t * csp,
                    uint8_t * bp, int * ifull_extrap, int verbose)
{
    int k, res, res2;
    int ibs = op->ibs_pi;
    int obs = op->obs_pi;
    int numbytes = csp->cur_in_num * ibs;
    int64_t lba = csp->cur_in_lba;
    int64_t offset = lba * ibs;
    int64_t my_lba;

    if (ifull_extrap)
        *ifull_extrap = 0;
    if (offset != csp->in_iter.filepos) {
        if (verbose > 2)
            pr2serr("moving if filepos: new_pos=%" PRId64 "\n",
                    (int64_t)offset);
        if (win32_set_file_pos(op->idip, offset, verbose))
            return SG_LIB_FILE_ERROR;
        csp->in_iter.filepos = offset;
    }
    res = win32_block_read(op, bp, numbytes, verbose);
    if (res < 0) {
        if ((-SG_LIB_CAT_MEDIUM_HARD == res) && (op->iflagp->coe)) {
            if (1 == csp->icbpt) {
                // Don't read again, this must be bad block
                memset(bp, 0, ibs);
                if ((res2 = coe_process_eio(op, lba)))
                    return res2;
                ++*ifull_extrap;
                csp->bytes_read += ibs;
                return 0;
            } else {
                my_lba = lba;
                for (k = 0; k < csp->icbpt;
                     ++k, ++my_lba, bp += ibs, offset += ibs) {
                    if (offset != csp->in_iter.filepos) {
                        if (verbose > 2)
                            pr2serr("moving if filepos: new_pos=%" PRId64
                                    "\n", (int64_t)offset);
                        if (win32_set_file_pos(op->idip, offset, verbose))
                            return SG_LIB_FILE_ERROR;
                        csp->in_iter.filepos = offset;
                    }
                    memset(bp, 0, ibs);
                    res = win32_block_read(op, bp, ibs, verbose);
                    if (ibs == res) {
                        zero_coe_limit_count(op);
                        csp->in_iter.filepos += ibs;
                        if (verbose > 2)
                            pr2serr("reading 1 block, lba=%" PRId64 " : "
                                    "okay\n", my_lba);
                    } else if (-SG_LIB_CAT_MEDIUM_HARD == res) {
                        if ((res2 = coe_process_eio(op, my_lba)))
                            return res2;
                    } else {
                        pr2serr("reading 1 block, lba=%" PRId64 " failed\n",
                                my_lba);
                        csp->leave_reason = SG_LIB_CAT_OTHER;
                        csp->icbpt = k;
                        csp->ocbpt = (k * ibs) / obs;
                        if (((k * ibs) % obs) > 0)
                            ++csp->ocbpt;
                        return 0;
                    }
                    ++*ifull_extrap;
                    csp->bytes_read += ibs;
                }
                return 0;
            }
        } else {
            pr2serr("read(win32_block), lba=%" PRId64 " error occurred\n",
                    lba);
            return (-SG_LIB_CAT_MEDIUM_HARD == res) ? -res : -1;
        }
    } else {
        if (res < numbytes) {
            /* assume no partial reads (i.e. non integral blocks) */
            csp->icbpt = res / ibs;
            csp->leave_after_write = true;
            csp->leave_reason = 0; /* assume at end rather than error */
            csp->ocbpt = res / obs;
            if (verbose > 1)
                pr2serr("short read, requested %d blocks, got %d blocks\n",
                        numbytes / ibs, csp->icbpt);
        }
        csp->in_iter.filepos += res;
        if (ifull_extrap)
            *ifull_extrap = csp->icbpt;
    }
    return 0;
}

/* Returns 0 on success, 1 on error */
int
win32_open_if(struct opts_t * op, int flags, int verbose)
{
    int blen;
    int ibs = op->ibs_pi;
    DWORD count, share_mode, err;
    DISK_GEOMETRY g;
    char b[80];

    blen = sizeof(b);
    if (verbose)
        pr2serr("CreateFile(%s , in)\n", op->idip->fn);
    share_mode = (O_EXCL & flags) ? 0 : (FILE_SHARE_READ | FILE_SHARE_WRITE);
    op->idip->fh = CreateFile(op->idip->fn,
                              GENERIC_READ | GENERIC_WRITE,
                              share_mode,
                              NULL,
                              OPEN_EXISTING,
                              0,
                              NULL);
    if (INVALID_HANDLE_VALUE == op->idip->fh) {
        err = GetLastError();
        pr2serr("CreateFile(in) failed, %s\n", win32_errmsg(err, b, blen));
        return 1;
    }
    if (0 == DeviceIoControl(op->idip->fh, IOCTL_DISK_GET_DRIVE_GEOMETRY,
                             NULL, 0, &g, sizeof(g), &count, NULL)) {
        err = GetLastError();
        pr2serr("DeviceIoControl(in, geometry) failed, %s\n",
                win32_errmsg(err, b, blen));
        return 1;
    }
    if ((int)g.BytesPerSector != ibs) {
        pr2serr("Specified in block size (%d) doesn't match device geometry "
                "block size: %d\n", ibs, (int)g.BytesPerSector);
        return 1;
    }
    return 0;
}

/* Returns 0 on success, 1 on error. */
int
win32_open_of(struct opts_t * op, int flags, int verbose)
{
    int blen;
    int obs = op->obs_pi;
    DWORD count, share_mode, err;
    DISK_GEOMETRY g;
    char b[80];

    blen = sizeof(b);
    if (verbose)
        pr2serr("CreateFile(%s , out)\n", op->odip->fn);
    share_mode = (O_EXCL & flags) ? 0 : (FILE_SHARE_READ | FILE_SHARE_WRITE);
    op->odip->fh = CreateFile(op->odip->fn,
                              GENERIC_READ | GENERIC_WRITE,
                              share_mode,
                              NULL,
                              OPEN_EXISTING,
                              0,
                              NULL);
    if (INVALID_HANDLE_VALUE == op->odip->fh) {
        err = GetLastError();
        pr2serr("CreateFile(out) failed, %s\n", win32_errmsg(err, b, blen));
        return 1;
    }
    if (0 == DeviceIoControl(op->odip->fh, IOCTL_DISK_GET_DRIVE_GEOMETRY,
                             NULL, 0, &g, sizeof(g), &count, NULL)) {
        err = GetLastError();
        pr2serr("DeviceIoControl(out, geometry) failed, %s\n",
                win32_errmsg(err, b, blen));
        return 1;
    }
    if ((int)g.BytesPerSector != obs) {
        pr2serr("Specified out block size (%d) doesn't match device geometry "
                "block size: %d\n", obs,
                (int)g.BytesPerSector);
        return 1;
    }
    return 0;
}

/* Returns 0 on success, 1 on error */
int
win32_set_file_pos(struct dev_info_t * dip, int64_t pos, int verbose)
{
    int blen;
    LONG lo32 = pos & 0xffffffff;
    LONG hi32 = (pos >> 32) & 0xffffffff;
    DWORD err;
    DWORD lo_ret;
    HANDLE fh;
    const char * cp;
    char b[80];

    blen = sizeof(b);
    fh = dip->fh;
    cp = dip->dir_n;
    if (verbose > 2)
        pr2serr("SetFilePointer( 0x%" PRIx64 ", %s)\n", pos, cp);
    lo_ret = SetFilePointer(fh, lo32, &hi32, FILE_BEGIN);
    if ((INVALID_SET_FILE_POINTER == lo_ret) &&
        (NO_ERROR != (err = GetLastError()))) {
        if (verbose)
            pr2serr("SetFilePointer failed to set pos=[0x%" PRIx64 "], %s\n",
                    pos, win32_errmsg(err, b, blen));
        return 1;
    }
    return 0;
}

/* Returns number read, -SG_LIB_CAT_MEDIUM_HARD or -1 on error */
int
win32_block_read(struct opts_t * op, uint8_t * bp, int num_bytes,
                 int verbose)
{
    int blen;
    DWORD num = num_bytes;
    DWORD howMany, err;
    char b[80];

    blen = sizeof(b);
    if (verbose > 2)
        pr2serr("ReadFile(num=%d, in)\n", num_bytes);
    if (ReadFile(op->idip->fh, bp, num, &howMany, NULL) == 0) {
        err = GetLastError();
        if (verbose)
            pr2serr("ReadFile failed, %s\n", win32_errmsg(err, b, blen));
        if (23 == err)
            return -SG_LIB_CAT_MEDIUM_HARD;
        else
            return -1;
    }
    return (int)howMany;
}

/* Returns number read from OFILE, -SG_LIB_CAT_MEDIUM_HARD or -1 on error */
int
win32_block_read_from_of(struct opts_t * op, uint8_t * bp,
                         int num_bytes, int verbose)
{
    int blen;
    DWORD num = num_bytes;
    DWORD howMany, err;
    char b[80];

    blen = sizeof(b);
    if (verbose > 2)
        pr2serr("ReadFile(num=%d, out)\n", num_bytes);
    if (ReadFile(op->odip->fh, bp, num, &howMany, NULL) == 0) {
        err = GetLastError();
        if (verbose)
            pr2serr("ReadFile(from_of) failed, %s\n",
                    win32_errmsg(err, b, blen));
        if (23 == err)
            return -SG_LIB_CAT_MEDIUM_HARD;
        else
            return -1;
    }
    return (int)howMany;
}

/* Returns number written, -SG_LIB_CAT_MEDIUM_HARD or -1 on error */
int
win32_block_write(struct opts_t * op, const uint8_t * bp, int num_bytes,
                  int verbose)
{
    int blen;
    DWORD num = num_bytes;
    DWORD howMany, err;
    char b[80];

    blen = sizeof(b);
    if (verbose > 2)
        pr2serr("WriteFile(num=%d, out)\n", num_bytes);
    if (WriteFile(op->odip->fh, bp, num, &howMany, NULL) == 0) {
        err = GetLastError();
        if (verbose)
            pr2serr("WriteFile failed, %s\n", win32_errmsg(err, b, blen));
        if (23 == err)
            return -SG_LIB_CAT_MEDIUM_HARD;
        else
            return -1;
    }
    return (int)howMany;
}

/* win32_get_blkdev_capacity() returns 0 -> success or -1 -> failure. If
 * successful writes back sector size (logical block size) using the sect_sz
 * pointer. Also writes back the number of sectors (logical blocks) on the
 * block device using num_sect pointer. Win32 version. */
int
win32_get_blkdev_capacity(struct opts_t * op, int which_arg,
                          int64_t * num_sect, int * sect_sz)
{
    int blen, fname_len;
    ULARGE_INTEGER total_bytes;
    DWORD count, err;
    int64_t byte_len, blks;
    HANDLE fh;
    const char * fname;
    DISK_GEOMETRY g;
    GET_LENGTH_INFORMATION gli;
    char dirName[64];
    char b[80];

    blen = sizeof(b);
    fh = (DDPT_ARG_IN == which_arg) ? op->idip->fh : op->odip->fh;
    fname = (DDPT_ARG_IN == which_arg) ? op->idip->fn : op->odip->fn;
    if (op->verbose > 2)
        pr2serr("win32_get_blkdev_capacity: for %s\n", fname);
    if (0 == DeviceIoControl(fh, IOCTL_DISK_GET_DRIVE_GEOMETRY, NULL, 0, &g,
                             sizeof(g), &count, NULL)) {
        if (op->verbose) {
            err = GetLastError();
            pr2serr("DeviceIoControl(blkdev, geometry) failed, %s\n",
                    win32_errmsg(err, b, blen));
        }
        *num_sect = 0;
        *sect_sz = 0;
        return -1;
    }
    *sect_sz = (int)g.BytesPerSector;

    /* IOCTL_DISK_GET_LENGTH_INFO not defined before XP */
    if (DeviceIoControl(fh, IOCTL_DISK_GET_LENGTH_INFO, NULL, 0, &gli,
                        sizeof(gli), &count, NULL)) {
        byte_len = gli.Length.QuadPart;
        *num_sect = byte_len / (int)g.BytesPerSector;
        return 0;
    } else if (op->verbose > 2) {
        err = GetLastError();
        pr2serr("DeviceIoControl(blkdev, length_info) failed, %s\n",
                win32_errmsg(err, b, blen));
    }

    /* Assume if device name finishes in digit then its physical */
    fname_len = (int)strlen(fname);
    if (isdigit((int)fname[fname_len - 1])) {
        blks = g.Cylinders.QuadPart;
        blks *= g.TracksPerCylinder;
        blks *= g.SectorsPerTrack;
        *num_sect = blks;
        return 0;
    }
    if ((fname_len < 4) || (fname_len > (int)sizeof(dirName))) {
        pr2serr("win32_get_blkdev_capacity: unable to process %s into "
                "directory name\n", fname);
        *num_sect = 0;
        return -1;
    }
    memcpy(dirName, fname + 4, fname_len - 4);
    dirName[fname_len - 4] = '\\';
    dirName[fname_len - 3] = '\0';

    if (GetDiskFreeSpaceEx(dirName, NULL, &total_bytes, NULL)) {
        byte_len = total_bytes.QuadPart;
        *num_sect = byte_len / (int)g.BytesPerSector;
    } else {
        if (op->verbose > 1) {
            err = GetLastError();
            pr2serr("GetDiskFreeSpaceEx(%s) failed, %s\n", dirName,
                    win32_errmsg(err, b, blen));
        }
        *num_sect = 0;
        return -1;
    }
    return 0;
}

#endif


