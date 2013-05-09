/*
 * Copyright (c) 2010-2013 Douglas Gilbert.
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
#include <sys/time.h>

#include "ddpt.h"

#include <windows.h>
#include <winioctl.h>

#ifndef SG_LIB_MINGW
/* cygwin */
#include <sys/ioctl.h>
#endif

#include "sg_lib.h"
#include "sg_cmds_basic.h"
#include "sg_cmds_extra.h"
#include "sg_pt.h"


/* Fetches system error message corresponding to errnum,
 * placing string in b not exceeding blen bytes. Returns
 * bytes placed in b (excluding trailing NULL) or -1 for
 * error. MS refers to them as "System Error Codes". */
static int
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
                      NULL) == 0)
        return -1;
    else {
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
    return len;
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
dd_filetype(const char * fn, int verbose)
{
    size_t len = strlen(fn);

    verbose = verbose;  /* suppress warning */
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

/* Adjust device file name for Windows */
void
win32_adjust_fns(struct opts_t * op)
{
    char b[INOUTF_SZ];
    char * fn_arr[2];
    char * cp;
    int k, j, len;

    memset(fn_arr, 0 , sizeof(fn_arr));
    fn_arr[0] = op->inf;
    fn_arr[1] = op->outf;
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
}

/* Main copy loop's read (input) for win32 block device. Returns 0 on
 * success, else SG_LIB_FILE_ERROR, SG_LIB_CAT_MEDIUM_HARD or -1 . */
int
win32_cp_read_block(struct opts_t * op, struct cp_state_t * csp,
                    unsigned char * wrkPos, int * ifull_extrap,
                    int verbose)
{
    int k, res, res2;
    int ibs = op->ibs;
    int64_t offset = op->skip * ibs;
    int64_t my_skip;
    int numbytes = csp->icbpt * ibs;

    if (ifull_extrap)
        *ifull_extrap = 0;
    if (offset != csp->if_filepos) {
        if (verbose > 2)
            fprintf(stderr, "moving if filepos: new_pos="
                    "%"PRId64"\n", (int64_t)offset);
        if (win32_set_file_pos(op, DDPT_ARG_IN, offset, verbose))
            return SG_LIB_FILE_ERROR;
        csp->if_filepos = offset;
    }
    res = win32_block_read(op, wrkPos, numbytes, verbose);
    if (res < 0) {
        if ((-SG_LIB_CAT_MEDIUM_HARD == res) && (op->iflagp->coe)) {
            if (1 == csp->icbpt) {
                // Don't read again, this must be bad block
                memset(wrkPos, 0, ibs);
                if ((res2 = coe_process_eio(op, op->skip)))
                    return res2;
                ++*ifull_extrap;
                csp->bytes_read += ibs;
                return 0;
            } else {
                my_skip = op->skip;
                for (k = 0; k < csp->icbpt;
                     ++k, ++my_skip, wrkPos += ibs, offset += ibs) {
                    if (offset != csp->if_filepos) {
                        if (verbose > 2)
                            fprintf(stderr, "moving if filepos: new_pos="
                                    "%"PRId64"\n", (int64_t)offset);
                        if (win32_set_file_pos(op, DDPT_ARG_IN, offset,
                            verbose))
                            return SG_LIB_FILE_ERROR;
                        csp->if_filepos = offset;
                    }
                    memset(wrkPos, 0, ibs);
                    res = win32_block_read(op, wrkPos, ibs, verbose);
                    if (ibs == res) {
                        zero_coe_limit_count(op);
                        csp->if_filepos += ibs;
                        if (verbose > 2)
                            fprintf(stderr, "reading 1 block, skip=%"PRId64
                                    " : okay\n", my_skip);
                    } else if (-SG_LIB_CAT_MEDIUM_HARD == res) {
                        if ((res2 = coe_process_eio(op, my_skip)))
                            return res2;
                    } else {
                        fprintf(stderr, "reading 1 block, skip=%"PRId64
                                " failed\n", my_skip);
                        csp->leave_reason = SG_LIB_CAT_OTHER;
                        csp->icbpt = k;
                        csp->ocbpt = (k * ibs) / op->obs;
                        if (((k * ibs) % op->obs) > 0)
                            ++csp->ocbpt;
                        return 0;
                    }
                    ++*ifull_extrap;
                    csp->bytes_read += ibs;
                }
                return 0;
            }
        } else {
            fprintf(stderr, "read(win32_block), skip=%"PRId64
                    " error occurred\n", op->skip);
            return (-SG_LIB_CAT_MEDIUM_HARD == res) ? -res : -1;
        }
    } else {
        if (res < numbytes) {
            /* assume no partial reads (i.e. non integral blocks) */
            csp->icbpt = res / ibs;
            ++csp->leave_after_write;
            csp->leave_reason = 0; /* assume at end rather than error */
            csp->ocbpt = res / op->obs;
            if (verbose > 1)
                fprintf(stderr, "short read, requested %d blocks, got "
                        "%d blocks\n", numbytes / ibs, csp->icbpt);
        }
        csp->if_filepos += res;
        if (ifull_extrap)
            *ifull_extrap = csp->icbpt;
    }
    return 0;
}

/* Returns 0 on success, 1 on error */
int
win32_open_if(struct opts_t * op, int verbose)
{
    DISK_GEOMETRY g;
    DWORD count, err;
    char b[80];

    if (verbose)
        fprintf(stderr, "CreateFile(%s , in)\n", op->inf);
    op->ib_fh = CreateFile(op->inf,
                              GENERIC_READ | GENERIC_WRITE,
                              FILE_SHARE_WRITE | FILE_SHARE_READ,
                              NULL,
                              OPEN_EXISTING,
                              0,
                              NULL);
    if (INVALID_HANDLE_VALUE == op->ib_fh) {
        err = GetLastError();
        if (win32_errmsg(err, b, sizeof(b)) < 0)
            fprintf(stderr, "CreateFile(in) failed, error=%ld [and "
                    "win32_errmsg() failed]\n", err);
        else
            fprintf(stderr, "CreateFile(in) failed, %s [%ld]\n", b, err);
        return 1;
    }
    if (0 == DeviceIoControl(op->ib_fh, IOCTL_DISK_GET_DRIVE_GEOMETRY,
                             NULL, 0, &g, sizeof(g), &count, NULL)) {
        fprintf(stderr, "DeviceIoControl(in, geometry) error=%ld\n",
                GetLastError());
        return 1;
    }
    if ((int)g.BytesPerSector != op->ibs) {
        fprintf(stderr, "Specified in block size (%d) doesn't match device "
                "geometry block size: %d\n", op->ibs,
                (int)g.BytesPerSector);
        return 1;
    }
    return 0;
}

/* Returns 0 on success, 1 on error. */
int
win32_open_of(struct opts_t * op, int verbose)
{
    DISK_GEOMETRY g;
    DWORD count, err;
    char b[80];

    if (verbose)
        fprintf(stderr, "CreateFile(%s , out)\n", op->outf);
    op->ob_fh = CreateFile(op->outf,
                              GENERIC_READ | GENERIC_WRITE,
                              FILE_SHARE_WRITE | FILE_SHARE_READ,
                              NULL,
                              OPEN_EXISTING,
                              0,
                              NULL);
    if (INVALID_HANDLE_VALUE == op->ob_fh) {
        err = GetLastError();
        if (win32_errmsg(err, b, sizeof(b)) < 0)
            fprintf(stderr, "CreateFile(out) failed, error=%ld [and "
                    "win32_errmsg() failed]\n", err);
        else
            fprintf(stderr, "CreateFile(out) failed, %s [%ld]\n", b, err);
        return 1;
    }
    if (0 == DeviceIoControl(op->ob_fh, IOCTL_DISK_GET_DRIVE_GEOMETRY,
                             NULL, 0, &g, sizeof(g), &count, NULL)) {
        fprintf(stderr, "DeviceIoControl(out, geometry) error=%ld\n",
                GetLastError());
        return 1;
    }
    if ((int)g.BytesPerSector != op->obs) {
        fprintf(stderr, "Specified out block size (%d) doesn't match device "
                "geometry block size: %d\n", op->obs,
                (int)g.BytesPerSector);
        return 1;
    }
    return 0;
}

/* Returns 0 on success, 1 on error */
int
win32_set_file_pos(struct opts_t * op, int which_arg, int64_t pos,
                   int verbose)
{
    LONG lo32 = pos & 0xffffffff;
    LONG hi32 = (pos >> 32) & 0xffffffff;
    DWORD err;
    DWORD lo_ret;
    HANDLE fh;
    const char * cp;

    fh = (DDPT_ARG_IN == which_arg) ? op->ib_fh : op->ob_fh;
    cp = (DDPT_ARG_IN == which_arg) ? "in" : "out";
    if (verbose > 2)
        fprintf(stderr, "SetFilePointer( 0x%"PRIx64", %s)\n", pos, cp);
    lo_ret = SetFilePointer(fh, lo32, &hi32, FILE_BEGIN);
    if ((INVALID_SET_FILE_POINTER == lo_ret) &&
        (NO_ERROR != (err = GetLastError()))) {
        if (verbose)
            fprintf(stderr, "SetFilePointer failed to set "
                    "pos=[0x%"PRIx64"], error=%ld\n", pos, err);
        return 1;
    }
    return 0;
}

/* Returns number read, -SG_LIB_CAT_MEDIUM_HARD or -1 on error */
int
win32_block_read(struct opts_t * op, unsigned char * bp, int num_bytes,
                 int verbose)
{
    DWORD num = num_bytes;
    DWORD howMany, err;
    char b[80];

    if (verbose > 2)
        fprintf(stderr, "ReadFile(num=%d, in)\n", num_bytes);
    if (ReadFile(op->ib_fh, bp, num, &howMany, NULL) == 0) {
        err = GetLastError();
        if (verbose) {
            if (win32_errmsg(err, b, sizeof(b)) < 0)
                fprintf(stderr, "ReadFile failed, error=%ld [and "
                        "win32_errmsg() failed]\n", err);
            else
                fprintf(stderr, "ReadFile failed, %s [%ld]\n", b, err);
        }
        if (23 == err)
            return -SG_LIB_CAT_MEDIUM_HARD;
        else
            return -1;
    }
    return (int)howMany;
}

/* Returns number read from OFILE, -SG_LIB_CAT_MEDIUM_HARD or -1 on error */
int
win32_block_read_from_of(struct opts_t * op, unsigned char * bp,
                         int num_bytes, int verbose)
{
    DWORD num = num_bytes;
    DWORD howMany, err;
    char b[80];

    if (verbose > 2)
        fprintf(stderr, "ReadFile(num=%d, out)\n", num_bytes);
    if (ReadFile(op->ob_fh, bp, num, &howMany, NULL) == 0) {
        err = GetLastError();
        if (verbose) {
            if (win32_errmsg(err, b, sizeof(b)) < 0)
                fprintf(stderr, "ReadFile failed, error=%ld [and "
                        "win32_errmsg() failed]\n", err);
            else
                fprintf(stderr, "ReadFile failed, %s [%ld]\n", b, err);
        }
        if (23 == err)
            return -SG_LIB_CAT_MEDIUM_HARD;
        else
            return -1;
    }
    return (int)howMany;
}

/* Returns number written, -SG_LIB_CAT_MEDIUM_HARD or -1 on error */
int
win32_block_write(struct opts_t * op, const unsigned char * bp,
                  int num_bytes, int verbose)
{
    DWORD num = num_bytes;
    DWORD howMany, err;
    char b[80];

    if (verbose > 2)
        fprintf(stderr, "WriteFile(num=%d, out)\n", num_bytes);
    if (WriteFile(op->ob_fh, bp, num, &howMany, NULL) == 0) {
        err = GetLastError();
        if (verbose) {
            if (win32_errmsg(err, b, sizeof(b)) < 0)
                fprintf(stderr, "WriteFile failed, error=%ld [and "
                        "win32_errmsg() failed]\n", err);
            else
                fprintf(stderr, "WriteFile failed, %s [%ld]\n", b, err);
        }
        if (23 == err)
            return -SG_LIB_CAT_MEDIUM_HARD;
        else
            return -1;
    }
    return (int)howMany;
}

/* get_blkdev_capacity() returns 0 -> success or -1 -> failure. If
 * successful writes back sector size (logical block size) using the sect_sz
 * pointer. Also writes back the number of sectors (logical blocks) on the
 * block device using num_sect pointer. Win32 version. */
int
get_blkdev_capacity(struct opts_t * op, int which_arg, int64_t * num_sect,
                    int * sect_sz)
{
    DISK_GEOMETRY g;
    GET_LENGTH_INFORMATION gli;
    ULARGE_INTEGER total_bytes;
    DWORD count;
    HANDLE fh;
    const char * fname;
    int64_t byte_len, blks;
    int fname_len;
    char dirName[64];

    fh = (DDPT_ARG_IN == which_arg) ? op->ib_fh : op->ob_fh;
    fname = (DDPT_ARG_IN == which_arg) ? op->inf : op->outf;
    if (op->verbose > 2)
        fprintf(stderr, "get_blkdev_capacity: for %s\n", fname);
    if (0 == DeviceIoControl(fh, IOCTL_DISK_GET_DRIVE_GEOMETRY, NULL, 0, &g,
                             sizeof(g), &count, NULL)) {
        if (op->verbose)
            fprintf(stderr, "DeviceIoControl(blkdev, geometry) error=%ld\n",
                    GetLastError());
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
    } else if (op->verbose > 2)
        fprintf(stderr, "DeviceIoControl(blkdev, length_info) "
                "error=%ld\n", GetLastError());

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
        fprintf(stderr, "get_blkdev_capacity: unable to process %s into "
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
    } else if (op->verbose > 1) {
            fprintf(stderr, "GetDiskFreeSpaceEx(%s) "
                    "error=%ld\n", dirName, GetLastError());
        *num_sect = 0;
        return -1;
    }
    return 0;
}

#endif
