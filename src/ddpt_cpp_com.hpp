/*
 * Copyright (c) 2026, Douglas Gilbert
 * All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 *
 */

#ifndef DDPT_CPP_COM_HPP
#define DDPT_CPP_COM_HPP

/* This is a C++ header file for the ddpt_sgl and ddpt_sparse utilities.
 * See ddpt_sgl.cpp and ddpt_spare.cpp for more information.
 */

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

/* Note that C++ headers (e.g.  '#include <vector>') placed towards the end
 * of this file */
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

#include "ddpt.h"

typedef std::vector<struct scat_gath_elem> sgl_vect;

static const uint8_t EXTENT_TYPE_UNKN { };   /* unknown, probably mapped */
static const uint8_t EXTENT_TYPE_MAPPED { };
static const uint8_t EXTENT_TYPE_UNMAPPED { };
static const uint8_t EXTENT_TYPE_ANCHORED { };


/* An LBA extent is a contiguous number of logical blocks starting
 * at a given LBA. struct lba_extent_w_typ holds an LBA extent in 16
 * bytes where the LBA is held in 8 bytes, the number of blocks is
 * is held in 7 bytes and the LBA Extent type is held in 1 byte.
 * It is designed to interwork with struct scat_gath_elem which is
 * 12 bytes long (8 bytes for the LBA, 4 bytes for the number of
 * logical blocks). (i.e. 8+8 bytes) want a 7 byte
 * num (56 bits) and a byte-sized lbae_typ
 * to indicate unknown/mapped/
 * unmapped/de-allocated/anchored .
 */
struct lba_extent_w_typ : public /* struct */ scat_gath_elem {
public:
    explicit lba_extent_w_typ() noexcept
        : scat_gath_elem{0, {0}} { }
    lba_extent_w_typ(const lba_extent_w_typ & lbae) noexcept
        : scat_gath_elem{lbae.lba, {lbae.com_num_typ}} { }
    lba_extent_w_typ(const lba_extent_w_typ && lbae) noexcept
        : scat_gath_elem{lbae.lba, {lbae.com_num_typ}} { }
    ~lba_extent_w_typ() noexcept { }

    lba_extent_w_typ(uint64_t a_lba, uint64_t a_num,
                     uint8_t a_lbae_typ = EXTENT_TYPE_UNKN) noexcept
        : scat_gath_elem{a_lba, {a_num}} { lbae_typ = a_lbae_typ; }
    lba_extent_w_typ(const scat_gath_elem & rhs_sgle) noexcept
        : scat_gath_elem{rhs_sgle.lba, {rhs_sgle.com_num_typ}} { }

    lba_extent_w_typ & operator= (const lba_extent_w_typ & rhs) noexcept
        { lba = rhs.lba; com_num_typ = rhs.com_num_typ; return *this; }
    lba_extent_w_typ & operator= (const scat_gath_elem & rhs) noexcept
        { lba = rhs.lba; com_num_typ = rhs.num; return *this; }

    uint64_t get_lba() const noexcept { return lba; }
    uint64_t get_num() const noexcept { return (uint64_t)num; }
    uint8_t get_typ() const noexcept { return lbae_typ; }

    void set_typ(uint8_t a_lbae_typ) noexcept { lbae_typ = a_lbae_typ; }
    void set_lba(uint64_t a_lba) noexcept { lba = a_lba; }
    void set_num(uint64_t a_num) noexcept { num = a_num; }

    bool operator< (const lba_extent_w_typ & a_lbae) const noexcept
        {
            if (lba < a_lbae.lba)
                return true;
            else if (lba > a_lbae.lba)
                return false;
            else if (num < a_lbae.num)
                return true;
            else
                return false;
        }
    bool operator< (const scat_gath_elem & a_sge) const noexcept
        {
            lba_extent_w_typ a_lbae { a_sge };

            return lba < a_lbae.lba;
        }
    bool lesser_lba_than(const lba_extent_w_typ & a_lbae) const noexcept
        { return (lba < a_lbae.lba); }

    uint64_t last_lba() const noexcept { return lba + num; }
    bool degenerate() const noexcept { return lba == 0; }
    bool overlap(const lba_extent_w_typ & a_lbae) const noexcept
        {
            if (degenerate() || a_lbae.degenerate())
                return false;
            if (lba == a_lbae.lba)
                return true;
            if (lba < a_lbae.lba) {
                if (lba + num >= a_lbae.lba)
                    return true;
            } else {
                if (a_lbae.lba + a_lbae.num >= lba)
                    return true;
            }
            return false;
        }
    bool includes(const lba_extent_w_typ & a_lbae) const noexcept
        {
            uint64_t my_last { last_lba() };
            uint64_t a_last { a_lbae.last_lba() };

            if (lba < a_lbae.lba)
                return false;
            else if (my_last > a_last)
                return false;
            return true;
        }

private:
    /*
     * inherit from struct scat_gath_elem (in ddpt.h) rather than use
     * composition (or duplication) here.
     */
#if 0
    struct scat_gath_elem sge;
#endif
#if 0
    uint64_t lba;       /* of start block */
    union {
        uint64_t com_num_typ;
        struct {
#if defined(__BIG_ENDIAN__) || (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
            uint64_t  lbae_typ : 8;
            uint64_t  num : 56;
#else   /* assume LITTLE_ENDIAN */
            uint64_t  num : 56;
            uint64_t  lbae_typ : 8;
#endif
        };
    };
#endif
};

void pr_statistics(const sgl_stats & sst, FILE * fp);

void pr_sgl(const struct scat_gath_elem * first_elemp, int num_elems,
            bool in_hex, FILE * fp);

typedef bool (*cmp_f_t)(const sgl_vect & sgl, int lhs, int rhs);

bool cmp_asc_lba(const sgl_vect & sgl, int lhs, int rhs);

bool cmp_des_lba(const sgl_vect & sgl, int lhs, int rhs);

bool cmp_asc_num(const sgl_vect & sgl, int lhs, int rhs);

bool cmp_des_num(const sgl_vect & sgl, int lhs, int rhs);

/* definition for function object to do indirect comparson via index_arr */
struct indir_comp_t {

    indir_comp_t(const sgl_vect & a_sgl, int typ) : sgl(a_sgl)
        {
            switch (typ) {
#if 1
            case 1: cmp_f = cmp_des_lba; break;
            case 2: cmp_f = cmp_asc_num; break;
            case 3: cmp_f = cmp_des_num; break;
            case 0: default: cmp_f = cmp_asc_lba; break;

#else
            case 1: cmp_f = &indir_comp_t::des_lba; break;
            case 2: cmp_f = &indir_comp_t::asc_num; break;
            case 3: cmp_f = &indir_comp_t::des_num; break;
            case 0: default: cmp_f = &indir_comp_t::asc_lba; break;
#endif
            }
        }

    bool operator()(int lhs, int rhs) const
#if 1
        { return cmp_f(sgl, lhs, rhs); }
#else
        { return (this->*cmp_f)(lhs, rhs); }
#endif
private:
    const sgl_vect & sgl;

#if 1

    cmp_f_t cmp_f;

#else

    // typedef bool (indir_comp_t::*cmp_f_t)(int lhs, int rhs) const;

    bool (indir_comp_t:: * cmp_f)(int lhs, int rhs) const;

    bool asc_lba(int lhs, int rhs) const
        { return sgl[lhs].lba < sgl[rhs].lba; }
    bool des_lba(int lhs, int rhs) const
        { return sgl[lhs].lba > sgl[rhs].lba; }
    bool asc_num(int lhs, int rhs) const
        { return sgl[lhs].num < sgl[rhs].num; }
    bool des_num(int lhs, int rhs) const
        { return sgl[lhs].num > sgl[rhs].num; }
#endif
};

bool non_overlap_check(const sgl_vect & sgl, const char * id_str, bool quiet);

int do_write2o_sgl(const char * bname, const char * extra, bool use_elem_opt,
                   sgl_vect & o_sgl, struct sgl_opts_t * op);

void sort2o_sgl(const sgl_vect & sort_sgl, const sgl_vect & twin_sgl,
                sgl_vect & out_sgl, sgl_vect & out_t_sgl,
                struct sgl_opts_t * op);

void sort_based_on(const sgl_vect & sort_sgl, std::vector<int> & index_arr,
                   int sort_cmp_val, int vb);

void rearrange_sgl(const sgl_vect & in_sgl,
                   const std::vector<int> & index_arr, sgl_vect & out_sgl,
                   bool b_vb);

void rearrange_twin_sgl(const sgl_vect & twin_sgl, const sgl_vect & ref_sgl,
                        const std::vector<int> & ref_index_arr,
                        sgl_vect & out_t_sgl, int do_hex, int vb);


#endif  /* DDPT_CPP_COM_HPP guard against multiple includes */
