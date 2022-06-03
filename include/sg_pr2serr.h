#ifndef SG_PR2SERR_H
#define SG_PR2SERR_H

/*
 * Copyright (c) 2004-2022 Douglas Gilbert.
 * All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the BSD_LICENSE file.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <inttypes.h>
#include <stdio.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* pr2serr and pr2ws are convenience functions that replace the somewhat
 * long-winded fprintf(stderr, ....). The second form (i.e. pr2ws() ) is for
 * internal library use and may place its output somewhere other than stderr;
 * it depends on the external variable sg_warnings_strm which can be set
 * with sg_set_warnings_strm(). By default it uses stderr. */

#if __USE_MINGW_ANSI_STDIO -0 == 1
#define __printf(a, b) __attribute__((__format__(gnu_printf, a, b)))
#elif defined(__GNUC__) || defined(__clang__)
#define __printf(a, b) __attribute__((__format__(printf, a, b)))
#else
#define __printf(a, b)
#endif

int pr2serr(const char * fmt, ...) __printf(1, 2);

int pr2ws(const char * fmt, ...) __printf(1, 2);

/* Want safe, 'n += snprintf(b + n, blen - n, ...)' style sequence of
 * functions. Returns number of chars placed in cp excluding the
 * trailing null char. So for cp_max_len > 0 the return value is always
 * < cp_max_len; for cp_max_len <= 1 the return value is 0 and no chars are
 * written to cp. Note this means that when cp_max_len = 1, this function
 * assumes that cp[0] is the null character and does nothing (and returns
 * 0). Linux kernel has a similar function called  scnprintf().  */
int sg_scnpr(char * cp, int cp_max_len, const char * fmt, ...) __printf(3, 4);

/* JSON support functions and structures follow. The prefix "sgj_" is used
 * for sg3_utils JSON functions, types and values. */

enum sgj_separator_t {
    SGJ_SEP_NONE = 0,
    SGJ_SEP_SPACE_1,
    SGJ_SEP_SPACE_2,
    SGJ_SEP_SPACE_3,
    SGJ_SEP_SPACE_4,
    SGJ_SEP_EQUAL_NO_SPACE,
    SGJ_SEP_EQUAL_1_SPACE,
    SGJ_SEP_COLON_NO_SPACE,
    SGJ_SEP_COLON_1_SPACE,
};

typedef void * sgj_opaque_p;

/* Apart from the pointers at the end the other fields are initialized
 * from the argument given to --json= . If there is no argument then
 * they initialized as shown. */
typedef struct sgj_state_t {
    /* the following set by default, the SG3_UTILS_JSON_OPTS envirinment
     * variable or command line argument to --json option, in that order. */
    bool pr_as_json;            /* = false */
    bool pr_exit_status;        /* 'e' (def: true) */
    bool pr_hex;                /* 'h' (def: false) */
    bool pr_leadin;             /* 'l' (def: true) */
    bool pr_output;             /* 'o' (def: false) */
    bool pr_pretty;             /* 'p' (def: true) */
    bool pr_string;             /* 's' (def: true) */
    char pr_format;             /*  (def: '\0') */
    int pr_indent_size;         /* digit (def: 4) */
    int verbose;                /* 'v' (def: 0) incremented each appearance */
    /* the following hold state information */
    int first_bad_char;         /* = '\0' */
    sgj_opaque_p basep;         /* base JSON object pointer */
    sgj_opaque_p outputp;       /* 'output' named JSON array pointer */
    sgj_opaque_p userp;         /* for temporary usage */
} sgj_state;

/* If jsp in non-NULL and jsp->pr_as_json is true then this call is ignored
 * unless jsp->pr_output is true. Otherwise this function prints to stdout
 * like printf(fmt, ...); note that no LF is added. In the jsp->pr_output
 * is true case, nothing is printed to stdout but instead is placed into the
 * JSON 'output" array (jsp->outputp) after some preprocessing. That
 * preprocessing involves removing a leading LF from 'fmt' (if present) and
 * up to two trailing LF characters. */
void sgj_pr_hr(sgj_state * jsp, const char * fmt, ...) __printf(2, 3);

/* Initializes the state object pointed to by jsp based on the argument
 * given to the right of --json= pointed to by j_optarg. If it is NULL
 * then state object gets its default values. Returns true if argument
 * to --json= is decoded properly, else returns false and places the
 * first "bad" character in jsp->first_bad_char . Note that no JSON
 * in-core tree needs to exist when this function is called. */
bool sgj_init_state(sgj_state * jsp, const char * j_optarg);

/* sgj_start() creates a JSON in-core tree and returns a pointer to it (or
 * NULL if the associated heap allocation fails). It should be paired with
 * sgj_finish() to clean up (i.e. remove all heap allocations) all the
 * elements (i.e. JSON objects and arrays) that have been placed in that
 * in-core tree. If jsp is NULL nothing further happens. Otherwise the pointer
 * to be returned is placed in jsp->basep. If jsp->pr_leadin is true and
 * util_name is non-NULL then a "utility_invoked" JSON object is made with
 * "name", and "version_date" object fields. If the jsp->pr_output field is
 * true a named array called "output" is added to the "utility_invoked" object
 * (creating it in the case when jsp->pr_leadin is false) and a pointer to
 * that array object is placed in jsp->objectp . The returned pointer is not
 * usually needed but if it is NULL then a heap allocation has failed. */
sgj_opaque_p sgj_start(const char * util_name, const char * ver_str,
                       int argc, char *argv[], sgj_state * jsp);

/* These are low level functions returning a pointer to a newly created JSON
 * object or array. If jsp is NULL or jsp->pr_as_json is false nothing happens
 * and NULL is returned. Note that this JSON object is _not_ placed in the
 * in-core tree controlled by jsp (jsp->basep); it may be added later as the
 * third argument to sgj_add_array_element(), for example. */
sgj_opaque_p sgj_new_unattached_object(sgj_state * jsp);
sgj_opaque_p sgj_new_unattached_array(sgj_state * jsp);

/* If jsp is NULL or jsp->pr_as_json is false nothing happens and NULL is
 * returned. Otherwise it creates a new named object (whose name is what
 * 'name' points to) at 'jop' with an empty object as its value; a pointer
 * to that empty object is returned. If 'jop' is NULL then jsp->basep is
 * used instead. The returned value should always be checked (for NULL)
 * and if not, used. */
sgj_opaque_p sgj_new_named_object(sgj_state * jsp, sgj_opaque_p jop,
                                  const char * name);

/* If jsp is NULL or jsp->pr_as_json is false nothing happens and NULL is
 * returned. Otherwise it creates a new named object (whose name is what
 * 'name' points to) at 'jop' with an empty array as its value; a pointer
 * to that empty array is returned.  If 'jop' is NULL then jsp->basep is
 * used instead. The returned value should always * be checked (for NULL)
 * and if not, used. */
sgj_opaque_p sgj_new_named_array(sgj_state * jsp, sgj_opaque_p jop,
                                 const char * name);

/* If either jsp or value is NULL or jsp->pr_as_json is false then nothing
 * happens and NULL is returned. The insertion point is at jop but if it is
 * NULL jsp->basep is used. If 'name' is non-NULL a new named JSON object is
 * added using 'name' and the associated value is a JSON string formed from
 * 'value'. If 'name' is NULL then 'jop' is assumed to be a JSON array and
 * a JSON string formed from 'value' is added. If successful returns a
 * a pointer newly formed JSON string. */
sgj_opaque_p sgj_add_val_s(sgj_state * jsp, sgj_opaque_p jop,
                           const char * name, const char * value);

/* If either jsp is NULL or jsp->pr_as_json is false then nothing happens and
 * NULL is returned. The insertion point is at jop but if it is NULL
 * jsp->basep is used. If 'name' is non-NULL a new named JSON object is
 * added using 'name' and the associated value is a JSON integer formed from
 * 'value'. If 'name' is NULL then 'jop' is assumed to be a JSON array and
 * a JSON integer formed from 'value' is added. If successful returns a
 * a pointer newly formed JSON integer. */
sgj_opaque_p sgj_add_val_i(sgj_state * jsp, sgj_opaque_p jop,
                           const char * name, int64_t value);

/* If either jsp is NULL or jsp->pr_as_json is false then nothing happens and
 * NULL is returned. The insertion point is at jop but if it is NULL
 * jsp->basep is used. If 'name' is non-NULL a new named JSON object is
 * added using 'name' and the associated value is a JSON boolean formed from
 * 'value'. If 'name' is NULL then 'jop' is assumed to be a JSON array and
 * a JSON boolean formed from 'value' is added. If successful returns a
 * a pointer newly formed JSON boolean. */
sgj_opaque_p sgj_add_val_b(sgj_state * jsp, sgj_opaque_p jop,
                           const char * name, bool value);

/* If jsp is NULL, jsp->pr_as_json is false or ua_jop is NULL nothing then
 * happens and NULL is returned. 'jop' is the insertion point but if it is
 * NULL jsp->basep is used instead. If 'name' is non-NULL a new named JSON
 * object is added using 'name' and the associated value is ua_jop. If 'name'
 * is NULL then 'jop' is assumed to be a JSON array and ua_jop is added to
 * it. If successful returns ua_jop . The "ua_" prefix stands for unattached.
 * Tha should be the case before invocation and it will be attached to jop
 * after a successful invocation. This means that ua_jop must have been
 * created by sgj_new_unattached_object() or similar. */
sgj_opaque_p sgj_add_val_o(sgj_state * jsp, sgj_opaque_p jop,
                           const char * name, sgj_opaque_p ua_jop);

/* The '_twin_' refers to generating output both for human readable and/or
 * JSON with a single invocation. If jsp is non_NULL and jsp->pr_output is
 * true then both JSON and human readable output is formed (and the latter is
 * placed in the jsp->outputp JSON array). The human readable form will have
 * leadin_sp spaces followed by 'name' then a separator, then 'value' with a
 * trailing LF. If 'name' is NULL then it and the separator are ignored. If
 * there is JSON output, then leadin_sp and sep are ignored. If 'jop' is NULL
 * then basep->basep is used. If 'name' is NULL then a JSON string object,
 * made from 'value' is added to the JSON array pointed to by 'jop'.
 * Otherwise a 'name'-d JSON object whose value is a JSON string object made
 * from 'value' is added at 'jop'. */
void sgj_pr_twin_vs(sgj_state * jsp, sgj_opaque_p jop, int leadin_sp,
                    const char * name, enum sgj_separator_t sep,
                    const char * value);

/* Similar to sgj_pr_twin_vs()'s description with 'JSON string object'
 * replaced by 'JSON integer object'. */
void sgj_pr_twin_vi(sgj_state * jsp, sgj_opaque_p jop, int leadin_sp,
                    const char * name, enum sgj_separator_t sep,
                    int64_t value);

/* Similar to sgj_pr_twin_vs()'s description with 'JSON string object'
 * replaced by 'JSON boolean object'. */
void sgj_pr_twin_vb(sgj_state * jsp, sgj_opaque_p jop, int leadin_sp,
                    const char * name, enum sgj_separator_t sep, bool value);

/* This function only produces JSON output if jsp is non-NULL and
 * jsp->pr_as_json is true. It adds a named object at 'jop' (or jop->basep
 * if jop is NULL) along with a value. If jsp->pr_hex is true then that
 * value is two sub-objects, one named 'i' with a 'value' as a JSON integer,
 * the other one named 'hex' with 'value' rendered as hex in a JSON string.
 * If jsp->pr_hex is false the there are no sub-objects and the 'value' is
 * rendered as JSON integer. */
void sgj_add_name_pair_ihex(sgj_state * jsp, sgj_opaque_p jop,
                            const char * name, uint64_t value);

/* This function only produces JSON output if jsp is non-NULL and
 * jsp->pr_as_json is true. It adds a named object at 'jop' (or jop->basep
 * if jop is NULL) along with a value. If jsp->pr_string is true then that
 * value is two sub-objects, one named 'i' with a 'val_i' as a JSON integer,
 * the other one named str_name with val_s rendered as a JSON string.
 * If jsp->pr_string is false the there are no sub-objects and the 'val_i' is
 * rendered as JSON integer. */
void sgj_add_name_pair_istr(sgj_state * jsp, sgj_opaque_p jop,
                            const char * name, int64_t val_i,
                            const char * str_name, const char * val_s);

/* Nothing in the in-core JSON tree is actually printed to 'fp' (typically
 * stdout) until this call is made. If jsp is NULL, jsp->pr_as_json is false
 * or jsp->basep is NULL then this function does nothing. If jsp->exit_status
 * is true then a new JSON object named "exit_status" and the 'exit_status'
 * value rendered as a JSON integer is appended to jsp->basep. The in-core
 * JSON tree with jsp->basep as its root is streamed to 'fp'. */
void sgj_pr2file(sgj_state * jsp, sgj_opaque_p jop, int exit_status,
                 FILE * fp);

/* This function is only needed if the pointer returned from either
 * sgj_new_unattached_object() or sgj_new_unattached_array() has not been
 * attached into the in-core JSON tree whose root is jsp->basep . */
void sgj_free_unattached(sgj_opaque_p jop);

/* If jsp is NULL or jsp->basep is NULL then this function does nothing.
 * This function does bottom up, heap freeing of all the in-core JSON
 * objects and arrays attached to the root JSON object assumed to be
 * found at jsp->basep . After this call jsp->basep, jsp->outputp and
 * jsp->userp will all be set to NULL.  */
void sgj_finish(sgj_state * jsp);


#ifdef __cplusplus
}
#endif

#endif
