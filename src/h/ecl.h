/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    ecl.h -- Main headers for development of ECL
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifndef ECL_H
#define ECL_H

#include <sys/types.h>		/* size_t, pthread_t, pthread_mutex_t */
#include <stddef.h>		/* NULL, ptrdiff_t */
#include <stdarg.h> 		/* va_list */
#include <setjmp.h> 		/* setjmp and buffers */
#if !defined(_MSC_VER) && !defined(__sun)
#include <stdint.h> 		/* int8_t and friends */
#else
typedef char int8_t;
typedef short int16_t;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
#endif
/* Microsoft VC++ does not have va_copy() */
#if defined(_MSC_VER) || !defined(va_copy)
#define va_copy(dst, src) \
   ((void) memcpy(&(dst), &(src), sizeof(va_list)))
#endif

#ifndef FIXNUM_BITS
#include <ecl/config.h>
#endif

/*
 * The Boehm-Demers-Weiser garbage collector contains wrappers for
 * dlopen and similar functions. These wrappers explicitely deactivate
 * garbage collection. Since we have explicitely deactivated scanning
 * shared libraries (alloc_2.d), we can get rid of this performance
 * penalty.
 */
#if defined(GBC_BOEHM_GENGC)
#ifdef dlopen
# undef dlopen
#endif
#ifdef dlclose
# undef dlclose
#endif
#endif

#ifdef ECL_THREADS
# if defined(_MSC_VER) || defined(mingw32)
#  define WIN32_LEAN_AND_MEAN 1 /* Do not include winsock.h */
#  include <windows.h>
   typedef HANDLE pthread_t;
   typedef HANDLE pthread_mutex_t;
   typedef HANDLE pthread_cond_t; /*Dummy, not really used*/
#  undef ERROR
#  ifdef GBC_BOEHM
#   define CreateThread GC_CreateThread
#  endif
# endif
# define start_critical_section()
# define end_critical_section()
#else
# define start_critical_section()
# define end_critical_section()
#endif

#include <ecl/object.h>
#include <ecl/stacks.h>
#include <ecl/external.h>
#include <ecl/eval.h>
#include <ecl/number.h>
#ifdef LOCATIVE
#include <ecl/unify.h>
#endif

#if defined(__cplusplus) || (defined(__GNUC__) && !defined(__STRICT_ANSI__))
#define ECL_INLINE inline
#else
#define ECL_INLINE
#endif

typedef void (*ecl_init_function_t)(cl_object block);

#endif /* ECL_H */
