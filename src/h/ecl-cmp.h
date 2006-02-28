/*
    ecl-cmp.h  -- Include file for compiled code.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or modify it under
    the terms of the GNU General Library Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifndef _MSC_VER
#include <sys/param.h>
#endif
#include <sys/types.h>
#include <stddef.h>
#include <stdio.h>
#include <stdarg.h>
#include <setjmp.h>
#include <math.h>
#include <float.h>
#ifndef _MSC_VER
#include <inttypes.h>
#else
typedef char int8_t;
typedef short int16_t;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
#endif

#include <ecl/config.h>

#ifdef ECL_THREADS
# if defined(_MSC_VER) || defined(mingw32)
#  include <windows.h>
   typedef HANDLE pthread_t;
   typedef HANDLE pthread_mutex_t;
#  undef ERROR
#  ifdef GBC_BOEHM
#   define CreateThread GC_CreateThread
#  endif
# else
#  include <pthread.h>
#  if defined(__APPLE__) || defined(freebsd)
#   define PTHREAD_MUTEX_ERROR_CHECK_NP PTHREAD_MUTEX_ERROR_CHECK_NP
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

#undef cs_check
#define	cs_check \
	if ((int *)(&narg) < cs_limit) \
		cs_overflow()

#define TRAMPOLINK(narg, vv, lk, cblock) \
	cl_va_list args; cl_va_start(args, narg, narg, 0); \
	return(link_call(vv, (cl_objectfn *)lk, cblock, narg, args))
