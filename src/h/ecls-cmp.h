/*
    ecls-cmp.h  -- Include file for compiled code.
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

#include "config.h"
#include "machines.h"
#include <stddef.h>
#include <stdio.h>
#include <stdarg.h>
#include <setjmp.h>
#include <math.h>
#include <inttypes.h>
#include <limits.h>
#include <sys/types.h>

#ifdef __GNUC__
#define VOID void
#define VOL volatile
#else
#define VOID char
#define VOL
#endif

#include "gmp.h"
#include "object.h"
#include "stacks.h"
#ifdef THREADS
#include "lwp.h"
#include "critical.h"
#endif THREADS
#include "external.h"
#include "lisp_external.h"
#include "eval.h"
#include "number.h"
#ifdef LOCATIVE
#include "unify.h"
#endif LOCATIVE

#define	cs_check \
	if ((int *)(&narg) < cs_limit) \
		cs_overflow()

#define LINK_ARGS	&narg
#ifdef CLOS
#define TRAMPOLINK(vv, lk) \
	static cl_object gfun = OBJNULL; \
	va_list args; va_start(args, narg); \
	if (gfun) return(gcall(narg, gfun, args)); \
	else return(link_call(vv, lk , &gfun, &narg))
#else
#define TRAMPOLINK(vv, lk) \
	va_list args; va_start(args, narg); \
	return(link_call(vv, lk , &narg))
#endif CLOS

#define	cclosure_call	funcall

#define Creturn(v)	return((VALUES(0)=(v),1))
#define Cexit		return(0)
