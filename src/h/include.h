/*
    include.h -- List of included files.
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


#ifdef __GNUC__
#define VOID void
#else
#define VOID char
#endif

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <setjmp.h>
#include <limits.h>
#include <ctype.h>
#include <signal.h>
#include <string.h>		/* for FreeBSD */
#include <sys/param.h>		/* includes <sys/signal.h> and <sys/types.h> */
#include <sys/types.h>		/* for EMX */

#include "machines.h"

#ifdef BSD
# include <sys/time.h>
#endif BSD

#ifdef SYSV

# ifndef MAXPATHLEN
#  define MAXPATHLEN	PATH_MAX
# endif MAXPATHLEN

#endif SYSV

#include "../gmp/gmp.h"
#include "object.h"

#include "vs.h"
#include "bds.h"
#include "frame.h"

#ifdef THREADS
# include "critical.h"
# include "lwp.h"
#endif THREADS

#include "external.h"
#include "lisp_external.h"
#include "lex.h"
#include "eval.h"
#include "cs.h"
#include "number.h"

/**********************************************************************/
