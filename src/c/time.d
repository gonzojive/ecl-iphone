/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    time.c -- Time routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <math.h>
#ifdef _MSC_VER
# undef complex
#endif
#include <time.h>
#ifdef HAVE_TIMES
# include <sys/times.h>
#endif
#ifdef HAVE_GETRUSAGE
# include <sys/time.h>
# include <sys/resource.h>
#endif
#ifdef HAVE_GETTIMEOFDAY
# include <sys/time.h>
#endif
#ifndef _MSC_VER
# include <unistd.h>
#endif
#include <ecl/internal.h>
#if defined(mingw32) || defined(_MSC_VER)
#include <windows.h>
#include <WinSock.h>
#endif

#ifdef darwin
#undef HAVE_NANOSLEEP
#endif

#if !defined(HAVE_GETTIMEOFDAY) && !defined(HAVE_GETRUSAGE) && !defined(mingw32) && !defined(_MSC_VER)
struct timeval {
	long tv_sec;
	long tv_usec;
};
#endif

static struct timeval beginning;

static void
get_real_time(struct timeval *tv)
{
#ifdef HAVE_GETTIMEOFDAY
	struct timezone tz;
	gettimeofday(tv, &tz);
#else
# if defined(mingw32) || defined(_MSC_VER)
	DWORD x = GetTickCount();
	tv->tv_sec = x / 1000;
	tv->tv_usec = (x % 1000) * 1000;
# else
	time_t = time(0);
	tv->tv_sec = time_t;
	tv->tv_usec = 0;
# endif
#endif
}

static void
get_run_time(struct timeval *tv)
{
#ifdef HAVE_GETRUSAGE
	struct rusage r;
	getrusage(RUSAGE_SELF, &r);
	*tv = r.ru_utime;
#else
# ifdef HAVE_TIMES
	struct tms buf;
	times(&buf);
	tv->tv_sec = buf.tms_utime / CLK_TCK;
	tv->tv_usec = (buf.tms_utime % CLK_TCK) * 1000000;
# else
#  if defined(mingw32) || defined(_MSC_VER)
	FILETIME creation_time;
	FILETIME exit_time;
	FILETIME kernel_time;
	FILETIME user_time;
	ULARGE_INTEGER ui_kernel_time;
	ULARGE_INTEGER ui_user_time;
	if (!GetProcessTimes(GetCurrentProcess(),
	                     &creation_time,
	                     &exit_time,
	                     &kernel_time,
	                     &user_time))
	    FEwin32_error("GetProcessTimes() failed", 0);
	ui_kernel_time.HighPart = kernel_time.dwHighDateTime;
	ui_kernel_time.LowPart  = kernel_time.dwLowDateTime;
	ui_user_time.HighPart = user_time.dwHighDateTime;
	ui_user_time.LowPart  = user_time.dwLowDateTime;
	ui_kernel_time.QuadPart += ui_user_time.QuadPart;
	ui_kernel_time.QuadPart /= 10000;
	tv->tv_sec = ui_kernel_time.QuadPart / 1000;
	tv->tv_usec = (ui_kernel_time.QuadPart % 1000) * 1000;
#  else
	get_real_time(tv);
#  endif
# endif
#endif
}

cl_fixnum
ecl_runtime(void)
{
	struct timeval tv;
	get_run_time(&tv);
	return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

cl_object
cl_sleep(cl_object z)
{
	double r;
#ifdef HAVE_NANOSLEEP
	struct timespec tm;
#endif
	/* INV: ecl_minusp() makes sure `z' is real */
	if (ecl_minusp(z))
		cl_error(9, @'simple-type-error', @':format-control',
			    make_constant_base_string("Not a non-negative number ~S"),
			    @':format-arguments', cl_list(1, z),
			    @':expected-type', @'real', @':datum', z);
#ifdef HAVE_NANOSLEEP
	r = ecl_to_double(z);
	tm.tv_sec = (time_t)floor(r);
	tm.tv_nsec = (long)((r - floor(r)) * 1e9);
	nanosleep(&tm, NULL);
#else
#if defined (mingw32) || defined(_MSC_VER)
	r = ecl_to_double(z) * 1000;
	Sleep((long)r);
#else
	z = ecl_round1(z);
	if (FIXNUMP(z))
		sleep(fix(z));
	else
		for(;;)
			sleep(1000);
#endif
#endif
	@(return Cnil)
}

static cl_object
timeval_to_time(long sec, long usec)
{
	/* This can be probably improved.  Right now, too many
	   rounding errors, but if we use the lisp routines then it
	   slows downs and measures become too imprecise. */
	double x = sec * 1000.0 + usec / 1000.0;
	return MAKE_FIXNUM((cl_fixnum)x);
}

cl_object
cl_get_internal_run_time()
{
	struct timeval tv;
	get_run_time(&tv);
	@(return timeval_to_time(tv.tv_sec, tv.tv_usec))
}

cl_object
cl_get_internal_real_time()
{
	struct timeval tv;
	get_real_time(&tv);
	@(return timeval_to_time(tv.tv_sec - beginning.tv_sec,
				 tv.tv_usec - beginning.tv_usec))
}

cl_object
cl_get_universal_time()
{
	cl_object utc = ecl_make_integer(time(0));
	@(return ecl_plus(utc, cl_core.Jan1st1970UT))
}

void
init_unixtime(void)
{
	get_real_time(&beginning);

	ECL_SET(@'internal-time-units-per-second', MAKE_FIXNUM(1000));

	cl_core.Jan1st1970UT =
	    ecl_times(MAKE_FIXNUM(24 * 60 * 60),
			 MAKE_FIXNUM(17 + 365 * 70));
}
