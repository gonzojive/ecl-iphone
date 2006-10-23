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
#ifndef _MSC_VER
# include <unistd.h>
#endif
#include <ecl/internal.h>

#if defined(mingw32) || defined(_MSC_VER)
#include <windows.h>
#endif

#ifndef HZ			/* usually from <sys/param.h> */
#define HZ 60
#endif
#ifdef darwin
#undef HAVE_NANOSLEEP
#endif

#if defined(mingw32) || defined(_MSC_VER)
static DWORD beginning;
#else
static time_t beginning;
#endif

cl_fixnum
ecl_runtime(void)
/*
   tms_utime is the CPU time used while executing instructions in the
   user space of the calling process, measured in 1/HZ seconds.
*/
{
#ifdef HAVE_TIMES
	struct tms buf;

	times(&buf);
	return(buf.tms_utime);
#else
	return 0;
#endif
}

cl_object
cl_sleep(cl_object z)
{
	double r;
#ifdef HAVE_NANOSLEEP
	struct timespec tm;
#endif
	/* INV: number_minusp() makes sure `z' is real */
	if (number_minusp(z))
		cl_error(9, @'simple-type-error', @':format-control',
			    make_constant_base_string("Not a non-negative number ~S"),
			    @':format-arguments', cl_list(1, z),
			    @':expected-type', @'real', @':datum', z);
#ifdef HAVE_NANOSLEEP
	r = object_to_double(z);
	tm.tv_sec = (time_t)floor(r);
	tm.tv_nsec = (long)((r - floor(r)) * 1e9);
	nanosleep(&tm, NULL);
#else
#if defined (mingw32) || defined(_MSC_VER)
	r = object_to_double(z) * 1000;
	Sleep((long)r);
#else
	z = round1(z);
	if (FIXNUMP(z))
		sleep(fix(z));
	else
		for(;;)
			sleep(1000);
#endif
#endif
	@(return Cnil)
}

cl_object
cl_get_internal_run_time()
{
#ifdef HAVE_TIMES
	struct tms buf;
	times(&buf);
	@(return MAKE_FIXNUM(buf.tms_utime))
#else
	return cl_get_internal_real_time();
#endif
}

cl_object
cl_get_internal_real_time()
{
#if defined(mingw32) || defined(_MSC_VER)
	@(return MAKE_FIXNUM((cl_fixnum)((GetTickCount() - beginning)/1000.0*HZ)))
#else
	@(return MAKE_FIXNUM((time(0) - beginning)*HZ))
#endif
}

void
init_unixtime(void)
{
#if defined(mingw32) || defined(_MSC_VER)
	beginning = GetTickCount();
#else
	beginning = time(0);
#endif

	ECL_SET(@'internal-time-units-per-second', MAKE_FIXNUM(HZ));

	cl_core.Jan1st1970UT =
	    number_times(MAKE_FIXNUM(24 * 60 * 60),
			 MAKE_FIXNUM(17 + 365 * 70));
}
