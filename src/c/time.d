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

#include "ecl.h"
#include <math.h>
#include <time.h>
#include <sys/times.h>
#include <unistd.h>

#ifndef HZ			/* usually from <sys/param.h> */
#define HZ 60
#endif
#ifdef darwin
#undef HAVE_NANOSLEEP
#endif

static time_t beginning;

int
runtime(void)
/*
   tms_utime is the CPU time used while executing instructions in the
   user space of the calling process, measured in 1/HZ seconds.
*/
{
	struct tms buf;

	times(&buf);
	return(buf.tms_utime);
}

static cl_object Jan1st1970UT;

cl_object
UTC_time_to_universal_time(cl_fixnum i)
{
	return number_plus(bignum1(i), Jan1st1970UT);
}

cl_object
cl_get_universal_time()
{
	@(return UTC_time_to_universal_time(time(0)))
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
			    make_simple_string("Not a non-negative number ~S"),
			    @':format-arguments', cl_list(1, z),
			    @':expected-type', @'real', @':datum', z);
#ifdef HAVE_NANOSLEEP
	r = object_to_double(z);
	tm.tv_sec = (time_t)floor(r);
	tm.tv_nsec = (long)((r - floor(r)) * 1e9);
	nanosleep(&tm, NULL);
#else
	z = round1(z);
	if (FIXNUMP(z))
		sleep(fix(z));
	else
		for(;;)
			sleep(1000);
#endif
	@(return Cnil)
}

cl_object
cl_get_internal_run_time()
{
	struct tms buf;

	times(&buf);
	@(return MAKE_FIXNUM(buf.tms_utime))
}

cl_object
cl_get_internal_real_time()
{
	@(return MAKE_FIXNUM((time(0) - beginning)*HZ))
}

/*
 * Return the hours west of Greenwich for the current timezone.
 *
 * Based on Lott's get_timezone() function from CMU Common Lisp.
 */
cl_object
si_get_local_time_zone()
{
  struct tm ltm, gtm;
  int mw;
  time_t when = 0L;

  ltm = *localtime(&when);
  gtm = *gmtime(&when);

  mw = (gtm.tm_min + 60 * gtm.tm_hour) - (ltm.tm_min + 60 * ltm.tm_hour);

  if ((gtm.tm_wday + 1) % 7 == ltm.tm_wday)
    mw -= 24*60;
  else if (gtm.tm_wday == (ltm.tm_wday + 1) % 7)
    mw += 24*60;

  @(return make_ratio(MAKE_FIXNUM(mw), MAKE_FIXNUM(60)))
}

/*
 * Return T if daylight saving is in effect at Universal Time UT, which
 * defaults to current time.
 *
 */
@(defun si::daylight-saving-time-p (&optional UT)
  struct tm *ltm;
  time_t when;
@
  if (narg == 0)
    when = time(0);
  else { /* narg == 1 */
    cl_object UTC = number_minus(UT, Jan1st1970UT);
    switch (type_of(UTC)) {
    case t_fixnum:
      when = fix(UTC);
      break;
    case t_bignum:
      when = big_to_long(UTC);
      break;
    default:
      FEerror("Universal Time out of range: ~A.", 1, UT);
    }
  }
  ltm = localtime(&when);
  @(return (ltm->tm_isdst ? Ct : Cnil))
@)

void
init_unixtime(void)
{
	beginning = time(0);

	SYM_VAL(@'internal-time-units-per-second') = MAKE_FIXNUM(HZ);

	Jan1st1970UT =
	  number_times(MAKE_FIXNUM(24 * 60 * 60),
		       MAKE_FIXNUM(17 + 365 * 70));
	ecl_register_static_root(&Jan1st1970UT);
}
