/*
    num_co.c -- Operations on floating-point numbers.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*
	IMPLEMENTATION-DEPENDENT

	This file contains those functions
	that know the representation of floating-point numbers.
*/

#include "ecls.h"
#include <math.h>
#ifndef HAVE_ISOC99
# define floorf floor
# define ceilf ceil
# define fabsf fabs
#endif

static cl_object plus_half, minus_half;


#ifdef VAX
/*
	radix = 2

	SEEEEEEEEHHHHHHH	The redundant most significant fraction bit
	HHHHHHHHHHHHHHHH	is not expressed.
	LLLLLLLLLLLLLLLL
	LLLLLLLLLLLLLLLL
*/
#endif
#ifdef IEEEFLOAT
# ifndef WORDS_BIGENDIAN
/*
	radix = 2

	LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL	The redundant most
	SEEEEEEEEEEEHHHHHHHHHHHHHHHHHHHH	significant fraction bit
						is not expressed.
*/
# else
/*
	radix = 2

	SEEEEEEEEEEEHHHHHHHHHHHHHHHHHHHH	The redundant most
	LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL	significant fraction bit
						is not expressed.
*/
# endif
#endif
#ifdef TAHOE
/*
        radix = 2

        SEEEEEEEEHHHHHHHHHHHHHHHHHHHHHHH       The redundant most significant
        LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL       fraction bit is not expressed.

*/
#endif

static void
integer_decode_double(double d, unsigned *hp, unsigned *lp, int *ep, int *sp)
{
	unsigned h, l;

	if (d == 0.0) {
		*hp = *lp = 0;
		*ep = 0;
		*sp = 1;
		return;
	}
	h = *((unsigned *)&d + HIND);
	l = *((unsigned *)&d + LIND);
#ifdef VAX
	*ep = ((h >> 7) & 0xff) - 128 - 56;
	h = ((h >> 15) & 0x1fffe) | (((h & 0x7f) | 0x80) << 17);
	l = ((l >> 16) & 0xffff) | (l << 16);
#endif VAX
#ifdef IEEEFLOAT
	*ep = ((h & 0x7ff00000) >> 20) - 1022 - 53;
	h = (h & 0x000fffff) | 0x00100000;
#endif IEEEFLOAT
#ifdef TAHOE
        *ep = ((h & 0x7f800000) >> 23) - 128 - 56;
        h = (h & 0x007fffff) | 0x00800000;
#endif
	*hp = h;
	*lp = l;
	*sp = (d > 0.0 ? 1 : -1);
}

#ifdef VAX
/*
	radix = 2

	SEEEEEEEEMMMMMMM	The redundant most significant fraction bit
	MMMMMMMMMMMMMMMM	is not expressed.
*/
#endif VAX
#ifdef IEEEFLOAT
/*
	radix = 2

	SEEEEEEEEMMMMMMMMMMMMMMMMMMMMMMM	The redundant most
						significant fraction bit
						is not expressed.
*/
#endif IEEEFLOAT
#ifdef TAHOE
/*
        radix = 2
        
        SEEEEEEEEMMMMMMMMMMMMMMMMMMMMMMM        The redundant most significant
                                                fraction bit is not expressed.
*/
#endif
static void
integer_decode_float(double d, int *mp, int *ep, int *sp)
{
	float f;
	int m;

	f = d;
	if (f == 0.0) {
		*mp = 0;
		*ep = 0;
		*sp = 1;
		return;
	}
	m = *(int *)(&f);
#ifdef VAX
	*ep = ((m >> 7) & 0xff) - 128 - 24;
	*mp = ((m >> 16) & 0xffff) | (((m & 0x7f) | 0x80) << 16);
#endif VAX
#ifdef IEEEFLOAT
	*ep = ((m & 0x7f800000) >> 23) - 126 - 24;
	*mp = (m & 0x007fffff) | 0x00800000;
#endif IEEEFLOAT
#ifdef TAHOE
        *ep = ((m & 0x7f800000) >> 23) - 128 -24;
        *mp = (m & 0x007fffff) | 0x00800000;
#endif
	*sp = (f > 0.0 ? 1 : -1);
}

static int
double_exponent(double value)
{
	int *d = (int*)&value;
	if (value == 0.0)
		return(0);
#ifdef VAX
	return(((d[0] >> 7) & 0xff) - 128);
#endif VAX
#ifdef IEEEFLOAT
	return(((d[HIND] & 0x7ff00000) >> 20) - 1022);
#endif IEEEFLOAT
#ifdef TAHOE
        return(((d[0] & 0x7f800000) >> 23) - 128);
#endif
}

static void
set_exponent(double *value, int e)
{
	unsigned int *d = (int*)value;
	if (*value == 0.0)
		return;
#ifdef VAX
	d[0] = (d[0] & 0xffff807f) | (((e + 128) << 7) & 0x7f80);
#endif VAX
#ifdef IEEEFLOAT
	d[HIND] = (d[HIND] & 0x800fffff) | (((e + 1022) << 20) & 0x7ff00000);
#endif IEEEFLOAT
#ifdef TAHOE
        d[0] = (d[0] & 0x807fffff) | (((e + 128) << 23) & 0x7f800000);
#endif
}


cl_object
double_to_integer(double d)
{
	int h, l, e, s;
	cl_object x;

	if (d == 0.0)
		return(MAKE_FIXNUM(0));
	integer_decode_double(d, &h, &l, &e, &s);

#if defined(VAX) || defined(TAHOE)
	if (e <= -32) {
		h >>= (-e) - 32;
		return(MAKE_FIXNUM(s*h));
	}
#endif
#ifdef IEEEFLOAT
	if (e <= -32) {
		e = (-e) - 32;
		if (e >= 32)
			return(MAKE_FIXNUM(0));
		h >>= e;
		return(MAKE_FIXNUM(s*h));
	}
#endif IEEEFLOAT
	if (h != 0)
		x = bignum2(h, l);
	else
		x = MAKE_FIXNUM(l);

	x = integer_shift(x, e);
	if (s < 0)
		x = number_negate(x);
	return(x);
}

static cl_object
number_remainder(cl_object x, cl_object y, cl_object q)
{
	cl_object z;

	z = number_times(q, y);
	z = number_minus(x, z);
	return(z);
}

/* Coerce X to single-float if one arg,
   otherwise coerce to same float type as second arg */

@(defun float (x &optional (y OBJNULL))
	enum type t = t_shortfloat;
@
	if (y != OBJNULL) {
		t = type_of(y);
		if (t != t_shortfloat && t != t_longfloat)
			FEtype_error_float(y);
	}
	switch (type_of(x)) {
	case t_fixnum:
		if (t == t_shortfloat)
			x = make_shortfloat(fix(x));
		else
			x = make_longfloat(fix(x));
		break;
	case t_bignum:
	case t_ratio: {
		double d = number_to_double(x);
		if (t == t_shortfloat)
			x = make_shortfloat(d);
		else
			x = make_longfloat(d);		
		break;
	}
	case t_shortfloat:
		if (t == t_longfloat)
			x = make_longfloat(sf(x));
		break;
	case t_longfloat:
		if (t == t_shortfloat)
			x = make_shortfloat(lf(x));
		break;
	default:
		FEtype_error_real(x);
	}
	@(return x)
@)

@(defun numerator (x)
	cl_object out;
@
	switch (type_of(x)) {
	case t_ratio:
		out = x->ratio.num;
		break;
	case t_fixnum:
	case t_bignum:
		out = x;
		break;
	default:
		FEwrong_type_argument(@'rational', x);
	}
	@(return out)
@)

@(defun denominator (x)
	cl_object out;
@
	switch (type_of(x)) {
	case t_ratio:
		out = x->ratio.den;
		break;
	case t_fixnum:
	case t_bignum:
		out = MAKE_FIXNUM(1);
		break;
	default:
		FEwrong_type_argument(@'rational', x);
	}
	@(return out)
@)

cl_object
floor1(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		VALUES(0) = x;
		VALUES(1) = MAKE_FIXNUM(0);
		break;
	case t_ratio:
		VALUES(0) = floor2(x->ratio.num, x->ratio.den);
		VALUES(1) = make_ratio(VALUES(1), x->ratio.den);
		break;
	case t_shortfloat: {
		float d = sf(x);
		float y = floorf(d);
		VALUES(0) = double_to_integer(y);
		VALUES(1) = make_shortfloat(d - y);
		break;
	}
	case t_longfloat: {
		double d = lf(x);
		double y = floor(d);
		VALUES(0) = double_to_integer(y);
		VALUES(1) = make_longfloat(d - y);
		break;
	}
	default:
		FEtype_error_real(x);
	}
	NValues = 2;
	return VALUES(0);
}

cl_object
floor2(cl_object x, cl_object y)
{
	switch(type_of(x)) {
	case t_fixnum:
		switch(type_of(y)) {
		case t_fixnum: {	/* FIX / FIX */
		  cl_fixnum a = fix(x), b = fix(y);
		  cl_fixnum q = a / b,  r = a % b;
		  if ((r^b) < 0 && r) {	/* opposite sign and some remainder*/
		    VALUES(0) = MAKE_FIXNUM(q-1);
		    VALUES(1) = MAKE_FIXNUM(r+b);
		  } else {
		    VALUES(0) = MAKE_FIXNUM(q);
		    VALUES(1) = MAKE_FIXNUM(r);
		  }
		  break;
		}
		case t_bignum: {	/* FIX / BIG */
		  if (number_plusp(x) != number_plusp(y)) {
		    VALUES(0) = MAKE_FIXNUM(-1);
		    VALUES(1) = number_plus(y, x);
		  } else {
		    VALUES(0) = MAKE_FIXNUM(0);
		    VALUES(1) = x;
		  }
		  break;
		}
		case t_ratio:		/* FIX / RAT */
		  floor2(number_times(x, y->ratio.den), y->ratio.num);
		  VALUES(1) = make_ratio(VALUES(1), y->ratio.den);
		  break;
		case t_shortfloat: {	/* FIX / SF */
		  float n = sf(y);
		  float p = fix(x) / n;
		  float q = floorf(p);
		  VALUES(0) = double_to_integer(q);
		  VALUES(1) = make_shortfloat((p - q)*n);
		  break;
		}
		case t_longfloat: {	/* FIX / LF */
		  double n = lf(y);
		  double p = fix(x) / n;
		  double q = floor(p);
		  VALUES(0) = double_to_integer(q);
		  VALUES(1) = make_longfloat((p - q)*n);
		  break;
		}
		default:
		  FEtype_error_real(y);
		}
		break;
	case t_bignum:
		switch(type_of(y)) {
		case t_fixnum: {	/* BIG / FIX */
		  cl_object q = big_register0_get();
		  cl_object r = big_register1_get();
		  cl_object j = big_register2_get();
		  mpz_set_si(j->big.big_num, fix(y));
		  mpz_fdiv_qr(q->big.big_num, r->big.big_num, x->big.big_num, j->big.big_num);
		  VALUES(0) = big_register_normalize(q);
		  VALUES(1) = big_register_normalize(r);
		  break;
		}
		case t_bignum: {	/* BIG / BIG */
		  cl_object q = big_register0_get();
		  cl_object r = big_register1_get();
		  mpz_fdiv_qr(q->big.big_num, r->big.big_num, x->big.big_num, y->big.big_num);
		  VALUES(0) = big_register_normalize(q);
		  VALUES(1) = big_register_normalize(r);
		  break;
		}
		case t_ratio:		/* BIG / RAT */
		  floor2(number_times(x, y->ratio.den), y->ratio.num);
		  VALUES(1) = make_ratio(VALUES(1), y->ratio.den);
		  break;
		case t_shortfloat: {	/* BIG / SF */
		  float n = sf(y);
		  float p = big_to_double(x) / n;
		  float q = floorf(p);
		  VALUES(0) = double_to_integer(q);
		  VALUES(1) = make_shortfloat((p - q)*n);
		  break;
		}
		case t_longfloat: {	/* BIG / LF */
		  double n = lf(y);
		  double p = big_to_double(x) / n;
		  double q = floor(p);
		  VALUES(0) = double_to_integer(q);
		  VALUES(1) = make_longfloat((p - q)*n);
		  break;
		}
		default:
		  FEtype_error_real(y);
		}
		break;
	case t_ratio:
		switch(type_of(y)) {
		case t_ratio:		/* RAT / RAT */
		  floor2(number_times(x->ratio.num, y->ratio.den),
			 number_times(x->ratio.den, y->ratio.num));
		  VALUES(1) = make_ratio(VALUES(1), number_times(x->ratio.den, y->ratio.den));
		  break;
		default:		/* RAT / ANY */
		  floor2(x->ratio.num, number_times(x->ratio.den, y));
		  VALUES(1) = make_ratio(VALUES(1), x->ratio.den);
		}
		break;
	case t_shortfloat: {		/* SF / ANY */
		float n = number_to_double(y);
		float p = sf(x)/n;
		float q = floorf(p);
		VALUES(0) = double_to_integer(q);
		VALUES(1) = make_shortfloat((p - q)*n);
		break;
	}
	case t_longfloat: {		/* LF / ANY */
		double n = number_to_double(y);
		double p = lf(x)/n;
		double q = floor(p);
		VALUES(0) = double_to_integer(q);
		VALUES(1) = make_longfloat((p - q)*n);
		break;
	}
	default:
		FEtype_error_real(x);
	}
	NValues = 2;
	return VALUES(0);
}
  
@(defun floor (x &optional (y OBJNULL))
@
	if (narg == 1)
		floor1(x);
	else
		floor2(x, y);
	returnn(VALUES(0));
@)

cl_object
ceiling1(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		VALUES(0) = x;
		VALUES(1) = MAKE_FIXNUM(0);
		break;
	case t_ratio:
		VALUES(0) = ceiling2(x->ratio.num, x->ratio.den);
		VALUES(1) = make_ratio(VALUES(1), x->ratio.den);
		break;
	case t_shortfloat: {
		double d = (double)(sf(x));
		double y = ceil(d);
		VALUES(0) = double_to_integer(y);
		VALUES(1) = make_shortfloat(d - y);
		break;
	}
	case t_longfloat: {
		double d = (double)(sf(x));
		double y = ceil(d);
		VALUES(0) = double_to_integer(y);
		VALUES(1) = make_longfloat(d - y);
		break;
	}
	default:
		FEtype_error_real(x);
	}
	NValues = 2;
	return VALUES(0);
}

cl_object
ceiling2(cl_object x, cl_object y)
{
	switch(type_of(x)) {
	case t_fixnum:
		switch(type_of(y)) {
		case t_fixnum: {	/* FIX / FIX */
		  cl_fixnum a = fix(x); cl_fixnum b = fix(y);
		  cl_fixnum q = a / b;  cl_fixnum r = a % b;
		  if ((r^b) > 0 && r) {	/* same signs and some remainder */
		    VALUES(0) = MAKE_FIXNUM(q+1);
		    VALUES(1) = MAKE_FIXNUM(r-b);
		  } else {
		    VALUES(0) = MAKE_FIXNUM(q);
		    VALUES(1) = MAKE_FIXNUM(r);
		  }
		  break;
		}
		case t_bignum: {	/* FIX / BIG */
		  if (number_plusp(x) != number_plusp(y)) {
		    VALUES(0) = MAKE_FIXNUM(0);
		    VALUES(1) = x;
		  } else {
		    VALUES(0) = MAKE_FIXNUM(1);
		    VALUES(1) = number_minus(x, y);
		  }
		  break;
		}
		case t_ratio:		/* FIX / RAT */
		  ceiling2(number_times(x, y->ratio.den), y->ratio.num);
		  VALUES(1) = make_ratio(VALUES(1), y->ratio.den);
		  break;
		case t_shortfloat: {	/* FIX / SF */
		  float n = sf(y);
		  float p = fix(x)/n;
		  float q = ceilf(p);
		  VALUES(0) = double_to_integer(q);
		  VALUES(1) = make_shortfloat((p - q)*n);
		  break;
		}
		case t_longfloat: {	/* FIX / LF */
		  double n = lf(y);
		  double p = fix(x)/n;
		  double q = ceil(p);
		  VALUES(0) = double_to_integer(q);
		  VALUES(1) = make_longfloat((p - q)*n);
		  break;
		}
		default:
		  FEtype_error_real(y);
		}
		break;
	case t_bignum:
		switch(type_of(y)) {
		case t_fixnum: {	/* BIG / FIX */
		  cl_object q = big_register0_get();
		  cl_object r = big_register1_get();
		  cl_object j = big_register2_get();
		  mpz_set_si(j->big.big_num, fix(y));
		  mpz_cdiv_qr(q->big.big_num, r->big.big_num, x->big.big_num, j->big.big_num);
		  VALUES(0) = big_register_normalize(q);
		  VALUES(1) = big_register_normalize(r);
		  break;
		}
		case t_bignum: {	/* BIG / BIG */
		  cl_object q = big_register0_get();
		  cl_object r = big_register1_get();
		  mpz_cdiv_qr(q->big.big_num, r->big.big_num, x->big.big_num, y->big.big_num);
		  VALUES(0) = big_register_normalize(q);
		  VALUES(1) = big_register_normalize(r);
		  break;
		}
		case t_ratio:		/* BIG / RAT */
		  ceiling2(number_times(x, y->ratio.den), y->ratio.num);
		  VALUES(1) = make_ratio(VALUES(1), y->ratio.den);
		  break;
		case t_shortfloat: {	/* BIG / SF */
		  float n = sf(y);
		  float p = big_to_double(x)/n;
		  float q = ceilf(p);
		  VALUES(0) = double_to_integer(q);
		  VALUES(1) = make_shortfloat((p - q)*n);
		  break;
		}
		case t_longfloat: {	/* BIG / LF */
		  double n = lf(y);
		  double p = big_to_double(x)/n;
		  double q = ceil(p);
		  VALUES(0) = double_to_integer(q);
		  VALUES(1) = make_longfloat((p - q)*n);
		  break;
		}
		default:
		  FEtype_error_real(y);
		}
		break;
	case t_ratio:
		switch(type_of(y)) {
		case t_ratio:		/* RAT / RAT */
		  ceiling2(number_times(x->ratio.num, y->ratio.den),
			   number_times(x->ratio.den, y->ratio.num));
		  VALUES(1) = make_ratio(VALUES(1), number_times(x->ratio.den, y->ratio.den));
		  break;
		default:		/* RAT / ANY */
		  ceiling2(x->ratio.num, number_times(x->ratio.den, y));
		  VALUES(1) = number_divide(VALUES(1), x->ratio.den);
		}
		break;
	case t_shortfloat: {		/* SF / ANY */
		float n = number_to_double(y);
		float p = sf(x)/n;
		float q = ceilf(p);
		VALUES(0) = double_to_integer(q);
		VALUES(1) = make_shortfloat((p - q)*n);
		break;
	}
	case t_longfloat: {		/* LF / ANY */
		double n = number_to_double(y);
		double p = lf(x)/n;
		double q = ceil(p);
		VALUES(0) = double_to_integer(q);
		VALUES(1) = make_longfloat((p - q)*n);
		break;
	}
	default:
		FEtype_error_real(x);
	}
	NValues = 2;
	return VALUES(0);
}
  
@(defun ceiling (x &optional (y OBJNULL))
@
	if (narg == 1)
		ceiling1(x);
	else
		ceiling2(x, y);
	returnn(VALUES(0));
@)

cl_object
truncate1(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		VALUES(0) = x;
		VALUES(1) = MAKE_FIXNUM(0);
		break;
	case t_ratio:
		VALUES(0) = truncate2(x->ratio.num, x->ratio.den);
		VALUES(1) = make_ratio(VALUES(1), x->ratio.den);
		break;
	case t_shortfloat: {
		float d = sf(x);
		float y = d > 0? floorf(d) : ceilf(d);
		VALUES(0) = double_to_integer(y);
		VALUES(1) = make_shortfloat(d - y);
		break;
	}
	case t_longfloat: {
		double d = lf(x);
		double y = d > 0? floor(d) : ceil(d);
		VALUES(0) = double_to_integer(y);
		VALUES(1) = make_longfloat(d - y);
		break;
	}
	default:
		FEtype_error_real(x);
	}
	NValues = 2;
	return VALUES(0);
}

cl_object
truncate2(cl_object x, cl_object y)
{
	if (number_plusp(x) != number_plusp(y))
		return ceiling2(x, y);
	else
		return floor2(x, y);
}

@(defun truncate (x &optional (y OBJNULL))
@
	if (narg == 1)
		truncate1(x);
	else
		truncate2(x, y);
	returnn(VALUES(0));
@)

cl_object
round1(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		VALUES(0) = x;
		VALUES(1) = MAKE_FIXNUM(0);
		break;
	case t_ratio:
		return round2(x->ratio.num, x->ratio.den);
	case t_shortfloat: {
		double d = (double)(sf(x));
		cl_object q = double_to_integer(d + (d>=0? 0.5 : -0.5));
		d -= number_to_double(q);
		if (d == 0.5) {
		  if (number_oddp(q)) {
			q = one_plus(q);
			d = -0.5;
		  }
		} else if (d == -0.5) {
		  if (number_oddp(q)) {
			q = one_minus(q);
			d = 0.5;
		  }
		}
		VALUES(0) = q;
		VALUES(1) = make_shortfloat(d);
		break;
	}
	case t_longfloat: {
		double d = lf(x);
		cl_object q = double_to_integer(d + (d>=0? 0.5 : -0.5));
		d -= number_to_double(q);
		if (d == 0.5) {
		  if (number_oddp(q)) {
			q = one_plus(q);
			d = -0.5;
		  }
		} else if (d == -0.5) {
		  if (number_oddp(q)) {
			q = one_minus(q);
			d = 0.5;
		  }
		}
		VALUES(0) = q;
		VALUES(1) = make_longfloat(d);
		break;
	}
	default:
		FEtype_error_real(x);
	}
	NValues = 2;
	return VALUES(0);
}

cl_object
round2(cl_object x, cl_object y)
{
	cl_object q;

	q = number_divide(x, y);
	switch (type_of(q)) {
	case t_fixnum:
	case t_bignum:
		VALUES(0) = q;
		VALUES(1) = MAKE_FIXNUM(0);
		break;
	case t_ratio: {
		cl_object q1 = integer_divide(q->ratio.num, q->ratio.den);
		cl_object r = number_minus(q, q1);
		int c = number_compare(r, plus_half);
		if (c > 0 || (c == 0 && number_oddp(q1))) {
			q1 = one_plus(q1);
		} else if (c < 0 || (c == 0 && number_oddp(q1))) {
			q1 = one_minus(q1);
		}
		VALUES(0) = q1;
		VALUES(1) = number_remainder(x, y, q1);
		break;
	}
	case t_shortfloat:
	case t_longfloat: {
		double d = number_to_double(q);
		cl_object q1 = double_to_integer(d + (d >= 0.0 ? 0.5 : -0.5));
		d -= number_to_double(q1);
		if (d == 0.5 && number_oddp(q1))
			q1 = one_plus(q1);
		if (d == -0.5 && number_oddp(q1))
			q1 = one_minus(q1);
		VALUES(0) = q1;
		VALUES(1) = number_remainder(x, y, q1);
		break;
	}
	default:
		FEerror("Complex arguments to round2 (~S, ~S)", 2, x, y);
	}
	NValues = 2;
	return VALUES(0);
}

@(defun round (x &optional (y OBJNULL))
@
	if (narg == 1)
		round1(x);
	else
		round2(x, y);
	returnn(VALUES(0));
@)


@(defun mod (x y)
@
	/* INV: #'floor always outputs two values */
	@floor(2, x, y);
	@(return VALUES(1))
@)


@(defun rem (x y)
@
	@truncate(2, x, y);
	@(return VALUES(1))
@)


@(defun decode_float (x)
	double d;
	int e, s;
	enum cl_type tx = type_of(x);
@
	switch (tx) {
	case t_shortfloat:
		d = sf(x); break;
	case t_longfloat:
		d = lf(x); break;
	default:
		FEtype_error_float(x);
	}
	if (d >= 0.0)
		s = 1;
	else {
		d = -d;
		s = -1;
	}
	e = double_exponent(d);
	set_exponent(&d, 0);
	if (tx == t_shortfloat) {
		@(return make_shortfloat(d)
			 MAKE_FIXNUM(e)
			 make_shortfloat(s))
	} else {
		@(return make_longfloat(d)
			 MAKE_FIXNUM(e)
			 make_longfloat(s))
	}
@)


@(defun scale_float (x y)
	double d;
	int e, k;
	enum cl_type tx = type_of(x);
@
	if (FIXNUMP(y))
		k = fix(y);
	else
		FEerror("~S is an illegal exponent.", 1, y);
	switch (tx) {
	case t_shortfloat:
		d = sf(x); break;
	case t_longfloat:
		d = lf(x); break;
	default:
		FEtype_error_float(x);
	}
	e = double_exponent(d) + k;
#if defined(VAX) || defined(TAHOE)
	if (e <= -128 || e >= 128)
#endif
#ifdef IEEEFLOAT
	if (tx == t_shortfloat && (e <= -126 || e >= 130) ||
	    tx == t_longfloat && (e <= -1022 || e >= 1026))
#endif IEEEFLOAT
		FEerror("~S is an illegal exponent.", 1, y);
	set_exponent(&d, e);
	@(return ((tx == t_shortfloat) ? make_shortfloat(d)
				       : make_longfloat(d)))
@)


@(defun float_radix (x)
	enum cl_type t = type_of(x);
@
	if (t != t_shortfloat && t != t_longfloat)
		FEtype_error_float(x);
	@(return MAKE_FIXNUM(2))
@)


@(defun float_sign (x &optional (y x))
	int negativep;
@
	switch (type_of(x)) {
	case t_shortfloat:
		negativep = sf(x) < 0; break;
	case t_longfloat:
		negativep = lf(x) < 0; break;
	default:
		FEtype_error_float(x);
	}
	switch (type_of(y)) {
	case t_shortfloat: {
		float f = sf(y);
		@(return make_shortfloat(negativep? -fabsf(f) : fabsf(f)))
	}
	case t_longfloat: {
		double f = lf(y);
		@(return make_longfloat(negativep? -fabs(f) : fabs(f)))
	}
	default:
		FEtype_error_float(x);
	}
@)

@(defun float_digits (x)
@
	switch (type_of(x)) {
	case t_shortfloat:
		x = MAKE_FIXNUM(6);
		break;
	case t_longfloat:
		x = MAKE_FIXNUM(14);
		break;
	default:
		FEtype_error_float(x);
	}
	@(return x)
@)


@(defun float_precision (x)
@
	switch (type_of(x)) {
	case t_shortfloat:
		@(return ((sf(x) == 0.0) ? MAKE_FIXNUM(0) : MAKE_FIXNUM(24)))
	case t_longfloat:
		@(return ((lf(x) == 0.0) ? MAKE_FIXNUM(0) : MAKE_FIXNUM(53)))
	default:
		FEtype_error_float(x);
	}
@)


@(defun integer_decode_float (x)
	int h, l, e, s;
@
	switch (type_of(x)) {
	case t_longfloat:
		integer_decode_double(lf(x), &h, &l, &e, &s);
		x = (h != 0) ? bignum2(h, l) : MAKE_FIXNUM(l);
		break;
	case t_shortfloat:
		integer_decode_float((double)(sf(x)), &h, &e, &s);
		x = MAKE_FIXNUM(h);
		break;
	default:
		FEtype_error_float(x);
	}
	@(return x MAKE_FIXNUM(e) MAKE_FIXNUM(s))
@)


@(defun complex (r &optional (i MAKE_FIXNUM(0)))
@	/* INV: make_complex() checks types */
	@(return make_complex(r, i))
@)


@(defun realpart (x)
@
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
	case t_shortfloat:
	case t_longfloat:
		break;
	case t_complex:
		x = x->complex.real;
		break;
	default:
		FEtype_error_number(x);
	}
	@(return x)
@)


@(defun imagpart (x)
@
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		x = MAKE_FIXNUM(0);
		break;
	case t_shortfloat:
		x = shortfloat_zero;
		break;
	case t_longfloat:
		x = longfloat_zero;
		break;
	case t_complex:
		x = x->complex.imag;
		break;
	default:
		FEtype_error_number(x);
	}
	@(return x)
@)

void
init_num_co(void)
{
	float smallest_float, biggest_float;
	double smallest_double, biggest_double;
	float float_epsilon, float_negative_epsilon;
	double double_epsilon, double_negative_epsilon;
	double lf1, lf2;
	float sf1, sf2;
	cl_object num;

#define LF_EQL(a,b) (lf1 = a, lf2 = b, lf1 == lf2)
#define SF_EQL(a,b) (sf1 = a, sf2 = b, sf1 == sf2)

#ifdef VAX
	l[0] = 0x80;
	l[1] = 0;
	smallest_float = *(float *)l;
	smallest_double = *(double *)l;
#endif VAX

#ifdef IEEEFLOAT
	((int *) &smallest_float)[0]= 1;
	((int *) &smallest_double)[HIND] = 0;
	((int *) &smallest_double)[LIND] = 1;
#endif IEEEFLOAT

#ifdef VAX
	l[0] = 0xffff7fff;
	l[1] = 0xffffffff;
	biggest_float = *(float *)l;
	biggest_double = *(double *)l;
#endif VAX

#ifdef IEEEFLOAT
	((unsigned int *) &biggest_float)[0]= (unsigned int)0x7f7fffff;
	((unsigned int *) &biggest_double)[HIND] = (unsigned int)0x7fefffff;
	((unsigned int *) &biggest_double)[LIND] = (unsigned int)0xffffffff;
#endif IEEEFLOAT

#ifdef TAHOE
        l[0] = 0x00800000;
        l[1] = 0;
        smallest_float = *(float *)l;
        smallest_double = *(double *)l;
#endif

/* We want the smallest number not satisfying something,
   and so we go quickly down, and then back up.  We have
   to use a function call for test, since in line code may keep
   too much precision, while the usual lisp eql,is not
   in line.
   We use SMALL as a multiple to come back up by.
*/

#define SMALL 1.05	

	for (float_epsilon = 1.0;
	     !SF_EQL((float)(1.0 + float_epsilon),(float)1.0);
	     float_epsilon /= 2.0)
		;
	while(SF_EQL((float)(1.0 + float_epsilon),(float)1.0))
	  float_epsilon=float_epsilon*SMALL;
	for (float_negative_epsilon = 1.0;
	     !SF_EQL((float)(1.0 - float_negative_epsilon) ,(float)1.0);
	     float_negative_epsilon /= 2.0)
		;
	while(SF_EQL((float)(1.0 - float_negative_epsilon) ,(float)1.0))
	  float_negative_epsilon=float_negative_epsilon*SMALL;
	for (double_epsilon = 1.0;
	     !(LF_EQL(1.0 + double_epsilon, 1.0));
	     double_epsilon /= 2.0)
		;
	while((LF_EQL(1.0 + double_epsilon, 1.0)))
	  double_epsilon=double_epsilon*SMALL;
	  ;
	for (double_negative_epsilon = 1.0;
	     !LF_EQL(1.0 - double_negative_epsilon , 1.0);
	     double_negative_epsilon /= 2.0)
		;
	while(LF_EQL(1.0 - double_negative_epsilon , 1.0))
	  double_negative_epsilon=double_negative_epsilon*SMALL;
	  ;

	num = make_shortfloat(biggest_float);
	make_constant("MOST-POSITIVE-SHORT-FLOAT", num);
	make_constant("MOST-POSITIVE-SINGLE-FLOAT", num);

	num = make_shortfloat(smallest_float);
	make_constant("LEAST-POSITIVE-SHORT-FLOAT", num);
	make_constant("LEAST-POSITIVE-SINGLE-FLOAT", num);

	num = make_shortfloat(-smallest_float);
	make_constant("LEAST-NEGATIVE-SHORT-FLOAT", num);
	make_constant("LEAST-NEGATIVE-SINGLE-FLOAT", num);

	num = make_shortfloat(-biggest_float);
	make_constant("MOST-NEGATIVE-SHORT-FLOAT", num);
	make_constant("MOST-NEGATIVE-SINGLE-FLOAT", num);

	num = make_longfloat(biggest_double);
	make_constant("MOST-POSITIVE-DOUBLE-FLOAT", num);
	make_constant("MOST-POSITIVE-LONG-FLOAT", num);

	num = make_longfloat(smallest_double);
	make_constant("LEAST-POSITIVE-DOUBLE-FLOAT", num);
	make_constant("LEAST-POSITIVE-LONG-FLOAT", num);

	num = make_longfloat(-smallest_double);
	make_constant("LEAST-NEGATIVE-DOUBLE-FLOAT", num);
	make_constant("LEAST-NEGATIVE-LONG-FLOAT", num);

	num = make_longfloat(-biggest_double);
	make_constant("MOST-NEGATIVE-DOUBLE-FLOAT", num);
	make_constant("MOST-NEGATIVE-LONG-FLOAT", num);

	num = make_shortfloat(float_epsilon);
	make_constant("SHORT-FLOAT-EPSILON", num);
	make_constant("SINGLE-FLOAT-EPSILON", num);
	num = make_longfloat(double_epsilon);
	make_constant("DOUBLE-FLOAT-EPSILON", num);
	make_constant("LONG-FLOAT-EPSILON", num);

	num = make_shortfloat(float_negative_epsilon);
	make_constant("SHORT-FLOAT-NEGATIVE-EPSILON", num);
	make_constant("SINGLE-FLOAT-NEGATIVE-EPSILON", num);
	num = make_longfloat(double_negative_epsilon);
	make_constant("DOUBLE-FLOAT-NEGATIVE-EPSILON", num);
	make_constant("LONG-FLOAT-NEGATIVE-EPSILON", num);

	plus_half = make_ratio(MAKE_FIXNUM(1), MAKE_FIXNUM(2));
	register_root(&plus_half);

	minus_half = make_ratio(MAKE_FIXNUM(-1), MAKE_FIXNUM(2));
	register_root(&minus_half);
}
