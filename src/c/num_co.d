/*
    num_co.c -- Operations on floating-point numbers.
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

/*
	IMPLEMENTATION-DEPENDENT

	This file contains those functions
	that know the representation of floating-point numbers.
*/

#include <ecl/ecl.h>
#include <float.h>
#include <math.h>
#ifdef _MSC_VER
# undef complex
#endif
#ifndef HAVE_ISOC99
# define floorf floor
# define ceilf ceil
# define fabsf fabs
# define frexpf frexp
# define ldexpf ldexp
# define cosf cos
# define coshf cosh
# define expf exp
# define logf log
# define sinf sin
# define sqrtf sqrt
# define tanf tan
# define tanhf tanh
#endif

cl_object
double_to_integer(double d)
{
	if (d <= MOST_POSITIVE_FIXNUM && d >= MOST_NEGATIVE_FIXNUM)
		return MAKE_FIXNUM((cl_fixnum)d);
	else {
		cl_object x = big_register0_get();
#ifdef WITH_GMP
		mpz_set_d(x->big.big_num, d);
#else  /* WITH_GMP */
                x->big.big_num = (big_num_t)d;
#endif /* WITH_GMP */
		return big_register_copy(x);
	}
}

cl_object
float_to_integer(float d)
{
	if (d <= MOST_POSITIVE_FIXNUM && d >= MOST_NEGATIVE_FIXNUM)
		return MAKE_FIXNUM((cl_fixnum)d);
	else {
		cl_object x = big_register0_get();
#ifdef WITH_GMP
		mpz_set_d(x->big.big_num, d);
#else  /* WITH_GMP */
                x->big.big_num = (big_num_t)d;
#endif /* WITH_GMP */
		return big_register_copy(x);
	}
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
	cl_type t = t_singlefloat;
@
	if (y != OBJNULL) {
		t = type_of(y);
		if (t != t_singlefloat && t != t_doublefloat)
			FEtype_error_float(y);
	}
	switch (type_of(x)) {
	case t_fixnum:
		if (t == t_singlefloat)
			x = make_singlefloat(fix(x));
		else
			x = make_doublefloat(fix(x));
		break;
	case t_bignum:
	case t_ratio: {
		double d = number_to_double(x);
		if (t == t_singlefloat)
			x = make_singlefloat(d);
		else
			x = make_doublefloat(d);		
		break;
	}
	case t_singlefloat:
		if (y && t == t_doublefloat)
			x = make_doublefloat(sf(x));
		break;
	case t_doublefloat:
		if (y && t == t_singlefloat)
			x = make_singlefloat(df(x));
		break;
	default:
		FEtype_error_real(x);
	}
	@(return x)
@)

cl_object
cl_numerator(cl_object x)
{
	cl_object out;

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
}

cl_object
cl_denominator(cl_object x)
{
	cl_object out;

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
}

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
	case t_singlefloat: {
		float d = sf(x);
		float y = floorf(d);
		VALUES(0) = float_to_integer(y);
		VALUES(1) = make_singlefloat(d - y);
		break;
	}
	case t_doublefloat: {
		double d = df(x);
		double y = floor(d);
		VALUES(0) = double_to_integer(y);
		VALUES(1) = make_doublefloat(d - y);
		break;
	}
	default:
		FEtype_error_real(x);
	}
	NVALUES = 2;
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
		  /* We must perform the division because there is the
		   * pathological case
		   *	x = MOST_NEGATIVE_FIXNUM
		   *    y = - MOST_NEGATIVE_FIXNUM
		   */
		  cl_object q = big_register0_get();
		  cl_object r = big_register1_get();
#ifdef WITH_GMP
		  cl_object j = big_register2_get();
		  mpz_set_si(j->big.big_num, fix(x));
		  mpz_fdiv_qr(q->big.big_num, r->big.big_num, j->big.big_num, y->big.big_num);
#else  /* WITH_GMP */
                  q->big.big_num = (big_num_t)fix(x) / y->big.big_num;
                  r->big.big_num = (big_num_t)fix(x) % y->big.big_num;
#endif /* WITH_GMP */
		  VALUES(0) = big_register_normalize(q);
		  VALUES(1) = big_register_normalize(r);
		  break;
		}
		case t_ratio:		/* FIX / RAT */
		  floor2(number_times(x, y->ratio.den), y->ratio.num);
		  VALUES(1) = make_ratio(VALUES(1), y->ratio.den);
		  break;
		case t_singlefloat: {	/* FIX / SF */
		  float n = sf(y);
		  float p = fix(x) / n;
		  float q = floorf(p);
		  VALUES(0) = float_to_integer(q);
		  VALUES(1) = make_singlefloat((p - q)*n);
		  break;
		}
		case t_doublefloat: {	/* FIX / DF */
		  double n = df(y);
		  double p = fix(x) / n;
		  double q = floor(p);
		  VALUES(0) = double_to_integer(q);
		  VALUES(1) = make_doublefloat((p - q)*n);
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
#ifdef WITH_GMP
		  cl_object j = big_register2_get();
		  mpz_set_si(j->big.big_num, fix(y));
		  mpz_fdiv_qr(q->big.big_num, r->big.big_num, x->big.big_num, j->big.big_num);
#else  /* WITH_GMP */
                  q->big.big_num = x->big.big_num / fix(y);
                  r->big.big_num = x->big.big_num % fix(y);
#endif /* WITH_GMP */
		  VALUES(0) = big_register_normalize(q);
		  VALUES(1) = big_register_normalize(r);
		  break;
		}
		case t_bignum: {	/* BIG / BIG */
		  cl_object q = big_register0_get();
		  cl_object r = big_register1_get();
#ifdef WITH_GMP
		  mpz_fdiv_qr(q->big.big_num, r->big.big_num, x->big.big_num, y->big.big_num);
#else  /* WITH_GMP */
                  q = x->big.big_num / y->big.big_num;
                  r = x->big.big_num % y->big.big_num;
#endif /* WITH_GMP */
		  VALUES(0) = big_register_normalize(q);
		  VALUES(1) = big_register_normalize(r);
		  break;
		}
		case t_ratio:		/* BIG / RAT */
		  floor2(number_times(x, y->ratio.den), y->ratio.num);
		  VALUES(1) = make_ratio(VALUES(1), y->ratio.den);
		  break;
		case t_singlefloat: {	/* BIG / SF */
		  float n = sf(y);
		  float p = big_to_double(x) / n;
		  float q = floorf(p);
		  VALUES(0) = float_to_integer(q);
		  VALUES(1) = make_singlefloat((p - q)*n);
		  break;
		}
		case t_doublefloat: {	/* BIG / DF */
		  double n = df(y);
		  double p = big_to_double(x) / n;
		  double q = floor(p);
		  VALUES(0) = double_to_integer(q);
		  VALUES(1) = make_doublefloat((p - q)*n);
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
		  VALUES(1) = number_divide(VALUES(1), x->ratio.den);
		}
		break;
	case t_singlefloat: {		/* SF / ANY */
		float n = number_to_double(y);
		float p = sf(x)/n;
		float q = floorf(p);
		VALUES(0) = float_to_integer(q);
		VALUES(1) = make_singlefloat((p - q)*n);
		break;
	}
	case t_doublefloat: {		/* DF / ANY */
		double n = number_to_double(y);
		double p = df(x)/n;
		double q = floor(p);
		VALUES(0) = double_to_integer(q);
		VALUES(1) = make_doublefloat((p - q)*n);
		break;
	}
	default:
		FEtype_error_real(x);
	}
	NVALUES = 2;
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
	case t_singlefloat: {
		float d = sf(x);
		float y = ceilf(d);
		VALUES(0) = float_to_integer(y);
		VALUES(1) = make_singlefloat(d - y);
		break;
	}
	case t_doublefloat: {
		double d = df(x);
		double y = ceil(d);
		VALUES(0) = double_to_integer(y);
		VALUES(1) = make_doublefloat(d - y);
		break;
	}
	default:
		FEtype_error_real(x);
	}
	NVALUES = 2;
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
		  /* We must perform the division because there is the
		   * pathological case
		   *	x = MOST_NEGATIVE_FIXNUM
		   *    y = - MOST_NEGATIVE_FIXNUM
		   */
		  cl_object q = big_register0_get();
		  cl_object r = big_register1_get();
#ifdef WITH_GMP
		  cl_object j = big_register2_get();
		  mpz_set_si(j->big.big_num, fix(x));
		  mpz_cdiv_qr(q->big.big_num, r->big.big_num, j->big.big_num, y->big.big_num);
#else  /* WITH_GMP */
                  q = (big_num_t)fix(x) / y->big.big_num;
                  r = (big_num_t)fix(x) % y->big.big_num;
#endif /* WITH_GMP */
		  VALUES(0) = big_register_normalize(q);
		  VALUES(1) = big_register_normalize(r);
		  break;
		}
		case t_ratio:		/* FIX / RAT */
		  ceiling2(number_times(x, y->ratio.den), y->ratio.num);
		  VALUES(1) = make_ratio(VALUES(1), y->ratio.den);
		  break;
		case t_singlefloat: {	/* FIX / SF */
		  float n = sf(y);
		  float p = fix(x)/n;
		  float q = ceilf(p);
		  VALUES(0) = float_to_integer(q);
		  VALUES(1) = make_singlefloat((p - q)*n);
		  break;
		}
		case t_doublefloat: {	/* FIX / DF */
		  double n = df(y);
		  double p = fix(x)/n;
		  double q = ceil(p);
		  VALUES(0) = double_to_integer(q);
		  VALUES(1) = make_doublefloat((p - q)*n);
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
#ifdef WITH_GMP
		  cl_object j = big_register2_get();
		  mpz_set_si(j->big.big_num, fix(y));
		  mpz_cdiv_qr(q->big.big_num, r->big.big_num, x->big.big_num, j->big.big_num);
#else  /* WITH_GMP */
                  q = x->big.big_num / fix(y);
                  r = x->big.big_num % fix(y);
#endif /* WITH_GMP */
		  VALUES(0) = big_register_normalize(q);
		  VALUES(1) = big_register_normalize(r);
		  break;
		}
		case t_bignum: {	/* BIG / BIG */
		  cl_object q = big_register0_get();
		  cl_object r = big_register1_get();
#ifdef WITH_GMP
		  mpz_cdiv_qr(q->big.big_num, r->big.big_num, x->big.big_num, y->big.big_num);
#else  /* WITH_GMP */
                  q->big.big_num = x->big.big_num / y->big.big_num;
                  r->big.big_num = x->big.big_num % y->big.big_num;
#endif /* WITH_GMP */
		  VALUES(0) = big_register_normalize(q);
		  VALUES(1) = big_register_normalize(r);
		  break;
		}
		case t_ratio:		/* BIG / RAT */
		  ceiling2(number_times(x, y->ratio.den), y->ratio.num);
		  VALUES(1) = make_ratio(VALUES(1), y->ratio.den);
		  break;
		case t_singlefloat: {	/* BIG / SF */
		  float n = sf(y);
		  float p = big_to_double(x)/n;
		  float q = ceilf(p);
		  VALUES(0) = float_to_integer(q);
		  VALUES(1) = make_singlefloat((p - q)*n);
		  break;
		}
		case t_doublefloat: {	/* BIG / DF */
		  double n = df(y);
		  double p = big_to_double(x)/n;
		  double q = ceil(p);
		  VALUES(0) = double_to_integer(q);
		  VALUES(1) = make_doublefloat((p - q)*n);
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
	case t_singlefloat: {		/* SF / ANY */
		float n = number_to_double(y);
		float p = sf(x)/n;
		float q = ceilf(p);
		VALUES(0) = float_to_integer(q);
		VALUES(1) = make_singlefloat((p - q)*n);
		break;
	}
	case t_doublefloat: {		/* DF / ANY */
		double n = number_to_double(y);
		double p = df(x)/n;
		double q = ceil(p);
		VALUES(0) = double_to_integer(q);
		VALUES(1) = make_doublefloat((p - q)*n);
		break;
	}
	default:
		FEtype_error_real(x);
	}
	NVALUES = 2;
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
	case t_singlefloat: {
		float d = sf(x);
		float y = d > 0? floorf(d) : ceilf(d);
		VALUES(0) = float_to_integer(y);
		VALUES(1) = make_singlefloat(d - y);
		break;
	}
	case t_doublefloat: {
		double d = df(x);
		double y = d > 0? floor(d) : ceil(d);
		VALUES(0) = double_to_integer(y);
		VALUES(1) = make_doublefloat(d - y);
		break;
	}
	default:
		FEtype_error_real(x);
	}
	NVALUES = 2;
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
		VALUES(0) = round2(x->ratio.num, x->ratio.den);
		VALUES(1) = make_ratio(VALUES(1), x->ratio.den);
		break;
	case t_singlefloat: {
		float d = sf(x);
		cl_object q = float_to_integer(d + (d>=0? 0.5 : -0.5));
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
		VALUES(1) = make_singlefloat(d);
		break;
	}
	case t_doublefloat: {
		double d = df(x);
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
		VALUES(1) = make_doublefloat(d);
		break;
	}
	default:
		FEtype_error_real(x);
	}
	NVALUES = 2;
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
		if (number_minusp(r)) {
			int c = number_compare(cl_core.minus_half, r);
			if (c > 0 || (c == 0 && number_oddp(q1))) {
				q1 = one_minus(q1);
			}
		} else {
			int c = number_compare(r, cl_core.plus_half);
			if (c > 0 || (c == 0 && number_oddp(q1))) {
				q1 = one_plus(q1);
			}
		}
		VALUES(0) = q1;
		VALUES(1) = number_remainder(x, y, q1);
		break;
	}
	default:
		VALUES(0) = q = round1(q);
		VALUES(1) = number_remainder(x, y, q);
	}
	NVALUES = 2;
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


cl_object
cl_mod(cl_object x, cl_object y)
{
	/* INV: #'floor always outputs two values */
	@floor(2, x, y);
	@(return VALUES(1))
}

cl_object
cl_rem(cl_object x, cl_object y)
{
	@truncate(2, x, y);
	@(return VALUES(1))
}

cl_object
cl_decode_float(cl_object x)
{
	int e, s;
	cl_type tx = type_of(x);

	switch (tx) {
	case t_singlefloat: {
		float d = sf(x);
		if (d >= 0.0)
			s = 1;
		else {
			d = -d;
			s = 0;
		}
		d = frexpf(d, &e);
		x = make_singlefloat(d);
		break;
	}
	case t_doublefloat: {
		double d = df(x);
		if (d >= 0.0)
			s = 1;
		else {
			d = -d;
			s = 0;
		}
		d = frexp(d, &e);
		x = make_doublefloat(d);
		break;
	}
	default:
		FEtype_error_float(x);
	}
	@(return x MAKE_FIXNUM(e) make_singlefloat(s))
}

cl_object
cl_scale_float(cl_object x, cl_object y)
{
	cl_fixnum k;

	if (FIXNUMP(y))
		k = fix(y);
	else
		FEerror("~S is an illegal exponent.", 1, y);
	switch (type_of(x)) {
	case t_singlefloat:
		x = make_singlefloat(ldexpf(sf(x), k));
		break;
	case t_doublefloat:
		x = make_doublefloat(ldexp(df(x), k));
		break;
	default:
		FEtype_error_float(x);
	}
	@(return x)
}

cl_object
cl_float_radix(cl_object x)
{
	cl_type t = type_of(x);

	if (t != t_singlefloat && t != t_doublefloat)
		FEtype_error_float(x);
	@(return MAKE_FIXNUM(FLT_RADIX))
}

@(defun float_sign (x &optional (y x))
	int negativep;
@
	switch (type_of(x)) {
	case t_singlefloat:
		negativep = sf(x) < 0; break;
	case t_doublefloat:
		negativep = df(x) < 0; break;
	default:
		FEtype_error_float(x);
	}
	switch (type_of(y)) {
	case t_singlefloat: {
		float f = sf(y);
		@(return make_singlefloat(negativep? -fabsf(f) : fabsf(f)))
	}
	case t_doublefloat: {
		double f = df(y);
		@(return make_doublefloat(negativep? -fabs(f) : fabs(f)))
	}
	default:
		FEtype_error_float(x);
	}
@)

cl_object
cl_float_digits(cl_object x)
{
	switch (type_of(x)) {
	case t_singlefloat:
		x = MAKE_FIXNUM(FLT_MANT_DIG);
		break;
	case t_doublefloat:
		x = MAKE_FIXNUM(DBL_MANT_DIG);
		break;
	default:
		FEtype_error_float(x);
	}
	@(return x)
}

cl_object
cl_float_precision(cl_object x)
{
	int precision;
	switch (type_of(x)) {
	case t_singlefloat: {
		float f = sf(x);
		if (f == 0.0) {
			precision = 0;
		} else {
			int exp;
			frexpf(f, &exp);
			if (exp >= FLT_MIN_EXP) {
				precision = FLT_MANT_DIG;
			} else {
				precision = FLT_MANT_DIG - (FLT_MIN_EXP - exp);
			}
		}
		break;
	}
	case t_doublefloat: {
		double f = df(x);
		if (f == 0.0) {
			precision = 0;
		} else {
			int exp;
			frexp(f, &exp);
			if (exp >= DBL_MIN_EXP) {
				precision = DBL_MANT_DIG;
			} else {
				precision = DBL_MANT_DIG - (DBL_MIN_EXP - exp);
			}
		}
		break;
	}
	default:
		FEtype_error_float(x);
	}
	@(return MAKE_FIXNUM(precision))
}

cl_object
cl_integer_decode_float(cl_object x)
{
	int e, s;

	switch (type_of(x)) {
	case t_doublefloat: {
		double d = df(x);
		if (d == 0.0) {
			e = 0;
			s = 1;
			x = MAKE_FIXNUM(0);
		} else {
			if (d < 0.0) {
				s = -1;
				d = frexp(-d, &e);
			} else {
				s = 1;
				d = frexp(d, &e);
			}
			x = double_to_integer(ldexp(d, DBL_MANT_DIG));
			e -= DBL_MANT_DIG;
		}
		break;
	}
	case t_singlefloat: {
		float d = sf(x);
		if (d == 0.0) {
			e = 0;
			s = 1;
			x = MAKE_FIXNUM(0);
		} else {
			if (d < 0.0) {
				s = -1;
				d = frexpf(-d, &e);
			} else {
				s = 1;
				d = frexpf(d, &e);
			}
			x = double_to_integer(ldexp(d, FLT_MANT_DIG));
			e -= FLT_MANT_DIG;
		}
		break;
	}
	default:
		FEtype_error_float(x);
	}
	@(return x MAKE_FIXNUM(e) MAKE_FIXNUM(s))
}


@(defun complex (r &optional (i MAKE_FIXNUM(0)))
@	/* INV: make_complex() checks types */
	@(return make_complex(r, i))
@)

cl_object
cl_realpart(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
	case t_singlefloat:
	case t_doublefloat:
		break;
	case t_complex:
		x = x->complex.real;
		break;
	default:
		FEtype_error_number(x);
	}
	@(return x)
}

cl_object
cl_imagpart(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		x = MAKE_FIXNUM(0);
		break;
	case t_singlefloat:
		x = cl_core.singlefloat_zero;
		break;
	case t_doublefloat:
		x = cl_core.doublefloat_zero;
		break;
	case t_complex:
		x = x->complex.imag;
		break;
	default:
		FEtype_error_number(x);
	}
	@(return x)
}
