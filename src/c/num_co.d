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

#include "ecl.h"
#include <float.h>
#include <math.h>
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

static cl_object plus_half, minus_half;

cl_object
double_to_integer(double d)
{
	if (d <= MOST_POSITIVE_FIXNUM && d >= MOST_NEGATIVE_FIXNUM)
		return MAKE_FIXNUM((cl_fixnum)d);
	else {
		cl_object x = big_register0_get();
		mpz_set_d(x->big.big_num, d);
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
		mpz_set_d(x->big.big_num, d);
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
	cl_type t = t_shortfloat;
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
		VALUES(0) = float_to_integer(y);
		VALUES(1) = make_shortfloat(d - y);
		break;
	}
	case t_longfloat: {
		double d = lf(x);
		double y = floor(d);
		VALUES(0) = float_to_integer(y);
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
		  VALUES(0) = float_to_integer(q);
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
		  VALUES(0) = float_to_integer(q);
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
		VALUES(0) = float_to_integer(q);
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
		float d = sf(x);
		float y = ceilf(d);
		VALUES(0) = float_to_integer(y);
		VALUES(1) = make_shortfloat(d - y);
		break;
	}
	case t_longfloat: {
		double d = lf(x);
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
		  VALUES(0) = float_to_integer(q);
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
		  VALUES(0) = float_to_integer(q);
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
		VALUES(0) = float_to_integer(q);
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
		VALUES(0) = float_to_integer(y);
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
	case t_shortfloat: {
		float d = sf(q);
		float aux = d + (d >= 0.0 ? 0.5 : -0.5);
		cl_object q1 = float_to_integer(aux);
		d -= aux;
		if (d == 0.5 && number_oddp(q1))
			q1 = one_plus(q1);
		if (d == -0.5 && number_oddp(q1))
			q1 = one_minus(q1);
		VALUES(0) = q1;
		VALUES(1) = number_remainder(x, y, q1);
		break;
	}
	case t_longfloat: {
		double d = lf(q);
		double aux = d + (d >= 0.0 ? 0.5 : -0.5);
		cl_object q1 = double_to_integer(aux);
		d -= aux;
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
	cl_type tx = type_of(x);
@
	switch (tx) {
	case t_shortfloat: {
		float d = sf(x);
		if (d >= 0.0)
			s = 1;
		else {
			d = -d;
			s = 0;
		}
		d = frexpf(d, &e);
		x = make_shortfloat(d);
		break;
	}
	case t_longfloat: {
		double d = lf(x);
		if (d >= 0.0)
			s = 1;
		else {
			d = -d;
			s = 0;
		}
		d = frexp(d, &e);
		x = make_shortfloat(d);
		break;
	}
	default:
		FEtype_error_float(x);
	}
	@(return x MAKE_FIXNUM(e) make_shortfloat(s))
@)


@(defun scale_float (x y)
	int k;
@
	if (FIXNUMP(y))
		k = fix(y);
	else
		FEerror("~S is an illegal exponent.", 1, y);
	switch (type_of(x)) {
	case t_shortfloat:
		x = make_shortfloat(ldexpf(sf(x), k));
		break;
	case t_longfloat:
		x = make_longfloat(ldexp(lf(x), k));
		break;
	default:
		FEtype_error_float(x);
	}
	@(return x)
@)


@(defun float_radix (x)
	cl_type t = type_of(x);
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
	unsigned int h, l;
	int e, s;
@
	switch (type_of(x)) {
	case t_longfloat: {
		double d = lf(x);
		if (d == 0.0) {
			e = 0;
			s = 1;
			x = MAKE_FIXNUM(0);
		} else {
			if (d < 0.0) {
				s = -1;
				d = -frexp(d, &e);
			} else {
				s = 1;
				d = frexp(d, &e);
			}
			x = double_to_integer(ldexp(d, DBL_MANT_DIG));
			e -= DBL_MANT_DIG;
		}
		break;
	}
	case t_shortfloat: {
		float d = sf(x);
		if (d == 0.0) {
			e = 0;
			s = 1;
			x = MAKE_FIXNUM(0);
		} else {
			if (d < 0.0) {
				s = -1;
				d = -frexpf(d, &e);
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
	cl_object num;

	num = make_shortfloat(FLT_MAX);
	SYM_VAL(@'MOST-POSITIVE-SHORT-FLOAT') = num;
	SYM_VAL(@'MOST-POSITIVE-SINGLE-FLOAT') = num;

	num = make_shortfloat(FLT_MIN);
	SYM_VAL(@'LEAST-POSITIVE-SHORT-FLOAT') = num;
	SYM_VAL(@'LEAST-POSITIVE-SINGLE-FLOAT') = num;

	num = make_shortfloat(-FLT_MIN);
	SYM_VAL(@'LEAST-NEGATIVE-SHORT-FLOAT') = num;
	SYM_VAL(@'LEAST-NEGATIVE-SINGLE-FLOAT') = num;

	num = make_shortfloat(-FLT_MAX);
	SYM_VAL(@'MOST-NEGATIVE-SHORT-FLOAT') = num;
	SYM_VAL(@'MOST-NEGATIVE-SINGLE-FLOAT') = num;

	num = make_longfloat(DBL_MAX);
	SYM_VAL(@'MOST-POSITIVE-DOUBLE-FLOAT') = num;
	SYM_VAL(@'MOST-POSITIVE-LONG-FLOAT') = num;

	num = make_longfloat(DBL_MIN);
	SYM_VAL(@'LEAST-POSITIVE-DOUBLE-FLOAT') = num;
	SYM_VAL(@'LEAST-POSITIVE-LONG-FLOAT') = num;

	num = make_longfloat(-DBL_MIN);
	SYM_VAL(@'LEAST-NEGATIVE-DOUBLE-FLOAT') = num;
	SYM_VAL(@'LEAST-NEGATIVE-LONG-FLOAT') = num;

	num = make_longfloat(-DBL_MAX);
	SYM_VAL(@'MOST-NEGATIVE-DOUBLE-FLOAT') = num;
	SYM_VAL(@'MOST-NEGATIVE-LONG-FLOAT') = num;

	num = make_shortfloat(FLT_EPSILON);
	SYM_VAL(@'SHORT-FLOAT-EPSILON') = num;
	SYM_VAL(@'SINGLE-FLOAT-EPSILON') = num;
	num = make_longfloat(DBL_EPSILON);
	SYM_VAL(@'DOUBLE-FLOAT-EPSILON') = num;
	SYM_VAL(@'LONG-FLOAT-EPSILON') = num;

	num = make_shortfloat(-FLT_EPSILON);
	SYM_VAL(@'SHORT-FLOAT-NEGATIVE-EPSILON') = num;
	SYM_VAL(@'SINGLE-FLOAT-NEGATIVE-EPSILON') = num;
	num = make_longfloat(-DBL_EPSILON);
	SYM_VAL(@'DOUBLE-FLOAT-NEGATIVE-EPSILON') = num;
	SYM_VAL(@'LONG-FLOAT-NEGATIVE-EPSILON') = num;

	plus_half = make_ratio(MAKE_FIXNUM(1), MAKE_FIXNUM(2));
	register_root(&plus_half);

	minus_half = make_ratio(MAKE_FIXNUM(-1), MAKE_FIXNUM(2));
	register_root(&minus_half);
}
