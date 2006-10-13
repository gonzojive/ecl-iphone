/*
    num_comp.c  -- Comparisons on numbers.
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

@(defun = (num &rest nums)
	int i;
@
	/* ANSI: Need not signal error for 1 argument */
	/* INV: For >= 2 arguments, number_equalp() performs checks */
	for (i = 1; i < narg; i++)
		if (!number_equalp(num, cl_va_arg(nums)))
			@(return Cnil)
	@(return Ct)
@)

/* Returns 1 if both numbers compare to equal */
int
number_equalp(cl_object x, cl_object y)
{
	double dx;
	/* INV: (= fixnum bignum) => 0 */
	/* INV: (= fixnum ratio) => 0 */
	/* INV: (= bignum ratio) => 0 */
 BEGIN:
	switch (type_of(x)) {
	case t_fixnum:
		switch (type_of(y)) {
		case t_fixnum:
		  	return x == y;
		case t_bignum:
		case t_ratio:
			return 0;
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return fix(x) == ecl_short_float(y);
#endif
		case t_singlefloat:
			return fix(x) == (double)sf(y);
		case t_doublefloat:
			return fix(x) == df(y);
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return fix(x) == ecl_long_float(y);
#endif
		case t_complex:
			goto Y_COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
			return 0;
		case t_bignum:
			return big_compare(x, y)==0;
		case t_ratio:
			return 0;
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
#endif
		case t_singlefloat:
		case t_doublefloat:
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
#endif
			y = cl_rational(y);
			goto BEGIN;
		case t_complex:
			goto Y_COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			return 0;
		case t_ratio:
			return (number_equalp(x->ratio.num, y->ratio.num) &&
				number_equalp(x->ratio.den, y->ratio.den));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
#endif
		case t_singlefloat:
		case t_doublefloat:
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
#endif
			y = cl_rational(y);
			goto BEGIN;
		case t_complex:
			goto Y_COMPLEX;
		default:
			FEtype_error_number(y);
		}
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		dx = ecl_short_float(x);
		goto FLOAT;
#endif
	case t_singlefloat:
		dx = sf(x);
		goto FLOAT;
	case t_doublefloat:
		dx = df(x);
	FLOAT:
		switch (type_of(y)) {
		case t_fixnum:
			return dx == fix(y);
		case t_bignum:
		case t_ratio:
			x = cl_rational(x);
			goto BEGIN;
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return dx == ecl_short_float(y);
#endif
		case t_singlefloat:
			return dx == sf(y);
		case t_doublefloat:
			return dx == df(y);
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return dx == ecl_long_float(y);
#endif
		case t_complex:
			goto Y_COMPLEX;
		default:
			FEtype_error_number(y);
		}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double dx = ecl_long_float(x);
		switch (type_of(y)) {
		case t_fixnum:
			return dx == fix(y);
		case t_bignum:
		case t_ratio:
			x = cl_rational(x);
			goto BEGIN;
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return dx == ecl_short_float(y);
#endif
		case t_singlefloat:
			return dx == sf(y);
		case t_doublefloat:
			return dx == df(y);
		case t_longfloat:
			return dx == ecl_long_float(y);
		case t_complex:
			goto Y_COMPLEX;
		default:
			FEtype_error_number(y);
		}
	}
#endif
	Y_COMPLEX:
		if (!number_zerop(y->complex.imag))
			return 0;
		return number_equalp(x, y->complex.real);
	case t_complex:
		if (type_of(y) == t_complex)
			return (number_equalp(x->complex.real, y->complex.real) &&
				number_equalp(x->complex.imag, y->complex.imag));
		if (REAL_TYPE(type_of(y))) {
			if (number_zerop(x->complex.imag))
				return number_equalp(x->complex.real, y) != 0;
			else
				return 0;
		}
		FEtype_error_number(y);
	default:
		FEtype_error_number(x);
	}
}

/*
	The value of number_compare(x, y) is

		-1	if	x < y
		0	if	x = y
		1	if	x > y.

	If x or y is not real, it fails.
*/
int
number_compare(cl_object x, cl_object y)
{
	cl_fixnum ix, iy;
	double dx, dy;
#ifdef ECL_LONG_FLOAT
	long double ldx, ldy;
#endif
	cl_type ty;
 BEGIN:
	ty = type_of(y);
	switch (type_of(x)) {
	case t_fixnum:
	  	ix = fix(x);
		switch (ty) {
		case t_fixnum:
		  	iy = fix(y);
			if (ix < iy)
				return(-1);
			else return(ix != iy);
		case t_bignum:
			/* INV: (= x y) can't be zero since fixnum != bignum */
			return big_sign(y) < 0? 1 : -1;
		case t_ratio:
			x = number_times(x, y->ratio.den);
			y = y->ratio.num;
			return(number_compare(x, y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			dx = (double)(ix);
			dy = (double)(ecl_short_float(y));
			goto DOUBLEFLOAT;
#endif
		case t_singlefloat:
			dx = (double)(ix);
			dy = (double)(sf(y));
			goto DOUBLEFLOAT;
		case t_doublefloat:
			dx = (double)(ix);
			dy = df(y);
			goto DOUBLEFLOAT;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			ldx = (long double)(ix);
			ldy = ecl_long_float(y);
			goto LONGFLOAT;
#endif
		default:
			FEtype_error_real(y);
		}
	case t_bignum:
		switch (ty) {
		case t_fixnum:
			return big_sign(x) < 0 ? -1 : 1;
		case t_bignum:
			return(big_compare(x, y));
		case t_ratio:
			x = number_times(x, y->ratio.den);
			y = y->ratio.num;
			return(number_compare(x, y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
#endif
		case t_singlefloat:
		case t_doublefloat:
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
#endif
			y = cl_rational(y);
			goto BEGIN;
		default:
			FEtype_error_real(y);
		}
	case t_ratio:
		switch (ty) {
		case t_fixnum:
		case t_bignum:
			y = number_times(y, x->ratio.den);
			x = x->ratio.num;
			return(number_compare(x, y));
		case t_ratio:
			return(number_compare(number_times(x->ratio.num,
							   y->ratio.den),
					      number_times(y->ratio.num,
							   x->ratio.den)));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
#endif
		case t_singlefloat:
		case t_doublefloat:
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
#endif
			y = cl_rational(y);
			goto BEGIN;
		default:
			FEtype_error_real(y);
		}
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		dx = (double)(ecl_short_float(x));
		goto DOUBLEFLOAT0;
#endif
	case t_singlefloat:
		dx = (double)(sf(x));
		goto DOUBLEFLOAT0;
	case t_doublefloat:
		dx = df(x);
	DOUBLEFLOAT0:
		switch (ty) {
		case t_fixnum:
			dy = (double)(fix(y));
			break;
		case t_bignum:
		case t_ratio:
			x = cl_rational(x);
			goto BEGIN;
		case t_singlefloat:
			dy = (double)(sf(y));
			break;
		case t_doublefloat:
			dy = df(y);
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			ldx = dx;
			ldy = ecl_long_float(y);
			goto LONGFLOAT;
#endif
		default:
			FEtype_error_real(y);
		}
	DOUBLEFLOAT:
		if (dx == dy)
			return(0);
		else if (dx < dy)
			return(-1);
		else
			return(1);
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		ldx = ecl_long_float(x);
		switch (ty) {
		case t_fixnum:
			ldy = (long double)fix(y);
			break;
		case t_bignum:
		case t_ratio:
			x = cl_rational(x);
			goto BEGIN;
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			ldy = ecl_short_float(y);
			break;
#endif
		case t_singlefloat:
			ldy = sf(y);
			break;
		case t_doublefloat:
			ldy = df(y);
			break;
		case t_longfloat:
			ldy = ecl_long_float(y);
			break;
		default:
			FEtype_error_real(y);
		}
	LONGFLOAT:
		if (ldx == ldy)
			return 0;
		else if (ldx < ldy)
			return -1;
		else
			return 1;
		break;
#endif
	default:
		FEtype_error_real(x);
	}
}

@(defun /= (&rest nums &aux numi)
	int i, j;
@
	if (narg == 0)
		FEwrong_num_arguments_anonym();
	numi = cl_va_arg(nums);
	for (i = 2; i<=narg; i++) {
		cl_va_list numb;
		cl_va_start(numb, narg, narg, 0);
		numi = cl_va_arg(nums);
		for (j = 1; j<i; j++)
			if (number_equalp(numi, cl_va_arg(numb)))
				@(return Cnil)
	}
	@(return Ct)
@)

static cl_object
monotonic(int s, int t, int narg, cl_va_list nums)
{
	cl_object c, d;

	if (narg == 0)
		FEwrong_num_arguments_anonym();
	/* INV: type check occurs in number_compare() */
	for (c = cl_va_arg(nums); --narg; c = d) {
		d = cl_va_arg(nums);
		if (s*number_compare(d, c) < t)
			return1(Cnil);
	}
	return1(Ct);
}

#define MONOTONIC(i, j) (cl_narg narg, ...) \
{ cl_va_list nums; cl_va_start(nums, narg, narg, 0); \
  return monotonic(i, j, narg, nums); }

cl_object @<= MONOTONIC( 1, 0)
cl_object @>= MONOTONIC(-1, 0)
cl_object @<  MONOTONIC( 1, 1)
cl_object @>  MONOTONIC(-1, 1)

@(defun max (max &rest nums)
@
	/* INV: type check occurs in number_compare() for the rest of
	   numbers, but for the first argument it happens in number_zerop(). */
	if (narg-- == 1) {
		number_zerop(max);
	} else do {
		cl_object numi = cl_va_arg(nums);
		if (number_compare(max, numi) < 0)
			max = numi;
	} while (--narg);
	@(return max)
@)

@(defun min (min &rest nums)
@
	/* INV: type check occurs in number_compare() for the rest of
	   numbers, but for the first argument it happens in number_zerop(). */
	if (narg-- == 1) {
		number_zerop(min);
	} else do {
		cl_object numi = cl_va_arg(nums);
		if (number_compare(min, numi) > 0)
			min = numi;
	} while (--narg);
	@(return min)
@)
