/*
    num_comp.c  -- Comparisons on numbers.
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

#include "ecls.h"

/*
   For sake of profiler, put @all-the-same before number_compare,
   so that all calls to number_compare are attributed to function =.
   Similarly for @monotonically-decreasing.
*/


@(defun = (num &rest nums)
	int i;
	cl_object numi;
@
	/* ANSI: Need not signal error for 1 argument */
	/* INV: For >= 2 arguments, number_equalp() performs checks */
	for (i = 1; i < narg; i++) {
		numi = va_arg(nums, cl_object);
		if (!number_equalp(num, numi))
			@(return Cnil)
	}
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
	switch (type_of(x)) {
	case t_fixnum:
		switch (type_of(y)) {
		case t_fixnum:
		  	return x == y;
		case t_bignum:
		case t_ratio:
			return 0;
		case t_shortfloat:
			return fix(x) == sf(y);
		case t_longfloat:
			return fix(x) == lf(y);
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
		case t_shortfloat:
			return sf(y) == number_to_double(x);
		case t_longfloat:
			return lf(y) == number_to_double(x);
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
				number_equalp(x->ratio.den, x->ratio.den));
		case t_shortfloat:
			return sf(y) == number_to_double(x);
		case t_longfloat:
			return lf(y) == number_to_double(x);
		case t_complex:
			goto Y_COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_shortfloat:
		dx = sf(x);
		goto FLOAT;
	case t_longfloat:
		dx = lf(x);
	FLOAT:
		switch (type_of(y)) {
		case t_fixnum:
			return dx == fix(y);
		case t_bignum:
		case t_ratio:
			return dx == number_to_double(y);
		case t_shortfloat:
			return dx == sf(y);
		case t_longfloat:
			return dx == lf(y);
		case t_complex:
			goto Y_COMPLEX;
		default:
			FEtype_error_number(y);
		}
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
	int ix, iy;
	double dx, dy;

	switch (type_of(x)) {
	case t_fixnum:
	  	ix = fix(x);
		switch (type_of(y)) {
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
		case t_shortfloat:
			dx = (double)(ix);
			dy = (double)(sf(y));
			goto LONGFLOAT;
		case t_longfloat:
			dx = (double)(ix);
			dy = lf(y);
			goto LONGFLOAT;
		default:
			FEtype_error_real(y);
		}
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
			return big_sign(x) < 0 ? -1 : 1;
		case t_bignum:
			return(big_compare(x, y));
		case t_ratio:
			x = number_times(x, y->ratio.den);
			y = y->ratio.num;
			return(number_compare(x, y));
		case t_shortfloat:
			dx = number_to_double(x);
			dy = (double)(sf(y));
			goto LONGFLOAT;
		case t_longfloat:
			dx = number_to_double(x);
			dy = lf(y);
			goto LONGFLOAT;
		default:
			FEtype_error_real(y);
		}
	case t_ratio:
		switch (type_of(y)) {
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
		case t_shortfloat:
			dx = number_to_double(x);
			dy = (double)(sf(y));
			goto LONGFLOAT;
		case t_longfloat:
			dx = number_to_double(x);
			dy = lf(y);
			goto LONGFLOAT;
		default:
			FEtype_error_real(y);
		}
	case t_shortfloat:
		dx = (double)(sf(x));
		goto LONGFLOAT0;
	case t_longfloat:
		dx = lf(x);
	LONGFLOAT0:
		switch (type_of(y)) {
		case t_fixnum:
			dy = (double)(fix(y));
			break;
		case t_bignum:
		case t_ratio:
			dy = number_to_double(y);
			break;
		case t_shortfloat:
			dy = (double)(sf(y));
			break;
		case t_longfloat:
			dy = lf(y);
			break;
		default:
			FEtype_error_real(y);
		}
	LONGFLOAT:
		if (dx == dy)
			return(0);
		else if (dx < dy)
			return(-1);
		else
			return(1);
	default:
		FEtype_error_real(x);
	}
}

@(defun /= (&rest nums)
	int i, j;
	va_list numb;
@
	if (narg == 0)
		FEtoo_few_arguments(&narg);
	if (narg == 1)
		@(return Ct)
	for (i = 0; i < narg; i++) {
	  cl_object numi = va_arg(nums, cl_object);
	  va_start(numb, narg);
	  for (j = 0; j < i; j++)
	    if (number_equalp(numi, va_arg(numb, cl_object)))
	      @(return Cnil)
	}
	@(return Ct)
@)

#define MONOTONIC(i, j) (int narg, ...) \
{ va_list nums; va_start(nums, narg); \
  return monotonic(i, j, narg, (cl_object *)nums); }

cl_object @<= MONOTONIC( 1, 0)
cl_object @>= MONOTONIC(-1, 0)
cl_object @<  MONOTONIC( 1, 1)
cl_object @>  MONOTONIC(-1, 1)

cl_object
monotonic(int s, int t, int narg, cl_object *nums)
{
	int i;

	if (narg == 0)
		FEtoo_few_arguments(&narg);
	/* INV: type check occurs in number_compare() */
	for (i = 1; i < narg; i++)
		if (s*number_compare(nums[i], nums[i-1]) < t)
			return1(Cnil);
	return1(Ct);
}

@(defun max (max &rest nums)
	cl_object numi;
	int i;
@
	/* INV: type check occurs in number_compare() */
	for (i = 1;  i < narg;  i++) {
	  numi = va_arg(nums, cl_object);
	  if (number_compare(max, numi) < 0)
	    max = numi;
	}
	@(return max)
@)

@(defun min (min &rest nums)
	cl_object numi;
	int i;
@	
	/* INV: type check occurs in number_compare() */
	va_start(nums, min);
	for (i = 1;  i < narg;  i++) {
	  numi = va_arg(nums, cl_object);
	  if (number_compare(min, numi) > 0)
			min = numi;
	}
	@(return min)
@)
