/*
    num_arith.c  -- Arithmetic operations
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecl.h"
#include <stdlib.h>

/*  (*		)  */

@(defun * (&rest nums)
	cl_object prod = MAKE_FIXNUM(1);
@
	/* INV: type check in number_times() */
	while (narg--)
		prod = number_times(prod, cl_va_arg(nums));
	@(return prod)
@)

cl_object
fixnum_times(cl_fixnum i, cl_fixnum j)
{
	cl_object x = big_register0_get();

	mpz_set_si(x->big.big_num, i);
	mpz_mul_si(x->big.big_num, x->big.big_num, (long int)j);
	return big_register_normalize(x);
}

static cl_object
big_times_fix(cl_object b, cl_fixnum i)
{
	cl_object z;

	if (i == 1)
		return(b);
	if (i == -1)
		return(big_minus(b));
	z = big_register0_get();
	mpz_mul_si(z->big.big_num, b->big.big_num, (long int)i);
	z = big_register_normalize(z);
	return(z);
}

static cl_object
big_times_big(cl_object x, cl_object y)
{
	cl_object z;
	z = big_register0_get();
	mpz_mul(z->big.big_num, x->big.big_num, y->big.big_num);
	z = big_register_normalize(z);
	return(z);
}

cl_object
number_times(cl_object x, cl_object y)
{
	cl_object z, z1;

	switch (type_of(x)) {
	case t_fixnum:
		switch (type_of(y)) {
		case t_fixnum:
			return fixnum_times(fix(x),fix(y));
		case t_bignum:
			return big_times_fix(y, fix(x));
		case t_ratio:
			z = number_times(x, y->ratio.num);
			z = make_ratio(z, y->ratio.den);
			return(z);
		case t_shortfloat:
			return make_shortfloat(fix(x) * sf(y));
		case t_longfloat:
			return make_longfloat(fix(x) * lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
			return big_times_fix(x, fix(y));
		case t_bignum:
			return big_times_big(x, y);
		case t_ratio:
			z = number_times(x, y->ratio.num);
			z = make_ratio(z, y->ratio.den);
			return(z);
		case t_shortfloat:
			return make_shortfloat(number_to_double(x) * sf(y));
		case t_longfloat:
			return make_longfloat(number_to_double(x) * lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = number_times(x->ratio.num, y);
			z = make_ratio(z, x->ratio.den);
			return(z);
		case t_ratio:
			z = number_times(x->ratio.num,y->ratio.num);
			z1 = number_times(x->ratio.den,y->ratio.den);
			z = make_ratio(z, z1);
			return(z);
		case t_shortfloat:
			return make_shortfloat(number_to_double(x) * sf(y));
		case t_longfloat:
			return make_longfloat(number_to_double(x) * lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_shortfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_shortfloat(fix(y) * sf(x));
		case t_bignum:
		case t_ratio:
			return make_shortfloat(number_to_double(y) * sf(x));
		case t_shortfloat:
			return make_shortfloat(sf(y) * sf(x));
		case t_longfloat:
			return make_longfloat(lf(y) * sf(x));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_longfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_longfloat(fix(y) * lf(x));
		case t_bignum:
		case t_ratio:
			return make_longfloat(number_to_double(y) * lf(x));
		case t_shortfloat:
			return make_longfloat(sf(y) * lf(x));
		case t_longfloat:
			return make_longfloat(lf(y) * lf(x));
		case t_complex: {
		COMPLEX: /* INV: x is real, y is complex */
			return make_complex(number_times(x, y->complex.real),
					    number_times(x, y->complex.imag));
		}
		default:
			FEtype_error_number(y);
		}
	case t_complex:
	{
		cl_object z11, z12, z21, z22;

		if (type_of(y) != t_complex) {
			cl_object aux = x;
			x = y; y = aux;
			goto COMPLEX;
		}
		z11 = number_times(x->complex.real, y->complex.real);
		z12 = number_times(x->complex.imag, y->complex.imag);
		z21 = number_times(x->complex.imag, y->complex.real);
		z22 = number_times(x->complex.real, y->complex.imag);
		return(make_complex(number_minus(z11, z12), number_plus(z21, z22)));
	}
	default:
		FEtype_error_number(x);
	}
}

/* (+          )   */
@(defun + (&rest nums)
	cl_object sum = MAKE_FIXNUM(0);
@
	/* INV: type check is in number_plus() */
	while (narg--)
		sum = number_plus(sum, cl_va_arg(nums));
	@(return sum)
@)

cl_object
number_plus(cl_object x, cl_object y)
{
	cl_fixnum i, j;
	cl_object z, z1;
	
	switch (type_of(x)) {
	case t_fixnum:
	        switch (type_of(y)) {
		case t_fixnum: {
			cl_fixnum k = fix(x) + fix(y);
			if (k >= MOST_NEGATIVE_FIXNUM && k <= MOST_POSITIVE_FIXNUM)
			  return(MAKE_FIXNUM(k));
			else
			  return(bignum1(k));
		}
		case t_bignum:
			if ((i = fix(x)) == 0)
				return(y);
			z = big_register0_get();
			if (i > 0)
				mpz_add_ui(z->big.big_num, y->big.big_num, (unsigned long)i);
			else
				mpz_sub_ui(z->big.big_num, y->big.big_num, (unsigned long)(-i));
		  	z = big_register_normalize(z);
			return(z);
		case t_ratio:
			z = number_times(x, y->ratio.den);
			z = number_plus(z, y->ratio.num);
			z = make_ratio(z, y->ratio.den);
			return(z);
		case t_shortfloat:
			return make_shortfloat(fix(x) + sf(y));
		case t_longfloat:
			return make_longfloat(fix(x) + lf(y));
		case t_complex:
		COMPLEX: /* INV: x is real, y is complex */
			return make_complex(number_plus(x, y->complex.real),
					    y->complex.imag);
		default:
			FEtype_error_number(y);
		}
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
			if ((j = fix(y)) == 0)
				return(x);
			z = big_register0_get();
			if (j > 0)
				mpz_add_ui(z->big.big_num, x->big.big_num, (unsigned long)j);
			else
				mpz_sub_ui(z->big.big_num, x->big.big_num, (unsigned long)(-j));
			z = big_register_normalize(z);
			return(z);
		case t_bignum:
			z = big_plus(x, y);
			z = big_normalize(z);
			return(z);
		case t_ratio:
			z = number_times(x, y->ratio.den);
			z = number_plus(z, y->ratio.num);
			z = make_ratio(z, y->ratio.den);
			return(z);
		case t_shortfloat:
			return make_shortfloat(number_to_double(x) + sf(y));
		case t_longfloat:
			return make_longfloat(number_to_double(x) + lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = number_times(x->ratio.den, y);
			z = number_plus(x->ratio.num, z);
			z = make_ratio(z, x->ratio.den);
			return(z);
		case t_ratio:
			z1 = number_times(x->ratio.num,y->ratio.den);
			z = number_times(x->ratio.den,y->ratio.num);
			z = number_plus(z1, z);
			z1 = number_times(x->ratio.den,y->ratio.den);
			z = make_ratio(z, z1);
			return(z);
		case t_shortfloat:
			return make_shortfloat(number_to_double(x) + sf(y));
		case t_longfloat:
			return make_longfloat(number_to_double(x) + lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_shortfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_shortfloat(fix(y) + sf(x));
		case t_bignum:
		case t_ratio:
			return make_shortfloat(number_to_double(y) + sf(x));
		case t_shortfloat:
			return make_shortfloat(sf(y) + sf(x));
		case t_longfloat:
			return make_longfloat(lf(y) + sf(x));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_longfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_longfloat(fix(y) + lf(x));
		case t_bignum:
		case t_ratio:
			return make_longfloat(number_to_double(y) + lf(x));
		case t_shortfloat:
			return make_longfloat(sf(y) + lf(x));
		case t_longfloat:
			return make_longfloat(lf(y) + lf(x));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_complex:
		if (type_of(y) != t_complex) {
			cl_object aux = x;
			x = y; y = aux;
			goto COMPLEX;
		}
		z = number_plus(x->complex.real, y->complex.real);
		z1 = number_plus(x->complex.imag, y->complex.imag);
		z = make_complex(z, z1);
		return(z);
	default:
		FEtype_error_number(x);
	}
}

/*  (-		)  */
@(defun - (num &rest nums)
	cl_object diff;
@
	/* INV: argument type check in number_{negate,minus}() */
	if (narg == 1)
		@(return number_negate(num))
	for (diff = num;  --narg; )
		diff = number_minus(diff, cl_va_arg(nums));
	@(return diff)
@)

cl_object
number_minus(cl_object x, cl_object y)
{
	cl_fixnum i, j, k;
	cl_object z, z1;
	
	switch (type_of(x)) {
	case t_fixnum:
		switch(type_of(y)) {
		case t_fixnum:
			if ((k = fix(x) - fix(y)) >= MOST_NEGATIVE_FIXNUM &&
			    k <= MOST_POSITIVE_FIXNUM)
			  return(MAKE_FIXNUM(k));
			else
			  return(bignum1(k));
		case t_bignum:
			z = big_register0_get();
			i = fix(x);
			if (i > 0)
				mpz_sub_ui(z->big.big_num, y->big.big_num, (unsigned long)i);
			else
				mpz_add_ui(z->big.big_num, y->big.big_num, (unsigned long)(-i));
			big_complement(z);
			z = big_register_normalize(z);
			return(z);
		case t_ratio:
			z = number_times(x, y->ratio.den);
			z = number_minus(z, y->ratio.num);
			z = make_ratio(z, y->ratio.den);
			return(z);
		case t_shortfloat:
			return make_shortfloat(fix(x) - sf(y));
		case t_longfloat:
			return make_longfloat(fix(x) - lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
			if ((j = fix(y)) == 0)
				return(x);
			z = big_register0_get();
			if (j > 0)
				mpz_sub_ui(z->big.big_num, x->big.big_num, (unsigned long)j);
			else
				mpz_add_ui(z->big.big_num, x->big.big_num, (unsigned long)(-j));
			z = big_register_normalize(z);
			return(z);
		case t_bignum:
			y = big_minus(y);
			z = big_plus(x, y);
			z = big_normalize(z);
			return(z);
		case t_ratio:
			z = number_times(x, y->ratio.den);
			z = number_minus(z, y->ratio.num);
			z = make_ratio(z, y->ratio.den);
			return(z);
		case t_shortfloat:
			return make_shortfloat(number_to_double(x) - sf(y));
		case t_longfloat:
			return make_longfloat(number_to_double(x) - lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = number_times(x->ratio.den, y);
			z = number_minus(x->ratio.num, z);
			z = make_ratio(z, x->ratio.den);
			return(z);
		case t_ratio:
			z = number_times(x->ratio.num,y->ratio.den);
			z1 = number_times(x->ratio.den,y->ratio.num);
			z = number_minus(z, z1);
			z1 = number_times(x->ratio.den,y->ratio.den);
			z = make_ratio(z, z1);
			return(z);
		case t_shortfloat:
			return make_shortfloat(number_to_double(x) - sf(y));
		case t_longfloat:
			return make_longfloat(number_to_double(x) - lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_shortfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_shortfloat(sf(x) - fix(y));
		case t_bignum:
		case t_ratio:
			return make_shortfloat(sf(x) - number_to_double(y));
		case t_shortfloat:
			return make_shortfloat(sf(x) - sf(y));
		case t_longfloat:
			return make_longfloat(sf(x) - lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_longfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_longfloat(lf(x) - fix(y));
		case t_bignum:
		case t_ratio:
			return make_longfloat(lf(x) - number_to_double(y));
		case t_shortfloat:
			return make_longfloat(lf(x) - sf(y));
		case t_longfloat:
			return make_longfloat(lf(x) - lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	COMPLEX:
		return make_complex(number_minus(x, y->complex.real),
				    number_negate(y->complex.imag));
	case t_complex:
		if (type_of(y) != t_complex) {
			z = number_minus(x->complex.real, y);
			z1 = x->complex.imag;
		} else {
			z = number_minus(x->complex.real, y->complex.real);
			z1 = number_minus(x->complex.imag, y->complex.imag);
		}
		return make_complex(z, z1);
	default:
		FEtype_error_number(x);
	}
}

cl_object
cl_conjugate(cl_object c)
{
	switch (type_of(c)) {
	case t_complex:
		c = make_complex(c->complex.real,
				 number_negate(c->complex.imag));
	case t_fixnum:
	case t_bignum:
	case t_ratio:
	case t_shortfloat:
	case t_longfloat:
		break;
	default:
		FEtype_error_number(c);
	}
	@(return c)
}

cl_object
number_negate(cl_object x)
{
	cl_object z, z1;

	switch (type_of(x)) {
	case t_fixnum: {
		cl_fixnum k = fix(x);
		if (-MOST_NEGATIVE_FIXNUM > MOST_POSITIVE_FIXNUM) {
		    if (k == MOST_NEGATIVE_FIXNUM)
			return(bignum1(- MOST_NEGATIVE_FIXNUM));
		    else
			return(MAKE_FIXNUM(-k));
		} else {
		    return MAKE_FIXNUM(-k);
		}
	}
	case t_bignum:
		z = big_register0_get();
		mpz_neg(z->big.big_num, x->big.big_num);
		return big_register_normalize(z);

	case t_ratio:
		z1 = number_negate(x->ratio.num);
		z = cl_alloc_object(t_ratio);
		z->ratio.num = z1;
		z->ratio.den = x->ratio.den;
		return(z);

	case t_shortfloat:
		z = cl_alloc_object(t_shortfloat);
		sf(z) = -sf(x);
		return(z);

	case t_longfloat:
		z = cl_alloc_object(t_longfloat);
		lf(z) = -lf(x);
		return(z);

	case t_complex:
		z = number_negate(x->complex.real);
		z1 = number_negate(x->complex.imag);
		z = make_complex(z, z1);
		return(z);

	default:
		FEtype_error_number(x);
	}
}

/*  (/		)  */
@(defun / (num &rest nums)
@
	/* INV: type check is in number_divide() */
	if (narg == 0)
		FEwrong_num_arguments(@'/');
	if (narg == 1)
		@(return number_divide(MAKE_FIXNUM(1), num))
	while (--narg)
		num = number_divide(num, cl_va_arg(nums));
	@(return num)
@)

cl_object
number_divide(cl_object x, cl_object y)
{
	cl_object z, z1, z2;

	if (number_zerop(y))
		FEdivision_by_zero(x, y);
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			if (number_minusp(y) == TRUE) {
				x = number_negate(x);
				y = number_negate(y);
			}
			z = make_ratio(x, y);
			return(z);
		case t_ratio:
			z = number_times(x, y->ratio.den);
			z = make_ratio(z, y->ratio.num);
			return(z);
		case t_shortfloat:
			return make_shortfloat(number_to_double(x) / sf(y));
		case t_longfloat:
			return make_longfloat(number_to_double(x) / lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = number_times(x->ratio.den, y);
			z = make_ratio(x->ratio.num, z);
			return(z);
		case t_ratio:
			z = number_times(x->ratio.num,y->ratio.den);
			z1 = number_times(x->ratio.den,y->ratio.num);
			z = make_ratio(z, z1);
			return(z);
		case t_shortfloat:
			return make_shortfloat(number_to_double(x) / sf(y));
		case t_longfloat:
			return make_longfloat(number_to_double(x) / lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_shortfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_shortfloat(sf(x) / fix(y));
		case t_bignum:
		case t_ratio:
			return make_shortfloat(sf(x) / number_to_double(y));
		case t_shortfloat:
			return make_shortfloat(sf(x) / sf(y));
		case t_longfloat:
			return make_longfloat(sf(x) / lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_longfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_longfloat(lf(x) / fix(y));
		case t_bignum:
		case t_ratio:
			return make_longfloat(lf(x) / number_to_double(y));
		case t_shortfloat:
			return make_longfloat(lf(x) / sf(y));
		case t_longfloat:
			return make_longfloat(lf(x) / lf(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_complex:
		if (type_of(y) != t_complex) {
			z1 = number_divide(x->complex.real, y);
			z2 = number_divide(x->complex.imag, y);
			return make_complex(z1, z2);
		} else if (1) {
			/* #C(z1 z2) = #C(xr xi) * #C(yr -yi) */
			z1 = number_plus(number_times(x->complex.real, y->complex.real),
					 number_times(x->complex.imag, y->complex.imag));
			z2 = number_minus(number_times(x->complex.imag, y->complex.real),
					  number_times(x->complex.real, y->complex.imag));
		} else {
		COMPLEX: /* INV: x is real, y is complex */
			/* #C(z1 z2) = x * #C(yr -yi) */
			z1 = number_times(x, y->complex.real);
			z2 = number_negate(number_times(x, y->complex.imag));
		}
		z  = number_plus(number_times(y->complex.real, y->complex.real),
				 number_times(y->complex.imag, y->complex.imag));
		z  = make_complex(number_divide(z1, z), number_divide(z2, z));
		return(z);
	default:
		FEtype_error_number(x);
	}
}

cl_object
integer_divide(cl_object x, cl_object y)
{
	cl_type tx, ty;

	tx = type_of(x);
	ty = type_of(y);
	if (tx == t_fixnum) {
 		if (ty == t_fixnum) {
			if (y == MAKE_FIXNUM(0))
				FEdivision_by_zero(x, y);
			return MAKE_FIXNUM(fix(x) / fix(y));
		}
		if (ty == t_bignum) {
			/* The only number "x" which can be a bignum and be
			 * as large as "-x" is -MOST_NEGATIVE_FIXNUM. However
			 * in newer versions of ECL we will probably choose
			 * MOST_NEGATIVE_FIXNUM = - MOST_POSITIVE_FIXNUM.
			 */
			if (-MOST_NEGATIVE_FIXNUM > MOST_POSITIVE_FIXNUM) {
				if (mpz_cmp_si(y->big.big_num, -fix(x)))
					return MAKE_FIXNUM(0);
				else
					return MAKE_FIXNUM(-1);
			} else {
				return MAKE_FIXNUM(0);
			}
		}
		FEtype_error_integer(y);
	}
	if (tx == t_bignum) {
		cl_object q = big_register0_get();
		if (ty == t_bignum) {
			mpz_tdiv_q(q->big.big_num, x->big.big_num, y->big.big_num);
		} else if (ty == t_fixnum) {
			long j = fix(y);
			mpz_tdiv_q_ui(q->big.big_num, x->big.big_num, (unsigned long)labs(j));
			if (j < 0)
				mpz_neg(q->big.big_num, q->big.big_num);
		} else {
			FEtype_error_integer(y);
		}
		return big_register_normalize(q);
	}
	FEtype_error_integer(x);
}

@(defun gcd (&rest nums)
	cl_object gcd;
@
	if (narg == 0)
		@(return MAKE_FIXNUM(0))
	/* INV: get_gcd() checks types */
	gcd = cl_va_arg(nums);
	if (narg == 1) {
		assert_type_integer(gcd);
		@(return (number_minusp(gcd) ? number_negate(gcd) : gcd))
	}
	while (--narg)
		gcd = get_gcd(gcd, cl_va_arg(nums));
	@(return gcd)
@)

cl_object
get_gcd(cl_object x, cl_object y)
{
	cl_type tx = type_of(x);
	cl_type ty = type_of(y);
	cl_object gcd;

	switch (tx) {
	case t_fixnum:
		if (ty == t_fixnum) {
			cl_fixnum i = fix(x);
			cl_fixnum j = fix(y);
			for (i = abs(i), j = abs(j); TRUE; ) {
				cl_fixnum k;
				if (i < j) {
					k = i;
					i = j;
					j = k;
				}
				if (j == 0)
					return(MAKE_FIXNUM(i));
				k = i % j;
				i = j;
				j = k;
			}
		} else {
			x = bignum1(fix(x));
		}
		break;
	case t_bignum:
		break;
	default:
		FEtype_error_integer(x);
	}
	switch (ty) {
	case t_fixnum:
		y = bignum1(fix(y));
	case t_bignum:
		gcd = big_register0_get();
		mpz_gcd(gcd->big.big_num, x->big.big_num, y->big.big_num);
		gcd = big_register_normalize(gcd);
		return(gcd);
	default:
		FEtype_error_integer(y);
	}
}

/*  (1+ x)  */
cl_object
@1+(cl_object x)
{
	/* INV: type check is in one_plus() */
	@(return one_plus(x))
}


cl_object
one_plus(cl_object x)
{
	cl_object z;
	
	switch (type_of(x)) {

	case t_fixnum:
 		if (x == MAKE_FIXNUM(MOST_POSITIVE_FIXNUM))
		  return(bignum1(MOST_POSITIVE_FIXNUM+1));
		return (cl_object)((cl_fixnum)x + ((cl_fixnum)MAKE_FIXNUM(1) - FIXNUM_TAG));
	case t_bignum:
		return(number_plus(x, MAKE_FIXNUM(1)));

	case t_ratio:
		z = number_plus(x->ratio.num, x->ratio.den);
		z = make_ratio(z, x->ratio.den);
		return(z);

	case t_shortfloat:
		z = cl_alloc_object(t_shortfloat);
		sf(z) = sf(x) + 1.0;
		return(z);

	case t_longfloat:
		z = cl_alloc_object(t_longfloat);
		lf(z) = lf(x) + 1.0;
		return(z);

	case t_complex:
		z = one_plus(x->complex.real);
		z = make_complex(z, x->complex.imag);
		return(z);

	default:
		FEtype_error_number(x);
	}
}

/*  (1-	x)  */
cl_object
@1-(cl_object x)
{	/* INV: type check is in one_minus() */
	@(return one_minus(x))
}

cl_object
one_minus(cl_object x)
{
	cl_object z;
	
	switch (type_of(x)) {

	case t_fixnum:
 		if (x == MAKE_FIXNUM(MOST_NEGATIVE_FIXNUM))
		  return(bignum1(MOST_NEGATIVE_FIXNUM-1));
		return (cl_object)((cl_fixnum)x - ((cl_fixnum)MAKE_FIXNUM(1) - FIXNUM_TAG));

	case t_bignum:
		return(number_minus(x, MAKE_FIXNUM(1)));

	case t_ratio:
		z = number_minus(x->ratio.num, x->ratio.den);
		z = make_ratio(z, x->ratio.den);
		return(z);

	case t_shortfloat:
		z = cl_alloc_object(t_shortfloat);
		sf(z) = sf(x) - 1.0;
		return(z);

	case t_longfloat:
		z = cl_alloc_object(t_longfloat);
		lf(z) = lf(x) - 1.0;
		return(z);

	case t_complex:
		z = one_minus(x->complex.real);
		z = make_complex(z, x->complex.imag);
		return(z);

	default:
		FEtype_error_real(x);
	}
}

@(defun lcm (&rest nums)
	cl_object lcm;
@
	if (narg == 0)
		@(return MAKE_FIXNUM(1))
	/* INV: get_gcd() checks types. By placing `numi' before `lcm' in
	   this call, we make sure that errors point to `numi' */
	lcm = cl_va_arg(nums);
	assert_type_integer(lcm);
	while (narg-- > 1) {
		cl_object numi = cl_va_arg(nums);
		cl_object t = number_times(lcm, numi);
		cl_object g = get_gcd(numi, lcm);
		if (g != MAKE_FIXNUM(0))
			lcm = number_divide(t, g);
	}
	@(return (number_minusp(lcm) ? number_negate(lcm) : lcm))
@)
