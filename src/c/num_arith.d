/* -*- mode: c; c-basic-offset: 8 -*- */
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

#include <ecl/ecl.h>
#include <stdlib.h>

/*  (*		)  */

@(defun * (&rest nums)
	cl_object prod = MAKE_FIXNUM(1);
@
	/* INV: type check in ecl_times() */
	while (narg--)
		prod = ecl_times(prod, cl_va_arg(nums));
	@(return prod)
@)

cl_object
fixnum_times(cl_fixnum i, cl_fixnum j)
{
	cl_object x = big_register0_get();

#ifdef WITH_GMP
	mpz_set_si(x->big.big_num, i);
	mpz_mul_si(x->big.big_num, x->big.big_num, (long int)j);
#else  /* WITH_GMP */
        x->big.big_num = (big_num_t)i * (big_num_t)j;
#endif /* WITH_GMP */
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
#ifdef WITH_GMP
	mpz_mul_si(z->big.big_num, b->big.big_num, (long int)i);
#else  /* WITH_GMP */
        z->big.big_num = b->big.big_num * i;
#endif /* WITH_GMP */
	z = big_register_normalize(z);
	return(z);
}

static cl_object
big_times_big(cl_object x, cl_object y)
{
	cl_object z;
	z = big_register0_get();
#ifdef WITH_GMP
	mpz_mul(z->big.big_num, x->big.big_num, y->big.big_num);
#else  /* WITH_GMP */
        z->big.big_num = x->big.big_num * y->big.big_num;
#endif /* WITH_GMP */
	z = big_register_normalize(z);
	return(z);
}

cl_object
ecl_times(cl_object x, cl_object y)
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
			z = ecl_times(x, y->ratio.num);
			z = ecl_make_ratio(z, y->ratio.den);
			return(z);
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(fix(x) * ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(fix(x) * sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(fix(x) * df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(fix(x) * ecl_long_float(y));
#endif
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
			z = ecl_times(x, y->ratio.num);
			z = ecl_make_ratio(z, y->ratio.den);
			return(z);
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(ecl_to_double(x) * ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) * sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) * df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(ecl_to_double(x) * ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = ecl_times(x->ratio.num, y);
			z = ecl_make_ratio(z, x->ratio.den);
			return(z);
		case t_ratio:
			z = ecl_times(x->ratio.num,y->ratio.num);
			z1 = ecl_times(x->ratio.den,y->ratio.den);
			z = ecl_make_ratio(z, z1);
			return(z);
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(ecl_to_double(x) * ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) * sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) * df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(ecl_to_double(x) * ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat: {
		float fx = ecl_short_float(x);
		switch (type_of(y)) {
		case t_fixnum:
			return make_shortfloat(fx * fix(y));
		case t_bignum:
		case t_ratio:
			return make_shortfloat(fx * ecl_to_double(y));
		case t_shortfloat:
			return make_shortfloat(fx * ecl_short_float(y));
		case t_singlefloat:
			return make_shortfloat(fx * sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(fx * sf(x));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(fx * ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	}
#endif
	case t_singlefloat: {
		float fx = sf(x);
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_singlefloat(fx * fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_singlefloat(fx * ecl_to_double(y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return ecl_make_singlefloat(fx * ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(fx * sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(fx * df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(fx * ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	}
	case t_doublefloat: {
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_doublefloat(df(x) * fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_doublefloat(df(x) * ecl_to_double(y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return ecl_make_doublefloat(df(x) * ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_doublefloat(df(x) * sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(df(x) * df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(df(x) * ecl_long_float(y));
#endif
		case t_complex: {
		COMPLEX: /* INV: x is real, y is complex */
			return ecl_make_complex(ecl_times(x, y->complex.real),
					    ecl_times(x, y->complex.imag));
		}
		default:
			FEtype_error_number(y);
		}
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double lx = ecl_long_float(x);
		switch (type_of(y)) {
		case t_fixnum:
			return make_longfloat(lx * fix(y));
		case t_bignum:
		case t_ratio:
			return make_longfloat(lx * ecl_to_double(y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_longfloat(lx * ecl_short_float(y));
#endif
		case t_singlefloat:
			return make_longfloat(lx * sf(y));
		case t_doublefloat:
			return make_longfloat(lx * df(y));
		case t_longfloat:
			return make_longfloat(lx * ecl_long_float(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	}
#endif
	case t_complex:
	{
		cl_object z11, z12, z21, z22;

		if (type_of(y) != t_complex) {
			cl_object aux = x;
			x = y; y = aux;
			goto COMPLEX;
		}
		z11 = ecl_times(x->complex.real, y->complex.real);
		z12 = ecl_times(x->complex.imag, y->complex.imag);
		z21 = ecl_times(x->complex.imag, y->complex.real);
		z22 = ecl_times(x->complex.real, y->complex.imag);
		return(ecl_make_complex(ecl_minus(z11, z12), ecl_plus(z21, z22)));
	}
	default:
		FEtype_error_number(x);
	}
}

/* (+          )   */
@(defun + (&rest nums)
	cl_object sum = MAKE_FIXNUM(0);
@
	/* INV: type check is in ecl_plus() */
	while (narg--)
		sum = ecl_plus(sum, cl_va_arg(nums));
	@(return sum)
@)

cl_object
ecl_plus(cl_object x, cl_object y)
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
#ifdef WITH_GMP
			if (i > 0)
				mpz_add_ui(z->big.big_num, y->big.big_num, (unsigned long)i);
			else
				mpz_sub_ui(z->big.big_num, y->big.big_num, (unsigned long)(-i));
#else  /* WITH_GMP */
                        z->big.big_num = y->big.big_num + i;
#endif /* WITH_GMP */
		  	z = big_register_normalize(z);
			return(z);
		case t_ratio:
			z = ecl_times(x, y->ratio.den);
			z = ecl_plus(z, y->ratio.num);
			z = ecl_make_ratio(z, y->ratio.den);
			return(z);
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(fix(x) + ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(fix(x) + sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(fix(x) + df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(fix(x) + ecl_long_float(y));
#endif
		case t_complex:
		COMPLEX: /* INV: x is real, y is complex */
			return ecl_make_complex(ecl_plus(x, y->complex.real),
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
#ifdef WITH_GMP
			if (j > 0)
				mpz_add_ui(z->big.big_num, x->big.big_num, (unsigned long)j);
			else
				mpz_sub_ui(z->big.big_num, x->big.big_num, (unsigned long)(-j));
#else  /* WITH_GMP */
                        z->big.big_num = x->big.big_num + j;
#endif /* WITH_GMP */
			z = big_register_normalize(z);
			return(z);
		case t_bignum:
			z = big_plus(x, y);
			z = big_normalize(z);
			return(z);
		case t_ratio:
			z = ecl_times(x, y->ratio.den);
			z = ecl_plus(z, y->ratio.num);
			z = ecl_make_ratio(z, y->ratio.den);
			return(z);
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(ecl_to_double(x) + ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) + sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) + df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(ecl_to_double(x) + ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = ecl_times(x->ratio.den, y);
			z = ecl_plus(x->ratio.num, z);
			z = ecl_make_ratio(z, x->ratio.den);
			return(z);
		case t_ratio:
			z1 = ecl_times(x->ratio.num,y->ratio.den);
			z = ecl_times(x->ratio.den,y->ratio.num);
			z = ecl_plus(z1, z);
			z1 = ecl_times(x->ratio.den,y->ratio.den);
			z = ecl_make_ratio(z, z1);
			return(z);
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(ecl_to_double(x) + ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) + sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) + df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(ecl_to_double(x) + ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_shortfloat(ecl_short_float(x) + fix(y));
		case t_bignum:
		case t_ratio:
			return make_shortfloat(ecl_short_float(x) + ecl_to_double(y));
		case t_shortfloat:
			return make_shortfloat(ecl_short_float(x) + ecl_short_float(y));
		case t_singlefloat:
			return make_shortfloat(ecl_short_float(x) + sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_short_float(x) + df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(ecl_short_float(x) + ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#endif
	case t_singlefloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_singlefloat(sf(x) + fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_singlefloat(sf(x) + ecl_to_double(y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(sf(x) + ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(sf(x) + sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(sf(x) + df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(sf(x) + ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_doublefloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_doublefloat(df(x) + fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_doublefloat(df(x) + ecl_to_double(y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return ecl_make_doublefloat(df(x) + ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_doublefloat(df(x) + sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(df(x) + df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(df(x) + ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_longfloat(ecl_long_float(x) + fix(y));
		case t_bignum:
		case t_ratio:
			return make_longfloat(ecl_long_float(x) + ecl_to_double(y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_longfloat(ecl_long_float(x) + ecl_short_float(y));
#endif
		case t_singlefloat:
			return make_longfloat(ecl_long_float(x) + sf(y));
		case t_doublefloat:
			return make_longfloat(ecl_long_float(x) + df(y));
		case t_longfloat:
			return make_longfloat(ecl_long_float(x) + ecl_long_float(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#endif
	case t_complex:
		if (type_of(y) != t_complex) {
			cl_object aux = x;
			x = y; y = aux;
			goto COMPLEX;
		}
		z = ecl_plus(x->complex.real, y->complex.real);
		z1 = ecl_plus(x->complex.imag, y->complex.imag);
		z = ecl_make_complex(z, z1);
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
		@(return ecl_negate(num))
	for (diff = num;  --narg; )
		diff = ecl_minus(diff, cl_va_arg(nums));
	@(return diff)
@)

cl_object
ecl_minus(cl_object x, cl_object y)
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
#ifdef WITH_GMP
			if (i > 0)
				mpz_sub_ui(z->big.big_num, y->big.big_num, (unsigned long)i);
			else
				mpz_add_ui(z->big.big_num, y->big.big_num, (unsigned long)(-i));
#else  /* WITH_GMP */
                        z->big.big_num = (big_num_t)i - y->big.big_num;
#endif /* WITH_GMP */
			big_complement(z);
			z = big_register_normalize(z);
			return(z);
		case t_ratio:
			z = ecl_times(x, y->ratio.den);
			z = ecl_minus(z, y->ratio.num);
			z = ecl_make_ratio(z, y->ratio.den);
			return(z);
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(fix(x) - ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(fix(x) - sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(fix(x) - df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(fix(x) - ecl_long_float(y));
#endif
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
#ifdef WITH_GMP
			if (j > 0)
				mpz_sub_ui(z->big.big_num, x->big.big_num, (unsigned long)j);
			else
				mpz_add_ui(z->big.big_num, x->big.big_num, (unsigned long)(-j));
#else  /* WITH_GMP */
                        z->big.big_num = x->big.big_num - j;
#endif /* WITH_GMP */
			z = big_register_normalize(z);
			return(z);
		case t_bignum:
			y = big_minus(y);
			z = big_plus(x, y);
			z = big_normalize(z);
			return(z);
		case t_ratio:
			z = ecl_times(x, y->ratio.den);
			z = ecl_minus(z, y->ratio.num);
			z = ecl_make_ratio(z, y->ratio.den);
			return(z);
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(ecl_to_double(x) - ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) - sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) - df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(ecl_to_double(x) - ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = ecl_times(x->ratio.den, y);
			z = ecl_minus(x->ratio.num, z);
			z = ecl_make_ratio(z, x->ratio.den);
			return(z);
		case t_ratio:
			z = ecl_times(x->ratio.num,y->ratio.den);
			z1 = ecl_times(x->ratio.den,y->ratio.num);
			z = ecl_minus(z, z1);
			z1 = ecl_times(x->ratio.den,y->ratio.den);
			z = ecl_make_ratio(z, z1);
			return(z);
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(ecl_to_double(x) - ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) - sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) - df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(ecl_to_double(x) - ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_shortfloat(ecl_short_float(x) - fix(y));
		case t_bignum:
		case t_ratio:
			return make_shortfloat(ecl_short_float(x) - ecl_to_double(y));
		case t_shortfloat:
			return make_shortfloat(ecl_short_float(x) - ecl_short_float(y));
		case t_singlefloat:
			return make_shortfloat(ecl_short_float(x) - sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_short_float(x) - df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(ecl_short_float(x) - ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#endif
	case t_singlefloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_singlefloat(sf(x) - fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_singlefloat(sf(x) - ecl_to_double(y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(sf(x) - ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(sf(x) - sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(sf(x) - df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(sf(x) - ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_doublefloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_doublefloat(df(x) - fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_doublefloat(df(x) - ecl_to_double(y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return ecl_make_doublefloat(df(x) - ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_doublefloat(df(x) - sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(df(x) - df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(df(x) - ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_longfloat(ecl_long_float(x) - fix(y));
		case t_bignum:
		case t_ratio:
			return make_longfloat(ecl_long_float(x) - ecl_to_double(y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_longfloat(ecl_long_float(x) - ecl_short_float(y));
#endif
		case t_singlefloat:
			return make_longfloat(ecl_long_float(x) - sf(y));
		case t_doublefloat:
			return make_longfloat(ecl_long_float(x) - df(y));
		case t_longfloat:
			return make_longfloat(ecl_long_float(x) - ecl_long_float(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#endif
	COMPLEX:
		return ecl_make_complex(ecl_minus(x, y->complex.real),
				    ecl_negate(y->complex.imag));
	case t_complex:
		if (type_of(y) != t_complex) {
			z = ecl_minus(x->complex.real, y);
			z1 = x->complex.imag;
		} else {
			z = ecl_minus(x->complex.real, y->complex.real);
			z1 = ecl_minus(x->complex.imag, y->complex.imag);
		}
		return ecl_make_complex(z, z1);
	default:
		FEtype_error_number(x);
	}
}

cl_object
cl_conjugate(cl_object c)
{
	switch (type_of(c)) {
	case t_complex:
		c = ecl_make_complex(c->complex.real,
				 ecl_negate(c->complex.imag));
	case t_fixnum:
	case t_bignum:
	case t_ratio:
	case t_singlefloat:
	case t_doublefloat:
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
#endif
		break;
	default:
		FEtype_error_number(c);
	}
	@(return c)
}

cl_object
ecl_negate(cl_object x)
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
#ifdef WITH_GMP
		mpz_neg(z->big.big_num, x->big.big_num);
#else  /* WITH_GMP */
                z->big.big_num = -(x->big.big_num);
#endif /* WITH_GMP */
		return big_register_normalize(z);

	case t_ratio:
		z1 = ecl_negate(x->ratio.num);
		z = cl_alloc_object(t_ratio);
		z->ratio.num = z1;
		z->ratio.den = x->ratio.den;
		return(z);

#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		return make_shortfloat(-ecl_shortfloat(x));
#endif
	case t_singlefloat:
		z = cl_alloc_object(t_singlefloat);
		sf(z) = -sf(x);
		return(z);

	case t_doublefloat:
		z = cl_alloc_object(t_doublefloat);
		df(z) = -df(x);
		return(z);
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return make_longfloat(-ecl_long_float(x));
#endif
	case t_complex:
		z = ecl_negate(x->complex.real);
		z1 = ecl_negate(x->complex.imag);
		z = ecl_make_complex(z, z1);
		return(z);

	default:
		FEtype_error_number(x);
	}
}

/*  (/		)  */
@(defun / (num &rest nums)
@
	/* INV: type check is in ecl_divide() */
	if (narg == 0)
		FEwrong_num_arguments(@'/');
	if (narg == 1)
		@(return ecl_divide(MAKE_FIXNUM(1), num))
	while (--narg)
		num = ecl_divide(num, cl_va_arg(nums));
	@(return num)
@)

cl_object
ecl_divide(cl_object x, cl_object y)
{
	cl_object z, z1, z2;

	if (ecl_zerop(y))
		FEdivision_by_zero(x, y);
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			if (ecl_minusp(y) == TRUE) {
				x = ecl_negate(x);
				y = ecl_negate(y);
			}
			z = ecl_make_ratio(x, y);
			return(z);
		case t_ratio:
			z = ecl_times(x, y->ratio.den);
			z = ecl_make_ratio(z, y->ratio.num);
			return(z);
#ifdef ECL_SHORT_FLAOT
		case t_shortfloat:
			return make_shortfloat(ecl_to_double(x) / ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) / sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) / df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(ecl_to_double(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_ratio:
		switch (type_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = ecl_times(x->ratio.den, y);
			z = ecl_make_ratio(x->ratio.num, z);
			return(z);
		case t_ratio:
			z = ecl_times(x->ratio.num,y->ratio.den);
			z1 = ecl_times(x->ratio.den,y->ratio.num);
			z = ecl_make_ratio(z, z1);
			return(z);
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(ecl_to_double(x) / ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(ecl_to_double(x) / sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_to_double(x) / df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(ecl_to_double(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_shortfloat(ecl_short_float(x) / fix(y));
		case t_bignum:
		case t_ratio:
			return make_shortfloat(ecl_short_float(x) / ecl_to_double(y));
		case t_shortfloat:
			return make_shortfloat(ecl_short_float(x) / ecl_short_float(y));
		case t_singlefloat:
			return make_shortfloat(ecl_short_float(x) / sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(ecl_short_float(x) / df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(ecl_short_float(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#endif
	case t_singlefloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_singlefloat(sf(x) / fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_singlefloat(sf(x) / ecl_to_double(y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_shortfloat(sf(x) / ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_singlefloat(sf(x) / sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(sf(x) / df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(sf(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
	case t_doublefloat:
		switch (type_of(y)) {
		case t_fixnum:
			return ecl_make_doublefloat(df(x) / fix(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_doublefloat(df(x) / ecl_to_double(y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return ecl_make_doublefloat(df(x) / ecl_short_float(y));
#endif
		case t_singlefloat:
			return ecl_make_doublefloat(df(x) / sf(y));
		case t_doublefloat:
			return ecl_make_doublefloat(df(x) / df(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return make_longfloat(df(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		switch (type_of(y)) {
		case t_fixnum:
			return make_longfloat(ecl_long_float(x) / fix(y));
		case t_bignum:
		case t_ratio:
			return make_longfloat(ecl_long_float(x) / ecl_to_double(y));
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			return make_longfloat(ecl_long_float(x) / ecl_short_float(y));
#endif
		case t_singlefloat:
			return make_longfloat(ecl_long_float(x) / sf(y));
		case t_doublefloat:
			return make_longfloat(ecl_long_float(x) / df(y));
		case t_longfloat:
			return make_longfloat(ecl_long_float(x) / ecl_long_float(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEtype_error_number(y);
		}
#endif
	case t_complex:
		if (type_of(y) != t_complex) {
			z1 = ecl_divide(x->complex.real, y);
			z2 = ecl_divide(x->complex.imag, y);
			return ecl_make_complex(z1, z2);
		} else if (1) {
			/* #C(z1 z2) = #C(xr xi) * #C(yr -yi) */
			z1 = ecl_plus(ecl_times(x->complex.real, y->complex.real),
					 ecl_times(x->complex.imag, y->complex.imag));
			z2 = ecl_minus(ecl_times(x->complex.imag, y->complex.real),
					  ecl_times(x->complex.real, y->complex.imag));
		} else {
		COMPLEX: /* INV: x is real, y is complex */
			/* #C(z1 z2) = x * #C(yr -yi) */
			z1 = ecl_times(x, y->complex.real);
			z2 = ecl_negate(ecl_times(x, y->complex.imag));
		}
		z  = ecl_plus(ecl_times(y->complex.real, y->complex.real),
				 ecl_times(y->complex.imag, y->complex.imag));
		z  = ecl_make_complex(ecl_divide(z1, z), ecl_divide(z2, z));
		return(z);
	default:
		FEtype_error_number(x);
	}
}

cl_object
ecl_integer_divide(cl_object x, cl_object y)
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
#ifdef WITH_GMP
				if (mpz_cmp_si(y->big.big_num, -fix(x)))
					return MAKE_FIXNUM(0);
				else
					return MAKE_FIXNUM(-1);
#else  /* WITH_GMP */
                                return y->big.big_num != -fix(x) ? MAKE_FIXNUM(0) : MAKE_FIXNUM(-1);
#endif /* WITH_GMP */
			} else {
				return MAKE_FIXNUM(0);
			}
		}
		FEtype_error_integer(y);
	}
	if (tx == t_bignum) {
		cl_object q = big_register0_get();
		if (ty == t_bignum) {
#ifdef WITH_GMP
			mpz_tdiv_q(q->big.big_num, x->big.big_num, y->big.big_num);
#else  /* WITH_GMP */
                        q->big.big_num = x->big.big_num / y->big.big_num;
#endif /* WITH_GMP */
		} else if (ty == t_fixnum) {
			long j = fix(y);
#ifdef WITH_GMP
			mpz_tdiv_q_ui(q->big.big_num, x->big.big_num, (unsigned long)labs(j));
			if (j < 0)
				mpz_neg(q->big.big_num, q->big.big_num);
#else  /* WITH_GMP */
                        q->big.big_num = x->big.big_num / j;
#endif /* WITH_GMP */
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
	/* INV: ecl_gcd() checks types */
	gcd = cl_va_arg(nums);
	if (narg == 1) {
		assert_type_integer(gcd);
		@(return (ecl_minusp(gcd) ? ecl_negate(gcd) : gcd))
	}
	while (--narg)
		gcd = ecl_gcd(gcd, cl_va_arg(nums));
	@(return gcd)
@)

cl_object
ecl_gcd(cl_object x, cl_object y)
{
	cl_type tx = type_of(x);
	cl_type ty = type_of(y);
	cl_object gcd;

	switch (tx) {
	case t_fixnum:
		if (ty == t_fixnum) {
			cl_fixnum i = fix(x);
			cl_fixnum j = fix(y);
			for (i = labs(i), j = labs(j); TRUE; ) {
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
#ifdef WITH_GMP
                mpz_gcd(gcd->big.big_num, x->big.big_num, y->big.big_num); /* FIXME!!! */
#else  /* WITH_GMP */
                {
                        big_num_t i = x->big.big_num, j = y->big.big_num;
                        while ( 1 ) {
                                big_num_t k;
                                if ( i<j ) {
                                        k = i;
                                        i = j;
                                        j = k;
                                }
                                if ( j == 0 ) {
                                        gcd->big.big_num = k;
                                        break;
                                }
                                k = i % j;
                                i = j;
                                j = k;
                        }
                }
#endif /* WITH_GMP */
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
	/* INV: type check is in ecl_one_plus() */
	@(return ecl_one_plus(x))
}


cl_object
ecl_one_plus(cl_object x)
{
	cl_object z;

	switch (type_of(x)) {

	case t_fixnum:
 		if (x == MAKE_FIXNUM(MOST_POSITIVE_FIXNUM))
		  return(bignum1(MOST_POSITIVE_FIXNUM+1));
		return (cl_object)((cl_fixnum)x + ((cl_fixnum)MAKE_FIXNUM(1) - FIXNUM_TAG));
	case t_bignum:
		return(ecl_plus(x, MAKE_FIXNUM(1)));

	case t_ratio:
		z = ecl_plus(x->ratio.num, x->ratio.den);
		z = ecl_make_ratio(z, x->ratio.den);
		return(z);

#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		return make_shortfloat(1.0 + ecl_short_float(x));
#endif
	case t_singlefloat:
		z = cl_alloc_object(t_singlefloat);
		sf(z) = sf(x) + 1.0;
		return(z);

	case t_doublefloat:
		z = cl_alloc_object(t_doublefloat);
		df(z) = df(x) + 1.0;
		return(z);

#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return make_longfloat(1.0 + ecl_long_float(x));
#endif

	case t_complex:
		z = ecl_one_plus(x->complex.real);
		z = ecl_make_complex(z, x->complex.imag);
		return(z);

	default:
		FEtype_error_number(x);
	}
}

/*  (1-	x)  */
cl_object
@1-(cl_object x)
{	/* INV: type check is in ecl_one_minus() */
	@(return ecl_one_minus(x))
}

cl_object
ecl_one_minus(cl_object x)
{
	cl_object z;
	
	switch (type_of(x)) {

	case t_fixnum:
 		if (x == MAKE_FIXNUM(MOST_NEGATIVE_FIXNUM))
		  return(bignum1(MOST_NEGATIVE_FIXNUM-1));
		return (cl_object)((cl_fixnum)x - ((cl_fixnum)MAKE_FIXNUM(1) - FIXNUM_TAG));

	case t_bignum:
		return(ecl_minus(x, MAKE_FIXNUM(1)));

	case t_ratio:
		z = ecl_minus(x->ratio.num, x->ratio.den);
		z = ecl_make_ratio(z, x->ratio.den);
		return(z);

#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		return make_shortfloat(ecl_short_float(x) - 1.0);
#endif

	case t_singlefloat:
		z = cl_alloc_object(t_singlefloat);
		sf(z) = sf(x) - 1.0;
		return(z);

	case t_doublefloat:
		z = cl_alloc_object(t_doublefloat);
		df(z) = df(x) - 1.0;
		return(z);

#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return make_longfloat(ecl_long_float(x) - 1.0);
#endif

	case t_complex:
		z = ecl_one_minus(x->complex.real);
		z = ecl_make_complex(z, x->complex.imag);
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
	/* INV: ecl_gcd() checks types. By placing `numi' before `lcm' in
	   this call, we make sure that errors point to `numi' */
	lcm = cl_va_arg(nums);
	assert_type_integer(lcm);
	while (narg-- > 1) {
		cl_object numi = cl_va_arg(nums);
		cl_object t = ecl_times(lcm, numi);
		cl_object g = ecl_gcd(numi, lcm);
		if (g != MAKE_FIXNUM(0))
			lcm = ecl_divide(t, g);
	}
	@(return (ecl_minusp(lcm) ? ecl_negate(lcm) : lcm))
@)
