/*
    number.c -- Numeric constants.
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

#include <math.h>
#include "ecl.h"

#ifndef HAVE_ISNANF
#define isnanf(x) isnan(x)
#endif

cl_object shortfloat_zero;
cl_object longfloat_zero;

cl_fixnum
fixint(cl_object x)
{
	if (FIXNUMP(x))
		return fix(x);
	if (type_of(x) == t_bignum) {
		if (x->big.big_size == 1 || x->big.big_size == -1)
			return big_to_long(x);
	}
	FEwrong_type_argument(@'fixnum', x);
}

cl_index
fixnnint(cl_object x)
{
	if (FIXNUMP(x)) {
		cl_fixnum i = fix(x);
		if (i >= 0)
			return i;
	} else if (type_of(x) == t_bignum) {
		if (x->big.big_size == 1)
			return big_to_ulong(x);
	}
	cl_error(9, @'simple-type-error', @':format-control',
		    make_simple_string("Not a non-negative fixnum ~S"),
		    @':format-arguments', cl_list(1,x),
		    @':expected-type', @'fixnum', @':datum', x);
}

cl_object
make_integer(cl_fixnum l)
{
	if (l > MOST_POSITIVE_FIXNUM || l < MOST_NEGATIVE_FIXNUM) {
		cl_object z = cl_alloc_object(t_bignum);
		mpz_init_set_si(z->big.big_num, l);
		return z;
	}
	return MAKE_FIXNUM(l);
}

cl_object
make_unsigned_integer(cl_index l)
{
	if (l > MOST_POSITIVE_FIXNUM) {
		cl_object z = cl_alloc_object(t_bignum);
		mpz_init_set_ui(z->big.big_num, l);
		return z;
	}
	return MAKE_FIXNUM(l);
}

cl_object
make_ratio(cl_object num, cl_object den)
{
	cl_object g, r;

	if (number_zerop(num))
		return(MAKE_FIXNUM(0));
	if (number_zerop(den))
		FEerror("Zero denominator.", 0);
	if (den == MAKE_FIXNUM(1))
		return(num);
	if (number_minusp(den)) {
		num = number_negate(num);
		den = number_negate(den);
	}
	g = get_gcd(num, den);
	num = integer_divide(num, g);
	den = integer_divide(den, g);
	if (den == MAKE_FIXNUM(1))
		return num;
	if (den == MAKE_FIXNUM(-1))
		return number_negate(num);
	r = cl_alloc_object(t_ratio);
	r->ratio.num = num;
	r->ratio.den = den;
	return(r);
}

cl_object
make_shortfloat(float f)
{
	cl_object x;

	if (f == (float)0.0)
		return(shortfloat_zero);
	if (isnanf(f) || !finite(f))
		FEerror("Not a number.",0);
	x = cl_alloc_object(t_shortfloat);
	sf(x) = f;
	return(x);
}

cl_object
make_longfloat(double f)
{
	cl_object x;

	if (f == (double)0.0)
		return(longfloat_zero);
	if (isnan(f) || !finite(f))
		FEerror("Not a number.",0);
	x = cl_alloc_object(t_longfloat);
	lf(x) = f;
	return(x);
}

cl_object
make_complex(cl_object r, cl_object i)
{
	cl_object c;

	/* Both R and I are promoted to a common type */
	switch (type_of(r)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		switch (type_of(i)) {
		case t_fixnum:
			if (i == MAKE_FIXNUM(0))
				return(r);
		case t_bignum:
		case t_ratio:
			break;
		case t_shortfloat:
			r = make_shortfloat((float)number_to_double(r));
			break;
		case t_longfloat:
			r = make_longfloat(number_to_double(r));
			break;
		default:
			FEtype_error_real(i);
		}
		break;
	case t_shortfloat:
		switch (type_of(i)) {
		case t_fixnum:
		case t_bignum:
		case t_ratio:
			i = make_shortfloat((float)number_to_double(i));
		case t_shortfloat:
			break;
		case t_longfloat:
			r = make_longfloat((double)(sf(r)));
			break;
		default:
			FEtype_error_real(i);
		}
		break;
	case t_longfloat:
		switch (type_of(i)) {
		case t_fixnum:
		case t_bignum:
		case t_ratio:
		case t_shortfloat:
			i = make_longfloat(number_to_double(i));
		case t_longfloat:
			break;
		default:
			FEtype_error_real(i);
		}
		break;
	default:
		FEtype_error_real(r);
	}			
	c = cl_alloc_object(t_complex);
	c->complex.real = r;
	c->complex.imag = i;
	return(c);
}

double
number_to_double(cl_object x)
{
	switch(type_of(x)) {
	case t_fixnum:
		return((double)(fix(x)));

	case t_bignum:
		return(big_to_double(x));

	case t_ratio:
		return(number_to_double(x->ratio.num) /
		       number_to_double(x->ratio.den));

	case t_shortfloat:
		return((double)(sf(x)));

	case t_longfloat:
		return(lf(x));

	default:
		FEtype_error_real(x);
	}
}

void
init_number(void)
{
	shortfloat_zero = cl_alloc_object(t_shortfloat);
	sf(shortfloat_zero) = (float)0.0;
	longfloat_zero = cl_alloc_object(t_longfloat);
	lf(longfloat_zero) = (double)0.0;
	ecl_register_static_root(&shortfloat_zero);
	ecl_register_static_root(&longfloat_zero);

  	SYM_VAL(@'most-positive-fixnum') = MAKE_FIXNUM(MOST_POSITIVE_FIXNUM);
	SYM_VAL(@'most-negative-fixnum') = MAKE_FIXNUM(MOST_NEGATIVE_FIXNUM);

	init_big();
	init_num_co();
	init_num_log();
	init_num_sfun();
	init_num_rand();
}
