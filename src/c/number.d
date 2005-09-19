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
#include <float.h>
#include "ecl.h"
#include "internal.h"

#ifndef HAVE_ISNANF
#define isnanf(x) isnan(x)
#endif

#ifndef M_PI
# ifdef PI
#  define M_PI PI
# else
#   define M_PI 3.14159265358979323846
# endif
#endif

cl_fixnum
fixint(cl_object x)
{
	if (FIXNUMP(x))
		return fix(x);
	if (type_of(x) == t_bignum) {
#ifdef WITH_GMP
		if (mpz_fits_slong_p(x->big.big_num)) {
			return mpz_get_si(x->big.big_num);
		}
#else  /* WITH_GMP */
                if ( !((cl_fixnum)x->big.big_num < x->big.big_num) )
                        return (cl_fixnum)x->big.big_num;
#endif /* WITH_GMP */
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
#ifdef WITH_GMP
		if (mpz_fits_ulong_p(x->big.big_num)) {
			return mpz_get_ui(x->big.big_num);
		}
#else  /* WITH_GMP */
                if ( x->big.big_num >= 0
                     && !((cl_fixnum)x->big.big_num < x->big.big_num) )
                        return (cl_fixnum)x->big.big_num;
#endif /* WITH_GMP */
	}
	cl_error(9, @'simple-type-error', @':format-control',
		    make_constant_string("Not a non-negative fixnum ~S"),
		    @':format-arguments', cl_list(1,x),
		    @':expected-type', cl_list(3, @'integer', MAKE_FIXNUM(0), MAKE_FIXNUM(MOST_POSITIVE_FIXNUM)),
		    @':datum', x);
}

cl_object
make_integer(cl_fixnum l)
{
	if (l > MOST_POSITIVE_FIXNUM || l < MOST_NEGATIVE_FIXNUM) {
		cl_object z = cl_alloc_object(t_bignum);
#ifdef WITH_GMP
		mpz_init_set_si(z->big.big_num, l);
#else  /* WITH_GMP */
                z->big.big_num = l;
#endif /* WITH_GMP */
		return z;
	}
	return MAKE_FIXNUM(l);
}

cl_object
make_unsigned_integer(cl_index l)
{
	if (l > MOST_POSITIVE_FIXNUM) {
		cl_object z = cl_alloc_object(t_bignum);
#ifdef WITH_GMP
		mpz_init_set_ui(z->big.big_num, l);
#else  /* WITH_GMP */
                z->big.big_num = l;
#endif /* WITH_GMP */
		return z;
	}
	return MAKE_FIXNUM(l);
}

cl_object
make_ratio(cl_object num, cl_object den)
{
	cl_object g, r;

	/* INV: the arguments NUM & DEN are integers */
	if (den == MAKE_FIXNUM(0))
		FEdivision_by_zero(num, den);
	if (num == MAKE_FIXNUM(0) || den == MAKE_FIXNUM(1))
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
		return(cl_core.shortfloat_zero);
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
		return(cl_core.longfloat_zero);
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

	case t_ratio: {
#ifdef WITH_GMP	
		double output;
                mpq_t aux;
		mpq_init(aux);
		if (FIXNUMP(x->ratio.num)) {
			mpz_set_si(mpq_numref(aux), fix(x->ratio.num));
		} else {
			mpz_set(mpq_numref(aux), x->ratio.num->big.big_num);
		}
		if (FIXNUMP(x->ratio.den)) {
			mpz_set_si(mpq_denref(aux), fix(x->ratio.den));
		} else {
			mpz_set(mpq_denref(aux), x->ratio.den->big.big_num);
		}
		output = mpq_get_d(aux);
		mpq_clear(aux);
		return output;
#else  /* WITH_GMP */
                return (double)(FIXNUMP(x->ratio.num) ? fix(x->ratio.num) : x->ratio.num->big.big_num) /
                     (double)(FIXNUMP(x->ratio.den) ? fix(x->ratio.den) : x->ratio.den->big.big_num);
#endif /* WITH_GMP */
                
	}
	case t_shortfloat:
		return((double)(sf(x)));

	case t_longfloat:
		return(lf(x));

	default:
		FEtype_error_real(x);
	}
}

cl_object
cl_rational(cl_object x)
{
	double d;

	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		break;
	case t_shortfloat:
		d = sf(x);
		goto GO_ON;
	case t_longfloat:
		d = lf(x);
	GO_ON:	if (d == 0.0) {
			x = MAKE_FIXNUM(0);
		} else {
			int e;
			d = frexp(d, &e);
			e -= DBL_MANT_DIG;
			x = double_to_integer(ldexp(d, DBL_MANT_DIG));
			x = number_times(cl_expt(MAKE_FIXNUM(FLT_RADIX),
						 MAKE_FIXNUM(e)),
					 x);
		}
		break;
	default:
		FEtype_error_number(x);
	}
	@(return x)
}

void
init_number(void)
{
	cl_object num;

	num = make_shortfloat(FLT_MAX);
	ECL_SET(@'MOST-POSITIVE-SHORT-FLOAT', num);
	ECL_SET(@'MOST-POSITIVE-SINGLE-FLOAT', num);

	num = make_shortfloat(-FLT_MAX);
	ECL_SET(@'MOST-NEGATIVE-SHORT-FLOAT', num);
	ECL_SET(@'MOST-NEGATIVE-SINGLE-FLOAT', num);

	num = make_shortfloat(FLT_MIN);
	ECL_SET(@'LEAST-POSITIVE-SHORT-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-SINGLE-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT', num);

	num = make_shortfloat(-FLT_MIN);
	ECL_SET(@'LEAST-NEGATIVE-SHORT-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-SINGLE-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT', num);

	num = make_longfloat(DBL_MAX);
	ECL_SET(@'MOST-POSITIVE-DOUBLE-FLOAT', num);
	ECL_SET(@'MOST-POSITIVE-LONG-FLOAT', num);

	num = make_longfloat(-DBL_MAX);
	ECL_SET(@'MOST-NEGATIVE-DOUBLE-FLOAT', num);
	ECL_SET(@'MOST-NEGATIVE-LONG-FLOAT', num);

	num = make_longfloat(DBL_MIN);
	ECL_SET(@'LEAST-POSITIVE-DOUBLE-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-LONG-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-NORMALIZED-LONG-FLOAT', num);

	num = make_longfloat(-DBL_MIN);
	ECL_SET(@'LEAST-NEGATIVE-DOUBLE-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-LONG-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT', num);

 	cl_core.shortfloat_zero = cl_alloc_object(t_shortfloat);
 	sf(cl_core.shortfloat_zero) = (float)0.0;
 	cl_core.longfloat_zero = cl_alloc_object(t_longfloat);
 	lf(cl_core.longfloat_zero) = (double)0.0;
	cl_core.plus_half = make_ratio(MAKE_FIXNUM(1), MAKE_FIXNUM(2));
	cl_core.minus_half = make_ratio(MAKE_FIXNUM(-1), MAKE_FIXNUM(2));
	cl_core.imag_unit =
	    make_complex(make_shortfloat(0.0), make_shortfloat(1.0));
	cl_core.minus_imag_unit =
	    make_complex(make_shortfloat(0.0), make_shortfloat(-1.0));
	cl_core.imag_two =
	    make_complex(make_shortfloat(0.0), make_shortfloat(2.0));

	ECL_SET(@'pi', make_longfloat(M_PI));

	init_big();

        ECL_SET(@'*random-state*', make_random_state(Ct));
}
