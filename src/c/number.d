/* -*- mode: c; c-basic-offset: 8 -*- */
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

#include <ecl/ecl.h>
#include <float.h>
#include <math.h>
#ifdef _MSC_VER
# undef complex
#endif
#include <ecl/internal.h>

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
		    make_constant_base_string("Not a non-negative fixnum ~S"),
		    @':format-arguments', cl_list(1,x),
		    @':expected-type', cl_list(3, @'integer', MAKE_FIXNUM(0), MAKE_FIXNUM(MOST_POSITIVE_FIXNUM)),
		    @':datum', x);
}

cl_fixnum
ecl_fixnum_in_range(cl_object fun, const char *what, cl_object value,
		    cl_fixnum min, cl_fixnum max)
{
	do {
		if (FIXNUMP(value)) {
			cl_fixnum output = fix(value);
			if ((min <= output) && (output <= max)) {
				return output;
			}
		}
		value = ecl_type_error(fun, what, value,
				       cl_list(3,@'integer',MAKE_FIXNUM(min),
					       MAKE_FIXNUM(max)));
	} while(1);
}

cl_object
ecl_make_integer(cl_fixnum l)
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
ecl_make_unsigned_integer(cl_index l)
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
ecl_make_ratio(cl_object num, cl_object den)
{
	cl_object g, r;

	/* INV: the arguments NUM & DEN are integers */
	if (den == MAKE_FIXNUM(0))
		FEdivision_by_zero(num, den);
	if (num == MAKE_FIXNUM(0) || den == MAKE_FIXNUM(1))
		return(num);
	if (ecl_minusp(den)) {
		num = ecl_negate(num);
		den = ecl_negate(den);
	}
	g = ecl_gcd(num, den);
	num = ecl_integer_divide(num, g);
	den = ecl_integer_divide(den, g);
	if (den == MAKE_FIXNUM(1))
		return num;
	if (den == MAKE_FIXNUM(-1))
		return ecl_negate(num);
	r = cl_alloc_object(t_ratio);
	r->ratio.num = num;
	r->ratio.den = den;
	return(r);
}

cl_object
ecl_make_singlefloat(float f)
{
	cl_object x;

	ecl_detect_fpe();
#ifdef signbit
	if ((f == (float)0.0) && !signbit(f)) {
		return(cl_core.singlefloat_zero);
	}
#endif
	if (isnan(f)) {
		cl_error(1, @'division-by-zero');
	}
	if (!isfinite(f)) {
		cl_error(1, @'floating-point-overflow');
	}
	x = cl_alloc_object(t_singlefloat);
	sf(x) = f;
	return(x);
}

cl_object
ecl_make_doublefloat(double f)
{
	cl_object x;

	ecl_detect_fpe();
#ifdef signbit
	if ((f == (double)0.0) && !signbit(f)) {
		return(cl_core.doublefloat_zero);
	}
#endif
	if (isnan(f)) {
		cl_error(1, @'division-by-zero');
	}
	if (!isfinite(f)) {
		cl_error(1, @'floating-point-overflow');
	}
	x = cl_alloc_object(t_doublefloat);
	df(x) = f;
	return(x);
}

#ifdef ECL_LONG_FLOAT
cl_object
make_longfloat(long double f)
{
	cl_object x;

	ecl_detect_fpe();
#ifdef signbit
	if ((f == (long double)0.0) && !signbit(f)) {
		return cl_core.longfloat_zero;
	}
#endif
	if (isnan(f)) {
		cl_error(1, @'division-by-zero');
	}
	if (!isfinite(f)) {
		cl_error(1, @'floating-point-overflow');
	}
	x = cl_alloc_object(t_longfloat);
	x->longfloat.value = f;
	return x;
}
#endif

cl_object
ecl_make_complex(cl_object r, cl_object i)
{
	cl_object c;
	cl_type ti;
 AGAIN:
	ti = type_of(i);
	/* Both R and I are promoted to a common type */
	switch (type_of(r)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		switch (ti) {
		case t_fixnum:
			if (i == MAKE_FIXNUM(0))
				return(r);
		case t_bignum:
		case t_ratio:
			break;
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			r = make_shortfloat((float)ecl_to_double(r));
			break;
#endif
		case t_singlefloat:
			r = ecl_make_singlefloat((float)ecl_to_double(r));
			break;
		case t_doublefloat:
			r = ecl_make_doublefloat(ecl_to_double(r));
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			r = make_longfloat(ecl_to_double(r));
			break;
#endif
		default:
			i = ecl_type_error(@'complex',"imaginary part", i, @'real');
			goto AGAIN;
		}
		break;
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		switch (ti) {
		case t_fixnum:
		case t_bignum:
		case t_ratio:
			i = make_shortfloat((float)ecl_to_double(i));
		case t_shortfloat:
			break;
		case t_singlefloat:
			r = ecl_make_singlefloat((float)ecl_short_float(r));
			break;
		case t_doublefloat:
			r = ecl_make_doublefloat((double)ecl_short_float(r));
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			r = make_longfloat((long double)ecl_short_float(r));
			break;
#endif
		default:
			i = ecl_type_error(@'complex',"imaginary part", i, @'real');
			goto AGAIN;
		}
		break;
#endif
	case t_singlefloat:
		switch (ti) {
		case t_fixnum:
		case t_bignum:
		case t_ratio:
			i = ecl_make_singlefloat((float)ecl_to_double(i));
			break;
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
			i = ecl_make_singlefloat(ecl_short_float(i));
			break;
#endif
		case t_singlefloat:
			break;
		case t_doublefloat:
			r = ecl_make_doublefloat((double)(sf(r)));
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			r = make_longfloat((long double)sf(r));
			break;
#endif
		default:
			i = ecl_type_error(@'complex',"imaginary part", i, @'real');
			goto AGAIN;
		}
		break;
	case t_doublefloat:
		switch (ti) {
		case t_fixnum:
		case t_bignum:
		case t_ratio:
#ifdef ECL_SHORT_FLOAT
		case t_shortfloat:
#endif
		case t_singlefloat:
			i = ecl_make_doublefloat(ecl_to_double(i));
		case t_doublefloat:
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			r = make_longfloat((long double)df(r));
			break;
#endif
		default:
			i = ecl_type_error(@'complex',"imaginary part", i, @'real');
			goto AGAIN;
		}
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		if (ti != t_longfloat)
			i = make_longfloat((long double)ecl_to_double(i));
		break;
#endif
	default:
		r = ecl_type_error(@'complex',"real part", r, @'real');
		goto AGAIN;

	}
	c = cl_alloc_object(t_complex);
	c->complex.real = r;
	c->complex.imag = i;
	return(c);
}

#ifdef ECL_LONG_FLOAT
long double
ecl_to_long_double(cl_object x)
{
	if (type_of(x) == t_longfloat) {
		return ecl_long_float(x);
	} else {
		return ecl_to_double(x);
	}
}
#endif

double
ecl_to_double(cl_object x)
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
#ifdef ECL_SHORT_FLOAT
	case t_singlefloat:
		return ecl_short_float(x);
#endif
	case t_singlefloat:
		return (double)sf(x);
	case t_doublefloat:
		return(df(x));
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return (double)ecl_long_float(x);
#endif
	default:
		FEtype_error_real(x);
	}
}

cl_object
cl_rational(cl_object x)
{
	double d;
 AGAIN:
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		break;
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		d = ecl_short_float(x);
		goto GO_ON;
#endif
	case t_singlefloat:
		d = sf(x);
		goto GO_ON;
	case t_doublefloat:
		d = df(x);
	GO_ON:	if (d == 0) {
			x = MAKE_FIXNUM(0);
		} else {
			int e;
			d = frexp(d, &e);
			e -= DBL_MANT_DIG;
			x = double_to_integer(ldexp(d, DBL_MANT_DIG));
			x = ecl_times(cl_expt(MAKE_FIXNUM(FLT_RADIX),
						 MAKE_FIXNUM(e)),
					 x);
		}
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		/* Fixme! We need a long double -> integer conversion! */
		long double d = ecl_long_float(x);
		if (d == 0) {
			x = MAKE_FIXNUM(0);
		} else {
			int e;
			d = frexpl(d, &e);
			e -= LDBL_MANT_DIG;
			x = double_to_integer(ldexp(d, LDBL_MANT_DIG));
			if (e != 0) {
				x = ecl_times(cl_expt(MAKE_FIXNUM(FLT_RADIX),
							 MAKE_FIXNUM(e)),
						 x);
			}
		}
		break;
	}
#endif
	default:
		x = ecl_type_error(@'rational',"argument",x,@'number');
		goto AGAIN;
	}
	@(return x)
}

#ifdef ECL_LONG_FLOAT
cl_object
long_double_to_integer(long double d)
{
	if (d <= MOST_POSITIVE_FIXNUM && d >= MOST_NEGATIVE_FIXNUM) {
		return MAKE_FIXNUM((cl_fixnum)d);
	} else if (-DBL_MAX <= d && d <= DBL_MAX) {
		return double_to_integer(d);
	} else {
		extern long double sqrtl(long double x);
		extern long double roundl(long double x);
		long double d1, d2;
		cl_object out;
		int e = 0;
		d = frexpl(d, &e);
		if (e < 0) {
			return MAKE_FIXNUM(0);
		}
		e -= LDBL_MANT_DIG;
		d1 = floor(ldexp(d, LDBL_MANT_DIG/2));
		d2 = ldexp(d, LDBL_MANT_DIG) - ldexp(d1, +LDBL_MANT_DIG/2);
		out = ecl_plus(cl_ash(long_double_to_integer(d1), MAKE_FIXNUM(LDBL_MANT_DIG/2)),
				  long_double_to_integer(d2));
		if (e > 0) {
			if (FLT_RADIX == 2) {
				out = ecl_ash(out, e);
			} else {
				out = ecl_times(cl_expt(MAKE_FIXNUM(FLT_RADIX), MAKE_FIXNUM(e)),
						   out);
			}
		}
		return out;
	}
}
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

void
init_number(void)
{
	cl_object num;

	num = ecl_make_singlefloat(FLT_MAX);
	ECL_SET(@'MOST-POSITIVE-SHORT-FLOAT', num);
	ECL_SET(@'MOST-POSITIVE-SINGLE-FLOAT', num);

	num = ecl_make_singlefloat(-FLT_MAX);
	ECL_SET(@'MOST-NEGATIVE-SHORT-FLOAT', num);
	ECL_SET(@'MOST-NEGATIVE-SINGLE-FLOAT', num);

	num = ecl_make_singlefloat(FLT_MIN);
	ECL_SET(@'LEAST-POSITIVE-SHORT-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-SINGLE-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT', num);

	num = ecl_make_singlefloat(-FLT_MIN);
	ECL_SET(@'LEAST-NEGATIVE-SHORT-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-SINGLE-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT', num);

	num = ecl_make_doublefloat(DBL_MAX);
	ECL_SET(@'MOST-POSITIVE-DOUBLE-FLOAT', num);
#ifdef ECL_LONG_FLOAT
	num = make_longfloat(LDBL_MAX);
#endif
	ECL_SET(@'MOST-POSITIVE-LONG-FLOAT', num);

	num = ecl_make_doublefloat(-DBL_MAX);
	ECL_SET(@'MOST-NEGATIVE-DOUBLE-FLOAT', num);
#ifdef ECL_LONG_FLOAT
	num = make_longfloat(-LDBL_MAX);
#endif
	ECL_SET(@'MOST-NEGATIVE-LONG-FLOAT', num);

	num = ecl_make_doublefloat(DBL_MIN);
	ECL_SET(@'LEAST-POSITIVE-DOUBLE-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT', num);
#ifdef ECL_LONG_FLOAT
	num = make_longfloat(LDBL_MIN);
#endif
	ECL_SET(@'LEAST-POSITIVE-LONG-FLOAT', num);
	ECL_SET(@'LEAST-POSITIVE-NORMALIZED-LONG-FLOAT', num);

	num = ecl_make_doublefloat(-DBL_MIN);
	ECL_SET(@'LEAST-NEGATIVE-DOUBLE-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT', num);
#ifdef ECL_LONG_FLOAT
	num = make_longfloat(-LDBL_MIN);
#endif
	ECL_SET(@'LEAST-NEGATIVE-LONG-FLOAT', num);
	ECL_SET(@'LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT', num);

 	cl_core.singlefloat_zero = cl_alloc_object(t_singlefloat);
 	sf(cl_core.singlefloat_zero) = (float)0;
 	cl_core.doublefloat_zero = cl_alloc_object(t_doublefloat);
 	df(cl_core.doublefloat_zero) = (double)0;
#ifdef ECL_LONG_FLOAT
 	cl_core.longfloat_zero = cl_alloc_object(t_longfloat);
 	cl_core.longfloat_zero->longfloat.value = (long double)0;
#endif
	cl_core.plus_half = ecl_make_ratio(MAKE_FIXNUM(1), MAKE_FIXNUM(2));
	cl_core.minus_half = ecl_make_ratio(MAKE_FIXNUM(-1), MAKE_FIXNUM(2));
	cl_core.imag_unit =
	    ecl_make_complex(ecl_make_singlefloat(0.0), ecl_make_singlefloat(1.0));
	cl_core.minus_imag_unit =
	    ecl_make_complex(ecl_make_singlefloat(0.0), ecl_make_singlefloat(-1.0));
	cl_core.imag_two =
	    ecl_make_complex(ecl_make_singlefloat(0.0), ecl_make_singlefloat(2.0));

#ifdef ECL_LONG_FLOAT
	ECL_SET(@'pi', make_longfloat((long double)ECL_PI_L));
#else
	ECL_SET(@'pi', ecl_make_doublefloat((double)ECL_PI_D));
#endif

	init_big();

        ECL_SET(@'*random-state*', ecl_make_random_state(Ct));
}
