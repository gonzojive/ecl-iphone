/*
    num_sfun.c  -- Trascendental functions.
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

#include "ecl.h"
#include <math.h>
#ifndef HAVE_ISOC99
# define expf exp
# define logf log
# define sqrtf sqrt
# define cosf cos
# define sinf sin
# define tanf tan
# define sinhf sinh
# define coshf cosh
# define tanhf tanh
#endif

#ifndef M_PI
# ifdef PI
#  define M_PI PI
# else
#   define M_PI 3.14159265358979323846
# endif
#endif

cl_object imag_unit, minus_imag_unit, imag_two;

cl_fixnum
fixnum_expt(cl_fixnum x, cl_fixnum y)
{
	cl_fixnum z = 1;
	while (y > 0)
		if (y%2 == 0) {
			x *= x;
			y /= 2;
		} else {
			z *= x;
			--y;
		}
	return(z);
}

cl_object
cl_exp(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return1(make_shortfloat(expf(number_to_double(x))));

	case t_shortfloat:
		return1(make_shortfloat(expf(sf(x))));

	case t_longfloat:
		return1(make_longfloat(exp(lf(x))));

	case t_complex: {
		cl_object y, y1;
	
		y = x->complex.imag;
		x = x->complex.real;
		x = cl_exp(x);
		y1 = cl_cos(y);
		y = cl_sin(y);
		y = make_complex(y1, y);
		x = number_times(x, y);
		return1(x);
	}

	default:
		FEtype_error_number(x);
	}
}

cl_object
cl_expt(cl_object x, cl_object y)
{
	cl_type ty;
	cl_object z;

	if (number_zerop(y)) {
		/* INV: The most specific numeric types come first. */
		cl_type tx = type_of(x);
		ty = type_of(y);
		switch ((ty > tx)? ty : tx) {
		case t_fixnum:
		case t_bignum:
		case t_ratio:
			return1(MAKE_FIXNUM(1));
		case t_shortfloat:
			return1(make_shortfloat(1.0));
		case t_longfloat:
			return1(make_longfloat(1.0));
		case t_complex:
			z = cl_expt(x->complex.real, y);
			z = make_complex(z, MAKE_FIXNUM(0));
			return1(z);
		default:
			FEtype_error_number(x);
		}
	}
	ty = type_of(y);
	if (number_zerop(x)) {
		if (!number_plusp(ty==t_complex?y->complex.real:y))
			FEerror("Cannot raise zero to the power ~S.", 1, y);
		return1(number_times(x, y));
	}
	if (ty == t_fixnum || ty == t_bignum) {
		if (number_minusp(y)) {
			z = number_negate(y);
			z = cl_expt(x, z);
			z = number_divide(MAKE_FIXNUM(1), z);
			return1(z);
		}
		z = MAKE_FIXNUM(1);
		do {
			/* INV: integer_divide outputs an integer */
			if (!number_evenp(y))
				z = number_times(z, x);
			x = number_times(x, x);
			y = integer_divide(y, MAKE_FIXNUM(2));
		} while (number_plusp(y));
		return1(z);
	}
	z = cl_log1(x);
	z = number_times(z, y);
	z = cl_exp(z);
	return1(z);
}

cl_object
cl_log1(cl_object x)
{
	cl_object r, i, a, p;

	if (type_of(x) == t_complex) {
		r = x->complex.real;
		i = x->complex.imag;
		goto COMPLEX;
	}
	if (number_zerop(x))
		FEerror("Zero is the logarithmic singularity.", 0);
	if (number_minusp(x)) {
		r = x;
		i = MAKE_FIXNUM(0);
		goto COMPLEX;
	}
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return1(make_shortfloat(logf(number_to_double(x))));

	case t_shortfloat:
		return1(make_shortfloat(logf(sf(x))));

	case t_longfloat:
		return1(make_longfloat(log(lf(x))));

	default:
		FEtype_error_number(x);
	}
COMPLEX:
	a = number_times(r, r);
	p = number_times(i, i);
	a = number_plus(a, p);
	a = cl_log1(a);
	a = number_divide(a, MAKE_FIXNUM(2));
	p = cl_atan2(i, r);
	x = make_complex(a, p);
	return1(x);
}

cl_object
cl_log2(cl_object x, cl_object y)
{
	if (number_zerop(y))
		FEerror("Zero is the logarithmic singularity.", 0);
	return1(number_divide(cl_log1(y), cl_log1(x)));
}

cl_object
cl_sqrt(cl_object x)
{
	cl_object z;

	if (type_of(x) == t_complex)
		goto COMPLEX;
	if (number_minusp(x))
		return1(make_complex(MAKE_FIXNUM(0), cl_sqrt(number_negate(x))));
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return1(make_shortfloat(sqrtf(number_to_double(x))));

	case t_shortfloat:
		return1(make_shortfloat(sqrtf(sf(x))));

	case t_longfloat:
		return1(make_longfloat(sqrt(lf(x))));

	default:
		FEtype_error_number(x);
	}

COMPLEX:
	z = make_ratio(MAKE_FIXNUM(1), MAKE_FIXNUM(2));
	z = cl_expt(x, z);
	return1(z);
}

cl_object
cl_atan2(cl_object y, cl_object x)
{
	cl_object z;
	double dy, dx, dz;

	dy = number_to_double(y);
	dx = number_to_double(x);
	if (dx > 0.0)
		if (dy > 0.0)
			dz = atan(dy / dx);
		else if (dy == 0.0)
			dz = 0.0;
		else
			dz = -atan(-dy / dx);
	else if (dx == 0.0)
		if (dy > 0.0)
			dz = M_PI / 2.0;
		else if (dy == 0.0)
			FEerror("Logarithmic singularity.", 0);
		else
			dz = -M_PI / 2.0;
	else
		if (dy > 0.0)
			dz = M_PI - atan(dy / -dx);
		else if (dy == 0.0)
			dz = M_PI;
		else
			dz = -M_PI + atan(-dy / -dx);
	if (type_of(x) == t_longfloat || type_of(y) == t_longfloat)
		z = make_longfloat(dz);
	else
		z = make_shortfloat(dz);
	return1(z);
}

cl_object
cl_atan1(cl_object y)
{
	cl_object z, z1;

	if (type_of(y) == t_complex) {
#if 0 /* FIXME! ANSI states it should be this first part */
		z = number_times(cl_core.imag_unit, y);
		z = cl_log1(one_plus(z)) +
		  cl_log1(number_minus(MAKE_FIXNUM(1), z));
		z = number_divide(z, number_times(MAKE_FIXNUM(2), cl_core.imag_unit));
#else
		z = number_times(cl_core.imag_unit, y);
		z = one_plus(z);
		z1 = number_times(y, y);
		z1 = one_plus(z1);
		z1 = cl_sqrt(z1);
		z = number_divide(z, z1);
		z = cl_log1(z);
		z = number_times(cl_core.minus_imag_unit, z);
#endif /* ANSI */
		return1(z);
	}
	return1(cl_atan2(y, MAKE_FIXNUM(1)));
}

cl_object
cl_sin(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return1(make_shortfloat(sinf(number_to_double(x))));
	case t_shortfloat:
		return1(make_shortfloat(sinf(sf(x))));
	case t_longfloat:
		return1(make_longfloat(sin(lf(x))));
	case t_complex: {
		/*
		  z = x + I y
		  z = x + I y
		  sin(z) = sinh(I z) = sinh(-y + I x)
		*/
		double dx = number_to_double(x->complex.real);
		double dy = number_to_double(x->complex.imag);
		double a = sin(dx) * cosh(dy);
		double b = cos(dx) * sinh(dy);
		if (type_of(x->complex.real) != t_longfloat)
			return1(make_complex(make_shortfloat(a), make_shortfloat(b)));
		return1(make_complex(make_longfloat(a), make_longfloat(b)));
	}
	default:
		FEtype_error_number(x);
	}
}

cl_object
cl_cos(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return1(make_shortfloat(cosf(number_to_double(x))));
	case t_shortfloat:
		return1(make_shortfloat(cosf(sf(x))));
	case t_longfloat:
		return1(make_longfloat(cos(lf(x))));
	case t_complex: {
		/*
		  z = x + I y
		  cos(z) = cosh(I z) = cosh(-y + I x)
		*/
		double dx = number_to_double(x->complex.real);
		double dy = number_to_double(x->complex.imag);
		double a =  cos(dx) * cosh(dy);
		double b = -sin(dx) * sinh(dy);
		if (type_of(x->complex.real) != t_longfloat)
			return1(make_complex(make_shortfloat(a), make_shortfloat(b)));
		return1(make_complex(make_longfloat(a), make_longfloat(b)));
	}
	default:
		FEtype_error_number(x);
	}
}

cl_object
cl_tan(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return1(make_shortfloat(tanf(number_to_double(x))));
	case t_shortfloat:
		return1(make_shortfloat(tanf(sf(x))));
	case t_longfloat:
		return1(make_longfloat(tan(lf(x))));
	case t_complex: {
		cl_object a = cl_sin(x);
		cl_object b = cl_cos(x);
		return1(number_divide(a, b));
	}
	default:
		FEtype_error_number(x);
	}
}

cl_object
cl_sinh(cl_object x)
{

	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return1(make_shortfloat(sinhf(number_to_double(x))));
	case t_shortfloat:
		return1(make_shortfloat(sinhf(sf(x))));
	case t_longfloat:
		return1(make_longfloat(sinh(lf(x))));
	case t_complex: {
		/*
		  z = x + I y
		  sinh(z) = (exp(z)-exp(-z))/2
		          = (exp(x)*(cos(y)+Isin(y))-exp(-x)*(cos(y)-Isin(y)))/2
			  = sinh(x)*cos(y) + Icosh(x)*sin(y);
		*/
		double dx = number_to_double(x->complex.real);
		double dy = number_to_double(x->complex.imag);
		double a = sinh(dx) * cos(dy);
		double b = cosh(dx) * sin(dy);
		if (type_of(x->complex.real) != t_longfloat)
			return1(make_complex(make_shortfloat(a), make_shortfloat(b)));
		return1(make_complex(make_longfloat(a), make_longfloat(b)));
	}
	default:
		FEtype_error_number(x);
	}
}

cl_object
cl_cosh(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return1(make_shortfloat(coshf(number_to_double(x))));
	case t_shortfloat:
		return1(make_shortfloat(coshf(sf(x))));
	case t_longfloat:
		return1(make_longfloat(cosh(lf(x))));
	case t_complex: {
		/*
		  z = x + I y
		  cosh(z) = (exp(z)+exp(-z))/2
		          = (exp(x)*(cos(y)+Isin(y))+exp(-x)*(cos(y)-Isin(y)))/2
			  = cosh(x)*cos(y) + Isinh(x)*sin(y);
		*/
		double dx = number_to_double(x->complex.real);
		double dy = number_to_double(x->complex.imag);
		double a = cosh(dx) * cos(dy);
		double b = sinh(dx) * sin(dy);
		if (type_of(x->complex.real) != t_longfloat)
			return1(make_complex(make_shortfloat(a), make_shortfloat(b)));
		return1(make_complex(make_longfloat(a), make_longfloat(b)));
	}
	default:
		FEtype_error_number(x);
	}
}

cl_object
cl_tanh(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return1(make_shortfloat(tanhf(number_to_double(x))));
	case t_shortfloat:
		return1(make_shortfloat(tanhf(sf(x))));
	case t_longfloat:
		return1(make_longfloat(tanh(lf(x))));
	case t_complex: {
		cl_object a = cl_sinh(x);
		cl_object b = cl_cosh(x);
		return1(number_divide(a, b));
	}
	default:
		FEtype_error_number(x);
	}
}

@(defun log (x &optional (y OBJNULL))
@	/* INV: type check in cl_log1() and cl_log2() */
	if (y == OBJNULL)
		@(return cl_log1(x))
	@(return cl_log2(y, x))
@)

@(defun atan (x &optional (y OBJNULL))
@	/* INV: type check in cl_atan() & cl_atan2() */
	if (y == OBJNULL)
		@(return cl_atan1(x))
	@(return cl_atan2(x, y))
@)
