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
	int z;

	z = 1;
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
number_exp(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return(make_shortfloat(expf(number_to_double(x))));

	case t_shortfloat:
		return(make_shortfloat(expf(sf(x))));

	case t_longfloat:
		return(make_longfloat(exp(lf(x))));

	case t_complex: {
		cl_object y, y1;
	
		y = x->complex.imag;
		x = x->complex.real;
		x = number_exp(x);
		y1 = number_cos(y);
		y = number_sin(y);
		y = make_complex(y1, y);
		x = number_times(x, y);
		return(x);
	}

	default:
		FEtype_error_number(x);
	}
}

cl_object
number_expt(cl_object x, cl_object y)
{
	cl_type tx, ty;
	cl_object z;

	if (y == MAKE_FIXNUM(0))
		switch (type_of(x)) {
		case t_fixnum:  case t_bignum:  case t_ratio:
			return(MAKE_FIXNUM(1));

		case t_shortfloat:
			return(make_shortfloat(1.0));

		case t_longfloat:
			return(make_longfloat(1.0));

		case t_complex:
			z = number_expt(x->complex.real, y);
			z = make_complex(z, MAKE_FIXNUM(0));
			return(z);

		default:
			FEtype_error_number(x);
		}
	ty = type_of(y);
	if (number_zerop(x)) {
		if (!number_plusp(ty==t_complex?y->complex.real:y))
			FEerror("Cannot raise zero to the power ~S.", 1, y);
		return(number_times(x, y));
	}
	if (ty == t_fixnum || ty == t_bignum) {
		if (number_minusp(y)) {
			z = number_negate(y);
			z = number_expt(x, z);
			z = number_divide(MAKE_FIXNUM(1), z);
			return(z);
		}
		z = MAKE_FIXNUM(1);
		do {
			/* INV: integer_divide outputs an integer */
			if (!number_evenp(y))
				z = number_times(z, x);
			x = number_times(x, x);
			y = integer_divide(y, MAKE_FIXNUM(2));
		} while (number_plusp(y));
		return z;
	}
	z = number_nlog(x);
	z = number_times(z, y);
	z = number_exp(z);
	return(z);
}

cl_object
number_nlog(cl_object x)
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
		return(make_shortfloat(logf(number_to_double(x))));

	case t_shortfloat:
		return(make_shortfloat(logf(sf(x))));

	case t_longfloat:
		return(make_longfloat(log(lf(x))));

	default:
		FEtype_error_number(x);
	}
COMPLEX:
	a = number_times(r, r);
	p = number_times(i, i);
	a = number_plus(a, p);
	a = number_nlog(a);
	a = number_divide(a, MAKE_FIXNUM(2));
	p = number_atan2(i, r);
	x = make_complex(a, p);
	return(x);
}

cl_object
number_log(cl_object x, cl_object y)
{
	if (number_zerop(y))
		FEerror("Zero is the logarithmic singularity.", 0);
	return(number_divide(number_nlog(y), number_nlog(x)));
}

cl_object
number_sqrt(cl_object x)
{
	cl_object z;

	if (type_of(x) == t_complex)
		goto COMPLEX;
	if (number_minusp(x))
		return make_complex(MAKE_FIXNUM(0), number_sqrt(number_negate(x)));
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return(make_shortfloat(sqrtf(number_to_double(x))));

	case t_shortfloat:
		return(make_shortfloat(sqrtf(sf(x))));

	case t_longfloat:
		return(make_longfloat(sqrt(lf(x))));

	default:
		FEtype_error_number(x);
	}

COMPLEX:
	z = make_ratio(MAKE_FIXNUM(1), MAKE_FIXNUM(2));
	z = number_expt(x, z);
	return(z);
}

cl_object
number_atan2(cl_object y, cl_object x)
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
	return(z);
}

cl_object
number_atan(cl_object y)
{
	cl_object z, z1;

	if (type_of(y) == t_complex) {
#if 0 /* FIXME! ANSI states it should be this first part */
		z = number_times(imag_unit, y);
		z = number_nlog(one_plus(z)) +
		  number_nlog(number_minus(MAKE_FIXNUM(1), z));
		z = number_divide(z, number_times(MAKE_FIXNUM(2), imag_unit));
#else
		z = number_times(imag_unit, y);
		z = one_plus(z);
		z1 = number_times(y, y);
		z1 = one_plus(z1);
		z1 = number_sqrt(z1);
		z = number_divide(z, z1);
		z = number_nlog(z);
		z = number_times(minus_imag_unit, z);
#endif /* ANSI */
		return(z);
	}
	return(number_atan2(y, MAKE_FIXNUM(1)));
}

cl_object
number_sin(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return(make_shortfloat(sinf(number_to_double(x))));
	case t_shortfloat:
		return(make_shortfloat(sinf(sf(x))));
	case t_longfloat:
		return(make_longfloat(sin(lf(x))));
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
			return make_complex(make_shortfloat(a), make_shortfloat(b));
		return make_complex(make_longfloat(a), make_longfloat(b));
	}
	default:
		FEtype_error_number(x);
	}
}

cl_object
number_cos(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return(make_shortfloat(cosf(number_to_double(x))));
	case t_shortfloat:
		return(make_shortfloat(cosf(sf(x))));
	case t_longfloat:
		return(make_longfloat(cos(lf(x))));
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
			return make_complex(make_shortfloat(a), make_shortfloat(b));
		return make_complex(make_longfloat(a), make_longfloat(b));
	}
	default:
		FEtype_error_number(x);
	}
}

cl_object
number_tan(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return(make_shortfloat(tanf(number_to_double(x))));
	case t_shortfloat:
		return(make_shortfloat(tanf(sf(x))));
	case t_longfloat:
		return(make_longfloat(tan(lf(x))));
	case t_complex: {
		cl_object a = number_sin(x);
		cl_object b = number_cos(x);
		return number_divide(a, b);
	}
	default:
		FEtype_error_number(x);
	}
}

cl_object
number_sinh(cl_object x)
{

	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return make_shortfloat(sinhf(number_to_double(x)));
	case t_shortfloat:
		return make_shortfloat(sinhf(sf(x)));
	case t_longfloat:
		return make_longfloat(sinh(lf(x)));
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
			return make_complex(make_shortfloat(a), make_shortfloat(b));
		return make_complex(make_longfloat(a), make_longfloat(b));
	}
	default:
		FEtype_error_number(x);
	}
}

cl_object
number_cosh(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return make_shortfloat(coshf(number_to_double(x)));
	case t_shortfloat:
		return make_shortfloat(coshf(sf(x)));
	case t_longfloat:
		return make_longfloat(cosh(lf(x)));
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
			return make_complex(make_shortfloat(a), make_shortfloat(b));
		return make_complex(make_longfloat(a), make_longfloat(b));
	}
	default:
		FEtype_error_number(x);
	}
}

cl_object
number_tanh(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return(make_shortfloat(tanhf(number_to_double(x))));
	case t_shortfloat:
		return(make_shortfloat(tanhf(sf(x))));
	case t_longfloat:
		return(make_longfloat(tanh(lf(x))));
	case t_complex: {
		cl_object a = number_sinh(x);
		cl_object b = number_cosh(x);
		return number_divide(a, b);
	}
	default:
		FEtype_error_number(x);
	}
}

@(defun exp (x)
@	/* INV: type check in number_exp() */
	@(return number_exp(x))
@)

@(defun expt (x y)
@	/* INV: type check in number_expt() */
	@(return number_expt(x, y))
@)

@(defun log (x &optional (y OBJNULL))
@	/* INV: type check in number_nlog() and number_log() */
	if (y == OBJNULL)
		@(return number_nlog(x))
	@(return number_log(y, x))
@)

@(defun sqrt (x)
@	/* INV: type check in number_sqrt() */
	@(return number_sqrt(x))
@)

@(defun sin (x)
@	/* INV: type check in number_sin() */
	@(return number_sin(x))
@)

@(defun cos (x)
@	/* INV: type check in number_cos() */
	@(return number_cos(x))
@)

@(defun tan (x)
@	/* INV: type check in number_tan() */
	@(return number_tan(x))
@)

@(defun atan (x &optional (y OBJNULL))
@	/* INV: type check in number_atan() & number_atan2() */
	if (y == OBJNULL)
		@(return number_atan(x))
	@(return number_atan2(x, y))
@)

@(defun sinh (x)
@	/* INV: type check in number_sin() */
	@(return number_sinh(x))
@)

@(defun cosh (x)
@	/* INV: type check in number_cos() */
	@(return number_cosh(x))
@)

@(defun tanh (x)
@	/* INV: type check in number_tan() */
	@(return number_tanh(x))
@)

void
init_num_sfun(void)
{
	imag_unit = make_complex(make_shortfloat(0.0), make_shortfloat(1.0));
	register_root(&imag_unit);
	minus_imag_unit = make_complex(make_shortfloat(0.0),
				       make_shortfloat(-1.0));
	register_root(&minus_imag_unit);
	imag_two = make_complex(make_shortfloat(0.0), make_shortfloat(2.0));
	register_root(&imag_two);

	make_constant("PI", make_longfloat(M_PI));
}
