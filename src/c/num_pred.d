/*
    num_pred.c  -- Predicates on numbers.
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

int
number_zerop(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
		return(x == MAKE_FIXNUM(0));

	case t_bignum:
	case t_ratio:
		return(0);

	case t_shortfloat:
		return(sf(x) == 0.0);

	case t_longfloat:
		return(lf(x) == 0.0);

	case t_complex:
		return(number_zerop(x->complex.real) &&
		       number_zerop(x->complex.imag));

	default:
		FEtype_error_number(x);
	}
}

int
number_plusp(cl_object x)
{
 RESTART:
	switch (type_of(x)) {
	case t_fixnum:
		return(fix(x) > 0);

	case t_bignum:
		return(big_sign(x) > 0);

	case t_ratio:
		/* INV: rat_den is always positive */
		x = x->ratio.num;
		goto RESTART;

	case t_shortfloat:
		return(sf(x) > 0.0);

	case t_longfloat:
		return(lf(x) > 0.0);

	default:
		FEtype_error_real(x);
	}
}

int
number_minusp(cl_object x)
{
 RESTART:
	switch (type_of(x)) {
	case t_fixnum:
		return(fix(x) < 0);

	case t_bignum:
		return(big_sign(x) < 0);

	case t_ratio:
		/* INV: rat_den is always positive */
		x = x->ratio.num;
		goto RESTART;

	case t_shortfloat:
		return(sf(x) < 0.0);

	case t_longfloat:
		return(lf(x) < 0.0);

	default:
		FEtype_error_real(x);
	}
}

int
number_oddp(cl_object x)
{
	if (FIXNUMP(x))
		return fix(x) & 1;
	if (type_of(x) == t_bignum)
		return big_odd_p(x);
	FEtype_error_integer(x);
}

int
number_evenp(cl_object x)
{
	if (FIXNUMP(x))
		return ~fix(x) & 1;
	if (type_of(x) == t_bignum)
		return big_even_p(x);
	FEtype_error_integer(x);
}

cl_object
cl_zerop(cl_object x)
{	/* INV: number_zerop() checks type */
	@(return (number_zerop(x) ? Ct : Cnil))
}

cl_object
cl_plusp(cl_object x)
{	/* INV: number_plusp()  checks type */
	@(return (number_plusp(x) ? Ct : Cnil))
}

cl_object
cl_minusp(cl_object x)
{	/* INV: number_minusp() checks type */
	@(return (number_minusp(x) ? Ct : Cnil))
}

cl_object
cl_oddp(cl_object x)
{	/* INV: number_oddp() checks type */
	@(return (number_oddp(x) ? Ct : Cnil))
}

cl_object
cl_evenp(cl_object x)
{	/* INV: number_evenp() checks_type */
	@(return (number_evenp(x) ? Ct : Cnil))
}
