/* -*- mode: c; c-basic-offset: 8 -*- */
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
ecl_zerop(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
		return(x == MAKE_FIXNUM(0));

	case t_bignum:
	case t_ratio:
		return(0);
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		return ecl_short_float(x) == 0.0;
#endif
	case t_singlefloat:
		return(sf(x) == 0.0);

	case t_doublefloat:
		return(df(x) == 0.0);
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return ecl_long_float(x) == 0.0;
#endif

	case t_complex:
		return(ecl_zerop(x->complex.real) &&
		       ecl_zerop(x->complex.imag));

	default:
		FEtype_error_number(x);
	}
}

int
ecl_plusp(cl_object x)
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
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		return ecl_short_float(x) > 0.0;
#endif
	case t_singlefloat:
		return sf(x) > 0.0;
	case t_doublefloat:
		return df(x) > 0.0;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return ecl_long_float(x) > 0.0;
#endif
	default:
		FEtype_error_real(x);
	}
}

int
ecl_minusp(cl_object x)
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

#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		return ecl_short_float(x) < 0.0;
#endif
	case t_singlefloat:
		return(sf(x) < 0.0);

	case t_doublefloat:
		return(df(x) < 0.0);
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return ecl_long_float(x) < 0.0;
#endif
	default:
		FEtype_error_real(x);
	}
}

int
ecl_oddp(cl_object x)
{
	if (FIXNUMP(x))
		return fix(x) & 1;
	if (type_of(x) == t_bignum)
		return big_odd_p(x);
	FEtype_error_integer(x);
}

int
ecl_evenp(cl_object x)
{
	if (FIXNUMP(x))
		return ~fix(x) & 1;
	if (type_of(x) == t_bignum)
		return big_even_p(x);
	FEtype_error_integer(x);
}

cl_object
cl_zerop(cl_object x)
{	/* INV: ecl_zerop() checks type */
	@(return (ecl_zerop(x) ? Ct : Cnil))
}

cl_object
cl_plusp(cl_object x)
{	/* INV: ecl_plusp()  checks type */
	@(return (ecl_plusp(x) ? Ct : Cnil))
}

cl_object
cl_minusp(cl_object x)
{	/* INV: ecl_minusp() checks type */
	@(return (ecl_minusp(x) ? Ct : Cnil))
}

cl_object
cl_oddp(cl_object x)
{	/* INV: ecl_oddp() checks type */
	@(return (ecl_oddp(x) ? Ct : Cnil))
}

cl_object
cl_evenp(cl_object x)
{	/* INV: ecl_evenp() checks_type */
	@(return (ecl_evenp(x) ? Ct : Cnil))
}
