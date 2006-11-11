/*
    num_rand.c  -- Random numbers.
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
#include <time.h>

static cl_object
rando(cl_object x, cl_object rs)
{
	cl_object z;
	double d = (double)(rs->random.value>>1) / (4294967296.0/2.0);
 AGAIN:
	if (number_minusp(x)) {
		goto ERROR;
	}
	switch (type_of(x)) {
	case t_fixnum:
		z = MAKE_FIXNUM((cl_fixnum)(fix(x) * d));
		break;
	case t_bignum:
		z = floor1(number_times(x, cl_rational(make_doublefloat(d))));
		break;
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		z = make_shortfloat(ecl_short_float(x) * (float)d);
		break;
#endif
	case t_singlefloat:
		z = make_singlefloat(sf(x) * (float)d);
		break;
	case t_doublefloat:
		z = make_doublefloat(df(x) * d);
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		z = make_longfloat(ecl_long_float(x) * (long double)d);
		break;
#endif
	default:
	ERROR:
		x = ecl_type_error(@'random',"limit",x,
				   c_string_to_object("(OR (INTEGER 0 *) (FLOAT 0 *))"));
		goto AGAIN;
	}
	return z;
}

cl_object
make_random_state(cl_object rs)
{
        cl_object z = cl_alloc_object(t_random);

	if (rs == Ct) {
		z->random.value = time(0);
	} else {
		if (Null(rs)) {
			rs = symbol_value(@'*random-state*');
		}
		if (type_of(rs) != t_random) {
			FEwrong_type_argument(@'random-state', rs);
		}
		z->random.value = rs->random.value;
	}
	return(z);
}

static void
advance_random_state(cl_object rs)
{
	rs->random.value
	= rs->random.value
	+ (rs->random.value<<2)
	+ (rs->random.value<<17)
	+ (rs->random.value<<27);
	rs->random.value = rs->random.value & 0xffffffff;
}


@(defun random (x &optional (rs symbol_value(@'*random-state*')))
@
	rs = ecl_check_cl_type(@'random', rs, t_random);
	advance_random_state(rs);
	@(return rando(x, rs));
@)

@(defun make_random_state (&optional (rs Cnil))
@
	@(return make_random_state(rs))
@)

cl_object
cl_random_state_p(cl_object x)
{
	@(return ((type_of(x) == t_random) ? Ct : Cnil))
}
