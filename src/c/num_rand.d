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

#include "ecl.h"
#include <time.h>

static cl_object
rando(cl_object x, cl_object rs)
{
	cl_type tx;
	cl_object z;
	double d;

	tx = type_of(x);
	if (number_compare(x, MAKE_FIXNUM(0)) != 1)
		FEwrong_type_argument(c_string_to_object("(REAL (0) *)"),
				      x);
	d = (double)(rs->random.value>>1) / (4294967296.0/2.0);
	d = number_to_double(x) * d;
	if (tx == t_fixnum) {
		z = MAKE_FIXNUM((cl_fixnum)d);
		return(z);
	} else if (tx == t_bignum) {
		z = double_to_integer(d);
		return(z);
	} else if (tx == t_shortfloat) {
		z = cl_alloc_object(t_shortfloat);
		sf(z) = (float)d;
		return(z);
	} else if (tx == t_longfloat) {
		z = cl_alloc_object(t_longfloat);
		lf(z) = d;
		return(z);
	} else
		FEerror("~S is not an integer nor a floating-point number.",
			1, x);
}

cl_object
make_random_state(cl_object rs)
{
        cl_object z;

	if (Null(rs)) {
		z = cl_alloc_object(t_random);
		z->random.value = symbol_value(@'*random-state*')->random.value;
		return(z);
	} else if (rs == Ct) {
		z = cl_alloc_object(t_random);
		z->random.value = time(0);
		return(z);
	} else if (type_of(rs) != t_random)
   		FEwrong_type_argument(@'random-state', rs);
	else {
		z = cl_alloc_object(t_random);
		z->random.value = rs->random.value;
		return(z);
	}
}

static void
advance_random_state(cl_object rs)
{
	rs->random.value
	= rs->random.value
	+ (rs->random.value<<2)
	+ (rs->random.value<<17)
	+ (rs->random.value<<27);
}


@(defun random (x &optional (rs symbol_value(@'*random-state*')))
@	
	if (type_of(rs) != t_random)
		FEwrong_type_argument(@'random-state', rs);
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

void
init_num_rand(void)
{
        SYM_VAL(@'*random-state*') = make_random_state(Ct);
}
