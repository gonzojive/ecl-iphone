/*
    predicate.c -- Predicates.
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

cl_object
cl_identity(cl_object x)
{
	@(return x)
}

cl_object
cl_null(cl_object x)
{
	@(return (Null(x) ? Ct : Cnil))
}

cl_object
cl_symbolp(cl_object x)
{
	@(return (SYMBOLP(x) ? Ct : Cnil))
}

cl_object
cl_atom(cl_object x)
{
	@(return (ATOM(x) ? Ct : Cnil))
}

cl_object
cl_consp(cl_object x)
{
	@(return (CONSP(x) ? Ct : Cnil))
}

cl_object
cl_listp(cl_object x)
{
	@(return ((Null(x) || CONSP(x)) ? Ct : Cnil))
}

cl_object
cl_numberp(cl_object x)
{
	cl_type t = type_of(x);
	@(return (NUMBER_TYPE(t) ? Ct : Cnil))
}

/*	Used in compiled code		*/
bool numberp(cl_object x)
{
  cl_type t = type_of(x);
  return(NUMBER_TYPE(t));
}

cl_object
cl_integerp(cl_object x)
{
	cl_type t = type_of(x);
	@(return ((t == t_fixnum || t == t_bignum) ? Ct : Cnil))
}

cl_object
cl_rationalp(cl_object x)
{
	cl_type t = type_of(x);
	@(return ((t == t_fixnum || t == t_bignum || t == t_ratio) ? Ct : Cnil))
}

cl_object
cl_floatp(cl_object x)
{
	cl_type t = type_of(x);
	@(return ((t == t_longfloat || t == t_shortfloat) ? Ct : Cnil))
}

cl_object
cl_realp(cl_object x)
{
	cl_type t = type_of(x);
	@(return (REAL_TYPE(t) ? Ct : Cnil))
}

cl_object
cl_complexp(cl_object x)
{
	@(return ((type_of(x) == t_complex) ? Ct : Cnil))
}

cl_object
cl_characterp(cl_object x)
{
	@(return (CHARACTERP(x) ? Ct : Cnil))
}

cl_object
cl_stringp(cl_object x)
{
	@(return ((type_of(x) == t_string) ? Ct : Cnil))
}

cl_object
cl_bit_vector_p(cl_object x)
{
	@(return ((type_of(x) == t_bitvector) ? Ct : Cnil))
}

cl_object
cl_vectorp(cl_object x)
{
	cl_type t = type_of(x);
	@(return ((t == t_vector || t == t_string || t == t_bitvector) ? Ct : Cnil))
}

cl_object
cl_simple_string_p(cl_object x)
{
	@(return ((type_of(x) == t_string &&
		     !x->string.adjustable &&
		     !x->string.hasfillp &&
		     Null(CAR(x->string.displaced))) ? Ct : Cnil))
}

cl_object
cl_simple_bit_vector_p(cl_object x)
{
	@(return ((type_of(x) == t_bitvector &&
		     !x->vector.adjustable &&
		     !x->vector.hasfillp &&
		     Null(CAR(x->vector.displaced))) ? Ct : Cnil))
}

cl_object
cl_simple_vector_p(cl_object x)
{
	cl_type t = type_of(x);
	@(return ((t == t_vector &&
		     !x->vector.adjustable &&
		     !x->vector.hasfillp &&
		     Null(CAR(x->vector.displaced)) &&
		     (cl_elttype)x->vector.elttype == aet_object) ? Ct : Cnil))
}

cl_object
cl_arrayp(cl_object x)
{
	cl_type t = type_of(x);
	@(return (ARRAY_TYPE(t) ? Ct : Cnil))
}

cl_object
cl_packagep(cl_object x)
{
	@(return ((type_of(x) == t_package) ? Ct : Cnil))
}

cl_object
cl_functionp(cl_object x)
{
	cl_type t;
	cl_object output;

	t = type_of(x);
	if (t == t_bytecodes || t == t_cfun || t == t_cclosure
#ifdef CLOS
	    || (t == t_instance && x->instance.isgf)
#endif
	    )
		output = Ct;
	else
		output = Cnil;
	@(return output)
}

cl_object
cl_compiled_function_p(cl_object x)
{
	cl_type t = type_of(x);
	@(return ((t == t_bytecodes || t == t_cfun || t == t_cclosure) ? Ct : Cnil))
}

cl_object
cl_commonp(cl_object x)
{
	cl_object output = (FALSE /* type_of(x) == t_spice */
#ifdef CLOS
		     || (type_of(x) == t_instance && x->instance.isgf)
#endif
		     ) ? Cnil : Ct;
	@(return output)
}

cl_object
cl_eq(cl_object x, cl_object y)
{
	@(return ((x == y) ? Ct : Cnil))
}

bool
eql(cl_object x, cl_object y)
{
	cl_type t;

	if (x == y)
		return(TRUE);
	if ((t = type_of(x)) != type_of(y))
		return(FALSE);
	switch (t) {

	case t_fixnum:
		return(fix(x) == fix(y));

	case t_bignum:
		return(big_compare(x, y) == 0);

	case t_ratio:
		return(eql(x->ratio.num, y->ratio.num) &&
		       eql(x->ratio.den, y->ratio.den));

	case t_shortfloat:
		return(sf(x) == sf(y));

	case t_longfloat:
		return(lf(x) == lf(y));

	case t_complex:
		if (eql(x->complex.real, y->complex.real) &&
		    eql(x->complex.imag, y->complex.imag))
			return(TRUE);
		else
			return(FALSE);

	case t_character:
		return(CHAR_CODE(x) == CHAR_CODE(y));

	default:
		return(FALSE);
	}
}

cl_object
cl_eql(cl_object x, cl_object y)
{
	@(return (eql(x, y) ? Ct : Cnil))
}

bool
equal(register cl_object x, cl_object y)
{
	register cl_type t;

BEGIN:
	if ((t = type_of(x)) != type_of(y))
		return(FALSE);
	if (x==y)
		return(TRUE);
	switch (t) {

	case t_cons:
		if (!equal(CAR(x), CAR(y)))
			return(FALSE);
		x = CDR(x);
		y = CDR(y);
		goto BEGIN;

	case t_symbol:
	case t_vector:
	case t_array:
		return FALSE;

	case t_fixnum:
		return(fix(x)==fix(y));

	case t_shortfloat:
		return(x->SF.SFVAL==y->SF.SFVAL);

	case t_longfloat:
		return(x->LF.LFVAL==y->LF.LFVAL);

	case t_string:
		return(string_eq(x, y));

	case t_bitvector: {
		cl_index i, ox, oy;

		if (x->vector.fillp != y->vector.fillp)
			return(FALSE);
		ox = x->vector.offset;
		oy = y->vector.offset;
		for (i = 0;  i < x->vector.fillp;  i++)
			if((x->vector.self.bit[(i+ox)/8] & (0200>>(i+ox)%8))
			 !=(y->vector.self.bit[(i+oy)/8] & (0200>>(i+oy)%8)))
				return(FALSE);
		return(TRUE);
	}

#ifdef CLOS
	case t_instance: {
		cl_index i, l = x->instance.length;

		if (CLASS_OF(x) != CLASS_OF(y))
			return(FALSE);
		if (l != y->instance.length)
			return(FALSE);
		for (i = 0;  i < l;  i++) {
			cl_object vx = x->instance.slots[i];
			cl_object vy = y->instance.slots[i];
			if (vx == OBJNULL) {
				if (vy != OBJNULL)
					return FALSE;
			} else if (vy == OBJNULL) {
				return FALSE;
			} else if (!equal(vx, vy)) {
				return FALSE;
			}
		}
		return TRUE;
	}
#else
	case t_structure:
	{
		int i;

		if (x->str.name != y->str.name)
			return(FALSE);
		for (i = 0;  i < x->str.length;  i++)
			if (!equal(x->str.self[i], y->str.self[i]))
				return(FALSE);
		return(TRUE);
	}
#endif /* CLOS */

	case t_pathname:
		return(equal(x->pathname.host, y->pathname.host) &&
		       equal(x->pathname.device, y->pathname.device) &&
		       equal(x->pathname.directory, y->pathname.directory) &&
		       equal(x->pathname.name, y->pathname.name) &&
		       equal(x->pathname.type, y->pathname.type) &&
		       equal(x->pathname.version, y->pathname.version));

	default:
		return(eql(x,y));
	}
}

cl_object
cl_equal(cl_object x, cl_object y)
{
	@(return (equal(x, y) ? Ct : Cnil))
}

bool
equalp(cl_object x, cl_object y)
{
	cl_type tx, ty;
	cl_index j;

BEGIN:
	if (eql(x, y))
		return(TRUE);
	tx = type_of(x);
	ty = type_of(y);

	switch (tx) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
	case t_shortfloat:
	case t_longfloat:
	case t_complex:
		if (ty == t_fixnum || ty == t_bignum || ty == t_ratio ||
		    ty == t_shortfloat || ty == t_longfloat ||
		    ty == t_complex)
			return number_equalp(x, y);
		else
			return FALSE;

	case t_vector:
	case t_string:
	case t_bitvector:
		if (ty == t_vector || ty == t_string || ty == t_bitvector) {
		  j = x->vector.fillp;
		  if (j != y->vector.fillp)
		    return FALSE;
		  goto ARRAY;
		}
		else
			return(FALSE);

	case t_array:
		if (ty == t_array && x->array.rank == y->array.rank) {
		  if (x->array.rank > 1) {
		    cl_index i = 0;
		    for (i = 0; i < x->array.rank; i++)
		      if (x->array.dims[i] != y->array.dims[i]) return(FALSE);
		  }
		  if (x->array.dim != y->array.dim)
		    return(FALSE);
		  j=x->array.dim;
		  goto ARRAY;
		}
		else
			return(FALSE);
	default:
	}
	if (tx != ty)
		return(FALSE);
	switch (tx) {
	case t_character:
		return(char_equal(x, y));

	case t_cons:
		if (!equalp(CAR(x), CAR(y)))
			return(FALSE);
		x = CDR(x);
		y = CDR(y);
		goto BEGIN;

#ifdef CLOS
	case t_instance: {
		cl_index i;

		if (CLASS_OF(x) != CLASS_OF(y))
			return(FALSE);
		for (i = 0;  i < x->instance.length;  i++)
			if (!equal(x->instance.slots[i], y->instance.slots[i]))
				return(FALSE);
		return(TRUE);
	}
#else
	case t_structure: {
		cl_index i;

		if (x->str.name != y->str.name)
			return(FALSE);
		for (i = 0;  i < x->str.length;  i++)
			if (!equalp(x->str.self[i], y->str.self[i]))
				return(FALSE);
		return(TRUE);
	}
#endif /* CLOS */

	case t_pathname:
		return(equal(x, y));

	default:
		return(FALSE);
	}

ARRAY:
	{
		cl_index i;

		for (i = 0;  i < j;  i++)
			if (!equalp(aref(x, i), aref(y, i)))
				return(FALSE);
		return(TRUE);
	}
}

cl_object
cl_equalp(cl_object x, cl_object y)
{
	@(return (equalp(x, y) ? Ct : Cnil))
}

cl_object
si_fixnump(cl_object x)
{
	@(return (FIXNUMP(x) ? Ct : Cnil))
}
