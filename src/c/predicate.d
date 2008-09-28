/* -*- mode: c; c-basic-offset: 8 -*- */
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

#include <ecl/ecl.h>
#include <math.h>

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
	@(return (ECL_NUMBER_TYPE_P(t) ? Ct : Cnil))
}

/*	Used in compiled code		*/
bool ecl_numberp(cl_object x)
{
  cl_type t = type_of(x);
  return ECL_NUMBER_TYPE_P(t);
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
	@(return (floatp(x)? Ct : Cnil))
}

bool
floatp(cl_object x)
{
	cl_type t = type_of(x);
	return (t == t_singlefloat) || (t == t_doublefloat)
#ifdef ECL_SHORT_FLOAT
		|| (t == t_shortfloat)
#endif
#ifdef ECL_LONG_FLOAT
		|| (t == t_longfloat)
#endif
		;
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

#ifdef ECL_UNICODE
cl_object
si_base_char_p(cl_object c)
{
	@(return ((CHARACTERP(c) && BASE_CHAR_P(c))? Ct : Cnil))
}
#endif

bool
ecl_stringp(cl_object x)
{
	cl_type t = type_of(x);
#ifdef ECL_UNICODE
	return t == t_base_string || t == t_string;
#else
	return t == t_base_string;
#endif
}

cl_object
cl_stringp(cl_object x)
{
	@(return (ecl_stringp(x)? Ct : Cnil))
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
#ifdef ECL_UNICODE
	@(return ((t == t_vector || t == t_string || t == t_base_string || t == t_bitvector) ? Ct : Cnil))
#else
	@(return ((t == t_vector || t == t_base_string || t == t_bitvector) ? Ct : Cnil))
#endif
}

cl_object
cl_simple_string_p(cl_object x)
{
#ifdef ECL_UNICODE
	cl_type t = type_of(x);
	@(return (((t == t_base_string || (t == t_string)) &&
		     !x->string.adjustable &&
		     !x->string.hasfillp &&
		     Null(CAR(x->string.displaced))) ? Ct : Cnil))
#else
	@(return ((type_of(x) == t_base_string &&
		     !x->base_string.adjustable &&
		     !x->base_string.hasfillp &&
		     Null(CAR(x->base_string.displaced))) ? Ct : Cnil))
#endif

}

#ifdef ECL_UNICODE
cl_object
si_base_string_p(cl_object x)
{
	@(return ((type_of(x) == t_base_string)? Ct : Cnil))
}
#endif

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
	if (t == t_bytecodes || t == t_bclosure || t == t_cfun
	    || t == t_cfunfixed || t == t_cclosure
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
	@(return ((t == t_bytecodes || t == t_bclosure || t == t_cfun
		   || t == t_cfunfixed || t == t_cclosure) ? Ct : Cnil))
}

cl_object
cl_eq(cl_object x, cl_object y)
{
	@(return ((x == y) ? Ct : Cnil))
}

#ifdef signbit
# define float_eql(a,b) (((a) == (b)) && (signbit((a)) == signbit((b))))
#else
# define float_eql(a,b) (((a) == (b)))
#endif

bool
ecl_eql(cl_object x, cl_object y)
{
	cl_type t;
	if (x == y)
		return(TRUE);
	if ((t = type_of(x)) != type_of(y))
		return(FALSE);
	switch (t) {
	case t_fixnum:
		return FALSE;
	case t_bignum:
		return (big_compare(x, y) == 0);
	case t_ratio:
		return (ecl_eql(x->ratio.num, y->ratio.num) &&
			ecl_eql(x->ratio.den, y->ratio.den));
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		return float_eql(ecl_short_float(x),ecl_short_float(y));
#endif
	case t_singlefloat:
		return float_eql(sf(x),sf(y));
	case t_doublefloat:
		return float_eql(df(x),df(y));
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return float_eql(ecl_long_float(x),ecl_long_float(y));
#endif
	case t_complex:
		return (ecl_eql(x->complex.real, y->complex.real) &&
			ecl_eql(x->complex.imag, y->complex.imag));
	case t_character:
		return(CHAR_CODE(x) == CHAR_CODE(y));
	default:
		return FALSE;
	}
}

cl_object
cl_eql(cl_object x, cl_object y)
{
	@(return (ecl_eql(x, y) ? Ct : Cnil))
}

bool
ecl_equal(register cl_object x, cl_object y)
{
	cl_type tx, ty;
BEGIN:
	if (x==y)
		return(TRUE);
	tx = type_of(x);
	ty = type_of(y);
	switch (tx) {
	case t_list:
		if (Null(x) || Null(y)) {
			/* If X is NIL, then X and Y must be EQ */
			return FALSE;
		}
		if (tx != ty || !ecl_equal(CAR(x), CAR(y)))
			return FALSE;
		x = CDR(x);
		y = CDR(y);
		goto BEGIN;
	case t_symbol:
	case t_vector:
	case t_array:
	case t_fixnum:
		return FALSE;
	case t_bignum:
		return (tx == ty) && (big_compare(x,y) == 0);
	case t_ratio:
		return (tx == ty) && ecl_eql(x->ratio.num, y->ratio.num) &&
			ecl_eql(x->ratio.den, y->ratio.den);
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		return (tx == ty) && (ecl_short_float(x) == ecl_short_float(y));
#endif
	case t_singlefloat:
		return (tx == ty) && (sf(x)==sf(y));

	case t_doublefloat:
		return (tx == ty) && (df(x)==df(y));
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return (tx == ty) && (ecl_long_float(x) == ecl_long_float(y));
#endif
	case t_complex:
		return (tx == ty) && ecl_eql(x->complex.real, y->complex.real) &&
			ecl_eql(x->complex.imag, y->complex.imag);
	case t_character:
		return (tx == ty) && (CHAR_CODE(x) == CHAR_CODE(y));
	case t_base_string:
#ifdef ECL_UNICODE
	case t_string:
		if (ty != t_base_string && ty != t_string)
			return FALSE;
#else
		if (ty != t_base_string)
			return FALSE;
#endif
		return ecl_string_eq(x, y);
	case t_bitvector: {
		cl_index i, ox, oy;
		if (ty != tx)
			return FALSE;
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
	case t_pathname:
		return ty == tx &&
		       ecl_equal(x->pathname.host, y->pathname.host) &&
		       ecl_equal(x->pathname.device, y->pathname.device) &&
		       ecl_equal(x->pathname.directory, y->pathname.directory) &&
		       ecl_equal(x->pathname.name, y->pathname.name) &&
		       ecl_equal(x->pathname.type, y->pathname.type) &&
		       ecl_equal(x->pathname.version, y->pathname.version);
	case t_foreign:
		return (tx == ty) && (x->foreign.data == y->foreign.data);
	default:
		return FALSE;
	}
}

cl_object
cl_equal(cl_object x, cl_object y)
{
	@(return (ecl_equal(x, y) ? Ct : Cnil))
}

bool
ecl_equalp(cl_object x, cl_object y)
{
	cl_type tx, ty;
	cl_index j;

BEGIN:
	if (ecl_eql(x, y))
		return(TRUE);
	tx = type_of(x);
	ty = type_of(y);

	switch (tx) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
#endif
	case t_singlefloat:
	case t_doublefloat:
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
#endif
	case t_complex:
		return ECL_NUMBER_TYPE_P(ty) && ecl_number_equalp(x, y);
	case t_vector:
	case t_base_string:
	case t_bitvector:
#ifdef ECL_UNICODE
	case t_string:
		if (ty != t_vector && ty != t_base_string && ty != t_bitvector
		    && ty != t_string)
			return FALSE;
#else
		if (ty != t_vector && ty != t_base_string && ty != t_bitvector)
			return FALSE;
#endif
		j = x->vector.fillp;
		if (j != y->vector.fillp)
			return FALSE;
		goto ARRAY;
	case t_array:
		if (ty != t_array || x->array.rank != y->array.rank)
			return FALSE;
		if (x->array.rank > 1) {
			cl_index i = 0;
			for (i = 0; i < x->array.rank; i++)
				if (x->array.dims[i] != y->array.dims[i]) return(FALSE);
		}
		if (x->array.dim != y->array.dim)
			return(FALSE);
		j=x->array.dim;
	ARRAY: {
		cl_index i;
		for (i = 0;  i < j;  i++)
			if (!ecl_equalp(ecl_aref(x, i), ecl_aref(y, i)))
				return(FALSE);
		return(TRUE);
	}
	default:;
	}
	if (tx != ty)
		return FALSE;
	switch (tx) {
	case t_character:
		return ecl_char_equal(x, y);
	case t_list:
		if (Null(x) || Null(y)) {
			/* X is NIL but it is not EQ to Y */
			return FALSE;
		}
		if (!ecl_equalp(CAR(x), CAR(y)))
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
			if (!ecl_equalp(x->instance.slots[i], y->instance.slots[i]))
				return(FALSE);
		return(TRUE);
	}
#else
	case t_structure: {
		cl_index i;

		if (x->str.name != y->str.name)
			return(FALSE);
		for (i = 0;  i < x->str.length;  i++)
			if (!ecl_equalp(x->str.self[i], y->str.self[i]))
				return(FALSE);
		return(TRUE);
	}
#endif /* CLOS */
	case t_pathname:
		return ecl_equal(x, y);
	case t_hashtable: {
		cl_index i;
		struct ecl_hashtable_entry *ex, *ey;

		if (x->hash.entries != y->hash.entries ||
		    x->hash.test != y->hash.test)
			return(FALSE);
		ex = x->hash.data;
		for (i = 0;  i < x->hash.size;  i++) {
			if (ex[i].key != OBJNULL) {
				ey = ecl_search_hash(ex[i].key, y);
				if (ey->key == OBJNULL || !ecl_equalp(ex[i].value, ey->value))
					return(FALSE);
			}
		}
		return TRUE;
	}
	case t_random:
		return ecl_equalp(x->random.value, y->random.value);
	default:
		return FALSE;
	}
}

cl_object
cl_equalp(cl_object x, cl_object y)
{
	@(return (ecl_equalp(x, y) ? Ct : Cnil))
}

cl_object
si_fixnump(cl_object x)
{
	@(return (FIXNUMP(x) ? Ct : Cnil))
}
