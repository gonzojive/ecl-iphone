/*
    typespec.c -- Type specifier routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecls.h"

/******************************* EXPORTS ******************************/

cl_object Squote;
cl_object Slambda;
cl_object Sspecial;

cl_object Ssubtypep;

cl_object
St,		Snil,		Scommon,	Ssequence,
Snull,		Scons,		Slist,		Ssymbol,
Sarray,		Svector,	Sbit_vector,	Sstring,
Ssimple_array,	Ssimple_vector,	Ssimple_string,	Ssimple_bit_vector,
Sfunction,	Spathname,	Scharacter,	Scompiled_function,
Snumber,	Srational,	Sfloat,		Sreal,
Sinteger,	Sratio,		Sshort_float,	Sstandard_char,
Sfixnum,	Scomplex,	Ssingle_float,	Spackage,
Sbignum,	Srandom_state,	Sdouble_float,	Sstream,
Sbit,		Sreadtable,	Slong_float,	Shash_table,
Ssigned_char,	Sunsigned_char,	Ssigned_short,	Sunsigned_short,
Sbase_char,	Sextended_char,	Slogical_pathname;

#ifdef THREADS
cl_object Scont, Sthread;
#endif THREADS

#ifdef CLOS
cl_object Sinstance, Sdispatch_function;
#endif

#ifdef LOCATIVE
cl_object Slocative;
#endif

cl_object Sstructure,	Ssatisfies,	Smember,	Snot,	Sor,	Sand;
cl_object Svalues,	Smod,		Ssigned_byte,	Sunsigned_byte;

cl_object SX;		/*  symbol *  */
cl_object Splusp;

cl_object TSnon_negative_integer;
cl_object TSpositive_number;

/******************************* ------- ******************************/

cl_object Skeyword;

/**********************************************************************/

void
FEtype_error_character(cl_object x) {
	FEwrong_type_argument(Scharacter, x);
}

void
FEtype_error_cons(cl_object x) {
	FEwrong_type_argument(Scons, x);
}

void
FEtype_error_number(cl_object x) {
	FEwrong_type_argument(Snumber, x);
}

void
FEtype_error_real(cl_object x) {
	FEwrong_type_argument(Sreal, x);
}

void
FEtype_error_float(cl_object x) {
	FEwrong_type_argument(Sfloat, x);
}

void
FEtype_error_integer(cl_object x) {
	FEwrong_type_argument(Sinteger, x);
}

void
FEtype_error_list(cl_object x) {
	FEwrong_type_argument(Slist, x);
}

void
FEtype_error_proper_list(cl_object x) {
	FEcondition(9, Ssimple_type_error, Kformat_control,
		    make_simple_string("Not a proper list ~D"),
		    Kformat_arguments, list(1, x),
		    Kexpected_type, Slist,
		    Kdatum, x);
}

void
FEtype_error_alist(cl_object x)
{
	FEcondition(9, Ssimple_type_error, Kformat_control,
		    make_simple_string("Not a valid association list ~D"),
		    Kformat_arguments, list(1, x),
		    Kexpected_type, Slist,
		    Kdatum, x);
}

void
FEtype_error_plist(cl_object x)
{
	FEcondition(9, Ssimple_type_error, Kformat_control,
		    make_simple_string("Not a valid property list ~D"),
		    Kformat_arguments, list(1, x),
		    Kexpected_type, Slist,
		    Kdatum, x);
}

void
FEcircular_list(cl_object x)
{
	/* FIXME: Is this the right way to rebind it? */
	bds_bind(Vprint_circle, Ct);
	FEcondition(9, Ssimple_type_error, Kformat_control,
		    make_simple_string("Circular list ~D"),
		    Kformat_arguments, list(1, x),
		    Kexpected_type, Slist,
		    Kdatum, x);
}

void
FEtype_error_index(cl_object x)
{
	FEcondition(9, Ssimple_type_error, Kformat_control,
		    make_simple_string("Index out of bounds ~D"),
		    Kformat_arguments, list(1, x),
		    Kexpected_type, Sfixnum,
		    Kdatum, x);
}

void
FEtype_error_string(cl_object s)
{
	FEwrong_type_argument(Sstring, s);
}

void
FEtype_error_stream(cl_object strm)
{
	FEwrong_type_argument(Sstream, strm);
}

/**********************************************************************/

void
assert_type_integer(cl_object p)
{
	enum type t = type_of(p);
	if (t != t_fixnum && t != t_bignum)
		FEtype_error_integer(p);
}

void
assert_type_non_negative_integer(cl_object p)
{
	enum type t = type_of(p);

	if (t == t_fixnum) {
		if (FIXNUM_PLUSP(p))
			return;
	} else if (t == t_bignum) {
		if (big_sign(p) >= 0)
			return;
	}
	FEwrong_type_argument(TSnon_negative_integer, p);
}

void
assert_type_character(cl_object p)
{
	if (!CHARACTERP(p))
		FEtype_error_character(p);
}

void
assert_type_symbol(cl_object p)
{
	if (!SYMBOLP(p))
		FEwrong_type_argument(Ssymbol, p);
}

void
assert_type_package(cl_object p)
{
	if (type_of(p) != t_package)
		FEwrong_type_argument(Spackage, p);
}

void
assert_type_string(cl_object p)
{
	if (type_of(p) != t_string)
		FEtype_error_string(p);
}

void
assert_type_cons(cl_object p)
{
	if (ATOM(p))
		FEwrong_type_argument(Scons, p);
}

void
assert_type_list(cl_object p)
{
	if (ATOM(p) && p != Cnil)
		FEtype_error_list(p);
}

void
assert_type_proper_list(cl_object p)
{
	if (ATOM(p) && p != Cnil)
		FEtype_error_list(p);
	if (list_length(p) == Cnil)
		FEcircular_list(p);
}

void
assert_type_stream(cl_object p)
{
	if (type_of(p) != t_stream)
		FEwrong_type_argument(Sstream, p);
}

void
assert_type_readtable(cl_object p)
{
	if (type_of(p) != t_readtable)
		FEwrong_type_argument(Sreadtable, p);
}

void
assert_type_hash_table(cl_object p)
{
	if (type_of(p) != t_hashtable)
		FEwrong_type_argument(Shash_table, p);
}

void
assert_type_array(cl_object p)
{
	if (!ARRAYP(p))
		FEwrong_type_argument(Sarray, p);
}

void
assert_type_vector(cl_object p)
{
	if (!VECTORP(p))
		FEwrong_type_argument(Svector, p);
}

cl_object
TYPE_OF(cl_object x)
{
	switch (type_of(x)) {
#ifdef CLOS
        case t_instance:
		{ cl_object cl = CLASS_OF(x);
		  if (CLASS_NAME(cl) != Cnil)
		    return(CLASS_NAME(cl));
		  else
		    return(cl);
		}
#endif

	case t_fixnum:
		return(Sfixnum);

	case t_bignum:
		return(Sbignum);

	case t_ratio:
		return(Sratio);

	case t_shortfloat:
		return(Sshort_float);

	case t_longfloat:
		return(Slong_float);

	case t_complex:
		return(Scomplex);

	case t_character: {
		int i = CHAR_CODE(x);
		if ((' ' <= i && i < '\177') || i == '\n')
			return(Sstandard_char);
		else
			return(Sbase_char);
	}

	case t_symbol:
		if (x == Cnil)
			return(Snull);
		if (x->symbol.hpack == keyword_package)
			return(Skeyword);
		else
			return(Ssymbol);

	case t_package:
		return(Spackage);

	case t_cons:
		return(Scons);

	case t_hashtable:
		return(Shash_table);

	case t_array:
		if (x->array.adjustable ||
		    Null(CAR(x->array.displaced)))
			return(Sarray);
		else
			return(Ssimple_array);

	case t_vector:
		if (x->vector.adjustable ||
		    x->vector.hasfillp ||
		    Null(CAR(x->vector.displaced)) ||
		    (enum aelttype)x->vector.elttype != aet_object)
			return(Svector);
		else
			return(Ssimple_vector);

	case t_string:
		if (x->string.adjustable ||
		    x->string.hasfillp ||
		    Null(CAR(x->string.displaced)))
			return(Sstring);
		else
			return(Ssimple_string);

	case t_bitvector:
		if (x->vector.adjustable ||
		    x->vector.hasfillp ||
		    Null(CAR(x->vector.displaced)))
			return(Sbit_vector);
		else
			return(Ssimple_bit_vector);

#ifndef CLOS
	case t_structure:
		return(x->str.name);
#endif CLOS

	case t_stream:
		return(Sstream);

	case t_readtable:
		return(Sreadtable);

	case t_pathname:
		if (x->pathname.logical)
			return Slogical_pathname;
		return(Spathname);

	case t_random:
		return(Srandom_state);

	case t_bytecodes:
	case t_cfun:
	case t_cclosure:
		return(Scompiled_function);

#ifdef THREADS
	case t_cont:
		return(Scont);

	case t_thread:
		return(Sthread);
#endif THREADS
#ifdef CLOS
	case t_gfun:
		return(Sdispatch_function);
#endif
#ifdef LOCATIVE
	      case t_locative:
		return(Slocative);
#endif

	default:
		error("not a lisp data object");
	}
}

@(defun type_of (x)
@
	@(return TYPE_OF(x))
@)

void
init_typespec(void)
{

	TSnon_negative_integer
	= CONS(Sinteger,
		    CONS(MAKE_FIXNUM(0), CONS(SX, Cnil)));
	register_root(&TSnon_negative_integer);
	TSpositive_number = CONS(Ssatisfies, CONS(Splusp, Cnil));
	register_root(&TSpositive_number);
}				
