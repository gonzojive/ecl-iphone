/*
    typespec.c -- Type specifier routines.
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


void
FEtype_error_character(cl_object x) {
	FEwrong_type_argument(@'character', x);
}

void
FEtype_error_cons(cl_object x) {
	FEwrong_type_argument(@'cons', x);
}

void
FEtype_error_number(cl_object x) {
	FEwrong_type_argument(@'number', x);
}

void
FEtype_error_real(cl_object x) {
	FEwrong_type_argument(@'real', x);
}

void
FEtype_error_float(cl_object x) {
	FEwrong_type_argument(@'float', x);
}

void
FEtype_error_integer(cl_object x) {
	FEwrong_type_argument(@'integer', x);
}

void
FEtype_error_list(cl_object x) {
	FEwrong_type_argument(@'list', x);
}

void
FEtype_error_proper_list(cl_object x) {
	cl_error(9, @'simple-type-error', @':format-control',
		    make_simple_string("Not a proper list ~D"),
		    @':format-arguments', cl_list(1, x),
		    @':expected-type', @'list',
		    @':datum', x);
}

void
FEtype_error_alist(cl_object x)
{
	cl_error(9, @'simple-type-error', @':format-control',
		    make_simple_string("Not a valid association list ~D"),
		    @':format-arguments', cl_list(1, x),
		    @':expected-type', @'list',
		    @':datum', x);
}

void
FEcircular_list(cl_object x)
{
	/* FIXME: Is this the right way to rebind it? */
	bds_bind(@'*print-circle*', Ct);
	cl_error(9, @'simple-type-error', @':format-control',
		    make_simple_string("Circular list ~D"),
		    @':format-arguments', cl_list(1, x),
		    @':expected-type', @'list',
		    @':datum', x);
}

void
FEtype_error_index(cl_object seq, cl_object ndx)
{
	cl_error(9, @'simple-type-error', @':format-control',
		    make_simple_string("~S is not a valid index within the sequence ~S"),
		    @':format-arguments', cl_list(2, seq, ndx),
		    @':expected-type', @'fixnum',
		    @':datum', ndx);
}

void
FEtype_error_string(cl_object s)
{
	FEwrong_type_argument(@'string', s);
}

void
FEtype_error_stream(cl_object strm)
{
	FEwrong_type_argument(@'stream', strm);
}

/**********************************************************************/

void
assert_type_integer(cl_object p)
{
	cl_type t = type_of(p);
	if (t != t_fixnum && t != t_bignum)
		FEtype_error_integer(p);
}

void
assert_type_non_negative_integer(cl_object p)
{
	cl_type t = type_of(p);

	if (t == t_fixnum) {
		if (FIXNUM_PLUSP(p))
			return;
	} else if (t == t_bignum) {
		if (big_sign(p) >= 0)
			return;
	}
	FEwrong_type_argument(c_string_to_object("(REAL 0 *)"), p);
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
		FEwrong_type_argument(@'symbol', p);
}

void
assert_type_package(cl_object p)
{
	if (type_of(p) != t_package)
		FEwrong_type_argument(@'package', p);
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
		FEwrong_type_argument(@'cons', p);
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
	if (cl_list_length(p) == Cnil)
		FEcircular_list(p);
}

void
assert_type_readtable(cl_object p)
{
	if (type_of(p) != t_readtable)
		FEwrong_type_argument(@'readtable', p);
}

void
assert_type_hash_table(cl_object p)
{
	if (type_of(p) != t_hashtable)
		FEwrong_type_argument(@'hash-table', p);
}

void
assert_type_array(cl_object p)
{
	if (!ARRAYP(p))
		FEwrong_type_argument(@'array', p);
}

void
assert_type_vector(cl_object p)
{
	if (!VECTORP(p))
		FEwrong_type_argument(@'vector', p);
}

cl_object
cl_type_of(cl_object x)
{
	cl_object t;
	switch (type_of(x)) {
#ifdef CLOS
        case t_instance:
		{ cl_object cl = CLASS_OF(x);
		  if (CLASS_NAME(cl) != Cnil)
		    t = CLASS_NAME(cl);
		  else
		    t = cl;
		}
		break;
#endif
	case t_fixnum:
		t = @'fixnum'; break;

	case t_bignum:
		t = @'bignum'; break;

	case t_ratio:
		t = @'ratio'; break;

	case t_shortfloat:
		t = @'short-float'; break;

	case t_longfloat:
		t = @'long-float'; break;

	case t_complex:
		t = @'complex'; break;

	case t_character: {
		int i = CHAR_CODE(x);
		if ((' ' <= i && i < '\177') || i == '\n')
			t = @'standard-char';
		else
			t = @'base-char';
		break;
	}

	case t_symbol:
		if (x == Cnil)
			t = @'null';
		else if (x == Ct)
			t = @'boolean';
		else if (x->symbol.hpack == cl_core.keyword_package)
			t = @'keyword';
		else
			t = @'symbol';
		break;

	case t_package:
		t = @'package'; break;

	case t_cons:
		t = @'cons'; break;

	case t_hashtable:
		t = @'hash-table'; break;

	case t_array:
		if (x->array.adjustable ||
		    Null(CAR(x->array.displaced)))
			t = @'array';
		else
			t = @'simple-array';
		break;

	case t_vector:
		if (x->vector.adjustable ||
		    x->vector.hasfillp ||
		    Null(CAR(x->vector.displaced)) ||
		    (cl_elttype)x->vector.elttype != aet_object)
			t = @'vector';
		else
			t = @'simple-vector';
		break;

	case t_string:
		if (x->string.adjustable ||
		    x->string.hasfillp ||
		    Null(CAR(x->string.displaced)))
			t = @'string';
		else
			t = @'simple-string';
		break;

	case t_bitvector:
		if (x->vector.adjustable ||
		    x->vector.hasfillp ||
		    Null(CAR(x->vector.displaced)))
			t = @'bit-vector';
		else
			t = @'simple-bit-vector';
		break;

#ifndef CLOS
	case t_structure:
		t = x->str.name; break;
#endif

	case t_stream:
		switch (x->stream.mode) {
		case smm_synonym:	t = @'synonym-stream'; break;
		case smm_broadcast:	t = @'broadcast-stream'; break;
		case smm_concatenated:	t = @'concatenated-stream'; break;
		case smm_two_way:	t =  @'two-way-stream'; break;
		case smm_string_input:
		case smm_string_output:	t = @'string-stream'; break;
		case smm_echo:		t = @'echo-stream'; break;
		default:		t = @'file-stream'; break;
		}
		break;

	case t_readtable:
		t = @'readtable'; break;

	case t_pathname:
		t = x->pathname.logical? @'logical-pathname' : @'pathname';
		break;

	case t_random:
		t = @'random-state'; break;

	case t_bytecodes:
	case t_cfun:
	case t_cclosure:
		t = @'function'; break;

#ifdef ECL_FFI
	case t_foreign:
		t = @'si::foreign-data'; break;
#endif
#ifdef ECL_THREADS
	case t_process:
		t = @'mp::process'; break;
	case t_lock:
		t = @'mp::lock'; break;
#endif
	default:
		error("not a lisp data object");
	}
	return1(t);
}
