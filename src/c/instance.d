/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    instance.c -- CLOS interface.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <string.h>

cl_object
ecl_allocate_instance(cl_object clas, cl_index size)
{
	cl_object x = cl_alloc_instance(size);
	cl_index i;
	CLASS_OF(x) = clas;
	for (i = 0;  i < size;  i++)
		x->instance.slots[i] = ECL_UNBOUND;
	return(x);
}

cl_object
si_allocate_raw_instance(cl_object orig, cl_object clas, cl_object size)
{
	cl_object output = ecl_allocate_instance(clas, fixnnint(size));
	if (orig == Cnil) {
		orig = output;
	} else {
		orig->instance.clas = clas;
		orig->instance.length = output->instance.length;
		orig->instance.slots = output->instance.slots;
	}
	@(return orig)
}

cl_object
si_instance_sig(cl_object x)
{
	@(return x->instance.sig);
}

cl_object
si_instance_sig_set(cl_object x)
{
	@(return (x->instance.sig = CLASS_SLOTS(CLASS_OF(x))));
}

cl_object
si_instance_class(cl_object x)
{
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'ext::instance', x);
	@(return CLASS_OF(x))
}

cl_object
si_instance_class_set(cl_object x, cl_object y)
{
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'ext::instance', x);
	if (type_of(y) != t_instance)
		FEwrong_type_argument(@'ext::instance', y);
	CLASS_OF(x) = y;
	@(return x)
}

cl_object
ecl_instance_ref(cl_object x, cl_fixnum i)
{
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'ext::instance', x);
	if (i < 0 || i >= (cl_fixnum)x->instance.length)
	        FEtype_error_index(x, MAKE_FIXNUM(i));
	return(x->instance.slots[i]);
}

cl_object
si_instance_ref(cl_object x, cl_object index)
{
	cl_fixnum i;

	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'ext::instance', x);
	if (!FIXNUMP(index) ||
	    (i = fix(index)) < 0 || i >= (cl_fixnum)x->instance.length)
	        FEtype_error_index(x, index);
	@(return x->instance.slots[i])
}

cl_object
si_instance_ref_safe(cl_object x, cl_object index)
{
	cl_fixnum i;

	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'ext::instance', x);
	if (!FIXNUMP(index) ||
	    (i = fix(index)) < 0 || i >= x->instance.length)
	        FEtype_error_index(x, index);
	x = x->instance.slots[i];
	if (x == ECL_UNBOUND)
		cl_error(5, @'unbound-slot', @':name', index, @':instance', x);
	@(return x)
}

cl_object
ecl_instance_set(cl_object x, cl_fixnum i, cl_object v)
{
        if (type_of(x) != t_instance)
                FEwrong_type_argument(@'ext::instance', x);
	if (i >= x->instance.length || i < 0)
	        FEtype_error_index(x, MAKE_FIXNUM(i));
	x->instance.slots[i] = v;
	return(v);
}

cl_object
si_instance_set(cl_object x, cl_object index, cl_object value)
{
	cl_fixnum i;

	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'ext::instance', x);
	if (!FIXNUMP(index) ||
	    (i = fix(index)) >= (cl_fixnum)x->instance.length || i < 0)
		FEtype_error_index(x, index);
	x->instance.slots[i] = value;
	@(return value)
}

cl_object
si_instancep(cl_object x)
{
	@(return ((type_of(x) == t_instance) ? Ct : Cnil))
}

cl_object
si_unbound()
{
	/* Returns an object that cannot be read or written and which
	   is used to represent an unitialized slot */
	@(return ECL_UNBOUND)
}

cl_object
si_sl_boundp(cl_object x)
{
	@(return ((x == ECL_UNBOUND) ? Cnil : Ct))
}

cl_object
si_sl_makunbound(cl_object x, cl_object index)
{
	cl_fixnum i;

	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'ext::instance', x);
	if (!FIXNUMP(index) ||
	    (i = fix(index)) >= x->instance.length || i < 0)
		FEtype_error_index(x, index);
	x->instance.slots[i] = ECL_UNBOUND;
	@(return x)
}

cl_object
si_copy_instance(cl_object x)
{
	cl_object y;

	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'ext::instance', x);
	y = ecl_allocate_instance(x->instance.clas, x->instance.length);
	y->instance.sig = x->instance.sig;
	memcpy(y->instance.slots, x->instance.slots,
	       x->instance.length * sizeof(cl_object));
	@(return y)
}

@(defun find-class (name &optional (errorp Ct) env)
	cl_object class;
@
	class = ecl_gethash_safe(name, SYM_VAL(@'si::*class-name-hash-table*'), Cnil);
	if (class == Cnil) {
		if (!Null(errorp))
			FEerror("No class named ~S.", 1, name);
	}
	@(return class)
@)

cl_object
cl_class_of(cl_object x)
{
	cl_object t;

	switch (type_of(x)) {
	case t_instance:
		return CLASS_OF(x);
	case t_fixnum:
	case t_bignum:
		t = @'integer'; break;
	case t_ratio:
		t = @'ratio'; break;
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
#endif
	case t_singlefloat:
	case t_doublefloat:
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
#endif
		t = @'float'; break;
		/* XXX t = @'long-float'; break; */
	case t_complex:
		t = @'complex'; break;
	case t_character:
		t = @'character'; break;
	case t_symbol:
		if (x->symbol.hpack == cl_core.keyword_package)
			t = @'keyword';
		else
			t = @'symbol';
		break;
	case t_package:
		t = @'package'; break;
	case t_list:
		t = Null(x)? @'null' : @'cons'; break;
	case t_hashtable:
		t = @'hash-table'; break;
	case t_array:
		t = @'array'; break;
	case t_vector:
		t = @'vector'; break;
#ifdef ECL_UNICODE
	case t_string:
		t = @'string'; break;
	case t_base_string:
		t = @'base-string'; break;
#else
	case t_base_string:
		t = @'string'; break;
#endif
	case t_bitvector:
		t = @'bit-vector'; break;
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
		t = @'pathname'; break;
	case t_random:
		t = @'random-state'; break;
	case t_bytecodes:
	case t_cfun:
	case t_cclosure:
		t = @'function'; break;
#ifdef ECL_THREADS
	case t_process:
		t = @'mp::process'; break;
	case t_lock:
		t = @'mp::lock'; break;
	case t_condition_variable:
		t = @'mp::condition-variable'; break;
#endif
	case t_codeblock:
		t = @'si::code-block'; break;
	case t_foreign:
		t = @'si::foreign-data'; break;
	case t_frame:
		t = @'si::frame'; break;
	default:
		ecl_internal_error("not a lisp data object");
	}
	t = cl_find_class(2, t, Cnil);
	if (t == Cnil)
		t = cl_find_class(1, Ct);
	@(return t)
}

cl_object
ecl_slot_value(cl_object x, const char *slot)
{
	cl_object slot_name = c_string_to_object(slot);
	return funcall(3, @'slot-value', x, slot_name);
}

cl_object
ecl_slot_value_set(cl_object x, const char *slot, cl_object value)
{
	cl_object slot_name = c_string_to_object(slot);
	cl_object slot_setter = c_string_to_object("(SETF SLOT-VALUE)");
	return funcall(4, ecl_fdefinition(slot_setter), value, x, slot_name);
}
