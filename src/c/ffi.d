/*
    ffi.c -- User defined data types and foreign functions interface.
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <string.h>
#include "ecl.h"

cl_object
ecl_make_foreign_data(cl_object tag, cl_index size, void *data)
{
	cl_object output = cl_alloc_object(t_foreign);
	output->foreign.tag = tag == Cnil ? @':void' : tag;
	output->foreign.size = size;
	output->foreign.data = (char*)data;
	return output;
}

cl_object
ecl_allocate_foreign_data(cl_object tag, cl_index size)
{
	cl_object output = cl_alloc_object(t_foreign);
	output->foreign.tag = tag;
	output->foreign.size = size;
	output->foreign.data = (char*)cl_alloc_atomic(size);
	return output;
}

void *
ecl_foreign_data_pointer_safe(cl_object f)
{
	if (type_of(f) != t_foreign)
		FEwrong_type_argument(@'si::foreign-data', f);
	return f->foreign.data;
}

char *
ecl_string_pointer_safe(cl_object f)
{
	cl_index l;
	unsigned char *s;
	assert_type_string(f);
	s = f->string.self;
	if (f->string.hasfillp && s[f->string.fillp] != 0) {
		FEerror("Cannot coerce a string with fill pointer to (char *)", 0);
	}
	return (char *)s;
}

cl_object
ecl_null_terminated_string(cl_object f)
{
	assert_type_string(f);
	if (f->string.hasfillp && f->string.self[f->string.fillp] != 0) {
		return cl_copy_seq(f);
	} else {
		return f;
	}
}

cl_object
si_allocate_foreign_data(cl_object tag, cl_object size)
{
	cl_object output = cl_alloc_object(t_foreign);
	cl_index bytes = fixnnint(size);
	output->foreign.tag = tag;
	output->foreign.size = bytes;
	output->foreign.data = bytes? cl_alloc_atomic(bytes) : NULL;
	@(return output)
}

cl_object
si_free_foreign_data(cl_object f)
{
	if (type_of(f) != t_foreign) {
		FEwrong_type_argument(@'si::foreign-data', f);
	}
	if (f->foreign.size) {
		cl_dealloc(f->foreign.data, f->foreign.size);
	}
	f->foreign.size = 0;
	f->foreign.data = NULL;
}

cl_object
si_foreign_data_address(cl_object f)
{
	if (type_of(f) != t_foreign) {
		FEwrong_type_argument(@'si::foreign-data', f);
	}
	@(return make_unsigned_integer((cl_index)f->foreign.data))
}

cl_object
si_foreign_data_tag(cl_object f)
{
	if (type_of(f) != t_foreign) {
		FEwrong_type_argument(@'si::foreign-data', f);
	}
	@(return f->foreign.tag);
}

cl_object
si_foreign_data_pointer(cl_object f, cl_object andx, cl_object asize,
			cl_object tag)
{
	cl_index ndx = fixnnint(andx);
	cl_index size = fixnnint(asize);
	cl_object output;

	if (type_of(f) != t_foreign) {
		FEwrong_type_argument(@'si::foreign-data', f);
	}
	if (ndx >= f->foreign.size || (f->foreign.size - ndx) < size) {
		FEerror("Out of bounds reference into foreign data type ~A.", 1, f);
	}
	output = cl_alloc_object(t_foreign);
	output->foreign.tag = tag;
	output->foreign.size = size;
	output->foreign.data = f->foreign.data + ndx;
	@(return output)
}

cl_object
si_foreign_data_ref(cl_object f, cl_object andx, cl_object asize, cl_object tag)
{
	cl_index ndx = fixnnint(andx);
	cl_index size = fixnnint(asize);
	cl_object output;

	if (type_of(f) != t_foreign) {
		FEwrong_type_argument(@'si::foreign-data', f);
	}
	if (ndx >= f->foreign.size || (f->foreign.size - ndx) < size) {
		FEerror("Out of bounds reference into foreign data type ~A.", 1, f);
	}
	output = ecl_allocate_foreign_data(tag, size);
	memcpy(output->foreign.data, f->foreign.data + ndx, size);
	@(return output)
}

cl_object
si_foreign_data_set(cl_object f, cl_object andx, cl_object value)
{
	cl_index ndx = fixnnint(andx);
	cl_index size, limit;
	cl_object output;

	if (type_of(f) != t_foreign) {
		FEwrong_type_argument(@'si::foreign-data', f);
	}
	if (type_of(value) != t_foreign) {
		FEwrong_type_argument(@'si::foreign-data', value);
	}
	size = value->foreign.size;
	limit = f->foreign.size;
	if (ndx >= limit || (limit - ndx) < size) {
		FEerror("Out of bounds reference into foreign data type ~A.", 1, f);
	}
	memcpy(f->foreign.data + ndx, value->foreign.data, size);
	@(return value)
}

cl_object
si_foreign_data_ref_elt(cl_object f, cl_object andx, cl_object tag)
{
	cl_object output;
	cl_index ndx = fixnnint(andx);
	cl_index limit = f->foreign.size;
	void *p;

	if (type_of(f) != t_foreign) {
		FEwrong_type_argument(@'si::foreign-data', f);
	}
	if (ndx >= limit) {
	ERROR:	FEerror("Out of bounds reference into foreign data type ~A.", 1, f);
	}
	p = (void*)(f->foreign.data + ndx);
	if (tag == @':byte') {
		if (ndx + sizeof(int8_t) > limit) goto ERROR;
		output = MAKE_FIXNUM(*(int8_t *)p);
	} else if (tag == @':unsigned-byte') {
		if (ndx + sizeof(uint8_t) > limit) goto ERROR;
		output = MAKE_FIXNUM(*(uint8_t *)p);
	} else if (tag == @':char') {
		if (ndx + sizeof(char) > limit) goto ERROR;
		output = CODE_CHAR(*(char *)p);
	} else if (tag == @':unsigned-char') {
		if (ndx + sizeof(unsigned char) > limit) goto ERROR;
		output = CODE_CHAR(*(unsigned char *)p);
	} else if (tag == @':short') {
		if (ndx + sizeof(short) > limit) goto ERROR;
		output = MAKE_FIXNUM(*(short *)p);
	} else if (tag == @':unsigned-short') {
		if (ndx + sizeof(unsigned short) > limit) goto ERROR;
		output = MAKE_FIXNUM(*(unsigned short *)p);
	} else if (tag == @':int') {
		if (ndx + sizeof(int) > limit) goto ERROR;
		output = make_integer(*(int *)p);
	} else if (tag == @':unsigned-int') {
		if (ndx + sizeof(unsigned int) > limit) goto ERROR;
		output = make_unsigned_integer(*(unsigned int *)p);
	} else if (tag == @':long') {
		if (ndx + sizeof(long) > limit) goto ERROR;
		output = make_integer(*(long *)p);
	} else if (tag == @':unsigned-long') {
		if (ndx + sizeof(unsigned long) > limit) goto ERROR;
		output = make_unsigned_integer(*(unsigned long *)p);
	} else if (tag == @':pointer-void') {
		if (ndx + sizeof(void *) > limit) goto ERROR;
		output = ecl_make_foreign_data(@':pointer-void', 0, *(void **)p);
	} else if (tag == @':cstring') {
		if (ndx + sizeof(char *) > limit) goto ERROR;
		output = *(char **)p ? make_simple_string(*(char **)p) : Cnil;
	} else if (tag == @':object') {
		if (ndx + sizeof(cl_object) > limit) goto ERROR;
		output = *(cl_object *)p;
	} else if (tag == @':float') {
		if (ndx + sizeof(float) > limit) goto ERROR;
		output = make_shortfloat(*(float *)p);
	} else if (tag == @':double') {
		if (ndx + sizeof(double) > limit) goto ERROR;
		output = make_longfloat(*(double *)p);
	} else {
		FEerror("~A does not denote a foreign type.", 1, tag);
	}
	@(return output)
}

cl_object
si_foreign_data_set_elt(cl_object f, cl_object andx, cl_object tag, cl_object value)
{
	cl_index ndx = fixnnint(andx);
	cl_index limit = f->foreign.size;
	void *p;

	if (type_of(f) != t_foreign) {
		FEwrong_type_argument(@'si::foreign-data', f);
	}
	if (ndx >= limit) {
	ERROR:	FEerror("Out of bounds reference into foreign data type ~A.", 1, f);
	}
	p = (void*)(f->foreign.data + ndx);
	if (tag == @':byte') {
		if (ndx + sizeof(int8_t) > limit) goto ERROR;
		*(int8_t *)p = fixint(value);
	} else if (tag == @':unsigned-byte') {
		if (ndx + sizeof(uint8_t) > limit) goto ERROR;
		*(uint8_t *)p = fixnnint(value);
	} else if (tag == @':char') {
		if (ndx + sizeof(char) > limit) goto ERROR;
		*(char *)p = char_code(value);
	} else if (tag == @':unsigned-char') {
		if (ndx + sizeof(unsigned char) > limit) goto ERROR;
		*(unsigned char*)p = char_code(value);
	} else if (tag == @':short') {
		if (ndx + sizeof(short) > limit) goto ERROR;
		*(short *)p = fixint(value);
	} else if (tag == @':unsigned-short') {
		if (ndx + sizeof(unsigned short) > limit) goto ERROR;
		*(unsigned short *)p = fixnnint(value);
	} else if (tag == @':int') {
		if (ndx + sizeof(int) > limit) goto ERROR;
		*(int *)p = fixint(value);
	} else if (tag == @':unsigned-int') {
		if (ndx + sizeof(unsigned int) > limit) goto ERROR;
		*(unsigned int *)p = fixnnint(value);
	} else if (tag == @':long') {
		if (ndx + sizeof(long) > limit) goto ERROR;
		*(long *)p = fixint(value);
	} else if (tag == @':unsigned-long') {
		if (ndx + sizeof(unsigned long) > limit) goto ERROR;
		*(unsigned long *)p = fixnnint(value);
	} else if (tag == @':pointer-void') {
		if (ndx + sizeof(void *) > limit) goto ERROR;
		*(void **)p = ecl_foreign_data_pointer_safe(value);
	} else if (tag == @':cstring') {
		if (ndx + sizeof(void *) > limit) goto ERROR;
		*(char **)p = value == Cnil ? NULL : value->string.self;
	} else if (tag == @':object') {
		if (ndx + sizeof(cl_object) > limit) goto ERROR;
		*(cl_object *)p = value;
	} else if (tag == @':float') {
		if (ndx + sizeof(float) > limit) goto ERROR;
		*(float *)p = object_to_float(value);
	} else if (tag == @':double') {
		if (ndx + sizeof(double) > limit) goto ERROR;
		*(double *)p = object_to_double(value);
	} else {
		FEerror("~A does not denote a foreign type.", 1, tag);
	}
	@(return value)
}

cl_object
si_size_of_foreign_elt_type(cl_object tag)
{
	cl_fixnum size;

	if (tag == @':byte') {
		size = sizeof(int8_t);
	} else if (tag == @':unsigned-byte') {
		size = sizeof(uint8_t);
	} else if (tag == @':char') {
		size = sizeof(char);
	} else if (tag == @':unsigned-char') {
		size = sizeof(unsigned char);
	} else if (tag == @':short') {
		size = sizeof(short);
	} else if (tag == @':unsigned-short') {
		size = sizeof(unsigned short);
	} else if (tag == @':int') {
		size = sizeof(int);
	} else if (tag == @':unsigned-int') {
		size = sizeof(unsigned int);
	} else if (tag == @':long') {
		size = sizeof(long);
	} else if (tag == @':unsigned-long') {
		size = sizeof(unsigned long);
	} else if (tag == @':pointer-void') {
		size = sizeof(void *);
        } else if (tag == @':cstring') {
		size = sizeof(char*);
	} else if (tag == @':object') {
		size = sizeof(cl_object);
	} else if (tag == @':float') {
		size = sizeof(float);
	} else if (tag == @':double') {
		size = sizeof(double);
	} else {
		FEerror("~A does not denote a foreign type.", 1, tag);
	}
	@(return MAKE_FIXNUM(size))
}

cl_object
si_null_pointer_p(cl_object f)
{
	if (type_of(f) != t_foreign)
		FEwrong_type_argument(@'si::foreign-data', f);
	@(return ((f->foreign.data == NULL)? Ct : Cnil))
}

cl_object
si_foreign_data_recast(cl_object f, cl_object size, cl_object tag)
{
	if (type_of(f) != t_foreign)
		FEwrong_type_argument(@'si::foreign-data', f);
	f->foreign.size = fixnnint(size);
	f->foreign.tag = tag;
	@(return f)
}

cl_object
si_load_foreign_module(cl_object filename)
{
	cl_object libraries;
	cl_object output;
	int i;

#ifdef ECL_THREADS
	mp_get_lock(1, symbol_value(@'mp::+load-compile-lock+'));
	CL_UNWIND_PROTECT_BEGIN {
#endif
	libraries = cl_core.libraries;
	for (i=0; i<libraries->vector.fillp; i++)
		if (cl_stringE(2, libraries->vector.self.t[i]->cblock.name, filename) != Cnil)
		{
			output = libraries->vector.self.t[i];
			goto OUTPUT;
		}
	output = ecl_library_open(filename);
	if (output->cblock.handle == NULL)
	{
		output = ecl_library_error(output);
		ecl_library_close(output);
	}
OUTPUT:
#ifdef ECL_THREADS
	(void)0; /* MSVC complains about missing ';' before '}' */
	} CL_UNWIND_PROTECT_EXIT {
	mp_giveup_lock(symbol_value(@'mp::+load-compile-lock+'));
	} CL_UNWIND_PROTECT_END;
#endif
	if (type_of(output) == t_codeblock)
		@(return output)
	else
		FEerror("LOAD-FOREIGN-MODULE: Could not load foreign module ~S (Error: ~S)", 2, filename, output);
}

cl_object
si_find_foreign_symbol(cl_object var, cl_object module, cl_object type, cl_object size)
{
	cl_object block;
	cl_object output;
	void *sym;

	block = (module == @':default' ? module : si_load_foreign_module(module));
	var = ecl_null_terminated_string(var);
	sym = ecl_library_symbol(block, var->string.self);
	if (sym == NULL) {
		if (block != @':default')
			output = ecl_library_error(block);
		goto OUTPUT;
	}
	output = ecl_make_foreign_data(type, object_to_fixnum(size), sym);
OUTPUT:
	if (type_of(output) == t_foreign)
		@(return output)
	else
		FEerror("FIND-FOREIGN-SYMBOL: Could not load foreign symbol ~S from module ~S (Error: ~S)", 3, var, module, output);
}
