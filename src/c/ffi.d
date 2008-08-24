/* -*- mode: c; c-basic-offset: 8 -*- */
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
#include <ecl/ecl.h>
#include <ecl/internal.h>

static const cl_object ecl_foreign_type_table[] = {
	@':char',
	@':unsigned-char',
	@':byte',
	@':unsigned-byte',
	@':short',
	@':unsigned-short',
	@':int',
	@':unsigned-int',
	@':long',
	@':unsigned-long',
	@':pointer-void',
	@':cstring',
	@':object',
	@':float',
	@':double',
	@':void'};

static const cl_object ecl_foreign_cc_table[] = {
	@':cdecl',
	@':stdcall'
};

static unsigned int ecl_foreign_type_size[] = {
	sizeof(char),
	sizeof(unsigned char),
	sizeof(int8_t),
	sizeof(uint8_t),
	sizeof(short),
	sizeof(unsigned short),
	sizeof(int),
	sizeof(unsigned int),
	sizeof(long),
	sizeof(unsigned long),
	sizeof(void *),
	sizeof(char *),
	sizeof(cl_object),
	sizeof(float),
	sizeof(double),
	0};

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
ecl_base_string_pointer_safe(cl_object f)
{
	cl_index l;
	unsigned char *s;
	/* FIXME! Is there a better function name? */
	f = ecl_check_cl_type(@'si::make-foreign-data-from-array', f, t_base_string);
	s = f->base_string.self;
	if (f->base_string.hasfillp && s[f->base_string.fillp] != 0) {
		FEerror("Cannot coerce a string with fill pointer to (char *)", 0);
	}
	return (char *)s;
}

cl_object
ecl_null_terminated_base_string(cl_object f)
{
	/* FIXME! Is there a better function name? */
	f = ecl_check_cl_type(@'si::make-foreign-data-from-array', f, t_base_string);
	if (f->base_string.hasfillp && f->base_string.self[f->base_string.fillp] != 0) {
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
		cl_dealloc(f->foreign.data);
	}
	f->foreign.size = 0;
	f->foreign.data = NULL;
}

cl_object
si_make_foreign_data_from_array(cl_object array)
{
	cl_object tag = Cnil;
	if (type_of(array) != t_array && type_of(array) != t_vector) {
		FEwrong_type_argument(@'array', array);
	}
	switch (array->array.elttype) {
	case aet_sf: tag = @':float'; break;
	case aet_df: tag = @':double'; break;
	case aet_fix: tag = @':int'; break;
	case aet_index: tag = @':unsigned-int'; break;
	default:
		FEerror("Cannot make foreign object from array with element type ~S.", 1, ecl_elttype_to_symbol(array->array.elttype));
		break;
	}
	@(return ecl_make_foreign_data(tag, 0, array->array.self.ch));
}

cl_object
si_foreign_data_address(cl_object f)
{
	if (type_of(f) != t_foreign) {
		FEwrong_type_argument(@'si::foreign-data', f);
	}
	@(return ecl_make_unsigned_integer((cl_index)f->foreign.data))
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

enum ecl_ffi_tag
ecl_foreign_type_code(cl_object type)
{
	int i;
	for (i = 0; i <= ECL_FFI_VOID; i++) {
		if (type == ecl_foreign_type_table[i])
			return (enum ecl_ffi_tag)i;
	}
	FEerror("~A does not denote an elementary foreign type.", 1, type);
	return ECL_FFI_VOID;
}

enum ecl_ffi_calling_convention
ecl_foreign_cc_code(cl_object cc)
{
	int i;
	for (i = 0; i <= ECL_FFI_CC_STDCALL; i++) {
		if (cc == ecl_foreign_cc_table[i])
			return (enum ecl_ffi_calling_convention)i;
	}
	FEerror("~A does no denote a valid calling convention.", 1, cc);
	return ECL_FFI_CC_CDECL;
}

cl_object
ecl_foreign_data_ref_elt(void *p, enum ecl_ffi_tag tag)
{
	switch (tag) {
	case ECL_FFI_CHAR:
		return CODE_CHAR(*(char *)p);
	case ECL_FFI_UNSIGNED_CHAR:
		return CODE_CHAR(*(unsigned char *)p);
	case ECL_FFI_BYTE:
		return MAKE_FIXNUM(*(int8_t *)p);
	case ECL_FFI_UNSIGNED_BYTE:
		return MAKE_FIXNUM(*(uint8_t *)p);
	case ECL_FFI_SHORT:
		return MAKE_FIXNUM(*(short *)p);
	case ECL_FFI_UNSIGNED_SHORT:
		return MAKE_FIXNUM(*(unsigned short *)p);
	case ECL_FFI_INT:
		return ecl_make_integer(*(int *)p);
	case ECL_FFI_UNSIGNED_INT:
		return ecl_make_unsigned_integer(*(unsigned int *)p);
	case ECL_FFI_LONG:
		return ecl_make_integer(*(long *)p);
	case ECL_FFI_UNSIGNED_LONG:
		return ecl_make_unsigned_integer(*(unsigned long *)p);
	case ECL_FFI_POINTER_VOID:
		return ecl_make_foreign_data(@':pointer-void', 0, *(void **)p);
	case ECL_FFI_CSTRING:
		return *(char **)p ? make_simple_base_string(*(char **)p) : Cnil;
	case ECL_FFI_OBJECT:
		return *(cl_object *)p;
	case ECL_FFI_FLOAT:
		return ecl_make_singlefloat(*(float *)p);
	case ECL_FFI_DOUBLE:
		return ecl_make_doublefloat(*(double *)p);
	case ECL_FFI_VOID:
		return Cnil;
	}
}

void
ecl_foreign_data_set_elt(void *p, enum ecl_ffi_tag tag, cl_object value)
{
	switch (tag) {
	case ECL_FFI_CHAR:
		*(char *)p = (char)ecl_base_char_code(value);
		break;
	case ECL_FFI_UNSIGNED_CHAR:
		*(unsigned char*)p = (unsigned char)ecl_base_char_code(value);
		break;
	case ECL_FFI_BYTE:
		*(int8_t *)p = fixint(value);
		break;
	case ECL_FFI_UNSIGNED_BYTE:
		*(uint8_t *)p = fixnnint(value);
		break;
	case ECL_FFI_SHORT:
		*(short *)p = fixint(value);
		break;
	case ECL_FFI_UNSIGNED_SHORT:
		*(unsigned short *)p = fixnnint(value);
		break;
	case ECL_FFI_INT:
		*(int *)p = fixint(value);
		break;
	case ECL_FFI_UNSIGNED_INT:
		*(unsigned int *)p = fixnnint(value);
		break;
	case ECL_FFI_LONG:
		*(long *)p = fixint(value);
		break;
	case ECL_FFI_UNSIGNED_LONG:
		*(unsigned long *)p = fixnnint(value);
		break;
	case ECL_FFI_POINTER_VOID:
		*(void **)p = ecl_foreign_data_pointer_safe(value);
		break;
	case ECL_FFI_CSTRING:
		*(char **)p = value == Cnil ? NULL : value->base_string.self;
		break;
	case ECL_FFI_OBJECT:
		*(cl_object *)p = value;
		break;
	case ECL_FFI_FLOAT:
		*(float *)p = ecl_to_float(value);
		break;
	case ECL_FFI_DOUBLE:
		*(double *)p = ecl_to_double(value);
		break;
	case ECL_FFI_VOID:
		break;
	}
}

cl_object
si_foreign_data_ref_elt(cl_object f, cl_object andx, cl_object type)
{
	cl_index ndx = fixnnint(andx);
	cl_index limit = f->foreign.size;
	enum ecl_ffi_tag tag = ecl_foreign_type_code(type);
	if (ndx >= limit || (ndx + ecl_foreign_type_size[tag] > limit)) {
		FEerror("Out of bounds reference into foreign data type ~A.", 1, f);
	}
	if (type_of(f) != t_foreign) {
		FEwrong_type_argument(@'si::foreign-data', f);
	}
	@(return ecl_foreign_data_ref_elt((void*)(f->foreign.data + ndx), tag))
}

cl_object
si_foreign_data_set_elt(cl_object f, cl_object andx, cl_object type, cl_object value)
{
	cl_index ndx = fixnnint(andx);
	cl_index limit = f->foreign.size;
	void *p;
	enum ecl_ffi_tag tag = ecl_foreign_type_code(type);
	if (ndx >= limit || ndx + ecl_foreign_type_size[tag] > limit) {
		FEerror("Out of bounds reference into foreign data type ~A.", 1, f);
	}
	if (type_of(f) != t_foreign) {
		FEwrong_type_argument(@'si::foreign-data', f);
	}
	ecl_foreign_data_set_elt((void*)(f->foreign.data + ndx), tag, value);
	@(return value)
}

cl_object
si_size_of_foreign_elt_type(cl_object type)
{
	enum ecl_ffi_tag tag = ecl_foreign_type_code(type);
	@(return MAKE_FIXNUM(ecl_foreign_type_size[tag]))
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
#if !defined(ENABLE_DLOPEN)
	FEerror("SI:LOAD-FOREIGN-MODULE does not work when ECL is statically linked", 0);
#else
	cl_object libraries;
	cl_object output;
	int i;

#ifdef ECL_THREADS
	mp_get_lock(1, ecl_symbol_value(@'mp::+load-compile-lock+'));
	CL_UNWIND_PROTECT_BEGIN {
#endif
	output = ecl_library_open(filename, 0);
	if (output->cblock.handle == NULL)
	{
		ecl_library_close(output);
		output = ecl_library_error(output);
	}
OUTPUT:
#ifdef ECL_THREADS
	(void)0; /* MSVC complains about missing ';' before '}' */
	} CL_UNWIND_PROTECT_EXIT {
	mp_giveup_lock(ecl_symbol_value(@'mp::+load-compile-lock+'));
	} CL_UNWIND_PROTECT_END;
#endif
	if (type_of(output) == t_codeblock) {
		output->cblock.locked |= 1;
		@(return output)
	} else {
		FEerror("LOAD-FOREIGN-MODULE: Could not load foreign module ~S (Error: ~S)", 2, filename, output);
	}
#endif
}

cl_object
si_find_foreign_symbol(cl_object var, cl_object module, cl_object type, cl_object size)
{
#if !defined(ENABLE_DLOPEN)
	FEerror("SI:FIND-FOREIGN-SYMBOL does not work when ECL is statically linked", 0);
#else
	cl_object block;
	cl_object output = Cnil;
	void *sym;

	block = (module == @':default' ? module : si_load_foreign_module(module));
	var = ecl_null_terminated_base_string(var);
	sym = ecl_library_symbol(block, var->base_string.self, 1);
	if (sym == NULL) {
		if (block != @':default')
			output = ecl_library_error(block);
		goto OUTPUT;
	}
	output = ecl_make_foreign_data(type, ecl_to_fixnum(size), sym);
OUTPUT:
	if (type_of(output) == t_foreign)
		@(return output)
	else
		FEerror("FIND-FOREIGN-SYMBOL: Could not load foreign symbol ~S from module ~S (Error: ~S)", 3, var, module, output);
#endif
}

#ifdef ECL_DYNAMIC_FFI

static void
ecl_fficall_overflow()
{
	FEerror("Stack overflow on SI:CALL-CFUN", 0);
}

void
ecl_fficall_prepare(cl_object return_type, cl_object arg_type, cl_object cc_type)
{
	struct ecl_fficall *fficall = cl_env.fficall;
	fficall->buffer_sp = fficall->buffer;
	fficall->buffer_size = 0;
	fficall->cstring = Cnil;
	fficall->cc = ecl_foreign_cc_code(cc_type);
        fficall->registers = ecl_fficall_prepare_extra(fficall->registers);
}

void
ecl_fficall_push_bytes(void *data, size_t bytes)
{
	struct ecl_fficall *fficall = cl_env.fficall;
	fficall->buffer_size += bytes;
	if (fficall->buffer_size >= ECL_FFICALL_LIMIT)
		ecl_fficall_overflow();
	memcpy(fficall->buffer_sp, (char*)data, bytes);
	fficall->buffer_sp += bytes;
}

void
ecl_fficall_push_int(int data)
{
	ecl_fficall_push_bytes(&data, sizeof(int));
}

void
ecl_fficall_align(int data)
{
	struct ecl_fficall *fficall = cl_env.fficall;
	if (data == 1)
		return;
	else {
		size_t sp = fficall->buffer_sp - fficall->buffer;
		size_t mask = data - 1;
		size_t new_sp = (sp + mask) & ~mask;
		if (new_sp >= ECL_FFICALL_LIMIT)
			ecl_fficall_overflow();
		fficall->buffer_sp = fficall->buffer + new_sp;
		fficall->buffer_size = new_sp;
	}
}

@(defun si::call-cfun (fun return_type arg_types args &optional (cc_type @':cdecl'))
	struct ecl_fficall *fficall = cl_env.fficall;
	void *cfun = ecl_foreign_data_pointer_safe(fun);
	cl_object object;
	enum ecl_ffi_tag return_type_tag = ecl_foreign_type_code(return_type);
@

	ecl_fficall_prepare(return_type, arg_types, cc_type);
	while (CONSP(arg_types)) {
		enum ecl_ffi_tag type;
		if (!CONSP(args)) {
			FEerror("In SI:CALL-CFUN, mismatch between argument types and argument list: ~A vs ~A", 0);
		}
		type = ecl_foreign_type_code(CAR(arg_types));
		if (type == ECL_FFI_CSTRING) {
			object = ecl_null_terminated_base_string(CAR(args));
			if (CAR(args) != object)
				fficall->cstring =
					CONS(object, fficall->cstring);
		} else {
			object = CAR(args);
		}
		ecl_foreign_data_set_elt(&fficall->output, type, object);
		ecl_fficall_push_arg(&fficall->output, type);
		arg_types = CDR(arg_types);
		args = CDR(args);
	}
	ecl_fficall_execute(cfun, fficall, return_type_tag);
	object = ecl_foreign_data_ref_elt(&fficall->output, return_type_tag);

	fficall->buffer_size = 0;
	fficall->buffer_sp = fficall->buffer;
	fficall->cstring = Cnil;

	@(return object)
@)

@(defun si::make-dynamic-callback (fun sym rtype argtypes &optional (cctype @':cdecl'))
	cl_object data;
	cl_object cbk;
@
	data = cl_list(3, fun, rtype, argtypes);
	cbk  = ecl_make_foreign_data(@':pointer-void', 0, ecl_dynamic_callback_make(data, ecl_foreign_cc_code(cctype)));

	si_put_sysprop(sym, @':callback', CONS(cbk, data));
	@(return cbk)
@)


#endif /* ECL_DYNAMIC_FFI */
