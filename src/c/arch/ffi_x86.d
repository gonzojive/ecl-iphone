/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    ffi_x86.c -- Nonportable component of the FFI
*/
/*
    Copyright (c) 2005, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <string.h>
#include <ecl/internal.h>

struct ecl_fficall_reg *
ecl_fficall_prepare_extra(struct ecl_fficall_reg *registers)
{
	/* No need to prepare registers */
	return 0;
}

void
ecl_fficall_push_arg(union ecl_ffi_values *data, enum ecl_ffi_tag type)
{
	int i;
	switch (type) {
	case ECL_FFI_CHAR: i = data->c;	goto INT;
	case ECL_FFI_UNSIGNED_CHAR: i = data->uc; goto INT;
	case ECL_FFI_BYTE: i = data->b; goto INT;
	case ECL_FFI_UNSIGNED_BYTE: i = data->ub; goto INT;
	case ECL_FFI_SHORT: i = data->s; goto INT;
	case ECL_FFI_UNSIGNED_SHORT: i = data->us; goto INT;
	case ECL_FFI_INT:
	case ECL_FFI_LONG:
	case ECL_FFI_UNSIGNED_INT:
	case ECL_FFI_UNSIGNED_LONG:
	case ECL_FFI_POINTER_VOID:
	case ECL_FFI_CSTRING:
	case ECL_FFI_OBJECT:
		i = data->i;
	INT:
		ecl_fficall_align(sizeof(int));
		ecl_fficall_push_int(i);
		break;
	case ECL_FFI_DOUBLE:
		ecl_fficall_align(sizeof(int));
		ecl_fficall_push_bytes(&data->d, sizeof(double));
		break;
	case ECL_FFI_FLOAT:
		ecl_fficall_align(sizeof(int));
		ecl_fficall_push_bytes(&data->f, sizeof(float));
		break;
	case ECL_FFI_VOID:
		FEerror("VOID is not a valid argument type for a C function", 0);
	}
}

void
ecl_fficall_execute(void *f_ptr, struct ecl_fficall *fficall, enum ecl_ffi_tag return_type)
{
	int bufsize = fficall->buffer_size;
	char* buf = fficall->buffer;
	char* stack_p;
#ifdef _MSC_VER
	__asm
	{
		mov	stack_p,esp
		sub	esp,bufsize
		mov	esi,buf
		mov	edi,esp
		mov	ecx,bufsize
		rep	movsb
	}
#else
	asm volatile (
	"movl	%%esp, %0\n\t"
	"subl	%1, %%esp\n\t"
	"movl	%2, %%esi\n\t"
	"movl	%%esp, %%edi\n\t"
	"rep\n\t"
        "movsb\n\t"
        : "=a" (stack_p) : "c" (bufsize), "d" (buf) : "%edi", "%esi");
#endif
	if (return_type <= ECL_FFI_UNSIGNED_LONG) {
		fficall->output.i = ((int (*)())f_ptr)();
	} else if (return_type == ECL_FFI_POINTER_VOID) {
		fficall->output.pv = ((void * (*)())f_ptr)();
	} else if (return_type == ECL_FFI_CSTRING) {
		fficall->output.pc = ((char * (*)())f_ptr)();
	} else if (return_type == ECL_FFI_OBJECT) {
		fficall->output.o = ((cl_object (*)())f_ptr)();
	} else if (return_type == ECL_FFI_FLOAT) {
		fficall->output.f = ((float (*)())f_ptr)();
	} else if (return_type == ECL_FFI_DOUBLE) {
		fficall->output.d = ((double (*)())f_ptr)();
	} else {
		((void (*)())f_ptr)();
	}
#ifdef _MSC_VER
	__asm mov esp,stack_p
#else
	asm volatile ("mov %0,%%esp" :: "a" (stack_p));
#endif
}

static void
ecl_dynamic_callback_execute(cl_object cbk_info, char *arg_buffer)
{
	cl_object fun, rtype, argtypes;
	cl_object result;
	cl_index i, size;
	union ecl_ffi_values output;
	enum ecl_ffi_tag tag;

	ECL_BUILD_STACK_FRAME(frame, aux);

	fun = CAR(cbk_info);
	rtype = CADR(cbk_info);
	argtypes = CADDR(cbk_info);

	arg_buffer += 4; /* Skip return address */
	for (i=0; !ecl_endp(argtypes); argtypes = CDR(argtypes), i++) {
		tag = ecl_foreign_type_code(CAR(argtypes));
		size = fix(si_size_of_foreign_elt_type(CAR(argtypes)));
		result = ecl_foreign_data_ref_elt(arg_buffer, tag);
		ecl_stack_frame_push(frame,result);
		{
			int mask = 3;
			int sp = (size + mask) & ~mask;
			arg_buffer += (sp);
		}
	}

	result = ecl_apply_from_stack_frame(frame, fun);
	ecl_stack_frame_close(frame);

	tag = ecl_foreign_type_code(rtype);
	memset(&output, 0, sizeof(output));
	ecl_foreign_data_set_elt(&output, tag, result);

	switch (tag) {
	case ECL_FFI_CHAR: i = output.c; goto INT;
	case ECL_FFI_UNSIGNED_CHAR: i = output.uc; goto INT;
	case ECL_FFI_BYTE: i = output.b; goto INT;
	case ECL_FFI_UNSIGNED_BYTE: i = output.ub; goto INT;
	case ECL_FFI_SHORT: i = output.s; goto INT;
	case ECL_FFI_UNSIGNED_SHORT: i = output.us; goto INT;
	case ECL_FFI_POINTER_VOID:
	case ECL_FFI_OBJECT:
	case ECL_FFI_CSTRING:
	case ECL_FFI_INT:
	case ECL_FFI_UNSIGNED_INT:
	case ECL_FFI_LONG:
	case ECL_FFI_UNSIGNED_LONG:
		i = output.i;
INT:
#ifdef _MSC_VER
		__asm mov eax,i
#else
		{
		register int eax asm("eax");
		eax = i;
		}
#endif
		return;
	case ECL_FFI_DOUBLE: {
#ifdef _MSC_VER
		__asm fld output.d
#else
		{
		asm("fldl (%0)" :: "a" (&output.d));
		}
#endif
		return;
	}
	case ECL_FFI_FLOAT: {
#ifdef _MSC_VER
		__asm fld output.f
#else
		{
		asm("flds (%0)" :: "a" (&output.f));
		}
#endif
		return;
	}
	case ECL_FFI_VOID:
		return;
	}
}

void*
ecl_dynamic_callback_make(cl_object data, enum ecl_ffi_calling_convention cc_type)
{
	/*
	 *	push	%esp				54
	 *	pushl	<data>				68 <addr32>
	 *	call	ecl_dynamic_callback_call	E8 <disp32>
	 * [ Here we could use also lea 4(%esp), %esp, but %ecx seems to be free ]
	 *	pop	%ecx				59
	 *	pop	%ecx				59
	 *	ret					c3
	 *	nop					90
	 *	nop					90
	 */
	char *buf = (char*)cl_alloc_atomic_align(sizeof(char)*16, 4);
	*(char*) (buf+0)  = 0x54;
	*(char*) (buf+1)  = 0x68;
	*(long*) (buf+2)  = (long)data;
	*(char*) (buf+6)  = 0xE8;
	*(long*) (buf+7)  = (long)ecl_dynamic_callback_execute - (long)(buf+11);
	*(char*) (buf+11) = 0x59;
	*(char*) (buf+12) = 0x59;
	if (cc_type == ECL_FFI_CC_CDECL) {
		*(char*) (buf+13) = 0xc3;
		*(short*)(buf+14) = 0x9090;
	} else {
		cl_object arg_types = CADDR(data);
		int byte_size = 0;
		int mask = 3;

		while (CONSP(arg_types)) {
			int sz = fix(si_size_of_foreign_elt_type(CAR(arg_types)));
			byte_size += ((sz+mask)&(~mask));
			arg_types = CDR(arg_types);
		}

		*(char*) (buf+13) = 0xc2;
		*(short*)(buf+14) = (short)byte_size;
	}

	return buf;
}
