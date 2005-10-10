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

#include <ecl.h>
#include <internal.h>

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
	register char *sp asm("esp");
	char *p1, *p2;
	int i;
	sp -= fficall->buffer_size;
	for (p1 = sp, p2 = fficall->buffer, i = fficall->buffer_size; i; i--) {
		*(p1++) = *(p2++);
	}
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
	sp += fficall->buffer_size;
}
