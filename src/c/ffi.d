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

#include "ecl.h"

#ifdef ECL_FFI

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
	if (type_of(f) != t_foreign)
		FEwrong_type_argument(@'si::foreign-data', f);
	if (f->foreign.size)
		cl_dealloc(f->foreign.data, f->foreign.size);
	f->foreign.size = 0;
	f->foreign.data = NULL;
}

cl_object
si_foreign_data_tag(cl_object f)
{
	if (type_of(f) != t_foreign)
		FEwrong_type_argument(@'si::foreign-data', f);
	@(return f->foreign.tag);
}

cl_object
ecl_make_foreign_data(cl_object tag, cl_index size, void *data)
{
	cl_object output = cl_alloc_object(t_foreign);
	output->foreign.tag = tag;
	output->foreign.size = size;
	output->foreign.data = (char*)data;
	return output;
}

#endif /* ECL_FFI */
