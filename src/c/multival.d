/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    multival.c -- Multiple Values.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include <ecl/ecl.h>

@(defun values (&rest args)
	int i;
@
	if (narg > ECL_MULTIPLE_VALUES_LIMIT)
		FEerror("Too many values in VALUES",0);
	NVALUES = narg;
	if (narg == 0)
		VALUES(0) = Cnil;
	else for (i = 0; i < narg; i++)
		VALUES(i) = cl_va_arg(args);
	returnn(VALUES(0));
@)

cl_object
cl_values_list(cl_object list)
{
	VALUES(0) = Cnil;
	for (NVALUES=0; !ecl_endp(list); list=CDR(list)) {
		if (NVALUES == ECL_MULTIPLE_VALUES_LIMIT)
			FEerror("Too many values in VALUES-LIST",0);
		VALUES(NVALUES++) = CAR(list);
	}
	returnn(VALUES(0));
}
