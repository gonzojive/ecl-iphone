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


#include "ecl.h"

@(defun values (&rest args)
	int i;
@
	/* INV: the number of arguments never exceeds VSSIZE */
	NValues = narg;
	if (narg == 0)
		VALUES(0) = Cnil;
	else for (i = 0; i < narg; i++)
		VALUES(i) = va_arg(args, cl_object);
	returnn(VALUES(0));
@)

@(defun values_list (list)
@
	VALUES(0) = Cnil;
	for (NValues=0; !endp(list); list=CDR(list)) {
		if (NValues == VSSIZE)
			FEerror("Too many values in VALUES-LIST",0);
		VALUES(NValues++) = CAR(list);
	}
	returnn(VALUES(0));
@)

void
init_multival(void)
{
	make_constant("MULTIPLE-VALUES-LIMIT",MAKE_FIXNUM(32));
}
