/*
    mapfun.c -- Mapping.
*/
/*
    Copyright (c) 1993, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecls.h"

@(defun mapcar (fun onelist &rest lists)
	cl_object res, *val = &res;
	cl_object cdrs[narg-1];
	cl_object cars[narg-1];	/* __GNUC__ */
	int i;
@
	cdrs[0] = onelist;
	for (--narg, i = 1; i < narg; i++)
		cdrs[i] = cl_nextarg(lists);
	res = Cnil;
	while (TRUE) {
		for (i = 0;  i < narg;  i++) {
			if (endp(cdrs[i]))
				@(return res)
			cars[i] = CAR(cdrs[i]);
			cdrs[i] = CDR(cdrs[i]);
		}
		*val = CONS(apply(narg, fun, cars), Cnil);
		val = &CDR(*val);
	}
@)

@(defun maplist (fun onelist &rest lists)
	cl_object res, *val = &res;
	cl_object cdrs[narg-1];
	cl_object cars[narg-1];	/* __GNUC__ */
	int i;
@
	cdrs[0] = onelist;
	for (--narg, i = 1; i < narg; i++)
		cdrs[i] = cl_nextarg(lists);
	res = Cnil;
	while (TRUE) {
		for (i = 0;  i < narg;  i++) {
			if (endp(cdrs[i]))
				@(return res)
			cars[i] = cdrs[i];
			cdrs[i] = CDR(cdrs[i]);
		}
		*val = CONS(apply(narg, fun, cars), Cnil);
		val = &CDR(*val);
	}
@)

@(defun mapc (fun onelist &rest lists)
	cl_object cdrs[narg-1];
	cl_object cars[narg-1];	/* __GNUC__ */
	int i;
@
	cdrs[0] = onelist;
	for (--narg, i = 1; i < narg; i++)
		cdrs[i] = va_arg(lists, cl_object);
	while (TRUE) {
		for (i = 0;  i < narg;  i++) {
			if (endp(cdrs[i]))
				@(return onelist)
			cars[i] = CAR(cdrs[i]);
			cdrs[i] = CDR(cdrs[i]);
		}
		apply(narg, fun, cars);
	}
@)

@(defun mapl (fun onelist &rest lists)
	cl_object cdrs[narg-1];
	cl_object cars[narg-1];	/* __GNUC__ */
	int i;
@
	cdrs[0] = onelist;
	for (--narg, i = 1; i < narg; i++)
		cdrs[i] = cl_nextarg(lists);
	while (TRUE) {
		for (i = 0;  i < narg;  i++) {
			if (endp(cdrs[i]))
				@(return onelist)
			cars[i] = cdrs[i];
			cdrs[i] = CDR(cdrs[i]);
		}
		apply(narg, fun, cars);
	}
@)

@(defun mapcan (fun onelist &rest lists)
	cl_object *x, res, *val = &res;
	cl_object cdrs[narg-1];
	cl_object cars[narg-1];	/* __GNUC__ */
	int i;
@
	cdrs[0] = onelist;
	for (--narg, i = 1; i < narg; i++)
		cdrs[i] = cl_nextarg(lists);
	res = Cnil;
	while (TRUE) {
		for (i = 0;  i < narg;  i++) {
			if (endp(cdrs[i]))
				@(return res)
			cars[i] = CAR(cdrs[i]);
			cdrs[i] = CDR(cdrs[i]);
		}
		*val = apply(narg, fun, cars);
		while (CONSP(*val))
			val = &CDR(*val);
	}
@)

@(defun mapcon (fun onelist &rest lists)
	cl_object res, *val = &res;
	cl_object cdrs[narg-1];
	cl_object cars[narg-1];	/* __GNUC__ */
	int i;
@
	cdrs[0] = onelist;
	for (--narg, i = 1; i < narg; i++)
		cdrs[i] = cl_nextarg(lists);
	res = Cnil;
	while (TRUE) {
		for (i = 0;  i < narg;  i++) {
			if (endp(cdrs[i]))
				@(return res)
			cars[i] = cdrs[i];
			cdrs[i] = CDR(cdrs[i]);
		}
		*val = apply(narg, fun, cars);
		while (CONSP(*val))
			val = &CDR(*val);
	}
@)
