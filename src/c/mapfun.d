/*
    mapfun.c -- Mapping.
*/
/*
    Copyright (c) 1993, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecl.h"

static cl_index
prepare_map(cl_va_list lists, cl_index *cdrs_sp)
{
	cl_index i, nlist = lists[0].narg;

	*cdrs_sp = cl_stack_index();
	if (nlist == 0)
		FEprogram_error("MAP*: Too few arguments.", 0);
	cl_stack_push_va_list(lists);
	for (i = 0; i<nlist; i++)
		cl_stack_push(Cnil);
	return nlist;
}

@(defun mapcar (fun &rest lists)
	cl_object res, *val = &res;
	cl_index i, nlist, cdrs_sp;
@
	nlist = prepare_map(lists, &cdrs_sp);
	res = Cnil;
	while (TRUE) {
		/* INV: The stack does not grow here. */
		cl_object *cdrs = cl_env.stack + cdrs_sp;
		cl_object *cars = cdrs + nlist;
		for (i = 0;  i < nlist;  i++) {
			if (endp(cdrs[i])) {
			 	cl_stack_set_index(cdrs_sp);
				@(return res)
			}
			cars[i] = CAR(cdrs[i]);
			cdrs[i] = CDR(cdrs[i]);
		}
		*val = CONS(cl_apply_from_stack(nlist, fun), Cnil);
		val = &CDR(*val);
	}
@)

@(defun maplist (fun &rest lists)
	cl_object res, *val = &res;
	cl_index i, nlist, cdrs_sp;
@
	nlist = prepare_map(lists, &cdrs_sp);
	res = Cnil;
	while (TRUE) {
		cl_object *cdrs = cl_env.stack + cdrs_sp;
		cl_object *cars = cdrs + nlist;
		for (i = 0;  i < nlist;  i++) {
			if (endp(cdrs[i])) {
				cl_stack_set_index(cdrs_sp);
				@(return res)
			}
			cars[i] = cdrs[i];
			cdrs[i] = CDR(cdrs[i]);
		}
		*val = CONS(cl_apply_from_stack(nlist, fun), Cnil);
		val = &CDR(*val);
	}
@)

@(defun mapc (fun &rest lists)
	cl_object onelist;
	cl_index i, nlist, cdrs_sp;
@
	nlist = prepare_map(lists, &cdrs_sp);
	onelist = cl_env.stack[cdrs_sp];
	while (TRUE) {
		cl_object *cdrs = cl_env.stack + cdrs_sp;
		cl_object *cars = cdrs + nlist;
		for (i = 0;  i < nlist;  i++) {
			if (endp(cdrs[i])) {
				cl_stack_set_index(cdrs_sp);
				@(return onelist)
			}
			cars[i] = CAR(cdrs[i]);
			cdrs[i] = CDR(cdrs[i]);
		}
		cl_apply_from_stack(nlist, fun);
	}
@)

@(defun mapl (fun &rest lists)
	cl_object onelist;
	cl_index i, nlist, cdrs_sp;
@
	nlist = prepare_map(lists, &cdrs_sp);
	onelist = cl_env.stack[cdrs_sp];
	while (TRUE) {
		cl_object *cdrs = cl_env.stack + cdrs_sp;
		cl_object *cars = cdrs + nlist;
		for (i = 0;  i < nlist;  i++) {
			if (endp(cdrs[i])) {
				cl_stack_set_index(cdrs_sp);
				@(return onelist)
			}
			cars[i] = cdrs[i];
			cdrs[i] = CDR(cdrs[i]);
		}
		cl_apply_from_stack(nlist, fun);
	}
@)

@(defun mapcan (fun &rest lists)
	cl_object res, *val = &res;
	cl_index i, nlist, cdrs_sp;
@
	nlist = prepare_map(lists, &cdrs_sp);
	res = Cnil;
	while (TRUE) {
		cl_object *cdrs = cl_env.stack + cdrs_sp;
		cl_object *cars = cdrs + nlist;
		for (i = 0;  i < nlist;  i++) {
			if (endp(cdrs[i])) {
				cl_stack_set_index(cdrs_sp);
				@(return res)
			}
			cars[i] = CAR(cdrs[i]);
			cdrs[i] = CDR(cdrs[i]);
		}
		*val = cl_apply_from_stack(nlist, fun);
		while (CONSP(*val))
			val = &CDR(*val);
	}
@)

@(defun mapcon (fun &rest lists)
	cl_object res, *val = &res;
	cl_index i, nlist, cdrs_sp;
@
	nlist = prepare_map(lists, &cdrs_sp);
	res = Cnil;
	while (TRUE) {
		cl_object *cdrs = cl_env.stack + cdrs_sp;
		cl_object *cars = cdrs + nlist;
		for (i = 0;  i < nlist;  i++) {
			if (endp(cdrs[i])) {
				cl_stack_set_index(cdrs_sp);
				@(return res)
			}
			cars[i] = cdrs[i];
			cdrs[i] = CDR(cdrs[i]);
		}
		*val = cl_apply_from_stack(nlist, fun);
		while (CONSP(*val))
			val = &CDR(*val);
	}
@)
