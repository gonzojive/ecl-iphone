/* -*- mode: c; c-basic-offset: 8 -*- */
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


#include <ecl/ecl.h>
#include <ecl/internal.h>

#define PREPARE_MAP(list, cdrs_frame, cars_frame, nargs) \
	struct ecl_stack_frame cdrs_frame_aux, cars_frame_aux; \
	cl_object cdrs_frame, cars_frame; \
	cl_index nargs; \
	cdrs_frame = ecl_stack_frame_from_va_list((cl_object)&cdrs_frame_aux, list); \
	cars_frame = ecl_stack_frame_copy((cl_object)&cars_frame_aux, cdrs_frame); \
	nargs = ECL_STACK_FRAME_SIZE(cars_frame); \
	if (nargs == 0) { \
		FEprogram_error("MAP*: Too few arguments", 0); \
	}


@(defun mapcar (fun &rest lists)
	cl_object res, *val = &res;
	cl_index i;
@ {
	PREPARE_MAP(lists, cdrs_frame, cars_frame, nargs);
	res = Cnil;
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < nargs;  i++) {
			cl_object cdr = ecl_stack_frame_elt(cdrs_frame, i);
			if (ecl_endp(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return res)
			}
			ecl_stack_frame_elt_set(cars_frame, i, CAR(cdr));
			ecl_stack_frame_elt_set(cdrs_frame, i, CDR(cdr));
		}
		*val = ecl_list1(ecl_apply_from_stack_frame(cars_frame, fun));
		val = &ECL_CONS_CDR(*val);
	}
} @)

@(defun maplist (fun &rest lists)
	cl_object res, *val = &res;
@ {
	PREPARE_MAP(lists, cdrs_frame, cars_frame, nargs);
	res = Cnil;
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < nargs;  i++) {
			cl_object cdr = ecl_stack_frame_elt(cdrs_frame, i);
			if (ecl_endp(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return res)
			}
			ecl_stack_frame_elt_set(cars_frame, i, cdr);
			ecl_stack_frame_elt_set(cdrs_frame, i, CDR(cdr));
		}
		*val = ecl_list1(ecl_apply_from_stack_frame(cars_frame, fun));
		val = &ECL_CONS_CDR(*val);
	}
} @)

@(defun mapc (fun &rest lists)
	cl_object onelist;
@ {
	PREPARE_MAP(lists, cdrs_frame, cars_frame, nargs);
	onelist = ecl_stack_frame_elt(cdrs_frame, 0);
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < nargs;  i++) {
			cl_object cdr = ecl_stack_frame_elt(cdrs_frame, i);
			if (ecl_endp(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return onelist)
			}
			ecl_stack_frame_elt_set(cars_frame, i, CAR(cdr));
			ecl_stack_frame_elt_set(cdrs_frame, i, CDR(cdr));
		}
		ecl_apply_from_stack_frame(cars_frame, fun);
	}
} @)

@(defun mapl (fun &rest lists)
	cl_object onelist;
@ {
	PREPARE_MAP(lists, cdrs_frame, cars_frame, nargs);
	onelist = ecl_stack_frame_elt(cdrs_frame, 0);
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < nargs;  i++) {
			cl_object cdr = ecl_stack_frame_elt(cdrs_frame, i);
			if (ecl_endp(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return onelist)
			}
			ecl_stack_frame_elt_set(cars_frame, i, cdr);
			ecl_stack_frame_elt_set(cdrs_frame, i, CDR(cdr));
		}
		ecl_apply_from_stack_frame(cars_frame, fun);
	}
} @)

@(defun mapcan (fun &rest lists)
	cl_object res, *val = &res;
@ {
	PREPARE_MAP(lists, cdrs_frame, cars_frame, nargs);
	res = Cnil;
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < nargs;  i++) {
			cl_object cdr = ecl_stack_frame_elt(cdrs_frame, i);
			if (ecl_endp(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return res)
			}
			ecl_stack_frame_elt_set(cars_frame, i, CAR(cdr));
			ecl_stack_frame_elt_set(cdrs_frame, i, CDR(cdr));
		}
		*val = ecl_apply_from_stack_frame(cars_frame, fun);
		while (CONSP(*val))
			val = &ECL_CONS_CDR(*val);
	}
} @)

@(defun mapcon (fun &rest lists)
	cl_object res, *val = &res;
@ {
	PREPARE_MAP(lists, cdrs_frame, cars_frame, nargs);
	res = Cnil;
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < nargs;  i++) {
			cl_object cdr = ecl_stack_frame_elt(cdrs_frame, i);
			if (ecl_endp(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return res)
			}
			ecl_stack_frame_elt_set(cars_frame, i, cdr);
			ecl_stack_frame_elt_set(cdrs_frame, i, CDR(cdr));
		}
		*val = ecl_apply_from_stack_frame(cars_frame, fun);
		while (CONSP(*val))
			val = &ECL_CONS_CDR(*val);
	}
} @)
