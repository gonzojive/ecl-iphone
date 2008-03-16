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

static void
prepare_map(cl_va_list lists, cl_object cdrs_frame, cl_object cars_frame)
{
	cl_index i;
	ecl_stack_frame_push_va_list(cdrs_frame, lists);
	if (cdrs_frame->frame.narg == 0) {
		FEprogram_error("MAP*: Too few arguments.", 0);
	}
	ecl_stack_frame_reserve(cars_frame, cdrs_frame->frame.narg);
	for (i = 0; i < cars_frame->frame.narg; i++) {
		ecl_stack_frame_elt_set(cars_frame, i, Cnil);
	}
}

@(defun mapcar (fun &rest lists)
	cl_object res, *val = &res;
	cl_index i;
@ {
	ECL_BUILD_STACK_FRAME(cars_frame,frame1);
	ECL_BUILD_STACK_FRAME(cdrs_frame,frame2);
	prepare_map(lists, cdrs_frame, cars_frame);
	res = Cnil;
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < cdrs_frame->frame.narg;  i++) {
			cl_object cdr = ecl_stack_frame_elt(cdrs_frame, i);
			if (ecl_endp(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return res)
			}
			ecl_stack_frame_elt_set(cars_frame, i, CAR(cdr));
			ecl_stack_frame_elt_set(cdrs_frame, i, CDR(cdr));
		}
		*val = CONS(ecl_apply_from_stack_frame(cars_frame, fun), Cnil);
		val = &CDR(*val);
	}
} @)

@(defun maplist (fun &rest lists)
	cl_object res, *val = &res;
@ {
	ECL_BUILD_STACK_FRAME(cars_frame,frame1);
	ECL_BUILD_STACK_FRAME(cdrs_frame,frame2);
	prepare_map(lists, cdrs_frame, cars_frame);
	res = Cnil;
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < cdrs_frame->frame.narg;  i++) {
			cl_object cdr = ecl_stack_frame_elt(cdrs_frame, i);
			if (ecl_endp(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return res)
			}
			ecl_stack_frame_elt_set(cars_frame, i, cdr);
			ecl_stack_frame_elt_set(cdrs_frame, i, CDR(cdr));
		}
		*val = CONS(ecl_apply_from_stack_frame(cars_frame, fun), Cnil);
		val = &CDR(*val);
	}
} @)

@(defun mapc (fun &rest lists)
	cl_object onelist;
@ {
	ECL_BUILD_STACK_FRAME(cars_frame,frame1);
	ECL_BUILD_STACK_FRAME(cdrs_frame,frame2);
	prepare_map(lists, cdrs_frame, cars_frame);
	onelist = ecl_stack_frame_elt(cdrs_frame, 0);
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < cdrs_frame->frame.narg;  i++) {
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
	ECL_BUILD_STACK_FRAME(cars_frame,frame1);
	ECL_BUILD_STACK_FRAME(cdrs_frame,frame2);
	prepare_map(lists, cdrs_frame, cars_frame);
	onelist = ecl_stack_frame_elt(cdrs_frame, 0);
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < cdrs_frame->frame.narg;  i++) {
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
	ECL_BUILD_STACK_FRAME(cars_frame,frame1);
	ECL_BUILD_STACK_FRAME(cdrs_frame,frame2);
	prepare_map(lists, cdrs_frame, cars_frame);
	res = Cnil;
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < cdrs_frame->frame.narg;  i++) {
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
			val = &CDR(*val);
	}
} @)

@(defun mapcon (fun &rest lists)
	cl_object res, *val = &res;
@ {
	ECL_BUILD_STACK_FRAME(cars_frame,frame1);
	ECL_BUILD_STACK_FRAME(cdrs_frame,frame2);
	prepare_map(lists, cdrs_frame, cars_frame);
	res = Cnil;
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < cdrs_frame->frame.narg;  i++) {
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
			val = &CDR(*val);
	}
} @)
