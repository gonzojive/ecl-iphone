/*
    error.c -- Error handling.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecl.h"
#include <signal.h>

void
cs_overflow(void)
{
#ifdef DOWN_STACK
	if (cs_limit < cs_org - cssize)
	  cs_limit -= CSGETA;
#else
	if (cs_limit > cs_org + cssize)
	  cs_limit += CSGETA;
#endif
	FEerror("Control stack overflow.", 0);
}

void
error(const char *s)
{
	printf("\nUnrecoverable error: %s\n", s);
	fflush(stdout);
#ifdef SIGIOT
	signal(SIGIOT, SIG_DFL); /* avoid getting into a loop with abort */
#endif
	abort();
}

void
internal_error(const char *s)
{
	printf("\nInternal error in %s()\n", s);
	fflush(stdout);
#ifdef SIGIOT
	signal(SIGIOT, SIG_DFL); /* avoid getting into a loop with abort */
#endif
	abort();
}

/*****************************************************************************/
/*		Support for Lisp Error Handler				     */
/*****************************************************************************/

cl_object null_string;

void
terminal_interrupt(bool correctable)
{
	funcall(2, @'si::terminal-interrupt', correctable? Ct : Cnil);
}

void
FEerror(char *s, int narg, ...)
{
	va_list args;
	va_start(args, narg);
	funcall(4, @'si::universal-error-handler',
		Cnil,                    /*  not correctable  */
		make_constant_string(s),	 /*  condition text  */
		va_grab_rest_args(narg, args));
	va_end(args);
}

cl_object
CEerror(char *err, int narg, ...)
{
	va_list args;
	va_start(args, narg);
	return funcall(4, @'si::universal-error-handler',
		       Ct,			/*  correctable  */
		       make_constant_string(err),	/*  continue-format-string  */
		       va_grab_rest_args(narg, args));
	va_end(args);
}

/***********************
 * Conditions signaler *
 ***********************/

void
FEcondition(int narg, cl_object name, ...)
{
	va_list args;
	va_start(args, name);
	funcall(4, @'si::universal-error-handler',
		Cnil,                    /*  not correctable  */
		name,                    /*  condition name  */
		va_grab_rest_args(--narg, args));
	va_end(args);
}

void
FEprogram_error(const char *s, int narg, ...)
{
	va_list args;
	gc(t_contiguous);
	va_start(args, narg);
	funcall(4, @'si::universal-error-handler',
		Cnil,                    /*  not correctable  */
		@'si::simple-program-error', /*  condition name  */
		list(4, @':format-control', make_constant_string(s),
		     @':format-arguments', va_grab_rest_args(narg, args)));
	va_end(args);
}

void
FEcontrol_error(const char *s, int narg, ...)
{
	va_list args;
	va_start(args, narg);
	funcall(4, @'si::universal-error-handler',
		Cnil,                    /*  not correctable  */
		@'si::simple-control-error', /*  condition name  */
		list(4, @':format-control', make_constant_string(s),
		     @':format-arguments', va_grab_rest_args(narg, args)));
	va_end(args);
}

void
FEcannot_open(cl_object fn)
{
	FEcondition(3, @'file-error', @':pathname', fn);
}

void
FEend_of_file(cl_object strm)
{
	FEcondition(3, @'end-of-file', @':stream', strm);
}

void
FEwrong_type_argument(cl_object type, cl_object value)
{
	FEcondition(5, @'type-error', @':datum', value, @':expected-type', type);
}

void
FEunbound_variable(cl_object sym)
{
	FEcondition(3, @'unbound-variable', @':name', sym);
}

void
FEundefined_function(cl_object fname)
{
	FEcondition(3, @'undefined-function', @':name', fname);
}

/*************
 * Shortcuts *
 *************/

void
FEtoo_few_arguments(int *nargp)
{
	cl_object fname = ihs_top_function_name();
	FEprogram_error("Function ~S requires more than ~R argument~:p.",
			2, fname, MAKE_FIXNUM(*nargp));
}

void
FEtoo_many_arguments(int *nargp)
{
	cl_object fname = ihs_top_function_name();
	FEprogram_error("Function ~S requires less than ~R argument~:p.",
			2, fname, MAKE_FIXNUM(*nargp));
}

void
FEinvalid_macro_call(cl_object name)
{
	FEerror("Invalid macro call to ~S.", 1, name);
}

void
FEinvalid_variable(char *s, cl_object obj)
{
	FEerror(s, 1, obj);
}

void
FEassignment_to_constant(cl_object v)
{
	FEprogram_error("SETQ: Tried to assign a value to the constant ~S.", 1, v);
}

void
FEinvalid_function(cl_object obj)
{
	FEwrong_type_argument(@'function', obj);
}

/*      bootstrap version                */
static
@(defun "universal_error_handler" (c err args)
@
	printf("\nLisp initialization error.\n");
	@print(1, err);
	@print(1, args);
#ifndef ALFA
	exit(0);
#endif
@)

void
check_arg_failed(int narg, int req)
{
	cl_object fname = ihs_top_function_name();
	FEprogram_error((narg < req)
			? "Function ~S requires ~R argument~:p,~%\
but only ~R ~:*~[were~;was~:;were~] supplied."
			: "Function ~S requires only ~R argument~:p,~%\
but ~R ~:*~[were~;was~:;were~] supplied.",
			3, fname, MAKE_FIXNUM(req), MAKE_FIXNUM(narg));
}

void
illegal_index(cl_object x, cl_object i)
{
	FEerror("~S is an illegal index to ~S.", 2, i, x);
}

void
FEtype_error_symbol(cl_object obj)
{
	FEwrong_type_argument(@'symbol', obj);
}

void
not_a_variable(cl_object obj)
{
	FEinvalid_variable("~S is not a variable.", obj);
}

/************************************
 * Higher level interface to errors *
 ************************************/

@(defun error (eformat &rest args)
@
	funcall(4, @'si::universal-error-handler',
		Cnil,
		eformat,
		va_grab_rest_args(narg-1, args));
@)

@(defun cerror (cformat eformat &rest args)
@
	return(funcall(4, @'si::universal-error-handler',
		       cformat,
		       eformat,
		       va_grab_rest_args(narg-2, args)));
@)

void
init_error(void)
{
	MF(@'si::universal-error-handler', universal_error_handler, Cnil);
	null_string = make_constant_string("");
	register_root(&null_string);
}
