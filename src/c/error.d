/*
    error.c -- Error handling.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecls.h"

/******************************* EXPORTS ******************************/

cl_object Sarithmetic_error, Scell_error, Scondition;
cl_object Scontrol_error, Sdivision_by_zero, Send_of_file;
cl_object Serror, Sfile_error, Sfloating_point_inexact;
cl_object Sfloating_point_invalid_operation, Sfloating_point_overflow;
cl_object Sfloating_point_underflow, Spackage_error, Sparse_error;
cl_object Sprint_not_readable, Sprogram_error, Sreader_error;
cl_object Sserious_condition, Ssimple_condition, Ssimple_error;
cl_object Ssimple_type_error, Ssimple_warning, Sstorage_condition;
cl_object Sstream_error, Sstyle_warning, Stype_error, Sunbound_slot;
cl_object Sunbound_variable, Sundefined_function, Swarning;

cl_object siSsimple_program_error, siSsimple_control_error;

cl_object Kpathname;			/* file-error */
cl_object Kdatum, Kexpected_type;	/* type-error */
cl_object Kformat_control, Kformat_arguments;	/* simple-condition */

/******************************* ------- ******************************/

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

cl_object siSuniversal_error_handler;

cl_object null_string;

cl_object siSterminal_interrupt;

void
terminal_interrupt(bool correctable)
{
	funcall(2, siSterminal_interrupt, correctable? Ct : Cnil);
}

void
FEerror(char *s, int narg, ...)
{
	va_list args;
	cl_object rest = Cnil, *r = &rest;

	va_start(args, narg);
	while (narg--)
	  r = &CDR(*r = CONS(va_arg(args, cl_object), Cnil));
	funcall(4, siSuniversal_error_handler,
		Cnil,                    /*  not correctable  */
		make_simple_string(s),	 /*  condition text  */
		rest);
}

cl_object
CEerror(char *err, int narg, ...)
{
	int i = narg;
	va_list args;
	cl_object rest = Cnil, *r = &rest;

	va_start(args, narg);
	while (i--)
	  r = &CDR(*r = CONS(va_arg(args, cl_object), Cnil));
	return funcall(4, siSuniversal_error_handler,
		       Ct,			/*  correctable  */
		       make_simple_string(err),	/*  continue-format-string  */
		       rest);
}

/***********************
 * Conditions signaler *
 ***********************/

void
FEcondition(int narg, cl_object name, ...)
{
	va_list args;
	cl_object rest = Cnil, *r = &rest;

	va_start(args, name);
	while (--narg) {
		*r = CONS(va_arg(args, cl_object), Cnil);
		r = &CDR(*r);
	}
	funcall(4, siSuniversal_error_handler,
		Cnil,                    /*  not correctable  */
		name,                    /*  condition name  */
		rest);
}

void
FEprogram_error(const char *s, int narg, ...)
{
	va_list args;
	cl_object rest = Cnil, *r = &rest;

	gc(t_contiguous);
	va_start(args, narg);
	while (narg--) {
		*r = CONS(va_arg(args, cl_object), Cnil);
		printf("%d\n",type_of(CAR(*r)));
		r = &CDR(*r);
	}
	funcall(4, siSuniversal_error_handler,
		Cnil,                    /*  not correctable  */
		siSsimple_program_error, /*  condition name  */
		list(4, Kformat_control, make_simple_string(s),
		     Kformat_arguments, rest));
}

void
FEcontrol_error(const char *s, int narg, ...)
{
	va_list args;
	cl_object rest = Cnil, *r = &rest;

	va_start(args, narg);
	while (narg--) {
		*r = CONS(va_arg(args, cl_object), Cnil);
		r = &CDR(*r);
	}
	funcall(4, siSuniversal_error_handler,
		Cnil,                    /*  not correctable  */
		siSsimple_control_error, /*  condition name  */
		list(4, Kformat_control, make_simple_string(s),
		     Kformat_arguments, rest));
}

void
FEcannot_open(cl_object fn)
{
	FEcondition(3, Sfile_error, Kpathname, fn);
}

void
FEend_of_file(cl_object strm)
{
	FEcondition(3, Send_of_file, Kstream, strm);
}

void
FEwrong_type_argument(cl_object type, cl_object value)
{
	FEcondition(5, Stype_error, Kdatum, value, Kexpected_type, type);
}

void
FEunbound_variable(cl_object sym)
{
	FEcondition(3, Sunbound_variable, Kname, sym);
}

void
FEundefined_function(cl_object fname)
{
	FEcondition(3, Sundefined_function, Kname, fname);
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
	FEwrong_type_argument(Sfunction, obj);
}

/*      bootstrap version                */
@(defun si::universal_error_handler (c err args)
@
	printf("\nLisp initialization error.\n");
	Lprint(1, err);
	Lprint(1, args);
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
	FEwrong_type_argument(Ssymbol, obj);
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
	int i;
	cl_object rest = Cnil, *r = &rest;
@
	for (i=narg-1; i; i--) {
	  *r = CONS(va_arg(args, cl_object), Cnil);
	  r = &CDR(*r);
	}
	funcall(4, siSuniversal_error_handler,
		Cnil,
		eformat,
		rest);
@)

@(defun cerror (cformat eformat &rest args)
	int i;
	cl_object rest = Cnil, *r = &rest;
@
	for (i=narg-2; i; i--) {
	  *r = CONS(va_arg(args, cl_object), Cnil);
	  r = &CDR(*r);
	}
	return(funcall(4, siSuniversal_error_handler,
		       cformat,
		       eformat,
		       rest));
@)

#if defined(FRAME_CHAIN) && !defined(RUNTIME)
static char *
get_current_frame(void)
{
  char *frame;
  GET_CURRENT_FRAME(frame);
  return frame;
}

@(defun si::backtrace ()
  char *this_frame, *next_frame, *next_pc;
  bool first = TRUE;
  cl_object sym;
  jmp_buf buf;
@
  /* ensure flushing of register caches */
  if (ecls_setjmp(buf) == 0) ecls_longjmp(buf, 1);

  this_frame = get_current_frame();
  while (TRUE) {
      next_frame = FRAME_CHAIN(this_frame);
      next_pc = FRAME_SAVED_PC(this_frame);
#ifdef DOWN_STACK
      if (next_frame == 0 || next_frame > (char *)cs_org) break;
#else
      if (next_frame < (char *)cs_org) break;
#endif
      sym = (cl_object)get_function_entry(next_pc);
      if (sym) {
	if (!first)
	  printf(" < ");
	else
	  first = FALSE;
	princ(sym, Cnil);
      }
/*
      else
	printf("FP: 0x%x, PC: 0x%x\n", next_frame, next_pc);
*/
      this_frame = next_frame;
  }
  @(return)
@)
#endif

void
init_error(void)
{
	null_string = make_simple_string("");
	register_root(&null_string);
}
