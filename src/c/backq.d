/*
    backq.c -- Backquote mechanism.
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
#include "internal.h"

/******************************* ------- ******************************/

#define	QUOTE	1
#define	EVAL	2
#define	LIST	3
#define	LISTX	4
#define	APPEND	5
#define	NCONC	6

extern int _cl_backq_car(cl_object *px);

static cl_object
kwote(cl_object x)
{
	cl_type t = type_of(x);
	if ((t == t_symbol &&
	     ((enum ecl_stype)x->symbol.stype != stp_constant || SYM_VAL(x) != x))
	    || t == t_cons || t == t_vector)
	   return(CONS(@'quote', CONS(x, Cnil)));
	else return(x);
}

/*
	_cl_backq_cdr(&x) puts result into x and returns one of

		QUOTE		the form should be quoted
		EVAL		the form should be evaluated
		LIST		the form should be applied to LIST
		LISTX		the form should be applied to LIST*
		APPEND		the form should be applied to APPEND
		NCONC		the form should be applied to NCONC
*/
static int
_cl_backq_cdr(cl_object *px)
{
	cl_object x = *px;
	int a, d;

	if (ATOM(x))
		return(QUOTE);
	if (CAR(x) == @'si::unquote') {
		*px = CADR(x);
		return(EVAL);
	}
	if (CAR(x) == @'si::unquote-splice' || CAR(x) == @'si::unquote-nsplice')
		FEerror(",@@ or ,. has appeared in an illegal position.", 0);
	{ cl_object ax, dx;
	  a = _cl_backq_car(&CAR(x));
	  d = _cl_backq_cdr(&CDR(x));
	  ax = CAR(x); dx = CDR(x);
	  if (d == QUOTE)
		switch (a) {
		case QUOTE:
			return(QUOTE);

		case EVAL:
			if (Null(dx))
				return(LIST);
			if (CONSP(dx) && Null(CDR(dx))) {
				CDR(x) = CONS(kwote(CAR(dx)), Cnil);
				return(LIST);
			}
			CDR(x) = CONS(kwote(dx), Cnil);
			return(LISTX);

		case APPEND:
		case NCONC:
			if (Null(dx)) {
				*px = ax;
				return(EVAL);
			      }
			CDR(x) = CONS(kwote(dx), Cnil);
			return(a);

		default:
			error("backquote botch");
		}
	  if (d == EVAL)
		switch (a) {
		case QUOTE:
			CAR(x) = kwote(ax);
			CDR(x) = CONS(dx, Cnil);
			return(LISTX);

		case EVAL:
			CDR(x) = CONS(dx, Cnil);
			return(LISTX);

		case APPEND:
		case NCONC:
			CDR(x) = CONS(dx, Cnil);
			return(a);

		default:
			error("backquote botch");
		}
	  if (d == a)
		return(d);
	  switch (d) {
	  case LIST:
		if (a == QUOTE) {
			CAR(x) = kwote(ax);
			return(LIST);
		}
		if (a == EVAL)
			return(LIST);
		CDR(x) = CONS(@'list', CDR(x));
		break;

	  case LISTX:
		if (a == QUOTE) {
			CAR(x) = kwote(ax);
			return(LISTX);
		}
		if (a == EVAL)
			return(LISTX);
		CDR(x) = CONS(@'list*', CDR(x));
		break;

	  case APPEND:
		CDR(x) = CONS(@'append', CDR(x));
		break;

	  case NCONC:
		CDR(x) = CONS(@'nconc', CDR(x));
		break;

	  default:
		error("backquote botch");
	  }
	  switch (a) {
	  case QUOTE:
		CAR(x) = kwote(ax);
		CDR(x) = CONS(CDR(x), Cnil);
		return(LISTX);

	  case EVAL:
		CDR(x) = CONS(CDR(x), Cnil);
		return(LISTX);

	  case APPEND:
	  case NCONC:
		CDR(x) = CONS(CDR(x), Cnil);
		return(a);

	  default:
		error("backquote botch");
	  }
	}
}

/*
	_cl_backq_car(&x) puts result into x and returns one of

		QUOTE		the form should be quoted
		EVAL		the form should be evaluated
		APPEND		the form should be appended
				into the outer form
		NCONC		the form should be nconc'ed
				into the outer form
*/
int
_cl_backq_car(cl_object *px)
{
	cl_object x = *px;
	int d;

	if (ATOM(x))
		return(QUOTE);
	if (CAR(x) == @'si::unquote') {
		*px = CADR(x);
		return(EVAL);
	}
	if (CAR(x) == @'si::unquote-splice') {
		*px = CADR(x);
		return(APPEND);
	}
	if (CAR(x) == @'si::unquote-nsplice') {
		*px = CADR(x);
		return(NCONC);
	}
	d = _cl_backq_cdr(px);
	switch (d) {
	case QUOTE:
	case EVAL:
		return(d);

	case LIST:
		*px = CONS(@'list', *px);
		break;

	case LISTX:
		*px = CONS(@'list*', *px);
		break;

	case APPEND:
		*px = CONS(@'append', *px);
		break;

	case NCONC:
		*px = CONS(@'nconc', *px);
		break;

	default:
		error("backquote botch");
	}
	return(EVAL);
}

static cl_object
backq(cl_object x)
{
	int a;

	a = _cl_backq_car(&x);
	if (a == APPEND || a == NCONC)
		FEerror(",@@ or ,. has appeared in an illegal position.", 0);
	if (a == QUOTE)
		return(kwote(x));
	return(x);
}

static cl_object
quasiquote_macro(cl_object whole, cl_object env)
{
	if (length(whole) != 2) {
		FEprogram_error("Syntax error: ~S.", 1, whole);
	}
	@(return backq(CADR(whole)))
}

void
init_backq(void)
{
	cl_def_c_macro(@'si::quasiquote', quasiquote_macro, 2);
}
