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

/******************************* EXPORTS ******************************/
#ifndef THREADS
int backq_level;
#endif
/******************************* ------- ******************************/

/* #define attach(x)	(*px = CONS(x, *px)) */
#define attach(s)	CDR(x) = CONS(s, CDR(x));

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
	     ((enum stype)x->symbol.stype != stp_constant || SYM_VAL(x) != x))
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

	cs_check(px);

	if (ATOM(x))
		return(QUOTE);
	if (CAR(x) == @'si::,') {
		*px = CDR(x);
		return(EVAL);
	}
	if (CAR(x) == @'si::,@' || CAR(x) == @'si::,.')
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
		attach(@'list');
		break;

	  case LISTX:
		if (a == QUOTE) {
			CAR(x) = kwote(ax);
			return(LISTX);
		}
		if (a == EVAL)
			return(LISTX);
		attach(@'list*');
		break;

	  case APPEND:
		attach(@'append');
		break;

	  case NCONC:
		attach(@'nconc');
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

	cs_check(px);

	if (ATOM(x))
		return(QUOTE);
	if (CAR(x) == @'si::,') {
		*px = CDR(x);
		return(EVAL);
	}
	if (CAR(x) == @'si::,@') {
		*px = CDR(x);
		return(APPEND);
	}
	if (CAR(x) == @'si::,.') {
		*px = CDR(x);
		return(NCONC);
	}
	d = _cl_backq_cdr(px);
	switch (d) {
	case QUOTE:
	case EVAL:
		return(d);

	case LIST:
/*		attach(@'list'); */
		*px = CONS(@'list', *px);
		break;

	case LISTX:
/*		attach(@'list*'); */
		*px = CONS(@'list*', *px);
		break;

	case APPEND:
/*		attach(@'append'); */
		*px = CONS(@'append', *px);
		break;

	case NCONC:
/*		attach(@'nconc'); */
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

static
@(defun "comma_reader" (in c)
	cl_object x, y;
@
	if (backq_level <= 0)
		FEerror("A comma has appeared out of a backquote.", 0);
	c = peek_char(FALSE, in);
	if (c == CODE_CHAR('@@')) {
		x = @'si::,@';
		read_char(in);
	} else if (c == CODE_CHAR('.')) {
		x = @'si::,.';
		read_char(in);
	} else
		x = @'si::,';
	--backq_level;
	y = read_object(in);
	backq_level++;
	@(return CONS(x, y))
@)

static
@(defun "backquote_reader" (in c)
@
	backq_level++;
	in = read_object(in);
	--backq_level;
	@(return backq(in))
@)

#define	make_cf(f)	make_cfun((f), Cnil, NULL);

void
init_backq(void)
{
	cl_object r;

	r = standard_readtable;
	r->readtable.table['`'].syntax_type = cat_terminating;
	r->readtable.table['`'].macro = make_cf((cl_objectfn)backquote_reader);
	r->readtable.table[','].syntax_type = cat_terminating;
	r->readtable.table[','].macro = make_cf((cl_objectfn)comma_reader);

	backq_level = 0;
}
