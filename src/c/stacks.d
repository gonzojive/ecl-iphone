/*
    stacks.c -- Binding/History/Frame stacks.
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
#ifdef HAVE_SYS_RESOURCE_H
# include <sys/time.h>
# include <sys/resource.h>
#endif

#ifndef THREADS
size_t bds_size;
bds_ptr bds_org;
bds_ptr bds_limit;
bds_ptr bds_top;

cl_index ihs_top;

size_t frs_size;
frame_ptr frs_org;
frame_ptr frs_limit;
frame_ptr frs_top;
frame_ptr nlj_fr;
cl_object nlj_tag;

int *cs_org;
int *cs_limit;
size_t cssize;

int NValues;
cl_object Values[VSSIZE];
#endif

cl_object @':catch', @':catchall', @':protect';

/********************* BINDING STACK ************************/

void
bds_overflow(void)
{
	--bds_top;
	if (bds_limit > bds_org + bds_size)
		error("bind stack overflow.");
	bds_limit += BDSGETA;
	FEerror("Bind stack overflow.", 0);
}

void
bds_unwind(bds_ptr new_bds_top)
{       register bds_ptr bds = bds_top;
	for (;  bds > new_bds_top;  bds--)
		SYM_VAL(bds->bds_sym) = bds->bds_val;
	bds_top = new_bds_top;
}

static bds_ptr
get_bds_ptr(cl_object x)
{
	bds_ptr p;

	if (FIXNUMP(x)) {
	  p = bds_org + fix(x);
	  if (bds_org <= p && p <= bds_top)
	    return(p);
	}
	FEerror("~S is an illegal bds index.", 1, x);
}

@(defun si::bds_top ()
@
	@(return MAKE_FIXNUM(bds_top - bds_org))
@)

@(defun si::bds_var (arg)
@
	@(return get_bds_ptr(arg)->bds_sym)
@)

@(defun si::bds_val (arg)
@
	@(return get_bds_ptr(arg)->bds_val)
@)

/******************** INVOCATION STACK **********************/

static cl_object
ihs_function_name(cl_object x)
{
	cl_object y;

	switch (type_of(x)) {
	case t_symbol:
		return(x);

	case t_bytecodes:
		y = x->bytecodes.data[0];
		if (Null(y))
			return(@'lambda');
		else
			return y;

	case t_cfun:
		return(x->cfun.name);

	default:
		return(Cnil);
	}
}

void
ihs_push(cl_object function)
{
	cl_stack_push(function);
	cl_stack_push(lex_env);
	cl_stack_push(MAKE_FIXNUM(ihs_top));
	ihs_top = cl_stack_index();
}

void
ihs_pop()
{
	cl_stack_set_index(ihs_top);
	ihs_top = fix(cl_stack_top[-1]);
	lex_env = cl_stack_top[-2];
	cl_stack_pop_n(3);
}

static cl_object *
get_ihs_ptr(cl_index n)
{
	cl_object *sp = &cl_stack[n];

	if (sp > cl_stack && sp <= cl_stack_top)
		return sp;
	FEerror("~S is an illegal ihs index.", 1, MAKE_FIXNUM(n));
}

static cl_index
ihs_prev(cl_index n)
{
	cl_object *sp = get_ihs_ptr(n);
	n = fixnnint(sp[-1]);
	return n;
}

static cl_index
ihs_next(cl_index n)
{
	cl_index h1 = ihs_top, h2 = ihs_top;
	while (h2 > n) {
		h1 = h2;
		h2 = ihs_prev(h1);
	}
	if (h2 == n)
		return h1;
	FEerror("Internal error: ihs record ~S not found.", 1, MAKE_FIXNUM(n));
}

cl_object
ihs_top_function_name(void)
{
	cl_index h = ihs_top;

	while (h > 0) {
		cl_object *sp = get_ihs_ptr(h);
		cl_object next_h = sp[-1];
		cl_object lex_env = sp[-2];
		cl_object name = ihs_function_name(sp[-3]);
		if (name != Cnil)
			return name;
		h = fixnnint(next_h);
	}
	return(Cnil);
}

@(defun si::ihs_top (name)
	cl_index h = ihs_top;
	cl_object *sp;
@
	name = ihs_function_name(name);
	while (h > 0) {
		cl_object *sp = get_ihs_ptr(h);
		cl_object fun = sp[-3];
		if (ihs_function_name(fun) == name)
			break;
		h = fixnnint(sp[-1]);
	}
	if (h == 0)
		h = ihs_top;
	@(return MAKE_FIXNUM(h))
@)

@(defun si::ihs-prev (x)
@
	@(return MAKE_FIXNUM(ihs_prev(fixnnint(x))))
@)

@(defun si::ihs-next (x)
@
	@(return MAKE_FIXNUM(ihs_next(fixnnint(x))))
@)

@(defun si::ihs_fun (arg)
@
	@(return get_ihs_ptr(fixnnint(arg))[-3])
@)

@(defun si::ihs_env (arg)
	cl_object lex;
@
	lex = get_ihs_ptr(ihs_next(fixnnint(arg)))[-2];
	@(return lex)
@)

/********************** FRAME STACK *************************/

static int frame_id = 0;

cl_object
new_frame_id(void)
{
  return(MAKE_FIXNUM(frame_id++));
}

int
frs_overflow(void)		/* used as condition in list.d */
{
	--frs_top;
	if (frs_limit > frs_org + frs_size)
		error("frame stack overflow.");
	frs_limit += FRSGETA;
	FEerror("Frame stack overflow.", 0);
}

frame_ptr
_frs_push(register enum fr_class class, register cl_object val)
{
	if (++frs_top >= frs_limit) frs_overflow();
	frs_top->frs_lex = lex_env;
	frs_top->frs_bds_top = bds_top;
	frs_top->frs_class = class;
	frs_top->frs_val = val;
	frs_top->frs_ihs = ihs_top;
	frs_top->frs_sp = cl_stack_index();
	return frs_top;
}

void
unwind(frame_ptr fr, cl_object tag)
{
	nlj_fr = fr;
	nlj_tag = tag;
	while (frs_top != fr
		&& frs_top->frs_class == FRS_CATCH)
	  --frs_top;
	lex_env = frs_top->frs_lex;
	ihs_top = frs_top->frs_ihs;
	bds_unwind(frs_top->frs_bds_top);
	cl_stack_set_index(frs_top->frs_sp);
	ecls_longjmp(frs_top->frs_jmpbuf, 1);
	/* never reached */
}

frame_ptr
frs_sch (cl_object frame_id)
{
	frame_ptr top;

	for (top = frs_top;  top >= frs_org;  top--)
		if (top->frs_val == frame_id && top->frs_class == FRS_CATCH)
			return(top);
	return(NULL);
}

frame_ptr
frs_sch_catch(cl_object frame_id)
{
	frame_ptr top;

	for(top = frs_top;  top >= frs_org  ;top--)
	  if ((top->frs_val == frame_id && top->frs_class == FRS_CATCH)
	      || top->frs_class == FRS_CATCHALL)
	    return(top);
	return(NULL);
}

static frame_ptr
get_frame_ptr(cl_object x)
{
	frame_ptr p;

	if (FIXNUMP(x)) {
	  p = frs_org + fix(x);
	  if (frs_org <= p && p <= frs_top)
	    return(p);
	}
	FEerror("~S is an illegal frs index.", 1, x);
}

@(defun si::frs_top ()
@
	@(return MAKE_FIXNUM(frs_top - frs_org))
@)

@(defun si::frs_bds (arg)
@
	@(return MAKE_FIXNUM(get_frame_ptr(arg)->frs_bds_top - bds_org))
@)

@(defun si::frs_class (arg)
	enum fr_class c;
	cl_object output;
@
	c = get_frame_ptr(arg)->frs_class;
	if (c == FRS_CATCH) output = @':catch';
	else if (c == FRS_PROTECT) output = @':protect';
	else if (c == FRS_CATCHALL) output = @':catchall';
	else FEerror("Unknown frs class was detected.", 0);
	@(return output)
@)

@(defun si::frs_tag (arg)
@
	@(return get_frame_ptr(arg)->frs_val)
@)

@(defun si::frs_ihs (arg)
@
	@(return MAKE_FIXNUM(get_frame_ptr(arg)->frs_ihs))
@)

@(defun si::sch_frs_base (fr ihs)
	frame_ptr x;
	cl_index y;
@
	y = fixnnint(ihs);
	for (x = get_frame_ptr(fr); x <= frs_top && x->frs_ihs < y; x++);
	@(return ((x > frs_top) ? Cnil : MAKE_FIXNUM(x - frs_org)))
@)

/********************* INITIALIZATION ***********************/

@(defun si::reset_stack_limits ()
@
	if (bds_top < bds_org + (bds_size - 2*BDSGETA))
		bds_limit = bds_org + (bds_size - 2*BDSGETA);
	else
		error("can't reset bds_limit.");
	if (frs_top < frs_org + (bds_size - 2*FRSGETA))
		frs_limit = frs_org + (frs_size - 2*FRSGETA);
	else
		error("can't reset frs_limit.");
#ifdef DOWN_STACK
	if (&narg > cs_org - cssize + 16)
		cs_limit = cs_org - cssize;
#else
	if (&narg < cs_org + cssize - 16)
		cs_limit = cs_org + cssize;
#endif
	else
		error("can't reset cs_limit.");

	@(return Cnil)
@)

void
alloc_stacks(int *new_cs_org)
{
#ifdef THREADS
	Values = main_lpd.lwp_Values;
#endif

	frs_size = FRSSIZE + 2*FRSGETA;
	frs_org = alloc(frs_size * sizeof(*frs_org));
	frs_top = frs_org-1;
	frs_limit = &frs_org[frs_size - 2*FRSGETA];
	bds_size = BDSSIZE + 2*BDSGETA;
	bds_org = alloc(bds_size * sizeof(*bds_org));
	bds_top = bds_org-1;
	bds_limit = &bds_org[bds_size - 2*BDSGETA];

	ihs_top = 0;

	cs_org = new_cs_org;
#if defined(HAVE_SYS_RESOURCE_H) && defined(RLIMIT_STACK)
	{
	  struct rlimit rl;
	  getrlimit(RLIMIT_STACK, &rl);
	  cssize = rl.rlim_cur/4 - 4*CSGETA;
	}
#else
	cssize = CSSIZE;
#endif
#ifdef DOWN_STACK
	/* Sanity check - in case rlimit is set too high */
	if (cs_org - cssize > cs_org) {
	  cssize = CSSIZE;
	}
	cs_limit = cs_org - cssize; /* in THREADS I'm assigning to the main thread clwp */
#else
	/* Sanity check - in case rlimit is set too high */
	if (cs_org + cssize < cs_org) {
	  cssize = CSSIZE;
	}
	cs_limit = cs_org + cssize;
#endif
}
