/*
    stacks.c -- Binding/History/Frame stacks.
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
#ifdef HAVE_SYS_RESOURCE_H
# include <sys/time.h>
# include <sys/resource.h>
#endif

#ifndef THREADS
size_t bds_size;
bds_ptr bds_org;
bds_ptr bds_limit;
bds_ptr bds_top;

struct ihs_frame ihs_org = { NULL, Cnil, Cnil, 0 };
ihs_ptr ihs_top;

size_t frs_size;
frame_ptr frs_org;
frame_ptr frs_limit;
frame_ptr frs_top;
frame_ptr nlj_fr;

int *cs_org;
int *cs_limit;
size_t cssize;

int NValues;
cl_object Values[VSSIZE];
#endif

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

cl_object
si_bds_top()
{
	@(return MAKE_FIXNUM(bds_top - bds_org))
}

cl_object
si_bds_var(cl_object arg)
{
	@(return get_bds_ptr(arg)->bds_sym)
}

cl_object
si_bds_val(cl_object arg)
{
	@(return get_bds_ptr(arg)->bds_val)
}

/******************** INVOCATION STACK **********************/

static cl_object
ihs_function_name(cl_object x)
{
	cl_object y;

	switch (type_of(x)) {
	case t_symbol:
		return(x);

	case t_bytecodes:
		y = x->bytecodes.name;
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

static ihs_ptr
get_ihs_ptr(cl_index n)
{
	ihs_ptr p = ihs_top;
	if (n > p->index)
		FEerror("~D is an illegal IHS index.", 1, MAKE_FIXNUM(n));
	while (n < p->index)
		p = p->next;
	return p;
}

cl_object
ihs_top_function_name(void)
{
	return ihs_function_name(ihs_top->function);
}

cl_object
si_ihs_top(cl_object name)
{
	@(return MAKE_FIXNUM(ihs_top->index))
}

cl_object
si_ihs_prev(cl_object x)
{
	@(return cl_1M(x))
}

cl_object
si_ihs_next(cl_object x)
{
	@(return cl_1P(x))
}

cl_object
si_ihs_fun(cl_object arg)
{
	@(return get_ihs_ptr(fixnnint(arg))->function)
}

cl_object
si_ihs_env(cl_object arg)
{
	@(return get_ihs_ptr(fixnnint(si_ihs_next(arg)))->lex_env)
}

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
_frs_push(register enum fr_class clas, register cl_object val)
{
	if (++frs_top >= frs_limit) frs_overflow();
	frs_top->frs_lex = lex_env;
	frs_top->frs_bds_top = bds_top;
	frs_top->frs_class = clas;
	frs_top->frs_val = val;
	frs_top->frs_ihs = ihs_top;
	frs_top->frs_sp = cl_stack_index();
	return frs_top;
}

void
unwind(frame_ptr fr)
{
	nlj_fr = fr;
	while (frs_top != fr
		&& frs_top->frs_class == FRS_CATCH)
	  --frs_top;
	lex_env = frs_top->frs_lex;
	ihs_top = frs_top->frs_ihs;
	bds_unwind(frs_top->frs_bds_top);
	cl_stack_set_index(frs_top->frs_sp);
	ecl_longjmp(frs_top->frs_jmpbuf, 1);
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

cl_object
si_frs_top()
{
	@(return MAKE_FIXNUM(frs_top - frs_org))
}

cl_object
si_frs_bds(cl_object arg)
{
	@(return MAKE_FIXNUM(get_frame_ptr(arg)->frs_bds_top - bds_org))
}

cl_object
si_frs_class(cl_object arg)
{
	enum fr_class c;
	cl_object output;

	c = get_frame_ptr(arg)->frs_class;
	if (c == FRS_CATCH) output = @':catch';
	else if (c == FRS_PROTECT) output = @':protect';
	else if (c == FRS_CATCHALL) output = @':catchall';
	else FEerror("Unknown frs class was detected.", 0);
	@(return output)
}

cl_object
si_frs_tag(cl_object arg)
{
	@(return get_frame_ptr(arg)->frs_val)
}

cl_object
si_frs_ihs(cl_object arg)
{
	@(return MAKE_FIXNUM(get_frame_ptr(arg)->frs_ihs->index))
}

cl_object
si_sch_frs_base(cl_object fr, cl_object ihs)
{
	frame_ptr x;
	cl_index y;

	y = fixnnint(ihs);
	for (x = get_frame_ptr(fr); x <= frs_top && x->frs_ihs->index < y; x++);
	@(return ((x > frs_top) ? Cnil : MAKE_FIXNUM(x - frs_org)))
}

/********************* INITIALIZATION ***********************/

cl_object
si_reset_stack_limits()
{
	volatile int foo = 0;
	if (bds_top < bds_org + (bds_size - 2*BDSGETA))
		bds_limit = bds_org + (bds_size - 2*BDSGETA);
	else
		error("can't reset bds_limit.");
	if (frs_top < frs_org + (bds_size - 2*FRSGETA))
		frs_limit = frs_org + (frs_size - 2*FRSGETA);
	else
		error("can't reset frs_limit.");
#ifdef DOWN_STACK
	if (&foo > cs_org - cssize + 16)
		cs_limit = cs_org - cssize;
#else
	if (&foo < cs_org + cssize - 16)
		cs_limit = cs_org + cssize;
#endif
	else
		error("can't reset cs_limit.");

	@(return Cnil)
}

void
init_stacks(int *new_cs_org)
{
#ifdef THREADS
	Values = main_lpd.lwp_Values;
#endif

	frs_size = FRSSIZE + 2*FRSGETA;
	frs_org = (frame_ptr)cl_alloc(frs_size * sizeof(*frs_org));
	frs_top = frs_org-1;
	frs_limit = &frs_org[frs_size - 2*FRSGETA];
	bds_size = BDSSIZE + 2*BDSGETA;
	bds_org = (bds_ptr)cl_alloc(bds_size * sizeof(*bds_org));
	bds_top = bds_org-1;
	bds_limit = &bds_org[bds_size - 2*BDSGETA];

	ihs_top = &ihs_org;
	ihs_org.function = @'si::top-level';
	ihs_org.lex_env = Cnil;
	ihs_org.index = 0;

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
