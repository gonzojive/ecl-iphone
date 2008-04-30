/* -*- mode: c; c-basic-offset: 8 -*- */
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

#include <ecl/ecl.h>
#ifdef HAVE_SYS_RESOURCE_H
# include <sys/time.h>
# include <sys/resource.h>
#endif

/********************* BINDING STACK ************************/

#ifdef ECL_THREADS
void
bds_bind(cl_object s, cl_object value)
{
	struct ecl_hashtable_entry *h = ecl_search_hash(s, cl_env.bindings_hash);
	struct bds_bd *slot = ++cl_env.bds_top;
	if (h->key == OBJNULL) {
		/* The previous binding was at most global */
		slot->symbol = s;
		slot->value = OBJNULL;
		ecl_sethash(s, cl_env.bindings_hash, value);
	} else {
		/* We have to save a dynamic binding */
		slot->symbol = h->key;
		slot->value = h->value;
		h->value = value;
	}
	s->symbol.dynamic |= 1;
}

void
bds_push(cl_object s)
{
	struct ecl_hashtable_entry *h = ecl_search_hash(s, cl_env.bindings_hash);
	struct bds_bd *slot = ++cl_env.bds_top;
	if (h->key == OBJNULL) {
		/* The previous binding was at most global */
		slot->symbol = s;
		slot->value = OBJNULL;
		ecl_sethash(s, cl_env.bindings_hash, s->symbol.value);
	} else {
		/* We have to save a dynamic binding */
		slot->symbol = h->key;
		slot->value = h->value;
	}
	s->symbol.dynamic |= 1;
}

void
bds_unwind1(void)
{
	struct bds_bd *slot = cl_env.bds_top--;
	cl_object s = slot->symbol;
	struct ecl_hashtable_entry *h = ecl_search_hash(s, cl_env.bindings_hash);
	if (slot->value == OBJNULL) {
		/* We have deleted all dynamic bindings */
		h->key = OBJNULL;
		h->value = OBJNULL;
		cl_env.bindings_hash->hash.entries--;
	} else {
		/* We restore the previous dynamic binding */
		h->value = slot->value;
	}
}

void
bds_unwind_n(int n)
{
	while (n--) bds_unwind1();
}

cl_object *
ecl_symbol_slot(cl_object s)
{
	if (Null(s))
		s = Cnil_symbol;
	if (s->symbol.dynamic) {
		struct ecl_hashtable_entry *h = ecl_search_hash(s, cl_env.bindings_hash);
		if (h->key != OBJNULL)
			return &h->value;
	}
	return &s->symbol.value;
}

cl_object
ecl_set_symbol(cl_object s, cl_object value)
{
	if (s->symbol.dynamic) {
		struct ecl_hashtable_entry *h = ecl_search_hash(s, cl_env.bindings_hash);
		if (h->key != OBJNULL) {
			return (h->value = value);
		}
	}
	return (s->symbol.value = value);
}
#endif

void
bds_overflow(void)
{
	--cl_env.bds_top;
	if (cl_env.bds_limit > cl_env.bds_org + cl_env.bds_size)
		ecl_internal_error("bind stack overflow.");
	cl_env.bds_limit += BDSGETA;
	FEerror("Bind stack overflow.", 0);
}

void
bds_unwind(bds_ptr new_bds_top)
{
	register bds_ptr bds = cl_env.bds_top;
	for (;  bds > new_bds_top;  bds--)
#ifdef ECL_THREADS
		bds_unwind1();
#else
		bds->symbol->symbol.value = bds->value;
#endif
	cl_env.bds_top = new_bds_top;
}

static bds_ptr
get_bds_ptr(cl_object x)
{
	bds_ptr p;

	if (FIXNUMP(x)) {
	  p = cl_env.bds_org + fix(x);
	  if (cl_env.bds_org <= p && p <= cl_env.bds_top)
	    return(p);
	}
	FEerror("~S is an illegal bds index.", 1, x);
}

cl_object
si_bds_top()
{
	@(return MAKE_FIXNUM(cl_env.bds_top - cl_env.bds_org))
}

cl_object
si_bds_var(cl_object arg)
{
	@(return get_bds_ptr(arg)->symbol)
}

cl_object
si_bds_val(cl_object arg)
{
	@(return get_bds_ptr(arg)->value)
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
	ihs_ptr p = cl_env.ihs_top;
	if (n > p->index)
		FEerror("~D is an illegal IHS index.", 1, MAKE_FIXNUM(n));
	while (n < p->index)
		p = p->next;
	return p;
}

cl_object
ihs_top_function_name(void)
{
	return ihs_function_name(cl_env.ihs_top->function);
}

cl_object
si_ihs_top(cl_object name)
{
	@(return MAKE_FIXNUM(cl_env.ihs_top->index))
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
	--cl_env.frs_top;
	if (cl_env.frs_limit > cl_env.frs_org + cl_env.frs_size)
		ecl_internal_error("frame stack overflow.");
	cl_env.frs_limit += FRSGETA;
	FEerror("Frame stack overflow.", 0);
}

ecl_frame_ptr
_frs_push(register cl_object val)
{
	ecl_frame_ptr output = ++cl_env.frs_top;
	if (output >= cl_env.frs_limit) frs_overflow();
	output->frs_lex = cl_env.lex_env;
	output->frs_bds_top = cl_env.bds_top;
	output->frs_val = val;
	output->frs_ihs = cl_env.ihs_top;
	output->frs_sp = cl_stack_index();
	return output;
}

void
ecl_unwind(ecl_frame_ptr fr)
{
	cl_env.nlj_fr = fr;
	while (cl_env.frs_top != fr && cl_env.frs_top->frs_val != ECL_PROTECT_TAG)
		--cl_env.frs_top;
	cl_env.lex_env = cl_env.frs_top->frs_lex;
	cl_env.ihs_top = cl_env.frs_top->frs_ihs;
	bds_unwind(cl_env.frs_top->frs_bds_top);
	cl_stack_set_index(cl_env.frs_top->frs_sp);
	ecl_longjmp(cl_env.frs_top->frs_jmpbuf, 1);
	/* never reached */
}

ecl_frame_ptr
frs_sch (cl_object frame_id)
{
	ecl_frame_ptr top;

	for (top = cl_env.frs_top;  top >= cl_env.frs_org;  top--)
		if (top->frs_val == frame_id)
			return(top);
	return(NULL);
}

static ecl_frame_ptr
get_frame_ptr(cl_object x)
{
	ecl_frame_ptr p;

	if (FIXNUMP(x)) {
	  p = cl_env.frs_org + fix(x);
	  if (cl_env.frs_org <= p && p <= cl_env.frs_top)
	    return(p);
	}
	FEerror("~S is an illegal frs index.", 1, x);
}

cl_object
si_frs_top()
{
	@(return MAKE_FIXNUM(cl_env.frs_top - cl_env.frs_org))
}

cl_object
si_frs_bds(cl_object arg)
{
	@(return MAKE_FIXNUM(get_frame_ptr(arg)->frs_bds_top - cl_env.bds_org))
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
	ecl_frame_ptr x;
	cl_index y;

	y = fixnnint(ihs);
	for (x = get_frame_ptr(fr); 
	     x <= cl_env.frs_top && x->frs_ihs->index < y;
	     x++);
	@(return ((x > cl_env.frs_top) ? Cnil : MAKE_FIXNUM(x - cl_env.frs_org)))
}

/********************* INITIALIZATION ***********************/

cl_object
si_reset_stack_limits()
{
	volatile int foo = 0;
	if (cl_env.bds_top < cl_env.bds_org + (cl_env.bds_size - 2*BDSGETA))
		cl_env.bds_limit = cl_env.bds_org + (cl_env.bds_size - 2*BDSGETA);
	else
		ecl_internal_error("can't reset bds_limit.");
	if (cl_env.frs_top < cl_env.frs_org + (cl_env.frs_size - 2*FRSGETA))
		cl_env.frs_limit = cl_env.frs_org + (cl_env.frs_size - 2*FRSGETA);
	else
		ecl_internal_error("can't reset frs_limit.");
#ifdef DOWN_STACK
	if (&foo > cl_env.cs_org - cl_env.cs_size + 16)
		cl_env.cs_limit = cl_env.cs_org - cl_env.cs_size;
#else
	if (&foo < cl_env.cs_org + cl_env.cs_size - 16)
		cl_env.cs_limit = cl_env.cs_org + cl_env.cs_size;
#endif
	else
		ecl_internal_error("can't reset cl_env.cs_limit.");

	@(return Cnil)
}

void
init_stacks(int *new_cs_org)
{
	static struct ihs_frame ihs_org = { NULL, NULL, NULL, 0};
	cl_index size;

	cl_env.frs_size = size = FRSSIZE + 2*FRSGETA;
	cl_env.frs_org = (ecl_frame_ptr)cl_alloc_atomic(size * sizeof(*cl_env.frs_org));
	cl_env.frs_top = cl_env.frs_org-1;
	cl_env.frs_limit = &cl_env.frs_org[size - 2*FRSGETA];
	cl_env.bds_size = size = BDSSIZE + 2*BDSGETA;
	cl_env.bds_org = (bds_ptr)cl_alloc_atomic(size * sizeof(*cl_env.bds_org));
	cl_env.bds_top = cl_env.bds_org-1;
	cl_env.bds_limit = &cl_env.bds_org[size - 2*BDSGETA];

	cl_env.ihs_top = &ihs_org;
	ihs_org.function = @'si::top-level';
	ihs_org.lex_env = Cnil;
	ihs_org.index = 0;

	cl_env.cs_org = new_cs_org;
#if defined(HAVE_SYS_RESOURCE_H) && defined(RLIMIT_STACK)
	{
	  struct rlimit rl;
	  getrlimit(RLIMIT_STACK, &rl);
	  cl_env.cs_size = rl.rlim_cur/4 - 4*CSGETA;
	}
#else
	cl_env.cs_size = CSSIZE;
#endif
#ifdef DOWN_STACK
	/* Sanity check - in case rlimit is set too high */
	if (cl_env.cs_org - cl_env.cs_size > cl_env.cs_org) {
	  cl_env.cs_size = CSSIZE;
	}
	cl_env.cs_limit = cl_env.cs_org - cl_env.cs_size; /* in THREADS I'm assigning to the main thread clwp */
#else
	/* Sanity check - in case rlimit is set too high */
	if (cl_env.cs_org + cl_env.cs_size < cl_env.cs_org) {
	  cl_env.cs_size = CSSIZE;
	}
	cl_env.cs_limit = cl_env.cs_org + cl_env.cs_size;
#endif
}
