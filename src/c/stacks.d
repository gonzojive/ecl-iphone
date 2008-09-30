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
#include <signal.h>
#include <string.h>
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
	if (slot >= cl_env.bds_limit) {
		bds_overflow();
		slot = cl_env.bds_top;
	}
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
	if (slot >= cl_env.bds_limit) {
		bds_overflow();
		slot = cl_env.bds_top;
	}
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
bds_unwind_n(int n)
{
	while (n--) bds_unwind1();
}

static void
bds_set_size(cl_index size)
{
	cl_index limit = (cl_env.bds_top - cl_env.bds_org);
	if (size <= limit) {
		FEerror("Cannot shrink the binding stack below ~D.", 1,
			ecl_make_unsigned_integer(limit));
	} else {
		bds_ptr org;
		org = cl_alloc_atomic(size * sizeof(*org));
		memcpy(org, cl_env.bds_org, (cl_env.bds_top - cl_env.bds_org) * sizeof(*org));
		cl_env.bds_top = org + (cl_env.bds_top - cl_env.bds_org);
		cl_env.bds_org = org;
		cl_env.bds_limit = org + (size - 2*BDSGETA);
		cl_env.bds_size = size;
	}
}

void
bds_overflow(void)
{
	cl_index size = cl_env.bds_size;
	bds_ptr org = cl_env.bds_org;
	bds_ptr last = org + size;
	if (cl_env.bds_limit >= last) {
		ecl_internal_error("Bind stack overflow, cannot grow larger.");
	}
	cl_env.bds_limit += BDSGETA;
	cl_cerror(6, make_constant_base_string("Extend stack size"),
		  @'ext::stack-overflow', @':size', MAKE_FIXNUM(size),
		  @':type', @'ext::binding-stack');
	bds_set_size(size + (size / 2));
}

void
bds_unwind(cl_index new_bds_top_index)
{
	bds_ptr new_bds_top = new_bds_top_index + cl_env.bds_org;
	bds_ptr bds = cl_env.bds_top;
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

	case t_bclosure:
		x = x->bclosure.code;

	case t_bytecodes:
		y = x->bytecodes.name;
		if (Null(y))
			return(@'lambda');
		else
			return y;

	case t_cfun:
	case t_cfunfixed:
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
	@(return get_ihs_ptr(fixnnint(arg))->lex_env)
}

/********************** FRAME STACK *************************/

static int frame_id = 0;

cl_object
new_frame_id(void)
{
  return(MAKE_FIXNUM(frame_id++));
}

static void
frs_set_size(cl_index size)
{
	cl_index limit = (cl_env.frs_top - cl_env.frs_org);
	if (size <= limit) {
		FEerror("Cannot shrink frame stack below ~D.", 1,
			ecl_make_unsigned_integer(limit));
	} else {
		ecl_frame_ptr org;
		org = cl_alloc_atomic(size * sizeof(*org));
		memcpy(org, cl_env.frs_org, (cl_env.frs_top - cl_env.frs_org) * sizeof(*org));
		cl_env.frs_top = org + (cl_env.frs_top - cl_env.frs_org);
		cl_env.frs_org = org;
		cl_env.frs_limit = org + (size - 2*FRSGETA);
		cl_env.frs_size = size;
	}
}

static void
frs_overflow(void)		/* used as condition in list.d */
{
	cl_index size = cl_env.frs_size;
	ecl_frame_ptr org = cl_env.frs_org;
	ecl_frame_ptr last = org + size;
	if (cl_env.frs_limit >= last) {
		ecl_internal_error("Frame stack overflow, cannot grow larger.");
	}
	cl_env.frs_limit += FRSGETA;
	cl_cerror(6, make_constant_base_string("Extend stack size"),
		  @'ext::stack-overflow', @':size', MAKE_FIXNUM(size),
		  @':type', @'ext::frame-stack');
	frs_set_size(size + size / 2);
}

ecl_frame_ptr
_frs_push(register cl_object val)
{
	ecl_frame_ptr output = ++cl_env.frs_top;
	if (output >= cl_env.frs_limit) {
		frs_overflow();
		output = cl_env.frs_top;
	}
	output->frs_bds_top_index = cl_env.bds_top - cl_env.bds_org;
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
	cl_env.ihs_top = cl_env.frs_top->frs_ihs;
	bds_unwind(cl_env.frs_top->frs_bds_top_index);
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
	@(return MAKE_FIXNUM(get_frame_ptr(arg)->frs_bds_top_index))
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

cl_object
si_set_stack_size(cl_object type, cl_object size)
{
	cl_index the_size = fixnnint(size);
	if (type == @'ext::frame-stack') {
		frs_set_size(the_size);
	} else if (type == @'ext::binding-stack') {
		bds_set_size(the_size);
	} else {
		cl_stack_set_size(the_size);
	}
	@(return)
}

void
init_stacks(struct cl_env_struct *env, int *new_cs_org)
{
	static struct ihs_frame ihs_org = { NULL, NULL, NULL, 0};
	cl_index size;

	env->frs_size = size = FRSSIZE + 2*FRSGETA;
	env->frs_org = (ecl_frame_ptr)cl_alloc_atomic(size * sizeof(*env->frs_org));
	env->frs_top = env->frs_org-1;
	env->frs_limit = &env->frs_org[size - 2*FRSGETA];
	env->bds_size = size = BDSSIZE + 2*BDSGETA;
	env->bds_org = (bds_ptr)cl_alloc_atomic(size * sizeof(*env->bds_org));
	env->bds_top = env->bds_org-1;
	env->bds_limit = &env->bds_org[size - 2*BDSGETA];

	env->ihs_top = &ihs_org;
	ihs_org.function = @'si::top-level';
	ihs_org.lex_env = Cnil;
	ihs_org.index = 0;

	env->cs_org = new_cs_org;
#if defined(HAVE_SYS_RESOURCE_H) && defined(RLIMIT_STACK)
	{
	  struct rlimit rl;
	  getrlimit(RLIMIT_STACK, &rl);
	  env->cs_size = rl.rlim_cur/4 - 4*CSGETA;
	}
#else
	env->cs_size = CSSIZE;
#endif
#ifdef DOWN_STACK
	/* Sanity check - in case rlimit is set too high */
	if (env->cs_org - env->cs_size > env->cs_org) {
		env->cs_size = CSSIZE;
	}
	env->cs_limit = env->cs_org - env->cs_size; /* in THREADS I'm assigning to the main thread clwp */
#else
	/* Sanity check - in case rlimit is set too high */
	if (env->cs_org + env->cs_size < env->cs_org) {
	  env->cs_size = CSSIZE;
	}
	env->cs_limit = env->cs_org + env->cs_size;
#endif
#if defined(HAVE_SIGPROCMASK) && defined(SA_SIGINFO)
	{
	stack_t new_stack;
	env->altstack_size = SIGSTKSZ + (sizeof(double)*16) + (sizeof(cl_object)*4);
	env->altstack = cl_alloc_atomic(env->altstack_size);
	memset(&new_stack, 0, sizeof(new_stack));
	new_stack.ss_size = env->altstack_size;
	new_stack.ss_sp = env->altstack;
	new_stack.ss_flags = 0;
	sigaltstack(&new_stack, NULL);
	}
#endif
}
