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

/************************ C STACK ***************************/

static void
cs_set_size(cl_env_ptr env, cl_index new_size)
{
	volatile int foo = 0;
	cl_index safety_area = ecl_get_option(ECL_OPT_C_STACK_SAFETY_AREA);
	new_size += 2*safety_area;
#ifdef ECL_DOWN_STACK
	if (&foo > env->cs_org - new_size + 16) {
		env->cs_limit = env->cs_org - new_size + 2*safety_area;
		if (env->cs_limit < env->cs_barrier)
			env->cs_barrier = env->cs_limit;
	}
#else
	if (&foo < env->cs_org + new_size - 16) {
		env->cs_limit = env->cs_org + new_size - 2*safety_area;
		if (env->cs_limit > env->cs_barrier)
			env->cs_barrier = env->cs_limit;
	}
#endif
	else
		ecl_internal_error("can't reset env->cs_limit.");
	env->cs_size = new_size;
}

void
ecl_cs_overflow(void)
{
	cl_env_ptr env = ecl_process_env();
	cl_index safety_area = ecl_get_option(ECL_OPT_C_STACK_SAFETY_AREA);
	cl_index size = env->cs_size;
#ifdef ECL_DOWN_STACK
	if (env->cs_limit > env->cs_org - size)
		env->cs_limit -= safety_area;
#else
	if (env->cs_limit < env->cs_org + size)
		env->cs_limit += safety_area;
#endif
	else
		ecl_internal_error("Cannot grow stack size.");
	cl_cerror(6, make_constant_base_string("Extend stack size"),
		  @'ext::stack-overflow', @':size', MAKE_FIXNUM(size),
		  @':type', @'ext::c-stack');
	size += size / 2;
	cs_set_size(env, size);
}


/********************* BINDING STACK ************************/

#ifdef ECL_THREADS
void
bds_bind(cl_object s, cl_object value)
{
	cl_env_ptr env = ecl_process_env();
	struct ecl_hashtable_entry *h = ecl_search_hash(s, cl_env.bindings_hash);
	struct bds_bd *slot = ++env->bds_top;
	if (slot >= env->bds_limit) {
		bds_overflow();
		slot = env->bds_top;
	}
	if (h->key == OBJNULL) {
		/* The previous binding was at most global */
		slot->symbol = s;
		slot->value = OBJNULL;
		ecl_sethash(s, env->bindings_hash, value);
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
	cl_env_ptr env = ecl_process_env();
	struct ecl_hashtable_entry *h = ecl_search_hash(s, env->bindings_hash);
	struct bds_bd *slot = ++env->bds_top;
	if (slot >= env->bds_limit) {
		bds_overflow();
		slot = env->bds_top;
	}
	if (h->key == OBJNULL) {
		/* The previous binding was at most global */
		slot->symbol = s;
		slot->value = OBJNULL;
		ecl_sethash(s, env->bindings_hash, s->symbol.value);
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
	cl_env_ptr env = ecl_process_env();
	struct bds_bd *slot = env->bds_top--;
	cl_object s = slot->symbol;
	struct ecl_hashtable_entry *h = ecl_search_hash(s, env->bindings_hash);
	if (slot->value == OBJNULL) {
		/* We have deleted all dynamic bindings */
		h->key = OBJNULL;
		h->value = OBJNULL;
		env->bindings_hash->hash.entries--;
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
		cl_env_ptr env = ecl_process_env();
		struct ecl_hashtable_entry *h = ecl_search_hash(s, env->bindings_hash);
		if (h->key != OBJNULL)
			return &h->value;
	}
	return &s->symbol.value;
}

cl_object
ecl_set_symbol(cl_object s, cl_object value)
{
	if (s->symbol.dynamic) {
		cl_env_ptr env = ecl_process_env();
		struct ecl_hashtable_entry *h = ecl_search_hash(s, env->bindings_hash);
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
bds_set_size(cl_env_ptr env, cl_index size)
{
	bds_ptr old_org = env->bds_org;
	cl_index limit = env->bds_top - old_org;
	if (size <= limit) {
		FEerror("Cannot shrink the binding stack below ~D.", 1,
			ecl_make_unsigned_integer(limit));
	} else {
		cl_index margin = ecl_get_option(ECL_OPT_BIND_STACK_SAFETY_AREA);
		bds_ptr org;
		org = ecl_alloc_atomic(size * sizeof(*org));

		ecl_disable_interrupts_env(env);
		memcpy(org, old_org, (limit + 1) * sizeof(*org));
		env->bds_top = org + limit;
		env->bds_org = org;
		env->bds_limit = org + (size - 2*margin);
		env->bds_size = size;
		ecl_enable_interrupts_env(env);

		cl_dealloc(old_org);
	}
}

void
bds_overflow(void)
{
	cl_env_ptr env = ecl_process_env();
	cl_index margin = ecl_get_option(ECL_OPT_BIND_STACK_SAFETY_AREA);
	cl_index size = env->bds_size;
	bds_ptr org = env->bds_org;
	bds_ptr last = org + size;
	if (env->bds_limit >= last) {
		ecl_internal_error("Bind stack overflow, cannot grow larger.");
	}
	env->bds_limit += margin;
	cl_cerror(6, make_constant_base_string("Extend stack size"),
		  @'ext::stack-overflow', @':size', MAKE_FIXNUM(size),
		  @':type', @'ext::binding-stack');
	bds_set_size(env, size + (size / 2));
}

void
bds_unwind(cl_index new_bds_top_index)
{
	cl_env_ptr env = ecl_process_env();
	bds_ptr new_bds_top = new_bds_top_index + env->bds_org;
	bds_ptr bds = env->bds_top;
	for (;  bds > new_bds_top;  bds--)
#ifdef ECL_THREADS
		bds_unwind1();
#else
		bds->symbol->symbol.value = bds->value;
#endif
	env->bds_top = new_bds_top;
}

static bds_ptr
get_bds_ptr(cl_object x)
{
	if (FIXNUMP(x)) {
		cl_env_ptr env = ecl_process_env();
		bds_ptr p = env->bds_org + fix(x);
		if (env->bds_org <= p && p <= env->bds_top)
			return(p);
	}
	FEerror("~S is an illegal bds index.", 1, x);
}

cl_object
si_bds_top()
{
	cl_env_ptr env = ecl_process_env();
	@(return MAKE_FIXNUM(env->bds_top - env->bds_org))
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
	cl_env_ptr env = ecl_process_env();
	ihs_ptr p = env->ihs_top;
	if (n > p->index)
		FEerror("~D is an illegal IHS index.", 1, MAKE_FIXNUM(n));
	while (n < p->index)
		p = p->next;
	return p;
}

cl_object
ihs_top_function_name(void)
{
	cl_env_ptr env = ecl_process_env();
	return ihs_function_name(env->ihs_top->function);
}

cl_object
si_ihs_top(cl_object name)
{
	cl_env_ptr env = ecl_process_env();
	@(return MAKE_FIXNUM(env->ihs_top->index))
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
	return MAKE_FIXNUM(frame_id++);
}

static void
frs_set_size(cl_env_ptr env, cl_index size)
{
	ecl_frame_ptr old_org = env->frs_top;
	cl_index limit = env->frs_top - old_org;
	if (size <= limit) {
		FEerror("Cannot shrink frame stack below ~D.", 1,
			ecl_make_unsigned_integer(limit));
	} else {
		cl_index margin = ecl_get_option(ECL_OPT_FRAME_STACK_SAFETY_AREA);
		ecl_frame_ptr org;
		size += 2*margin;
		org = ecl_alloc_atomic(size * sizeof(*org));

		ecl_disable_interrupts_env(env);
		memcpy(org, old_org, (limit + 1) * sizeof(*org));
		env->frs_top = org + limit;
		env->frs_org = org;
		env->frs_limit = org + (size - 2*margin);
		env->frs_size = size;
		ecl_enable_interrupts_env(env);

		cl_dealloc(old_org);
	}
}

static void
frs_overflow(void)		/* used as condition in list.d */
{
	cl_env_ptr env = ecl_process_env();
	cl_index margin = ecl_get_option(ECL_OPT_FRAME_STACK_SAFETY_AREA);
	cl_index size = env->frs_size;
	ecl_frame_ptr org = env->frs_org;
	ecl_frame_ptr last = org + size;
	if (env->frs_limit >= last) {
		ecl_internal_error("Frame stack overflow, cannot grow larger.");
	}
	env->frs_limit += margin;
	cl_cerror(6, make_constant_base_string("Extend stack size"),
		  @'ext::stack-overflow', @':size', MAKE_FIXNUM(size),
		  @':type', @'ext::frame-stack');
	frs_set_size(env, size + size / 2);
}

ecl_frame_ptr
_frs_push(register cl_object val)
{
	cl_env_ptr env = ecl_process_env();
	ecl_frame_ptr output = ++env->frs_top;
	if (output >= env->frs_limit) {
		frs_overflow();
		output = env->frs_top;
	}
	output->frs_bds_top_index = env->bds_top - env->bds_org;
	output->frs_val = val;
	output->frs_ihs = env->ihs_top;
	output->frs_sp = cl_stack_index();
	return output;
}

void
ecl_unwind(ecl_frame_ptr fr)
{
	cl_env_ptr env = ecl_process_env();
	env->nlj_fr = fr;
	while (env->frs_top != fr && env->frs_top->frs_val != ECL_PROTECT_TAG)
		--env->frs_top;
	env->ihs_top = env->frs_top->frs_ihs;
	bds_unwind(env->frs_top->frs_bds_top_index);
	cl_stack_set_index(env->frs_top->frs_sp);
	ecl_longjmp(env->frs_top->frs_jmpbuf, 1);
	/* never reached */
}

ecl_frame_ptr
frs_sch (cl_object frame_id)
{
	cl_env_ptr env = ecl_process_env();
	ecl_frame_ptr top;
	for (top = env->frs_top;  top >= env->frs_org;  top--)
		if (top->frs_val == frame_id)
			return(top);
	return(NULL);
}

static ecl_frame_ptr
get_frame_ptr(cl_object x)
{
	if (FIXNUMP(x)) {
		cl_env_ptr env = ecl_process_env();
		ecl_frame_ptr p = env->frs_org + fix(x);
		if (env->frs_org <= p && p <= env->frs_top)
			return p;
	}
	FEerror("~S is an illegal frs index.", 1, x);
}

cl_object
si_frs_top()
{
	cl_env_ptr env = ecl_process_env();
	@(return MAKE_FIXNUM(env->frs_top - env->frs_org))
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
	cl_env_ptr env = ecl_process_env();
	ecl_frame_ptr x;
	cl_index y = fixnnint(ihs);
	for (x = get_frame_ptr(fr); 
	     x <= env->frs_top && x->frs_ihs->index < y;
	     x++);
	@(return ((x > env->frs_top) ? Cnil : MAKE_FIXNUM(x - env->frs_org)))
}

/********************* INITIALIZATION ***********************/

cl_object
si_set_stack_size(cl_object type, cl_object size)
{
	cl_env_ptr env = ecl_process_env();
	cl_index the_size = fixnnint(size);
	if (type == @'ext::frame-stack') {
		frs_set_size(env, the_size);
	} else if (type == @'ext::binding-stack') {
		bds_set_size(env, the_size);
	} else if (type == @'ext::c-stack') {
		cs_set_size(env, the_size);
	} else {
		cl_stack_set_size(the_size);
	}
	@(return)
}

void
init_stacks(cl_env_ptr env, int *new_cs_org)
{
	static struct ihs_frame ihs_org = { NULL, NULL, NULL, 0};
	cl_index size, margin;

	margin = ecl_get_option(ECL_OPT_FRAME_STACK_SAFETY_AREA);
	size = ecl_get_option(ECL_OPT_FRAME_STACK_SIZE) + 2 * margin;
	env->frs_size = size;
	env->frs_org = (ecl_frame_ptr)ecl_alloc_atomic(size * sizeof(*env->frs_org));
	env->frs_top = env->frs_org-1;
	env->frs_limit = &env->frs_org[size - 2*margin];

	margin = ecl_get_option(ECL_OPT_BIND_STACK_SAFETY_AREA);
	size = ecl_get_option(ECL_OPT_BIND_STACK_SIZE) + 2 * margin;
	env->bds_size = size;
	env->bds_org = (bds_ptr)ecl_alloc_atomic(size * sizeof(*env->bds_org));
	env->bds_top = env->bds_org-1;
	env->bds_limit = &env->bds_org[size - 2*margin];

	env->ihs_top = &ihs_org;
	ihs_org.function = @'si::top-level';
	ihs_org.lex_env = Cnil;
	ihs_org.index = 0;

	env->cs_org = new_cs_org;
	env->cs_barrier = new_cs_org;
#if defined(HAVE_SYS_RESOURCE_H) && defined(RLIMIT_STACK)
	{
		struct rlimit rl;
		cl_index size;
		getrlimit(RLIMIT_STACK, &rl);
		if (rl.rlim_cur != RLIM_INFINITY) {
			size = rl.rlim_cur / sizeof(cl_fixnum) / 2;
			if (size > ecl_get_option(ECL_OPT_C_STACK_SIZE))
				ecl_set_option(ECL_OPT_C_STACK_SIZE, size);
#ifdef ECL_DOWN_STACK
			env->cs_barrier = env->cs_org - rl.rlim_cur - 1024;
#else
			env->cs_barrier = env->cs_org + rl.rlim_cur + 1024;
#endif
		}
	}
#endif
	cs_set_size(env, ecl_get_option(ECL_OPT_C_STACK_SIZE));

#if defined(HAVE_SIGPROCMASK) && defined(SA_SIGINFO)
	if (ecl_get_option(ECL_OPT_SIGALTSTACK_SIZE)) {
		stack_t new_stack;
		cl_index size = ecl_get_option(ECL_OPT_SIGALTSTACK_SIZE);
		if (size < SIGSTKSZ) {
			size = SIGSTKSZ + (sizeof(double)*16) +
				(sizeof(cl_object)*4);
		}
		env->altstack_size = size;
		env->altstack = ecl_alloc_atomic(size);
		memset(&new_stack, 0, sizeof(new_stack));
		new_stack.ss_size = env->altstack_size;
		new_stack.ss_sp = env->altstack;
		new_stack.ss_flags = 0;
		sigaltstack(&new_stack, NULL);
	}
#endif
#ifdef SA_SIGINFO
	env->interrupt_info = ecl_alloc_atomic(sizeof(siginfo_t));
#endif
}
