/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    alloc_2.c -- Memory allocation based on the Boehmn GC.
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#if defined(ECL_THREADS) && !defined(_MSC_VER)
#include <pthread.h>
#endif
#include <stdio.h>
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/page.h>
#ifdef ECL_WSOCK
#include <winsock.h>
#endif

#ifdef GBC_BOEHM

#if GBC_BOEHM == 0
#include <ecl/gc/private/gc_priv.h>
#endif

static void finalize_queued();

/**********************************************************
 *		OBJECT ALLOCATION			  *
 **********************************************************/

#ifdef alloc_object
#undef alloc_object
#endif

static size_t type_size[t_end];

cl_object
cl_alloc_object(cl_type t)
{
	cl_object obj;

	/* GC_MALLOC already resets objects */
	switch (t) {
	case t_fixnum:
		return MAKE_FIXNUM(0); /* Immediate fixnum */
	case t_character:
		return CODE_CHAR(' '); /* Immediate character */
	case t_codeblock:
		obj = (cl_object)GC_MALLOC(sizeof(struct ecl_codeblock));
		obj->cblock.locked = 0;
		obj->cblock.links = Cnil;
		obj->cblock.name = Cnil;
		obj->cblock.next = Cnil;
		obj->cblock.data_text = NULL;
		obj->cblock.data = NULL;
		obj->cblock.data_text_size = 0;
		obj->cblock.data_size = 0;
		obj->cblock.handle = NULL;
		break;
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
#endif
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
#endif
	case t_singlefloat:
	case t_doublefloat:
		obj = (cl_object)GC_MALLOC_ATOMIC(type_size[t]);
		break;
	case t_bignum:
	case t_ratio:
	case t_complex:
	case t_symbol:
	case t_package:
	case t_hashtable:
	case t_array:
	case t_vector:
	case t_base_string:
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_bitvector:
	case t_stream:
	case t_random:
	case t_readtable:
	case t_pathname:
	case t_bytecodes:
	case t_bclosure:
	case t_cfun:
	case t_cfunfixed:
	case t_cclosure:
#ifdef CLOS
	case t_instance:
#else
	case t_structure:
#endif
#ifdef ECL_THREADS
	case t_process:
        case t_lock:
        case t_condition_variable:
#endif
	case t_foreign:
		obj = (cl_object)GC_MALLOC(type_size[t]);
		break;
	default:
		printf("\ttype = %d\n", t);
		ecl_internal_error("alloc botch.");
	}
	obj->d.t = t;
	return obj;
}

#ifdef make_cons
#undef make_cons
#endif

cl_object
ecl_cons(cl_object a, cl_object d)
{
	struct ecl_cons *obj = GC_MALLOC(sizeof(struct ecl_cons));
#ifdef ECL_SMALL_CONS
	obj->car = a;
	obj->cdr = d;
	return ECL_PTR_CONS(obj);
#else
	obj->t = t_list;
	obj->car = a;
	obj->cdr = d;
	return (cl_object)obj;
#endif
}

cl_object
ecl_list1(cl_object a)
{
	struct ecl_cons *obj = GC_MALLOC(sizeof(struct ecl_cons));
#ifdef ECL_SMALL_CONS
	obj->car = a;
	obj->cdr = Cnil;
	return ECL_PTR_CONS(obj);
#else
	obj->t = t_list;
	obj->car = a;
	obj->cdr = Cnil;
	return (cl_object)obj;
#endif
}

cl_object
cl_alloc_instance(cl_index slots)
{
	cl_object i;
	i = cl_alloc_object(t_instance);
	i->instance.slots = (cl_object *)cl_alloc(sizeof(cl_object) * slots);
	i->instance.length = slots;
	return i;
}

void *
ecl_alloc_uncollectable(size_t size)
{
	return GC_MALLOC_UNCOLLECTABLE(size);
}

void
ecl_free_uncollectable(void *pointer)
{
	GC_FREE(pointer);
}

static int alloc_initialized = FALSE;

extern void (*GC_push_other_roots)();
extern void (*GC_start_call_back)();
static void (*old_GC_push_other_roots)();
static void stacks_scanner();

void
init_alloc(void)
{
	int i;
	if (alloc_initialized) return;
	alloc_initialized = TRUE;
	/*
	 * Garbage collector restrictions: we set up the garbage collector
	 * library to work as follows
	 *
	 * 1) The garbage collector shall not scan shared libraries
	 *    explicitely.
	 * 2) We only detect objects that are referenced by a pointer to
	 *    the begining or to the first byte.
	 * 3) Out of the incremental garbage collector, we only use the
	 *    generational component.
	 */
	GC_no_dls = 1;
	GC_all_interior_pointers = 0;
	GC_time_limit = GC_TIME_UNLIMITED;
	GC_init();
	if (ecl_get_option(ECL_INCREMENTAL_GC)) {
		GC_enable_incremental();
	}
	GC_register_displacement(1);
#if 0
	GC_init_explicit_typing();
#endif
	GC_clear_roots();
	GC_disable();

#define init_tm(x,y,z) type_size[x] = (z)
	for (i = 0; i < t_end; i++) {
		type_size[i] = 0;
	}
	init_tm(t_singlefloat, "SINGLE-FLOAT", /* 8 */
		sizeof(struct ecl_singlefloat));
	init_tm(t_list, "CONS", sizeof(struct ecl_cons)); /* 12 */
	init_tm(t_doublefloat, "DOUBLE-FLOAT", /* 16 */
		sizeof(struct ecl_doublefloat));
	init_tm(t_bytecodes, "BYTECODES", sizeof(struct ecl_bytecodes));
	init_tm(t_bclosure, "BCLOSURE", sizeof(struct ecl_bclosure));
	init_tm(t_base_string, "BASE-STRING", sizeof(struct ecl_base_string)); /* 20 */
#ifdef ECL_UNICODE
	init_tm(t_string, "STRING", sizeof(struct ecl_string));
#endif
	init_tm(t_array, "ARRAY", sizeof(struct ecl_array)); /* 24 */
	init_tm(t_pathname, "PATHNAME", sizeof(struct ecl_pathname)); /* 28 */
	init_tm(t_symbol, "SYMBOL", sizeof(struct ecl_symbol)); /* 32 */
	init_tm(t_package, "PACKAGE", sizeof(struct ecl_package)); /* 36 */
	init_tm(t_codeblock, "CODEBLOCK", sizeof(struct ecl_codeblock));
	init_tm(t_bignum, "BIGNUM", sizeof(struct ecl_bignum));
	init_tm(t_ratio, "RATIO", sizeof(struct ecl_ratio));
	init_tm(t_complex, "COMPLEX", sizeof(struct ecl_complex));
	init_tm(t_hashtable, "HASH-TABLE", sizeof(struct ecl_hashtable));
	init_tm(t_vector, "VECTOR", sizeof(struct ecl_vector));
	init_tm(t_bitvector, "BIT-VECTOR", sizeof(struct ecl_vector));
	init_tm(t_stream, "STREAM", sizeof(struct ecl_stream));
	init_tm(t_random, "RANDOM-STATE", sizeof(struct ecl_random));
	init_tm(t_readtable, "READTABLE", sizeof(struct ecl_readtable));
	init_tm(t_cfun, "CFUN", sizeof(struct ecl_cfun));
	init_tm(t_cfunfixed, "CFUN", sizeof(struct ecl_cfun));
	init_tm(t_cclosure, "CCLOSURE", sizeof(struct ecl_cclosure));
#ifndef CLOS
	init_tm(t_structure, "STRUCTURE", sizeof(struct ecl_structure));
#else
	init_tm(t_instance, "INSTANCE", sizeof(struct ecl_instance));
#endif /* CLOS */
	init_tm(t_foreign, "FOREIGN", sizeof(struct ecl_foreign));
#ifdef ECL_THREADS
	init_tm(t_process, "PROCESS", sizeof(struct ecl_process));
	init_tm(t_lock, "LOCK", sizeof(struct ecl_lock));
	init_tm(t_condition_variable, "CONDITION-VARIABLE",
                sizeof(struct ecl_condition_variable));
#endif
#ifdef ECL_LONG_FLOAT
	init_tm(t_longfloat, "LONG-FLOAT", sizeof(struct ecl_long_float));
#endif

	old_GC_push_other_roots = GC_push_other_roots;
	GC_push_other_roots = stacks_scanner;
	GC_start_call_back = (void (*)())finalize_queued;
	GC_java_finalization = 1;
	GC_enable();
}

/**********************************************************
 *		FINALIZATION				  *
 **********************************************************/

static void
standard_finalizer(cl_object o)
{
	switch (o->d.t) {
#ifdef ENABLE_DLOPEN
	case t_codeblock:
		ecl_library_close(o);
		break;
#endif
	case t_stream:
		cl_close(1, o);
		break;
#ifdef ECL_THREADS
	case t_lock:
#if defined(_MSC_VER) || defined(mingw32)
		CloseHandle(o->lock.mutex);
#else
		pthread_mutex_destroy(&o->lock.mutex);
#endif
		break;
	case t_condition_variable:
#if defined(_MSC_VER) || defined(mingw32)
		CloseHandle(o->condition_variable.cv);
#else
		pthread_cond_destroy(&o->condition_variable.cv);
#endif
		break;
#endif
	default:;
	}
}

static void
group_finalizer(cl_object l, cl_object no_data)
{
	CL_NEWENV_BEGIN {
		while (CONSP(l)) {
			cl_object record = ECL_CONS_CAR(l);
			cl_object o = ECL_CONS_CAR(record);
			cl_object procedure = ECL_CONS_CDR(record);
			l = ECL_CONS_CDR(l);
			if (procedure != Ct) {
				funcall(2, procedure, o);
			}
			standard_finalizer(o);
		}
	} CL_NEWENV_END;
}

static void
queueing_finalizer(cl_object o, cl_object finalizer)
{
	if (finalizer != Cnil && finalizer != NULL) {
		/* Only nonstandard finalizers are queued */
		if (finalizer == Ct) {
			CL_NEWENV_BEGIN {
				standard_finalizer(o);
			} CL_NEWENV_END;
		} else {
			/* Note the way we do this: finalizers might
			   get executed as a consequence of these calls. */
			volatile cl_object aux = ACONS(o, finalizer, Cnil);
			cl_object l = cl_core.to_be_finalized;
			if (ATOM(l)) {
				GC_finalization_proc ofn;
				void *odata;
				cl_core.to_be_finalized = aux;
				GC_register_finalizer_no_order(aux, (GC_finalization_proc*)group_finalizer, NULL, &ofn, &odata);
			} else {
				ECL_RPLACD(aux, ECL_CONS_CDR(l));
				ECL_RPLACD(l, aux);
			}
		}
	}
}

cl_object
si_get_finalizer(cl_object o)
{
	cl_object output;
	GC_finalization_proc ofn;
	void *odata;
	GC_register_finalizer_no_order(o, (GC_finalization_proc)0, 0, &ofn, &odata);
	if (ofn == 0) {
		output = Cnil;
	} else if (ofn == (GC_finalization_proc)queueing_finalizer) {
		output = (cl_object)odata;
	} else {
		output = Cnil;
	}
	GC_register_finalizer_no_order(o, ofn, odata, &ofn, &odata);
	@(return output)
}

cl_object
si_set_finalizer(cl_object o, cl_object finalizer)
{
	GC_finalization_proc ofn;
	void *odata;
	if (finalizer == Cnil) {
		GC_register_finalizer_no_order(o, (GC_finalization_proc)0, 0, &ofn, &odata);
	} else {
		GC_register_finalizer_no_order(o, (GC_finalization_proc)queueing_finalizer, finalizer, &ofn, &odata);
	}
	@(return)
}

cl_object
si_gc_stats(cl_object enable)
{
	cl_object old_status = cl_core.gc_stats? Ct : Cnil;
	cl_core.gc_stats = (enable != Cnil);
	if (cl_core.bytes_consed == Cnil) {
#ifndef WITH_GMP
		cl_core.bytes_consed = MAKE_FIXNUM(0);
		cl_core.gc_counter = MAKE_FIXNUM(0);
#else
		cl_core.bytes_consed = cl_alloc_object(t_bignum);
		mpz_init2(cl_core.bytes_consed->big.big_num, 128);
		cl_core.gc_counter = cl_alloc_object(t_bignum);
		mpz_init2(cl_core.gc_counter->big.big_num, 128);
#endif
	}
	@(return
	  big_register_normalize(cl_core.bytes_consed)
	  big_register_normalize(cl_core.gc_counter)
	  old_status)
}

/*
 * This procedure is invoked after garbage collection. It invokes
 * finalizers for all objects that are to be reclaimed by the
 * colector. Note that we cannot cons because this procedure is
 * invoked with the garbage collection lock on.
 */
static void
finalize_queued()
{
	cl_object l = cl_core.to_be_finalized;
	if (cl_core.gc_stats) {
#ifdef WITH_GMP
		/* Sorry, no gc stats if you do not use bignums */
#if GBC_BOEHM == 0
		mpz_add_ui(cl_core.bytes_consed->big.big_num,
			   cl_core.bytes_consed->big.big_num,
			   GC_words_allocd * sizeof(cl_index));
#else
		/* This is not accurate and may wrap around. We try
		   to detect this assuming that an overflow in an
		   unsigned integer will produce an smaller
		   integer.*/
		static cl_index bytes = 0;
		cl_index new_bytes = GC_get_total_bytes();
		if (bytes > new_bytes) {
			cl_index wrapped;
			wrapped = ~((cl_index)0) - bytes;
			mpz_add_ui(cl_core.bytes_consed->big.big_num,
				   cl_core.bytes_consed->big.big_num,
				   wrapped);
			bytes = new_bytes;
		}
		mpz_add_ui(cl_core.bytes_consed->big.big_num,
			   cl_core.bytes_consed->big.big_num,
			   new_bytes - bytes);
#endif
		mpz_add_ui(cl_core.gc_counter->big.big_num,
			   cl_core.gc_counter->big.big_num,
			   1);
#endif
	}
	if (l != Cnil) {
		cl_core.to_be_finalized = Cnil;
	}
}


/**********************************************************
 *		GARBAGE COLLECTOR			  *
 **********************************************************/

static void
ecl_mark_env(struct cl_env_struct *env)
{
#if 1
	if (env->stack) {
		GC_push_conditional((void *)env->stack, (void *)env->stack_top, 1);
		GC_set_mark_bit((void *)env->stack);
	}
	if (env->frs_top) {
		GC_push_conditional((void *)env->frs_org, (void *)(env->frs_top+1), 1);
		GC_set_mark_bit((void *)env->frs_org);
	}
	if (env->bds_top) {
		GC_push_conditional((void *)env->bds_org, (void *)(env->bds_top+1), 1);
		GC_set_mark_bit((void *)env->bds_org);
	}
#endif
#if 0
	GC_push_all(&(env->lex_env), &(env->lex_env)+1);
	GC_push_all(&(env->string_pool), &(env->print_base));
#if !defined(ECL_CMU_FORMAT)
	GC_push_all(&(env->queue), &(env->qh));
#endif
	GC_push_all(env->big_register, env->big_register + 3);
	if (env->nvalues)
		GC_push_all(env->values, env->values + env->nvalues + 1);
#else
	/*memset(env->values[env->nvalues], 0, (64-env->nvalues)*sizeof(cl_object));*/
#ifdef ECL_THREADS
	/* When using threads, "env" is a pointer to memory allocated by ECL. */
	GC_push_conditional((void *)env, (void *)(env + 1), 1);
	GC_set_mark_bit((void *)env);
#else
	/* When not using threads, "env" is a statically allocated structure. */
	GC_push_all((void *)env, (void *)(env + 1));
#endif
#endif
}

static void
stacks_scanner()
{
	cl_object l;
	l = cl_core.libraries;
	if (l) {
		int i;
		for (i = 0; i < l->vector.fillp; i++) {
			cl_object dll = l->vector.self.t[i];
			if (dll->cblock.locked) {
				GC_push_conditional((void *)dll, (void *)(&dll->cblock + 1), 1);
				GC_set_mark_bit((void *)dll);
			}
		}
		GC_set_mark_bit((void *)l->vector.self.t);
	}
	GC_push_all((void *)(&cl_core), (void *)(&cl_core + 1));
	GC_push_all((void *)cl_symbols, (void *)(cl_symbols + cl_num_symbols_in_core));
#ifdef ECL_THREADS
	l = cl_core.processes;
	if (l == OBJNULL) {
		ecl_mark_env(&cl_env);
	} else {
		l = cl_core.processes;
		loop_for_on_unsafe(l) {
			cl_object process = ECL_CONS_CAR(l);
			struct cl_env_struct *env = process->process.env;
			ecl_mark_env(env);
		} end_loop_for_on;
	}
#else
	ecl_mark_env(&cl_env);
#endif
	if (old_GC_push_other_roots)
		(*old_GC_push_other_roots)();
}

/**********************************************************
 *		MALLOC SUBSTITUTION			  *
 **********************************************************/

#if 0 && defined(NEED_MALLOC)
#undef malloc
#undef calloc
#undef free
#undef cfree
#undef realloc

void *
malloc(size_t size)
{
	return GC_MALLOC(size);
}

void
free(void *ptr)
{
	GC_free(ptr);
}

void *
realloc(void *ptr, size_t size)
{
	return GC_realloc(ptr, size);
}

void *
calloc(size_t nelem, size_t elsize)
{
	char *ptr;
	size_t i;
	ptr = GC_MALLOC(i = nelem*elsize);
	memset(ptr, 0 , i);
	return ptr;
}

void
cfree(void *ptr)
{
	GC_free(ptr);
}

#define ALLOC_ALIGNED(f, size, align) \
	((align) <= 4 ? (int)(f)(size) : \
	   ((align) * (((unsigned)(f)(size + (size ? (align) - 1 : 0)) + (align) - 1)/(align))))

void *
memalign(size_t align, size_t size)
{
	return (void *)ALLOC_ALIGNED(GC_MALLOC, size, align);
}

# ifdef WANT_VALLOC
char *
valloc(size_t size)
{
	return memalign(getpagesize(), size);
}
# endif /* WANT_VALLOC */
#endif /* NEED_MALLOC */


/**********************************************************
 *		GARBAGE COLLECTION			  *
 **********************************************************/

void
ecl_register_root(cl_object *p)
{
	GC_add_roots((char*)p, (char*)(p+1));
}

cl_object
si_gc(cl_object area)
{
	GC_gcollect();
	@(return)
}

cl_object
si_gc_dump()
{
	GC_dump();
	@(return)
}

#endif /* GBC_BOEHM */
