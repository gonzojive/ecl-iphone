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

#ifdef ECL_THREADS
#include <pthread.h>
#endif
#include "ecl.h"
#include "internal.h"
#include "page.h"
#include "gc/gc.h"
#include "gc/private/gc_priv.h"

#ifdef GBC_BOEHM

/**********************************************************
 *		OBJECT ALLOCATION			  *
 **********************************************************/

struct typemanager tm_table[(int)t_end];

#ifdef alloc_object
#undef alloc_object
#endif

static void
finalize(cl_object o, cl_object data)
{
	CL_NEWENV_BEGIN {
	switch (type_of(o)) {
#ifdef ENABLE_DLOPEN
	case t_codeblock:
		cl_mapc(2, @'si::unlink-symbol', o->cblock.links);
		if (o->cblock.handle != NULL) {
			printf("\n;;; Freeing library %s\n", o->cblock.name?
			       o->cblock.name->string.self : "<anonymous>");
			ecl_library_close(o);
		}
#ifdef ECL_DYNAMIC_VV
		/* GC_free(o->cblock.data); */
#endif
		break;
#endif
	case t_stream:
		if (o->stream.file != NULL)
			fclose(o->stream.file);
		o->stream.file = NULL;
		break;
#ifdef ECL_THREADS
	case t_lock:
		pthread_mutex_destroy(&o->lock.mutex);
		break;
#endif
	default:;
	}
	} CL_NEWENV_END;
}

cl_object
cl_alloc_object(cl_type t)
{
	register cl_object obj;
	register struct typemanager *tm;

	switch (t) {
	case t_fixnum:
		return MAKE_FIXNUM(0); /* Immediate fixnum */
	case t_character:
		return CODE_CHAR(' '); /* Immediate character */
	default:;
	}
	if (t < t_start || t >= t_end) {
		printf("\ttype = %d\n", t);
		error("alloc botch.");
	}
	tm = tm_of(t);

	obj = (cl_object)GC_MALLOC(tm->tm_size);
	obj->d.t = t;
	/* GC_MALLOC already resets objects */
	if (t == t_stream
#ifdef ENABLE_DLOPEN
	    || t == t_codeblock
#endif
#ifdef ENABLE_THREADS
	    || t == t_lock
#endif
	) {
		GC_finalization_proc ofn;
		void *odata;
		GC_register_finalizer_no_order(obj, finalize, NULL, &ofn, &odata);
	}
	return obj;
}

#ifdef make_cons
#undef make_cons
#endif

cl_object
make_cons(cl_object a, cl_object d)
{
	cl_object obj;

	obj = (cl_object)GC_MALLOC(sizeof(struct ecl_cons));
	obj->d.t = (short)t_cons;
	CAR(obj) = a;
	CDR(obj) = d;

	return obj;
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

static void
init_tm(cl_type t, char *name, cl_index elsize)
{
	struct typemanager *tm = &tm_table[(int)t];
	tm->tm_name = name;
	tm->tm_size = elsize;
}

static int alloc_initialized = FALSE;

extern void (*GC_push_other_roots)();
static void (*old_GC_push_other_roots)();

void
init_alloc(void)
{
	static void stacks_scanner();

	if (alloc_initialized) return;
	alloc_initialized = TRUE;

	GC_no_dls = 1;
#if 0
	GC_init_explicit_typing();
#endif

	init_tm(t_shortfloat, "SHORT-FLOAT", /* 8 */
		sizeof(struct ecl_shortfloat));
	init_tm(t_cons, "CONS", sizeof(struct ecl_cons)); /* 12 */
	init_tm(t_longfloat, "LONG-FLOAT", /* 16 */
		sizeof(struct ecl_longfloat));
	init_tm(t_bytecodes, "bBYTECODES", sizeof(struct ecl_bytecodes));
	init_tm(t_string, "STRING", sizeof(struct ecl_string)); /* 20 */
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
	init_tm(t_cclosure, "CCLOSURE", sizeof(struct ecl_cclosure));
#ifndef CLOS
	init_tm(t_structure, "STRUCTURE", sizeof(struct ecl_structure));
#else
	init_tm(t_instance, "INSTANCE", sizeof(struct ecl_instance));
#endif /* CLOS */
#ifdef ECL_FFI
	init_tm(t_instance, "FOREIGN", sizeof(struct ecl_foreign));
#endif
#ifdef ECL_THREADS
	init_tm(t_process, "PROCESS", sizeof(struct ecl_process));
	init_tm(t_lock, "LOCK", sizeof(struct ecl_lock));
#endif

	old_GC_push_other_roots = GC_push_other_roots;
	GC_push_other_roots = stacks_scanner;
}

/**********************************************************
 *		GARBAGE COLLECTOR			  *
 **********************************************************/

static void
ecl_mark_env(struct cl_env_struct *env)
{
#if 1
	if (env->stack) {
		GC_push_conditional((ptr_t)env->stack, (ptr_t)env->stack_top, 1);
		GC_set_mark_bit((ptr_t)env->stack);
	}
	if (env->frs_top) {
		GC_push_conditional((ptr_t)env->frs_org, (ptr_t)(env->frs_top+1), 1);
		GC_set_mark_bit((ptr_t)env->frs_org);
	}
	if (env->bds_top) {
		GC_push_conditional((ptr_t)env->bds_org, (ptr_t)(env->bds_top+1), 1);
		GC_set_mark_bit((ptr_t)env->bds_org);
	}
#endif
#if 0
	GC_push_all(&(env->lex_env), &(env->lex_env)+1);
	GC_push_all(&(env->token), &(env->print_base));
	GC_push_all(&(env->circle_stack), &(env->qh));
	GC_push_all(env->big_register, env->big_register + 3);
	if (env->nvalues)
		GC_push_all(env->values, env->values + env->nvalues + 1);
#else
	/*memset(env->values[env->nvalues], 0, (64-env->nvalues)*sizeof(cl_object));*/
#ifdef ECL_THREADS
	/* When using threads, "env" is a pointer to memory allocated by ECL. */
	GC_push_conditional(env, env + 1);
	GC_set_mark_bit(env);
#else
	/* When not using threads, "env" is a statically allocated structure. */
	GC_push_all((ptr_t)env, (ptr_t)(env + 1));
#endif
#endif
}

static void
stacks_scanner(void)
{
#ifdef ECL_THREADS
	cl_object l = cl_core.processes;
	struct cl_env_struct cl_env_ptr;
	if (l == OBJNULL) {
		ecl_mark_env(&cl_env);
	} else {
		for (l = cl_core.processes; l != Cnil; l = CDR(l)) {
			cl_object process = CAR(l);
			struct cl_env_struct *env = process->process.env;
			ecl_mark_env(env);
		}
	}
#else
	ecl_mark_env(&cl_env);
#endif
	GC_push_all((ptr_t)&cl_core, (ptr_t)(&cl_core + 1));
	GC_push_all((ptr_t)cl_symbols, (ptr_t)(cl_symbols + cl_num_symbols_in_core));
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
