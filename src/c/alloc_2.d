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
#include <ecl/ecl.h>
#include <ecl/internal.h>
#include <ecl/page.h>
#ifdef ECL_WSOCK
#include <winsock.h>
#endif

#ifdef GBC_BOEHM

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
	case t_cfun:
	case t_cclosure:
#ifdef CLOS
	case t_instance:
#else
	case t_structure:
#endif
#ifdef ECL_THREADS
	case t_process:
        case t_lock:
#endif
	case t_foreign:
		obj = (cl_object)GC_MALLOC(type_size[t]);
		break;
	default:
		printf("\ttype = %d\n", t);
		error("alloc botch.");
	}
	obj->d.t = t;
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

void *
ecl_alloc_uncollectable(size_t size)
{
	return GC_MALLOC_UNCOLLECTABLE(size);
}

void
ecl_free_uncollectable(void *pointer)
{
	return GC_FREE(pointer);
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

	GC_no_dls = 1;
	GC_init();
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
	init_tm(t_cons, "CONS", sizeof(struct ecl_cons)); /* 12 */
	init_tm(t_doublefloat, "DOUBLE-FLOAT", /* 16 */
		sizeof(struct ecl_doublefloat));
	init_tm(t_bytecodes, "BYTECODES", sizeof(struct ecl_bytecodes));
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
#endif
	default:;
	}
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
			/* Notice the way in which we do this. The inner calls to GC_alloc
			 * may cause other finalizers to be invoked. */
			volatile cl_object aux = ACONS(o, finalizer, Cnil);
			CDR(aux) = cl_core.to_be_finalized;
			cl_core.to_be_finalized = aux;
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

/*
 * This procedure is invoked after garbage collection. It invokes
 * finalizers for all objects that are to be reclaimed by the
 * colector.
 */
static void
finalize_queued()
{
	cl_object l = cl_core.to_be_finalized;
	if (l == Cnil)
		return;
	CL_NEWENV_BEGIN {
		cl_core.to_be_finalized = Cnil;
		do {
			cl_object record = CAR(l);
			cl_object o = CAR(record);
			cl_object procedure = CDR(record);
			l = CDR(l);
			if (procedure != Ct) {
				funcall(2, procedure, o);
			}
			standard_finalizer(o);
		} while (l != Cnil);
	} CL_NEWENV_END;
}


/**********************************************************
 *		GARBAGE COLLECTOR			  *
 **********************************************************/

static void
ecl_mark_env(struct cl_env_struct *env)
{
#if 1
	if (env->stack) {
		GC_push_conditional((GC_PTR)env->stack, (GC_PTR)env->stack_top, 1);
		GC_set_mark_bit((GC_PTR)env->stack);
	}
	if (env->frs_top) {
		GC_push_conditional((GC_PTR)env->frs_org, (GC_PTR)(env->frs_top+1), 1);
		GC_set_mark_bit((GC_PTR)env->frs_org);
	}
	if (env->bds_top) {
		GC_push_conditional((GC_PTR)env->bds_org, (GC_PTR)(env->bds_top+1), 1);
		GC_set_mark_bit((GC_PTR)env->bds_org);
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
	GC_push_all((GC_PTR)env, (GC_PTR)(env + 1));
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
				GC_push_conditional((GC_PTR)dll, (GC_PTR)(&dll->cblock + 1), 1);
				GC_set_mark_bit((GC_PTR)dll);
			}
		}
		GC_set_mark_bit((GC_PTR)l->vector.self.t);
	}
	GC_push_all((GC_PTR)(&cl_core), (GC_PTR)(&cl_core + 1));
	GC_push_all((GC_PTR)cl_symbols, (GC_PTR)(cl_symbols + cl_num_symbols_in_core));
#ifdef ECL_THREADS
	l = cl_core.processes;
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
