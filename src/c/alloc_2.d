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

#include "ecl.h"
#include "page.h"
#include "gc.h"
#include "private/gc_priv.h"

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
	switch (type_of(o)) {
#ifdef ENABLE_DLOPEN
	case t_codeblock:
	AGAIN:
		/*
		printf("\n;;; Freeing library %s \n", o->cblock.name?
		       o->cblock.name->string.self : "<anonymous>");
		*/
		if (o->cblock.handle != NULL) {
			dlclose(o->cblock.handle);
			GC_free(o->cblock.data);
		} else {
			o = o->cblock.next;
			if (o != NULL && o->cblock.handle != NULL)
				goto AGAIN;
		}
		break;
#endif
	case t_stream:
		if (o->stream.file != NULL)
			fclose(o->stream.file);
		o->stream.file = NULL;
		break;
	default:
	}
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

	obj = (cl_object)GC_MALLOC(sizeof(struct cons));
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

static void (*old_GC_push_other_roots)();

void
init_alloc(void)
{
	static void stacks_scanner();

	if (alloc_initialized) return;
	alloc_initialized = TRUE;

	/* GC_no_dls = 1; */
#if 0
	GC_init_explicit_typing();
#endif

	init_tm(t_shortfloat, "SHORT-FLOAT", /* 8 */
		sizeof(struct shortfloat_struct));
	init_tm(t_cons, "CONS", sizeof(struct cons)); /* 12 */
	init_tm(t_longfloat, "LONG-FLOAT", /* 16 */
		sizeof(struct longfloat_struct));
	init_tm(t_bytecodes, "bBYTECODES", sizeof(struct bytecodes));
	init_tm(t_string, "STRING", sizeof(struct string)); /* 20 */
	init_tm(t_array, "ARRAY", sizeof(struct array)); /* 24 */
	init_tm(t_pathname, "PATHNAME", sizeof(struct pathname)); /* 28 */
	init_tm(t_symbol, "SYMBOL", sizeof(struct symbol)); /* 32 */
	init_tm(t_package, "PACKAGE", sizeof(struct package)); /* 36 */
	init_tm(t_codeblock, "CODEBLOCK", sizeof(struct codeblock));
	init_tm(t_bignum, "BIGNUM", sizeof(struct bignum));
	init_tm(t_ratio, "RATIO", sizeof(struct ratio));
	init_tm(t_complex, "COMPLEX", sizeof(struct complex));
	init_tm(t_hashtable, "HASH-TABLE", sizeof(struct hashtable));
	init_tm(t_vector, "VECTOR", sizeof(struct vector));
	init_tm(t_bitvector, "BIT-VECTOR", sizeof(struct vector));
	init_tm(t_stream, "STREAM", sizeof(struct stream));
	init_tm(t_random, "RANDOM-STATE", sizeof(struct random));
	init_tm(t_readtable, "READTABLE", sizeof(struct readtable));
	init_tm(t_cfun, "CFUN", sizeof(struct cfun));
	init_tm(t_cclosure, "CCLOSURE", sizeof(struct cclosure));
#ifndef CLOS
	init_tm(t_structure, "STRUCTURE", sizeof(struct structure));
#else
	init_tm(t_instance, "INSTANCE", sizeof(struct instance));
#endif /* CLOS */
#ifdef ECL_FFI
	init_tm(t_instance, "FOREIGN", sizeof(struct foreign));
#endif
#ifdef THREADS
	init_tm(t_cont, "CONT", sizeof(struct cont));
	init_tm(t_thread, "THREAD", sizeof(struct thread));
#endif /* THREADS */

	old_GC_push_other_roots = GC_push_other_roots;
	GC_push_other_roots = stacks_scanner;
}

/**********************************************************
 *		GARBAGE COLLECTOR			  *
 **********************************************************/

static void
stacks_scanner(void)
{
#if 1
	if (cl_stack) {
		GC_push_conditional(cl_stack, cl_stack_top,1);
		GC_set_mark_bit(cl_stack);
	}
	if (frs_top && (frs_top >= frs_org)) {
		GC_push_conditional(frs_org, frs_top+1,1);
		GC_set_mark_bit(frs_org);
	}
	if (bds_top && (bds_top >= bds_org)) {
		GC_push_conditional(bds_org, bds_top+1,1);
		GC_set_mark_bit(bds_org);
	}
#endif
	if (NValues)
		GC_push_all(Values, Values+NValues+1);
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
}

#endif /* GBC_BOEHM */
