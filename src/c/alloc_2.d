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

#ifdef GBC_BOEHM

/**********************************************************
 *		OBJECT ALLOCATION			  *
 **********************************************************/

struct typemanager tm_table[(int)t_end];

#ifdef alloc_object
#undef alloc_object
#endif

cl_object
alloc_object(cl_type t)
{
	register cl_object obj;
	register struct typemanager *tm;

	switch (t) {
	case t_fixnum:
	  return MAKE_FIXNUM(0); /* Immediate fixnum */
	case t_character:
	  return CODE_CHAR(' '); /* Immediate character */
	default:
	}
	if (t < t_start || t >= t_end) {
	  printf("\ttype = %d\n", t);
	  error("alloc botch.");
	}
	tm = tm_of(t);

	start_critical_section(); 
	obj = GC_malloc(tm->tm_size);
	obj->d.t = t;
	/* GC_malloc already resets objects */
	end_critical_section();

	return obj;
}

#ifdef make_cons
#undef make_cons
#endif

cl_object
make_cons(cl_object a, cl_object d)
{
	cl_object obj;

	start_critical_section(); 

	obj = GC_malloc(sizeof(struct cons));
	obj->d.t = (short)t_cons;
	CAR(obj) = a;
	CDR(obj) = d;

	end_critical_section();

	return obj;
}

cl_object
alloc_instance(cl_index slots)
{
	cl_object i;
	i = alloc_object(t_instance);
	i->instance.slots = alloc_align(sizeof(cl_object) * slots, sizeof(cl_object));
	i->instance.length = slots;
	return i;
}

void *
alloc(size_t n)
{
	void *output;
	start_critical_section(); 
	output = GC_malloc(n);
	end_critical_section();
	return output;
}

void *
alloc_atomic(size_t n)
{
	void *output;
	start_critical_section(); 
	output = GC_malloc_atomic(n);
	end_critical_section();
	return output;
}

/*
 * adds a contblock to the list of available ones, pointed by cb_pointer,
 * sorted by increasing size.
 */
void
dealloc(void *p, size_t s)
{
	GC_free(p);	
}

/*
 * align must be a power of 2 representing the alignment boundary
 * required for the block.
 */
void *
alloc_align(size_t size, size_t align)
{
	char *output;
	start_critical_section();
	align--;
	output = GC_malloc(size + align);
	output = (char*)(((cl_fixnum)output + align) & ~align);
	end_critical_section();
	return output;
}

/*
 * align must be a power of 2 representing the alignment boundary
 * required for the block.
 */
void *
alloc_atomic_align(size_t size, size_t align)
{
	char *output;
	start_critical_section();
	align--;
	output = GC_malloc(size + align);
	output = (char*)(((cl_fixnum)output + align) & ~align);
	end_critical_section();
	return output;
}

static void
init_tm(cl_type t, char *name, size_t elsize)
{
	struct typemanager *tm = &tm_table[(int)t];
	tm->tm_name = name;
	tm->tm_size = elsize;
}

static int alloc_initialized = FALSE;

void
init_alloc(void)
{
	if (alloc_initialized) return;
	alloc_initialized = TRUE;

	GC_no_dls = 1;

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
	init_tm(t_gfun, "GFUN", sizeof(struct gfun));
#endif CLOS
#ifdef THREADS
	init_tm(t_cont, "CONT", sizeof(struct cont));
	init_tm(t_thread, "THREAD", sizeof(struct thread));
#endif THREADS
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

/* FIXME! Shouldn't this be thread safe? */
void *
malloc(size_t size)
{
	return GC_malloc(size);
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
	ptr = GC_malloc(i = nelem*elsize);
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
	return (void *)ALLOC_ALIGNED(GC_malloc, size, align);
}

# ifdef WANT_VALLOC
char *
valloc(size_t size)
{
	return memalign(getpagesize(), size);
}
# endif WANT_VALLOC
#endif NEED_MALLOC


/**********************************************************
 *		GARBAGE COLLECTION			  *
 **********************************************************/

cl_object @'si::*gc-verbose*';
cl_object @'si::*gc-message*';

void
register_root(cl_object *p)
{
	GC_add_roots(p, p+1);
}

@(defun gc (area)
@
	gc(0);
	@(return)
@)

void
gc(cl_type new_name)
{
	start_critical_section();
	GC_gcollect();
	end_critical_section();
}

#endif /* GBC_BOEHM */
