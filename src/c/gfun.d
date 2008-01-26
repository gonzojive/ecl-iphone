/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    gfun.c -- Dispatch for generic functions.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <string.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>
#include "newhash.h"

static void
reshape_instance(cl_object x, int delta)
{
	cl_fixnum size = x->instance.length + delta;
	cl_object aux = ecl_allocate_instance(CLASS_OF(x), size);
	memcpy(aux->instance.slots, x->instance.slots,
	       (delta < 0 ? aux->instance.length : x->instance.length) *
	       sizeof(cl_object));
	x->instance = aux->instance;
}

/* this turns any instance into a funcallable (apart from a builtin generic function)
   or back into an ordinary instance */

cl_object
si_set_raw_funcallable(cl_object instance, cl_object function)
{
	if (type_of(instance) != t_instance)
		FEwrong_type_argument(@'ext::instance', instance);
        if (Null(function)) {
		if (instance->instance.isgf == 2) {
                        int        length          = instance->instance.length-1;
                        cl_object *slots           = (cl_object*)cl_alloc(sizeof(cl_object)*(length));
			instance->instance.isgf    = 2;
                        memcpy(slots, instance->instance.slots, sizeof(cl_object)*(length));
			instance->instance.slots   = slots;
			instance->instance.length  = length;
		        instance->instance.isgf = 0;
		}
	} else	{
		if (instance->instance.isgf == 0) {
                        int        length          = instance->instance.length+1;
                        cl_object *slots           = (cl_object*)cl_alloc(sizeof(cl_object)*length);
                        memcpy(slots, instance->instance.slots, sizeof(cl_object)*(length-1));
			instance->instance.slots   = slots;
			instance->instance.length  = length;
			instance->instance.isgf    = 2;
		}
		instance->instance.slots[instance->instance.length-1] = function;
	}
	@(return instance)
}

cl_object
clos_set_funcallable_instance_function(cl_object x, cl_object function_or_t)
{
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'ext::instance', x);
	if (x->instance.isgf == ECL_USER_DISPATCH) {
		reshape_instance(x, -1);
		x->instance.isgf = ECL_NOT_FUNCALLABLE;
	}
	if (function_or_t == Ct)
	{
		x->instance.isgf = ECL_STANDARD_DISPATCH;
	} else if (function_or_t == Cnil) {
		x->instance.isgf = ECL_NOT_FUNCALLABLE;
	} else if (Null(cl_functionp(function_or_t))) {
		FEwrong_type_argument(@'function', function_or_t);
	} else {
		reshape_instance(x, +1);
		x->instance.slots[x->instance.length - 1] = function_or_t;
		x->instance.isgf = ECL_USER_DISPATCH;
	}
	@(return x)
}

cl_object
si_generic_function_p(cl_object x)
{
	@(return (((type_of(x) != t_instance) &&
		   (x->instance.isgf))? Ct : Cnil))
}

/**********************************************************************
 * METHOD HASH
 */

#define RECORD_KEY(e) ((e)[0])
#define RECORD_VALUE(e) ((e)[1])
#define RECORD_GEN(e) (((cl_fixnum*)(e+2))[0])

static cl_object
do_clear_method_hash(struct cl_env_struct *env, cl_object target)
{
	cl_object table = env->method_hash;
	cl_index i, total_size = table->vector.dim;
	if (target == Ct) {
		env->method_generation = 0;
		for (i = 0; i < total_size; i+=3) {
			table->vector.self.t[i] = OBJNULL;
			table->vector.self.t[i+1] = OBJNULL;
			table->vector.self.fix[i+2] = 0;
		}
#ifdef ECL_THREADS
		env->method_hash_clear_list = Cnil;
#endif
	} else {
		for (i = 0; i < total_size; i+=3) {
			cl_object key = table->vector.self.t[i];
			if (key != OBJNULL) {
				if (target == key->vector.self.t[0]) {
					table->vector.self.t[i] = OBJNULL;
					table->vector.self.fix[i+2] = 0;
				}
			}
		}
	}
}

void
_ecl_set_method_hash_size(struct cl_env_struct *env, cl_index size)
{
	cl_index i;
	env->method_spec_vector =
		si_make_vector(Ct, /* element type */
			       MAKE_FIXNUM(64), /* Maximum size */
			       Ct, /* adjustable */
			       MAKE_FIXNUM(0), /* fill pointer */
			       Cnil, /* displaced */
			       Cnil);
	env->method_hash =
		si_make_vector(Ct, /* element type */
			       MAKE_FIXNUM(3*size), /* Maximum size */
			       Cnil, /* adjustable */
			       Cnil, /* fill pointer */
			       Cnil, /* displaced */
			       Cnil);
	do_clear_method_hash(env, Ct);
}

cl_object
si_clear_gfun_hash(cl_object what)
{
	/*
	 * This function clears the generic function call hashes selectively.
	 *	what = Ct means clear the hash completely
	 *	what = generic function, means cleans only these entries
	 * If we work on a multithreaded environment, we simply enqueue these
	 * operations and wait for the destination thread to update its own hash.
	 */
#ifdef ECL_THREADS
	cl_object list;
	THREAD_OP_LOCK();
	list = cl_core.processes;
	for (; list != Cnil; list = CDR(list)) {
		cl_object process = CAR(list);
		struct cl_env_struct *env = process->process.env;
		env->method_hash_clear_list = CONS(what, env->method_hash_clear_list);
	}
	THREAD_OP_UNLOCK();
#else
	do_clear_method_hash(&cl_env, what);
#endif
}

static cl_index
vector_hash_key(cl_object keys)
{
	cl_index c, n, a = GOLDEN_RATIO, b = GOLDEN_RATIO;
	for (c = 0, n = keys->vector.fillp; n >= 3; ) {
		c += keys->vector.self.index[--n];
		b += keys->vector.self.index[--n];
		a += keys->vector.self.index[--n];
		mix(a, b, c);
	}
	switch (n) {
	case 2:	b += keys->vector.self.index[--n];
	case 1:	a += keys->vector.self.index[--n];
		c += keys->vector.dim;
		mix(a,b,c);
	}
	return c;
}


/*
 * variation of ecl_gethash from hash.d, which takes an array of objects as key
 * It also assumes that entries are never removed except by clrhash.
 */

static cl_object *
search_method_hash(cl_object keys, cl_object table)
{
	cl_index argno = keys->vector.fillp;
	cl_index i = vector_hash_key(keys);
	cl_index total_size = table->vector.dim;
	cl_fixnum min_gen, gen;
	cl_object *min_e;
	int k;
	i = i % total_size;
	i = i - (i % 3);
	min_gen = cl_env.method_generation;
	min_e = 0;
	for (k = 20; k--; ) {
		cl_object *e = table->vector.self.t + i;
		cl_object hkey = RECORD_KEY(e);
		if (hkey == OBJNULL) {
			min_gen = -1;
			min_e = e;
			if (RECORD_VALUE(e) == OBJNULL) {
				/* This record is not only deleted but empty
				 * Hence we cannot find our method ahead */
				break;
			}
			/* Else we only know that the record has been
			 * delete, but we might find our data ahead. */
		} else if (argno == hkey->vector.fillp) {
			cl_index n;
			for (n = 0; n < argno; n++) {
				if (keys->vector.self.t[n] !=
				    hkey->vector.self.t[n])
					goto NO_MATCH;
			}
			min_e = e;
			goto FOUND;
		} else if (min_gen >= 0) {
		NO_MATCH:
			/* Unless we have found a deleted record, keep
			 * looking for the oldest record that we can
			 * overwrite with the new data. */
			gen = RECORD_GEN(e);
			if (gen < min_gen) {
				min_gen = gen;
				min_e = e;
			}
		}
		i += 3;
		if (i >= total_size) i = 0;
	}
	if (min_e == 0) {
		ecl_internal_error("search_method_hash");
	}
	RECORD_KEY(min_e) = OBJNULL;
	cl_env.method_generation++;
 FOUND:
	/*
	 * Once we have reached here, we set the new generation of
	 * this record and perform a global shift so that the total
	 * generation number does not become too large and we can
	 * expire some elements.
	 */
	RECORD_GEN(min_e) = gen = cl_env.method_generation;
	if (gen >= total_size/2) {
		cl_object *e = table->vector.self.t;
		gen = 0.5*gen;
		cl_env.method_generation -= gen;
		for (i = table->vector.dim; i; i-= 3, e += 3) {
			cl_fixnum g = RECORD_GEN(e) - gen;
			if (g <= 0) {
				RECORD_KEY(e) = OBJNULL;
				RECORD_VALUE(e) = Cnil;
				g = 0;
			}
			RECORD_GEN(e) = g;
		}
	}
	return min_e;
}

static cl_object
get_spec_vector(cl_narg narg, cl_object gf, cl_object *args)
{
	cl_object spec_how_list = GFUN_SPEC(gf);
	cl_object vector = cl_env.method_spec_vector;
	cl_object *argtype = vector->vector.self.t;
	int spec_no;

	argtype[0] = gf;
	for (spec_no = 1; spec_how_list != Cnil;) {
		cl_object spec_how = CAR(spec_how_list);
		cl_object spec_type = CAR(spec_how);
		int spec_position = fix(CDR(spec_how));
		if (spec_position >= narg)
			FEwrong_num_arguments(gf);
		argtype[spec_no++] =
			(ATOM(spec_type) ||
			 Null(ecl_memql(args[spec_position], spec_type))) ?
			cl_class_of(args[spec_position]) :
			args[spec_position];
		if (spec_no > vector->vector.dim)
			return OBJNULL;
		spec_how_list = CDR(spec_how_list);
	}
	vector->vector.fillp = spec_no;
	return vector;
}

static cl_object
compute_applicable_method(cl_narg narg, cl_object gf, cl_object *args)
{
	/* method not cached */
	cl_object methods, arglist, func;
	int i;
	for (i = narg, arglist = Cnil; i-- > 0; ) {
		arglist = CONS(args[i], arglist);
	}
	methods = funcall(3, @'compute-applicable-methods', gf, arglist);
	if (methods == Cnil) {
		func = funcall(3, @'no-applicable-method', gf, arglist);
		args[0] = 0;
		return func;
	} else {
		return funcall(4, @'clos::compute-effective-method', gf,
			       GFUN_COMB(gf), methods);
	}
}

static cl_object
standard_dispatch(cl_narg narg, cl_object gf, cl_object *args)
{
	cl_object vector;
#ifdef ECL_THREADS
	/* See whether we have to clear the hash from some generic functions right now. */
	if (cl_env.method_hash_clear_list != Cnil) {
		cl_object clear_list;
		THREAD_OP_LOCK();
		clear_list = cl_env.method_hash_clear_list;
		for ( ; clear_list != Cnil ; clear_list = CDR(clear_list)) {
			do_clear_method_hash(&cl_env, CAR(clear_list));
		}
		cl_env.method_hash_clear_list = Cnil;
		THREAD_OP_UNLOCK();
	}
#endif
	vector = get_spec_vector(narg, gf, args);
	if (vector == OBJNULL) {
		return compute_applicable_method(narg, gf, args);
	} else {
		cl_object table = cl_env.method_hash;
		cl_object *e = search_method_hash(vector, table);
		if (RECORD_KEY(e) != OBJNULL) {
			return RECORD_VALUE(e);
		} else {
			cl_object keys = cl_copy_seq(vector);
			cl_object func = compute_applicable_method(narg, gf, args);
			if (RECORD_KEY(e) != OBJNULL) {
				/* The cache might have changed while we
				 * computed applicable methods */
				e = search_method_hash(vector, table);
			}
			RECORD_KEY(e) = keys;
			RECORD_VALUE(e) = func;
			return func;
		}
	}
}

cl_object
_ecl_compute_method(cl_narg narg, cl_object gf, cl_object *args)
{
	switch (gf->instance.isgf) {
	case ECL_STANDARD_DISPATCH:
		return standard_dispatch(narg, gf, args);
	case ECL_USER_DISPATCH:
		return gf->instance.slots[gf->instance.length - 1];
	default:
		FEinvalid_function(gf);
	}
}
