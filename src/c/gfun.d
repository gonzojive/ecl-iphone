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

#include "ecl.h"
#include "internal.h"

cl_object
si_set_funcallable(cl_object instance, cl_object flag)
{
	if (type_of(instance) != t_instance)
		FEwrong_type_argument(@'instance', instance);
	instance->instance.isgf = !Null(flag);
	@(return instance)
}

cl_object
si_generic_function_p(cl_object instance)
{
	@(return (((type_of(instance) != t_instance) &&
		   (instance->instance.isgf))? Ct : Cnil))
}

/*
 * variation of gethash from hash.d, which takes an array of objects as key
 * It also assumes that entries are never removed except by clrhash.
 */

static struct hashtable_entry *
get_meth_hash(cl_object *keys, int argno, cl_object hashtable)
{
	int hsize;
	struct hashtable_entry *e, *htable;
	cl_object hkey, tlist;
	register cl_index i = 0;
	int k, n; /* k added by chou */
	bool b = 1;

	hsize = hashtable->hash.size;
	htable = hashtable->hash.data;
	for (n = 0; n < argno; n++)
	  i += (cl_index)keys[n] / 4; /* instead of:
				   i += hash_eql(keys[n]);
				   i += hash_eql(Cnil);
				 */
	for (i %= hsize, k = 0; k < hsize;  i = (i + 1) % hsize, k++) {
	  e = &htable[i];
	  hkey = e->key;
	  if (hkey == OBJNULL)
	    return(e);
	  for (n = 0, tlist = hkey; b && (n < argno);
	       n++, tlist = CDR(tlist))
	    b &= (keys[n] == CAR(tlist));
	  if (b)
	    return(&htable[i]);
	}
	internal_error("get_meth_hash");
}

static void
set_meth_hash(cl_object *keys, int argno, cl_object hashtable, cl_object value)
{
	struct hashtable_entry *e;
	cl_object keylist, *p;
	
	if (hashtable->hash.entries + 1 >= fix(hashtable->hash.threshold))
		extend_hashtable(hashtable);
	e = get_meth_hash(keys, argno, hashtable);
	if (e->key == OBJNULL)
		hashtable->hash.entries++;
	keylist = Cnil;
	for (p = keys + argno; p > keys; p--) keylist = CONS(p[-1], keylist);
	e->key = keylist;
	e->value = value;
}

cl_object
compute_method(int narg, cl_object gf, cl_object *args)
{
	cl_object func;
	int i, spec_no;
	struct hashtable_entry *e;
	cl_object spec_how_list = GFUN_SPEC(gf);
	cl_object table = GFUN_HASH(gf);
	cl_object argtype[narg]; /* __GNUC__ */

	for (i = 0, spec_no = 0; spec_how_list != Cnil; i++) {
		cl_object spec_how = CAR(spec_how_list);
		if (spec_how != Cnil) {
			if (i >= narg)
				FEwrong_num_arguments(gf);
			argtype[spec_no++] =
				(ATOM(spec_how) ||
				 Null(memql(args[i], spec_how))) ?
				cl_type_of(args[i]) :
				args[i];
		}
		spec_how_list = CDR(spec_how_list);
	}

	e = get_meth_hash(argtype, spec_no, table);

	if (e->key == OBJNULL) { 
		/* method not cached */
		cl_object methods, meth_comb, arglist = Cnil;
		i = narg;
		while (i-- > 0)
			arglist = CONS(args[i], arglist);
		methods = funcall(3, @'compute-applicable-methods', gf,
				  arglist);
		func = funcall(4, @'si::compute-effective-method', gf,
			       GFUN_COMB(gf), methods);
		/* update cache */
		set_meth_hash(argtype, spec_no, table, func);
	} else {
		/* method is already cached */
		func = e->value;
	}
	return func;
}

cl_object
si_set_compiled_function_name(cl_object fn, cl_object new_name)
{
	cl_type t = type_of(fn);

	if (t == t_cfun)
		@(return (fn->cfun.name = new_name))
	if (t == t_bytecodes)
		@(return (fn->bytecodes.name = new_name))
	FEerror("~S is not a compiled-function.", 1, fn);
}
