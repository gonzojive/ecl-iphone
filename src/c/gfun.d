/*
    gfun.c -- Dispatch for generic functions.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecls.h"

/*
 * Functions defined in the PCL code.
 */
cl_object @'si::compute-applicable-methods';
cl_object @'si::compute-effective-method';
cl_object @'si::generic-function-method-combination';
cl_object @'si::generic-function-method-combination-args';

@(defun si::allocate_gfun (name arg_no ht)
	cl_object x;
	int n, i;
@
	if (type_of(ht) != t_hashtable)
		FEwrong_type_argument(@'hash-table', ht);

	x = alloc_object(t_gfun);
	x->gfun.specializers = NULL; /* for GC sake */
	x->gfun.name = name;
	x->gfun.method_hash = ht;
	n = fixnnint(arg_no);
	x->gfun.arg_no = n;
	x->gfun.specializers = alloc_align(sizeof(cl_object)*n, sizeof(cl_object));
	for (i = 0;  i < n;  i++)
		x->gfun.specializers[i] = OBJNULL;
	x->gfun.instance = Cnil;
	@(return x)
@)

@(defun si::gfun_name (x)
@
	if (type_of(x) != t_gfun)
		FEwrong_type_argument(@'dispatch-function', x);
	@(return x->gfun.name)
@)

@(defun si::gfun_name_set (x name)
@
	if (type_of(x) != t_gfun)
		FEwrong_type_argument(@'dispatch-function', x);
	x->gfun.name = name;
	@(return x)
@)

@(defun si::gfun_method_ht (x)
@
	if (type_of(x) != t_gfun)
		FEwrong_type_argument(@'dispatch-function', x);
	@(return x->gfun.method_hash)
@)

@(defun si::gfun_method_ht_set (x y)
@
	if (type_of(x) != t_gfun)
		FEwrong_type_argument(@'dispatch-function', x);
	if (type_of(y) != t_hashtable)
		FEwrong_type_argument(@'hash-table', y);
	x->gfun.method_hash = y;
	@(return x)
@)

@(defun si::gfun_spec_how_ref (x y)
	int i;
@
	if (type_of(x) != t_gfun)
		FEwrong_type_argument(@'dispatch-function', x);
	if (!FIXNUMP(y) ||
	    (i = fix(y)) < 0 || i >= x->gfun.arg_no)
		FEerror("~S is an illegal spec_how index.", 1, y);
	@(return x->gfun.specializers[i])
@)

@(defun si::gfun_spec_how_set (x y spec)
	int i;
@
	if (type_of(x) != t_gfun)
		FEwrong_type_argument(@'dispatch-function', x);
	if (!FIXNUMP(y) || (i = fix(y)) >= x->gfun.arg_no)
		FEerror("~S is an illegal spec_how index.", 1, y);
	x->gfun.specializers[i] = spec;
	@(return spec)
@)

@(defun si::gfun_instance (x)
@
	if (type_of(x) != t_gfun)
		FEwrong_type_argument(@'dispatch-function', x);
	@(return x->gfun.instance)
@)

@(defun si::gfun_instance_set (x y)
@
	if (type_of(x) != t_gfun)
		FEwrong_type_argument(@'dispatch-function', x);
	if (type_of(y) != t_instance)
		FEwrong_type_argument(@'instance', y);
	x->gfun.instance = y;
	@(return x)
@)

@(defun si::gfunp (x)
@
	@(return ((type_of(x) == t_gfun)? Ct : Cnil))
@)


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
	register int i = 0;
	int k, n; /* k added by chou */
	bool b = 1;

	hsize = hashtable->hash.size;
	htable = hashtable->hash.data;
	for (n = 0; n < argno; n++)
	  i += (int)keys[n] / 4; /* instead of:
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

@(defun si::method_ht_get (keylist table)
	struct hashtable_entry *e;
@
	{  int i, argn = length(keylist);
	   cl_object keys[argn];	/* __GNUC__ */

	   for (i = 0; i < argn; i++, keylist = CDR(keylist))
	     keys[i] = CAR(keylist);
	   e = get_meth_hash(keys, argn, table);
	}
	@(return ((e->key == OBJNULL)? Cnil : e->value))
@)

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
gcall(int narg, cl_object fun, cl_object *args)
{
	cl_object func;

	{ int i, spec_no;
	  struct hashtable_entry *e;
	  cl_object *spec_how = fun->gfun.specializers;
	  cl_object argtype[narg]; /* __GNUC__ */

	  if (narg < fun->gfun.arg_no)
	    FEerror("Generic function ~S requires more than ~R argument~:p.",
		    2, fun->gfun.name, MAKE_FIXNUM(narg));
	  for (i = 0, spec_no = 0; i < fun->gfun.arg_no; i++, spec_how++) {
	    if (*spec_how != Cnil)
	      argtype[spec_no++] = (ATOM(*spec_how) ||
				    !member_eq(args[i], *spec_how)) ?
				      TYPE_OF(args[i]) :
					args[i];
	  }

	  e = get_meth_hash(argtype, spec_no, fun->gfun.method_hash);

	  if (e->key == OBJNULL)  { 
	    /* method not cached */
	    register cl_object gf = fun->gfun.instance;
	    cl_object methods, meth_comb, meth_args, arglist = Cnil;

	    i = narg;
	    while (i-- > 0)
	      arglist = CONS(args[i], arglist);
	    methods = funcall(3, siScompute_applicable_methods, gf, arglist);
	    meth_comb = funcall(2, siSgeneric_function_method_combination, gf);
	    meth_args = funcall(2, siSgeneric_function_method_combination_args,gf);
	    func = funcall(5, siScompute_effective_method, gf, methods,
			   meth_comb, meth_args);
	  
	    /* update cache */
	    set_meth_hash(argtype, spec_no, fun->gfun.method_hash, func);
	  } else
	    /* method is already cached */
	    func = e->value;
	}
	switch (type_of(func)) {

	case t_cfun:
	  return APPLY(narg, *func->cfun.entry, args);

	case t_cclosure:
	  { int i; CSTACK(narg+1);
	    CPUSH(func->cclosure.env);
	    for (i = 0; i < narg; i++)
	      CPUSH(*args++);
#ifdef CCALL
	    return CCALL(narg+1, func->cclosure.entry);
#else
	    return APPLY(narg+1, func->cclosure.entry, CSTACK_BOT);
#endif CCALL
	  }
	case t_bytecodes:
	  return apply(narg, func, args);

	default:
	  FEinvalid_function(func);
	}
}

@(defun si::set_compiled_function_name (fn new_name)
	enum cl_type t = type_of(fn);
@
	if (t == t_cfun)
		@(return (fn->cfun.name = new_name))
	if (t == t_bytecodes)
		@(return (fn->bytecodes.data[0] = new_name))
	FEerror("~S is not a compiled-function.", 1, fn);
@)
