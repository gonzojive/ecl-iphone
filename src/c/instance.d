/*
    instance.c -- CLOS interface.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecl.h"

cl_object
ecl_allocate_instance(cl_object clas, int size)
{
	cl_object x = cl_alloc_instance(size);
	int i;
	CLASS_OF(x) = clas;
	for (i = 0;  i < size;  i++)
		x->instance.slots[i] = OBJNULL;
	return(x);
}

cl_object
si_allocate_raw_instance(cl_object clas, cl_object size)
{
	if (type_of(clas) != t_instance)
	  FEwrong_type_argument(@'instance', clas);

	@(return ecl_allocate_instance(clas, fixnnint(size)))
}

/* corr is a list of (newi . oldi) describing which of the new slots
   retains a value from an old slot
 */
cl_object
si_change_instance(cl_object x, cl_object clas, cl_object size, cl_object corr)
{
	int nslot, i;
	cl_object * oldslots;

	if (type_of(x) != t_instance)
	  FEwrong_type_argument(@'instance', x);

	if (type_of(clas) != t_instance)
	  FEwrong_type_argument(@'instance', clas);

	nslot = fixnnint(size);
	CLASS_OF(x) = clas;
	x->instance.length = nslot;
	oldslots = x->instance.slots;
	x->instance.slots = (cl_object *)cl_alloc_align(sizeof(cl_object)*nslot,sizeof(cl_object));
	for (i = 0;  i < nslot;  i++) {
	  if (!Null(corr) && fix(CAAR(corr)) == i) {
	    x->instance.slots[i] = oldslots[fix(CDAR(corr))];
	    corr = CDR(corr);
	  }
	  else
	    x->instance.slots[i] = OBJNULL;
	}
	@(return) /* FIXME! Is this what we need? */
}

cl_object
si_instance_class(cl_object x)
{
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	@(return CLASS_OF(x))
}

cl_object
si_instance_class_set(cl_object x, cl_object y)
{
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	if (type_of(y) != t_instance)
		FEwrong_type_argument(@'instance', y);
	CLASS_OF(x) = y;
	@(return x)
}

cl_object
instance_ref(cl_object x, int i)
{
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	if (i >= x->instance.length || i < 0)
	        FEerror("~S is an illegal slot index1.",1,i);
	return(x->instance.slots[i]);
}

cl_object
si_instance_ref(cl_object x, cl_object index)
{
	cl_fixnum i;

	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	if (!FIXNUMP(index) ||
	    (i = fix(index)) < 0 || i >= x->instance.length)
		FEerror("~S is an illegal slot index.", 1, index);
	@(return x->instance.slots[i])
}

cl_object
si_instance_ref_safe(cl_object x, cl_object index)
{
	cl_fixnum i;

	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	if (!FIXNUMP(index) ||
	    (i = fix(index)) < 0 || i >= x->instance.length)
		FEerror("~S is an illegal slot index.", 1, index);
	x = x->instance.slots[i];
	if (x == OBJNULL)
		FEerror("Slot index ~S unbound", 1, index);
	@(return x)
}

cl_object
instance_set(cl_object x, int i, cl_object v)
{
        if (type_of(x) != t_instance)
                FEwrong_type_argument(@'instance', x);
	if (i >= x->instance.length || i < 0)
	        FEerror("~S is an illegal slot index2.", 1, i);
	x->instance.slots[i] = v;
	return(v);
}

cl_object
si_instance_set(cl_object x, cl_object index, cl_object value)
{
	cl_fixnum i;

	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	if (!FIXNUMP(index) ||
	    (i = fix(index)) >= x->instance.length || i < 0)
		FEerror("~S is an illegal slot index.", 1, index);
	x->instance.slots[i] = value;
	@(return value)
}

cl_object
si_instancep(cl_object x)
{
	@(return ((type_of(x) == t_instance) ? Ct : Cnil))
}

cl_object
si_unbound()
{
	/* Returns an object that cannot be read or written and which
	   is used to represent an unitialized slot */
	@(return OBJNULL)
}

cl_object
si_sl_boundp(cl_object x)
{
	@(return ((x == OBJNULL) ? Cnil : Ct))
}

cl_object
si_sl_makunbound(cl_object x, cl_object index)
{
	cl_fixnum i;

	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	if (!FIXNUMP(index) ||
	    (i = fix(index)) >= x->instance.length || i < 0)
		FEerror("~S is an illegal slot index.", 1, index);
	x->instance.slots[i] = OBJNULL;
	@(return x)
}

cl_object
ecl_copy_instance(cl_object x)
{
	cl_object y;

	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	y = ecl_allocate_instance(x->instance.clas, x->instance.length);
	memcpy(y->instance.slots, x->instance.slots,
	       x->instance.length * sizeof(cl_object));
	@(return y)
}
