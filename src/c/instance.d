/*
    instance.c -- CLOS interface.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecls.h"

/******************************* EXPORTS ******************************/

cl_object @'print-object';

/******************************* ------- ******************************/

cl_object
allocate_instance(cl_object class, int size)
{
	cl_object x = alloc_instance(size);
	int i;
	x->instance.class = class;
	for (i = 0;  i < size;  i++)
		x->instance.slots[i] = OBJNULL;
	return(x);
}

@(defun si::allocate_instance (class size)
@
	if (type_of(class) != t_instance)
	  FEwrong_type_argument(@'instance', class);

	@(return allocate_instance(class, fixnnint(size)))
@)

/* corr is a list of (newi . oldi) describing which of the new slots
   retains a value from an old slot
 */
@(defun si::change_instance (x class size corr)
	int nslot, i;
	cl_object * oldslots;
@
	if (type_of(x) != t_instance)
	  FEwrong_type_argument(@'instance', x);

	if (type_of(class) != t_instance)
	  FEwrong_type_argument(@'instance', class);

	nslot = fixnnint(size);
	x->instance.class = class;
	x->instance.length = nslot;
	oldslots = x->instance.slots;
	x->instance.slots = alloc_align(sizeof(cl_object)*nslot,sizeof(cl_object));
	for (i = 0;  i < nslot;  i++) {
	  if (!Null(corr) && fix(CAAR(corr)) == i) {
	    x->instance.slots[i] = oldslots[fix(CDAR(corr))];
	    corr = CDR(corr);
	  }
	  else
	    x->instance.slots[i] = OBJNULL;
	}
	@(return) /* FIXME! Is this what we need? */
@)

@(defun si::instance_class (x)
@
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	@(return x->instance.class)
@)

@(defun si::instance_class_set (x y)
@
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	if (type_of(y) != t_instance)
		FEwrong_type_argument(@'instance', y);
	x->instance.class = y;
	@(return x)
@)

cl_object
instance_ref(cl_object x, int i)
{
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	if (i >= x->instance.length || i < 0)
	        FEerror("~S is an illegal slot index1.",1,i);
	return(x->instance.slots[i]);
}

@(defun si::instance_ref (x index)
	cl_fixnum i;
@
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	if (!FIXNUMP(index) ||
	    (i = fix(index)) < 0 || i >= x->instance.length)
		FEerror("~S is an illegal slot index.", 1, index);
	@(return x->instance.slots[i])
@)

@(defun si::instance_ref_safe (x index)
	cl_fixnum i;
@
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	if (!FIXNUMP(index) ||
	    (i = fix(index)) < 0 || i >= x->instance.length)
		FEerror("~S is an illegal slot index.", 1, index);
	x = x->instance.slots[i];
	if (x == OBJNULL)
		FEerror("Slot index ~S unbound", 1, index);
	@(return x->instance.slots[i])
@)

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

@(defun si::instance_set (x index value)
	cl_fixnum i;
@
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	if (!FIXNUMP(index) ||
	    (i = fix(index)) >= x->instance.length || i < 0)
		FEerror("~S is an illegal slot index.", 1, index);
	x->instance.slots[i] = value;
	@(return value)
@)

@(defun si::instancep (x)
@
	@(return ((type_of(x) == t_instance) ? Ct : Cnil))
@)

@(defun si::sl_boundp (x)
@
	@(return ((x == OBJNULL) ? Cnil : Ct))
@)

@(defun si::sl_makunbound (x index)
	cl_fixnum i;
@
	if (type_of(x) != t_instance)
		FEwrong_type_argument(@'instance', x);
	if (!FIXNUMP(index) ||
	    (i = fix(index)) >= x->instance.length || i < 0)
		FEerror("~S is an illegal slot index.", 1, index);
	x->instance.slots[i] = OBJNULL;
	@(return x)
@)
