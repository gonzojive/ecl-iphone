/*
    clos.c -- CLOS bootstrap.
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

/******************************* EXPORTS ******************************/

cl_object class_class, class_object, class_built_in;

/******************************* ------- ******************************/

@(defun find-class (name &optional (errorp Ct) env)
	cl_object class;
@
	class = gethash_safe(name, SYM_VAL(@'si::*class-name-hash-table*'), Cnil);
	if (class == Cnil) {
		if (!Null(errorp))
			FEerror("No class named ~S.", 1, name);
	}
	@(return class)
@)	

void
init_clos(void)
{
	SYM_VAL(@'si::*class-name-hash-table*') =
	    cl__make_hash_table(@'eq', MAKE_FIXNUM(1024), /* size */
				make_shortfloat(1.5), /* rehash-size */
				make_shortfloat(0.7)); /* rehash-threshold */

	/* booting Class CLASS */
	
  	class_class = cl_alloc_instance(4);
	ecl_register_static_root(&class_class);
	CLASS_OF(class_class) = class_class;
	CLASS_NAME(class_class) = @'class';
	CLASS_SUPERIORS(class_class) = Cnil;
	CLASS_INFERIORS(class_class) = Cnil;
	CLASS_SLOTS(class_class) = OBJNULL;	/* filled later */

	sethash(@'class', SYM_VAL(@'si::*class-name-hash-table*'), class_class);

	/* booting Class BUILT-IN-CLASS */
	
  	class_built_in = cl_alloc_instance(4);
	ecl_register_static_root(&class_built_in);
	CLASS_OF(class_built_in) = class_class;
	CLASS_NAME(class_built_in) = @'built-in-class';
	CLASS_SUPERIORS(class_built_in) = CONS(class_class, Cnil);
	CLASS_INFERIORS(class_built_in) = Cnil;
	CLASS_SLOTS(class_built_in) = OBJNULL;	/* filled later */

	sethash(@'built-in-class', SYM_VAL(@'si::*class-name-hash-table*'), class_built_in);

	/* booting Class T (= OBJECT) */
	
  	class_object = cl_alloc_instance(4);
	ecl_register_static_root(&class_object);
	CLASS_OF(class_object) = class_built_in;
	CLASS_NAME(class_object) = Ct;
	CLASS_SUPERIORS(class_object) = Cnil;
	CLASS_INFERIORS(class_object) = CONS(class_class, Cnil);
	CLASS_SLOTS(class_object) = Cnil;

	sethash(Ct, SYM_VAL(@'si::*class-name-hash-table*'), class_object);

	/* complete now Class CLASS */
	CLASS_SUPERIORS(class_class) = CONS(class_object, Cnil);
	CLASS_INFERIORS(class_class) = CONS(class_built_in, Cnil);
}
