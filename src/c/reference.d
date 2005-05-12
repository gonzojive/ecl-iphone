/*
    reference.c -- Reference in Constants and Variables.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecl.h"
#include "ecl-inl.h"

/*
	Symbol-function returns
                function-closure		for function
		(macro . function-closure)	for macros
		special				for special forms.
*/
cl_object
cl_symbol_function(cl_object sym)
{
	cl_object output;

	assert_type_symbol(sym);
	if (sym->symbol.isform)
		output = @'special';
	else if (SYM_FUN(sym) == Cnil)
		FEundefined_function(sym);
	else if (sym->symbol.mflag)
		output = CONS(@'si::macro', SYM_FUN(sym));
	else
		output = SYM_FUN(sym);
	@(return output)
}

cl_object
cl_fdefinition(cl_object fname)
{
	@(return ((SYMBOLP(fname))? cl_symbol_function(fname) : ecl_fdefinition(fname)))
}

cl_object
cl_fboundp(cl_object fname)
{
	if (SYMBOLP(fname)) {
		@(return ((fname->symbol.isform || SYM_FUN(fname) != Cnil)? Ct : Cnil))
	} else if (CONSP(fname)) {
		if (CAR(fname) == @'setf') {
			cl_object sym = CDR(fname);
			if (CONSP(sym)) {
				sym = CAR(sym);
				if (SYMBOLP(sym))
					@(return si_get_sysprop(sym, @'si::setf-symbol'))
			}
		}
	}
	FEinvalid_function_name(fname);
}

cl_object
ecl_fdefinition(cl_object fun)
{
	cl_type t = type_of(fun);
	cl_object output;

	if (t == t_symbol) {
		output = SYM_FUN(fun);
		if (output == Cnil)
			FEundefined_function(fun);
		if (fun->symbol.isform || fun->symbol.mflag)
			FEundefined_function(fun);
	} else if (t == t_cons) {
		if (!CONSP(CDR(fun)))
			FEinvalid_function_name(fun);
		if (CAR(fun) == @'setf') {
			output = si_get_sysprop(CADR(fun), @'si::setf-symbol');
			if (Null(output))
				FEundefined_function(fun);
		} else if (CAR(fun) == @'lambda') {
			return si_make_lambda(Cnil, CDR(fun));
		} else {
			FEinvalid_function(fun);
		}
	} else {
		FEinvalid_function(fun);
	}
	return output;
}

cl_object
si_coerce_to_function(cl_object fun)
{
	cl_type t = type_of(fun);
	if (!(t == t_cfun || t == t_cclosure
#ifdef CLOS
	      || (t == t_instance && fun->instance.isgf)
#endif
		)) {
	    fun = ecl_fdefinition(fun);
	}
	@(return fun)
}

cl_object
cl_symbol_value(cl_object sym)
{
	if (!SYMBOLP(sym))
		FEtype_error_symbol(sym);
	if (SYM_VAL(sym) == OBJNULL)
		FEunbound_variable(sym);
	@(return SYM_VAL(sym))
}

cl_object
cl_boundp(cl_object sym)
{
	if (!SYMBOLP(sym))
		FEtype_error_symbol(sym);
	@(return ((SYM_VAL(sym) == OBJNULL)? Cnil : Ct))
}

@(defun macro_function (sym &optional env)
	cl_object fd;
@
	if (!SYMBOLP(sym))
		FEtype_error_symbol(sym);
	if (Null(env))
		fd = Cnil;
	else {
		fd = search_macro(sym, env);
		if (!Null(fd)) @(return fd)
	}
	if (sym->symbol.mflag)
		fd = SYM_FUN(sym);
	@(return fd)
@)

cl_object
cl_special_operator_p(cl_object form)
{
	if (!SYMBOLP(form))
		FEtype_error_symbol(form);
	@(return (form->symbol.isform? Ct : Cnil))
}
