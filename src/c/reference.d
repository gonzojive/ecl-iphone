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

cl_object
cl_fboundp(cl_object sym)
{
	cl_object output;

	if (!SYMBOLP(sym)) {
		cl_object sym1 = setf_namep(sym);
		if (sym1 != OBJNULL)
			sym = sym1;
		else
			FEtype_error_symbol(sym);
	}
	if (sym->symbol.isform)
		output = Ct;
	else if (SYM_FUN(sym) == OBJNULL)
		output = Cnil;
	else
		output = Ct;
	@(return output)
}

cl_object
symbol_function(cl_object sym)
{
	if (!SYMBOLP(sym)) {
		cl_object sym1 = setf_namep(sym);
		if (sym1 != OBJNULL)
			sym = sym1;
		else
			FEtype_error_symbol(sym);
	}
	if (sym->symbol.isform || sym->symbol.mflag)
		FEinvalid_function(sym);
	if (SYM_FUN(sym) == OBJNULL)
		FEundefined_function(sym);
	return(SYM_FUN(sym));
}

/*
	Symbol-function returns
                function-closure		for function
		(macro . function-closure)	for macros
		(special . address)		for special forms.
	(if defined CLOS it returns also
		generic-function                for generic functions)
*/
cl_object
cl_symbol_function(cl_object sym)
{
	cl_object output;

	assert_type_symbol(sym);
	if (sym->symbol.isform)
		output = @'special';
	else if (SYM_FUN(sym) == OBJNULL)
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
	if (!SYMBOLP(fname)) {
		cl_object sym = setf_namep(fname);
		if (sym == OBJNULL)
			FEtype_error_symbol(fname);
		fname = sym;
	}
	return cl_symbol_function(fname);
}

cl_object
si_coerce_to_function(cl_object fun)
{
	cl_type t = type_of(fun);

	if (t == t_symbol) {
		if ((SYM_FUN(fun) == OBJNULL) || fun->symbol.mflag)
			FEundefined_function(fun);
		else
			@(return SYM_FUN(fun))
	} else if (t == t_cons && CAR(fun) == @'lambda') {
		return si_make_lambda(Cnil, CDR(fun));
	} else {
	  	cl_object setf_sym = setf_namep(fun);
		if ((setf_sym != OBJNULL) && (SYM_FUN(setf_sym) != OBJNULL))
			@(return SYM_FUN(setf_sym))
		else
			FEinvalid_function(fun);
	}
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
