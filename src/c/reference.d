/*
    reference.c -- Reference in Constants and Variables.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecls.h"
#include "ecls-inl.h"

#define SBOUNDP(sym) (SYM_VAL(sym) == OBJNULL)
#define FBOUNDP(sym) (SYM_FUN(sym) == OBJNULL)

@(defun fboundp (sym)
	cl_object output;
@
	if (!SYMBOLP(sym)) {
		cl_object sym1 = setf_namep(sym);
		if (sym1 != OBJNULL)
			sym = sym1;
		else
			FEtype_error_symbol(sym);
	}
	if (sym->symbol.isform)
		output = Ct;
	else if (FBOUNDP(sym))
		output = Cnil;
	else
		output = Ct;
	@(return output)
@)

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
	if (FBOUNDP(sym))
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
@(defun symbol_function (sym)
	cl_object output;
@
	if (!SYMBOLP(sym)) {
		cl_object sym1 = setf_namep(sym);
		if (sym1 != OBJNULL)
			sym = sym1;
		else
			FEtype_error_symbol(sym);
	}
	if (sym->symbol.isform)
		output = Sspecial;
	else if (FBOUNDP(sym))
		FEundefined_function(sym);
	else if (sym->symbol.mflag)
		output = CONS(Smacro, SYM_FUN(sym));
	else
		output = SYM_FUN(sym);
	@(return output)
@)

@(defun si::coerce_to_function (fun)
	enum cl_type t = type_of(fun);
@
	if (t == t_symbol) {
		cl_object fd = lex_fun_sch(fun);
		if (!Null(fd))
			return CADDR(fd);
		else if (FBOUNDP(fun) || fun->symbol.mflag)
			FEundefined_function(fun);
		else
			@(return SYM_FUN(fun))
	} else if (t == t_cons && CAR(fun) == Slambda) {
		return siLmake_lambda(2, Cnil, CDR(fun));
	} else {
	  	cl_object setf_sym = setf_namep(fun);
		if ((setf_sym != OBJNULL) && !FBOUNDP(setf_sym))
			@(return SYM_FUN(setf_sym))
		else
			FEinvalid_function(fun);
	}
@)

@(defun symbol_value (sym)
@
	if (!SYMBOLP(sym))
		FEtype_error_symbol(sym);
	if (SBOUNDP(sym))
		FEunbound_variable(sym);
	@(return SYM_VAL(sym))
@)

@(defun boundp (sym)
@
	if (!SYMBOLP(sym))
		FEtype_error_symbol(sym);
	@(return (SBOUNDP(sym)? Cnil : Ct))
@)

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

@(defun special_form_p (form)
@
	if (!SYMBOLP(form))
		FEtype_error_symbol(form);
	@(return (form->symbol.isform? Ct : Cnil))
@)
