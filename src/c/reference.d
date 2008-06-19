/* -*- mode: c; c-basic-offset: 8 -*- */
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

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>

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
	int type = ecl_symbol_type(sym);
	if (type & stp_special_form) {
		output = @'special';
	} else if (SYM_FUN(sym) == Cnil) {
		FEundefined_function(sym);
	} else if (type & stp_macro) {
		output = CONS(@'si::macro', SYM_FUN(sym));
	} else {
		output = SYM_FUN(sym);
	}
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
	if (Null(fname)) {
		@(return Cnil);
	} else if (SYMBOLP(fname)) {
		@(return (((fname->symbol.stype & stp_special_form)
			   || SYM_FUN(fname) != Cnil)? Ct : Cnil))
	} else if (LISTP(fname)) {
		if (CAR(fname) == @'setf') {
			cl_object sym = CDR(fname);
			if (CONSP(sym) && CDR(sym) == Cnil) {
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
		if (fun->symbol.stype & (stp_macro | stp_special_form))
			FEundefined_function(fun);
	} else if (Null(fun)) {
		FEundefined_function(fun);
	} else if (t == t_list) {
		cl_object sym = CDR(fun);
		if (!CONSP(sym))
			FEinvalid_function_name(fun);
		if (CAR(fun) == @'setf') {
			if (CDR(sym) != Cnil)
				FEinvalid_function_name(fun);
			sym = CAR(sym);
			if (type_of(sym) != t_symbol)
				FEinvalid_function_name(fun);
			output = si_get_sysprop(sym, @'si::setf-symbol');
			if (Null(output))
				FEundefined_function(fun);
		} else if (CAR(fun) == @'lambda') {
			return si_make_lambda(Cnil, sym);
		} else if (CAR(fun) == @'ext::lambda-block') {
			return si_make_lambda(CAR(sym), CDR(sym));
		} else {
			FEinvalid_function_name(fun);
		}
	} else {
		FEinvalid_function_name(fun);
	}
	return output;
}

cl_object
si_coerce_to_function(cl_object fun)
{
	cl_type t = type_of(fun);
	if (!(t == t_cfun || t == t_cfunfixed || t == t_cclosure
	      || t == t_bytecodes || t == t_bclosure
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
	cl_object value;
	if (Null(sym)) {
		value = sym;
	} else {
		if (!SYMBOLP(sym)) {
			FEtype_error_symbol(sym);
		}
		value = SYM_VAL(sym);
		if (value == OBJNULL)
			FEunbound_variable(sym);
	}
	@(return value)
}

cl_object
cl_boundp(cl_object sym)
{
	cl_object output;
	if (Null(sym)) {
		output = Ct;
	} else {
		if (!SYMBOLP(sym))
			FEtype_error_symbol(sym);
		if (SYM_VAL(sym) == OBJNULL)
			output = Cnil;
		else
			output = Ct;
	}
	@(return output)
}

cl_object
cl_special_operator_p(cl_object form)
{
	int special = ecl_symbol_type(form) & stp_special_form;
	@(return (special? Ct : Cnil))
}
