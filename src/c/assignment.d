/*
    assignment.c  -- Assignment.
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
#include <string.h>

cl_object
cl_set(cl_object var, cl_object val)
{
	if (!SYMBOLP(var))
		FEtype_error_symbol(var);
	if (var->symbol.stype == stp_constant)
		FEinvalid_variable("Cannot assign to the constant ~S.", var);
	return1(SYM_VAL(var) = val);
}

cl_object
setf_namep(cl_object fun_spec)
{	cl_object cdr;
	int intern_flag; 
	if (CONSP(fun_spec) && !endp(cdr = CDR(fun_spec)) &&
	    endp(CDR(cdr)) && CAR(fun_spec) == @'setf') {
	  cl_object sym, fn_name = CAR(cdr);
	  cl_object fn_str = fn_name->symbol.name;
	  int l = fn_str->string.fillp + 7;
	  cl_object string = cl_alloc_simple_string(l);
	  char *str = string->string.self;
	  strncpy(str, "(SETF ", 6);
	  strncpy(str + 6, fn_str->string.self, fn_str->string.fillp);
	  str[l-1] = ')';
	  sym = intern(string, fn_name->symbol.hpack, &intern_flag);
	  return(sym);
	} else return(OBJNULL);
}

cl_object
si_setf_namep(cl_object arg)
{
	cl_object x;

	x = setf_namep(arg);
	@(return ((x != OBJNULL) ? x : Cnil))
}

@(defun si::fset (fun def &optional macro pprint)
	cl_type t;
	bool mflag;
@
	mflag = !Null(macro);
	if (!SYMBOLP(fun)) {
		cl_object sym = setf_namep(fun);
		if (sym == OBJNULL)
			FEtype_error_symbol(fun);
		if (mflag)
			FEerror("Cannot define a macro with name (SETF ~S).", 1, fun);
		fun = CADR(fun);
		putprop(fun, sym, @'si::setf-symbol');
		remprop(fun, @'si::setf-lambda');
		remprop(fun, @'si::setf-method');
		remprop(fun, @'si::setf-update');
		fun = sym;
	}
	if (fun->symbol.isform && !mflag)
		FEerror("~S, a special form, cannot be redefined as a function.",
			1, fun);
	clear_compiler_properties(fun);
	if (fun->symbol.hpack->pack.locked && SYM_FUN(fun) != OBJNULL)
	  funcall(3, @'warn', make_simple_string("~S is being redefined."), fun);
	t = type_of(def);
	if (t == t_bytecodes || t == t_cfun || t == t_cclosure) {
	        SYM_FUN(fun) = def;
#ifdef CLOS
	} else if (t == t_gfun) {
		SYM_FUN(fun) = def;
#endif
	} else {
		FEinvalid_function(def);
	}
	fun->symbol.mflag = !Null(macro);
	if (pprint != Cnil)
		fun->symbol.plist
		= putf(fun->symbol.plist, pprint, @'si::pretty-print-format');
	@(return fun)
@)

cl_object
cl_makunbound(cl_object sym)
{
	if (!SYMBOLP(sym))
		FEtype_error_symbol(sym);
	if ((enum stype)sym->symbol.stype == stp_constant)
		FEinvalid_variable("Cannot unbind the constant ~S.", sym);
	SYM_VAL(sym) = OBJNULL;
	@(return sym)
}
	
cl_object
cl_fmakunbound(cl_object sym)
{
	if (!SYMBOLP(sym)) {
		cl_object sym1 = setf_namep(sym);
		if (sym1 == OBJNULL)
			FEtype_error_symbol(sym);
		sym = CADR(sym);
		remprop(sym, @'si::setf-lambda');
		remprop(sym, @'si::setf-method');
		remprop(sym, @'si::setf-update');
		cl_fmakunbound(sym1);
		@(return sym)
	}
	clear_compiler_properties(sym);
#ifdef PDE
	remprop(sym, @'defun');
#endif
	if (sym->symbol.hpack->pack.locked && SYM_FUN(sym) != OBJNULL)
	  funcall(3, @'warn', make_simple_string("~S is being redefined."), sym);
	SYM_FUN(sym) = OBJNULL;
	sym->symbol.mflag = FALSE;
	@(return sym)
}

void
clear_compiler_properties(cl_object sym)
{
	si_unlink_symbol(sym);
	funcall(2, @'si::clear-compiler-properties', sym);
}

cl_object
si_clear_compiler_properties(cl_object sym)
{
	@(return sym)
}

#ifdef PDE
void
record_source_pathname(cl_object sym, cl_object def)
{
  if (symbol_value(@'si::*record-source-pathname-p*') != Cnil)
    (void)funcall(3, @'si::record-source-pathname', sym, def);
}
#endif /* PDE */

void
init_assignment(void)
{
#ifdef PDE
	SYM_VAL(@'si::*record-source-pathname-p*') = Cnil;
#endif
}
