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
set(cl_object var, cl_object val)
{
	if (!SYMBOLP(var))
		FEtype_error_symbol(var);
	if (var->symbol.stype == stp_constant)
		FEinvalid_variable("Cannot assign to the constant ~S.", var);
	return (SYM_VAL(var) = val);
}

@(defun set (var val)
@
	@(return set(var, val))
@)

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
	  char *str = (char *)cl_alloc_atomic(l+1);
	  string->string.self = str;
	  strncpy(str, "(SETF ", 6);
	  strncpy(str + 6, fn_str->string.self, fn_str->string.fillp);
	  str[l-1] = ')';
	  str[l] = '\0';
	  sym = intern(string, fn_name->symbol.hpack, &intern_flag);
	  return(sym);
	} else return(OBJNULL);
}

@(defun si::setf_namep (arg)
	cl_object x;
@
	x = setf_namep(arg);
	@(return ((x != OBJNULL) ? x : Cnil))
@)

@(defun si::fset (fun def &optional macro pprint)
	cl_type t;
@
	if (!SYMBOLP(fun)) {
		cl_object sym = setf_namep(fun);
		if (sym == OBJNULL)
			FEtype_error_symbol(fun);
		fun = CADR(fun);
		putprop(fun, sym, @'si::setf-symbol');
		remprop(fun, @'si::setf-lambda');
		remprop(fun, @'si::setf-method');
		remprop(fun, @'si::setf-update');
		fun = sym;
	}
	if (fun->symbol.isform) {
		if (fun->symbol.mflag) {
			if (symbol_value(@'si::*inhibit-macro-special*') != Cnil)
				fun->symbol.isform = FALSE;
		} else if (symbol_value(@'si::*inhibit-macro-special*') != Cnil)
			FEerror("~S, a special form, cannot be redefined.", 1, fun);
	}
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

@(defun makunbound (sym)
@
	if (!SYMBOLP(sym))
		FEtype_error_symbol(sym);
	if ((enum stype)sym->symbol.stype == stp_constant)
		FEinvalid_variable("Cannot unbind the constant ~S.", sym);
	SYM_VAL(sym) = OBJNULL;
	@(return sym)
@)
	
@(defun fmakunbound (sym)
@
	if (!SYMBOLP(sym)) {
		cl_object sym1 = setf_namep(sym);
		if (sym1 == OBJNULL)
			FEtype_error_symbol(sym);
		sym = CADR(sym);
		remprop(sym, @'si::setf-lambda');
		remprop(sym, @'si::setf-method');
		remprop(sym, @'si::setf-update');
		@fmakunbound(1, sym1);
		@(return sym)
	}
	if (sym->symbol.isform) {
	  if (sym->symbol.mflag) {
	    if (symbol_value(@'si::*inhibit-macro-special*') != Cnil)
	      sym->symbol.isform = FALSE;
	  } else if (symbol_value(@'si::*inhibit-macro-special*') != Cnil)
	    FEerror("~S, a special form, cannot be redefined.", 1, sym);
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
@)

void
clear_compiler_properties(cl_object sym)
{
	@si::unlink-symbol(1, sym);
	if (symbol_value(@'si::*inhibit-macro-special*') != Cnil)
		(void)funcall(2, @'si::clear-compiler-properties', sym);
}

@(defun si::clear_compiler_properties (sym)
@
	@(return sym)
@)

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
	SYM_VAL(@'si::*inhibit-macro-special*') = Cnil;
#ifdef PDE
	SYM_VAL(@'si::*record-source-pathname-p*') = Cnil;
#endif
}
