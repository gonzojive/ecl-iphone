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

@(defun si::fset (fname def &optional macro pprint)
	cl_object sym = si_function_block_name(fname);
	cl_type t;
	bool mflag;
@
	if (Null(cl_functionp(def)))
		FEinvalid_function(def);
	if (sym->symbol.hpack != Cnil && sym->symbol.hpack->pack.locked)
		funcall(3, @'warn', make_simple_string("~S is being redefined."), fname);
	mflag = !Null(macro);
	if (sym->symbol.isform && !mflag)
		FEerror("Given that ~S is a special form, ~S cannot be defined as a function.",
			2, sym, fname);
	if (SYMBOLP(fname)) {
		sym->symbol.mflag = mflag;
		SYM_FUN(sym) = def;
		clear_compiler_properties(sym);
		if (pprint == Cnil)
			si_rem_sysprop(sym, @'si::pretty-print-format');
		else
			si_put_sysprop(sym, @'si::pretty-print-format', pprint);
	} else {
		if (mflag)
			FEerror("~S is not a valid name for a macro.", 1, fname);
		si_put_sysprop(sym, @'si::setf-symbol', def);
		si_rem_sysprop(sym, @'si::setf-lambda');
		si_rem_sysprop(sym, @'si::setf-method');
		si_rem_sysprop(sym, @'si::setf-update');
	}
	@(return fname)
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
cl_fmakunbound(cl_object fname)
{
	cl_object sym = si_function_block_name(fname);

	if (sym->symbol.hpack != Cnil && sym->symbol.hpack->pack.locked)
		funcall(3, @'warn', make_simple_string("~S is being redefined."),
			fname);
	if (SYMBOLP(fname)) {
		clear_compiler_properties(sym);
#ifdef PDE
		cl_remprop(fname, @'defun');
#endif
		SYM_FUN(sym) = OBJNULL;
		sym->symbol.mflag = FALSE;
	} else {
		cl_remprop(sym, @'si::setf-symbol');
		cl_remprop(sym, @'si::setf-lambda');
		cl_remprop(sym, @'si::setf-method');
		cl_remprop(sym, @'si::setf-update');
	}
	@(return fname)
}

void
clear_compiler_properties(cl_object sym)
{
	if (ecl_booted) {
		si_unlink_symbol(sym);
		funcall(2, @'si::clear-compiler-properties', sym);
	}
}

#ifdef PDE
void
record_source_pathname(cl_object sym, cl_object def)
{
  if (symbol_value(@'si::*record-source-pathname-p*') != Cnil)
    (void)funcall(3, @'si::record-source-pathname', sym, def);
}
#endif /* PDE */

static cl_object system_properties = OBJNULL;

cl_object
si_get_sysprop(cl_object sym, cl_object prop)
{
	cl_object plist = gethash_safe(sym, system_properties, Cnil);
	@(return ecl_getf(plist, prop, Cnil));
}

cl_object
si_put_sysprop(cl_object sym, cl_object prop, cl_object value)
{
	cl_object plist;
	assert_type_symbol(sym);
	plist = gethash_safe(sym, system_properties, Cnil);
	sethash(sym, system_properties, si_put_f(plist, value, prop));
	@(return value);
}

cl_object
si_rem_sysprop(cl_object sym, cl_object prop)
{
	cl_object plist, found;
	assert_type_symbol(sym);
	plist = gethash_safe(sym, system_properties, Cnil);
	plist = si_rem_f(plist, prop);
	found = VALUES(1);
	sethash(sym, system_properties, plist);
	@(return found);
}

void
init_assignment(void)
{
#ifdef PDE
	SYM_VAL(@'si::*record-source-pathname-p*') = Cnil;
#endif
	ecl_register_root(&system_properties);
	system_properties =
	    cl__make_hash_table(@'eq', MAKE_FIXNUM(1024), /* size */
				make_shortfloat(1.5), /* rehash-size */
				make_shortfloat(0.7)); /* rehash-threshold */
}
