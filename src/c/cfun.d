/*
    cfun.c -- Compiled functions.
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
#include <string.h>	/* for memmove() */

#ifdef PDE
cl_object @'defun', @'defmacro';
#endif

cl_object
make_cfun(cl_objectfn self, cl_object name, cl_object cblock)
{
	cl_object cf;

	cf = cl_alloc_object(t_cfun);
	cf->cfun.entry = self;
	cf->cfun.name = name;
	cf->cfun.block = cblock;
	return(cf);
}

cl_object
make_cclosure(cl_objectfn self, cl_object env, cl_object block)
{
	cl_object cc;

	cc = cl_alloc_object(t_cclosure);
	cc->cclosure.entry = self;
	cc->cclosure.env = env;
	cc->cclosure.block = block;
	return(cc);
}

void
MF(cl_object sym, cl_objectfn self, cl_object block)
{
	cl_object cf;

	if (!SYMBOLP(sym))
		FEtype_error_symbol(sym);
	if (sym->symbol.isform && sym->symbol.mflag)
		sym->symbol.isform = FALSE;
	clear_compiler_properties(sym);
#ifdef PDE
	record_source_pathname(sym, @'defun');
#endif
	cf = cl_alloc_object(t_cfun);
	cf->cfun.entry = self;
	cf->cfun.name = sym;
	cf->cfun.block = block;
	SYM_FUN(sym) = cf;
	sym->symbol.mflag = FALSE;
}

void
MM(cl_object sym, cl_objectfn self, cl_object block)
{
	cl_object cf;

	if (!SYMBOLP(sym))
		FEtype_error_symbol(sym);
	if (sym->symbol.isform && sym->symbol.mflag)
		sym->symbol.isform = FALSE;
	clear_compiler_properties(sym);
#ifdef PDE
	record_source_pathname(sym, @'defmacro');
#endif
	cf = cl_alloc_object(t_cfun);
	cf->cfun.entry = self;
	cf->cfun.name = sym;
	cf->cfun.block = block;
	SYM_FUN(sym) = cf;
	sym->symbol.mflag = TRUE;
}

cl_object
make_function(const char *s, cl_objectfn f)
{
	cl_object x;

	x = make_ordinary(s);
	SYM_FUN(x) = make_cfun(f, x, NULL);
	x->symbol.mflag = FALSE;
	return(x);
}

cl_object
make_si_function(const char *s, cl_objectfn f)
{
	cl_object x;

	x = make_si_ordinary(s);
	SYM_FUN(x) = make_cfun(f, x, NULL);
	x->symbol.mflag = FALSE;
	return(x);
}

@(defun si::compiled_function_name (fun)
	cl_object output;
@
	switch(type_of(fun)) {
	case t_bytecodes:
		output = fun->bytecodes.data[0]; break;
	case t_cfun:
		output = fun->cfun.name; break;
	case t_cclosure:
		output = Cnil; break;
	default:
		FEerror("~S is not a compiled-function.", 1, fun);
	}
	@(return output)
@)

@(defun si::compiled_function_source (fun)
	cl_object output;
@
	switch(type_of(fun)) {
	case t_bytecodes:
		if (!Null(fun->bytecodes.lex))
			output = Cnil;
		else {
			output = fun->bytecodes.data[fun->bytecodes.size-1];
			if (!CONSP(output)) output = Cnil;
		}
		break;
	case t_cfun:
	case t_cclosure:
		output = Cnil; break;
	default:
		FEerror("~S is not a compiled-function.", 1, fun);
	}
	@(return output)
@)

@(defun si::compiled_function_block (fun)
       cl_object output;
@
       switch(type_of(fun)) {
	case t_cfun:
		output = fun->cfun.block; break;
	case t_cclosure:
		output = fun->cclosure.block; break;
	default:
		FEerror("~S is not a compiled-function.", 1, fun);
	}
	@(return output)
@)
