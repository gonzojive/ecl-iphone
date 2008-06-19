/* -*- mode: c; c-basic-offset: 8 -*- */
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

#include <ecl/ecl.h>
#include <string.h>	/* for memmove() */

cl_object
cl_make_cfun(void *c_function, cl_object name, cl_object cblock, int narg)
{
	cl_object cf;

	cf = cl_alloc_object(t_cfunfixed);
	cf->cfun.entry = c_function;
	cf->cfun.name = name;
	cf->cfun.block = cblock;
	cf->cfun.narg = narg;
	if (narg < 0 || narg > C_ARGUMENTS_LIMIT)
	    FEprogram_error("cl_make_cfun: function requires too many arguments.",0);
	return(cf);
}

cl_object
cl_make_cfun_va(void *c_function, cl_object name, cl_object cblock)
{
	cl_object cf;

	cf = cl_alloc_object(t_cfun);
	cf->cfun.entry = c_function;
	cf->cfun.name = name;
	cf->cfun.block = cblock;
	cf->cfun.narg = -1;
	return(cf);
}

cl_object
cl_make_cclosure_va(void *c_function, cl_object env, cl_object block)
{
	cl_object cc;

	cc = cl_alloc_object(t_cclosure);
	cc->cclosure.entry = c_function;
	cc->cclosure.env = env;
	cc->cclosure.block = block;
	return(cc);
}

void
cl_def_c_function(cl_object sym, void *c_function, int narg)
{
	si_fset(2, sym,
		cl_make_cfun(c_function, sym, ecl_symbol_value(@'si::*cblock*'), narg));
}

void
cl_def_c_macro(cl_object sym, void *c_function, int narg)
{
	si_fset(3, sym,
		(narg >= 0)?
		cl_make_cfun(c_function, sym, ecl_symbol_value(@'si::*cblock*'), 2):
		cl_make_cfun_va(c_function, sym, ecl_symbol_value(@'si::*cblock*')),
		Ct);
}

void
cl_def_c_function_va(cl_object sym, void *c_function)
{
	si_fset(2, sym,
		cl_make_cfun_va(c_function, sym, ecl_symbol_value(@'si::*cblock*')));
}

cl_object
si_compiled_function_name(cl_object fun)
{
	cl_object output;

	switch(type_of(fun)) {
	case t_bclosure:
		fun = fun->bclosure.code;
	case t_bytecodes:
		output = fun->bytecodes.name; break;
	case t_cfun:
	case t_cfunfixed:
		output = fun->cfun.name; break;
	case t_cclosure:
		output = Cnil; break;
	default:
		FEinvalid_function(fun);
	}
	@(return output)
}

cl_object
cl_function_lambda_expression(cl_object fun)
{
	cl_object output, name = Cnil, lex = Cnil;

	switch(type_of(fun)) {
	case t_bclosure:
		lex = fun->bclosure.lex;
		fun = fun->bclosure.code;
	case t_bytecodes:
		name = fun->bytecodes.name;
		output = fun->bytecodes.definition;
		if (!CONSP(output))
		    output = Cnil;
		else if (name == Cnil)
		    output = cl_cons(@'lambda', output);
		else if (name != @'si::bytecodes')
		    output = @list*(3, @'ext::lambda-block', name, output);
		break;
	case t_cfun:
	case t_cfunfixed:
		name = fun->cfun.name;
		lex = Cnil;
		output = Cnil;
		break;
	case t_cclosure:
		name = Cnil;
		lex = Ct;
		output = Cnil;
		break;
#ifdef CLOS
	case t_instance:
		if (fun->instance.isgf) {
			name = Cnil;
			lex = Cnil;
			output = Cnil;
			break;
		}
#endif
	default:
		FEinvalid_function(fun);
	}
	@(return output lex name)
}

cl_object
si_compiled_function_block(cl_object fun)
{
       cl_object output;

       switch(type_of(fun)) {
       case t_cfun:
       case t_cfunfixed:
	       output = fun->cfun.block; break;
       case t_cclosure:
	       output = fun->cclosure.block; break;
       default:
	       FEerror("~S is not a compiled-function.", 1, fun);
       }
       @(return output)
}
