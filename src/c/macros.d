/*
    macros.c -- Macros.
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

/******************************* REQUIRES ******************************/

/* Requires expand-defmacro, from lsp/defmacro.lsp */

/******************************* EXPORTS ******************************/

cl_object Vmacroexpand_hook;
cl_object siSexpand_defmacro;
cl_object siVinhibit_macro_special;

/******************************* ------- ******************************/

/*
	MACRO_DEF is an internal function which, given a form, returns
	the expansion function if the form is a macro form.  Otherwise,
	MACRO_DEF returns NIL.
*/
cl_object
search_symbol_macro(cl_object name, cl_object env)
{
	cl_object record = assq(name, CAR(env));
	if (CONSP(record) && CADR(record) == siSsymbol_macro)
	  return CADDR(record);
	return Cnil;
}

cl_object
search_macro(cl_object name, cl_object env)
{
	return lex_sch(CDR(env), name, Smacro);
}

cl_object
macro_def(cl_object form, cl_object env)
{
	cl_object head, fd;

	if (ATOM(form)) {
		if (!SYMBOLP(form))
			return Cnil;
		/* First look for SYMBOL-MACROLET definitions */
		fd = search_symbol_macro(form, env);
		return fd;
	}
	head = CAR(form);
	if (!SYMBOLP(head))
		return(Cnil);
	fd = search_macro(head, env);
	if (!Null(fd))
		return fd;
	else if (head->symbol.mflag)
		return(SYM_FUN(head));
	else
		return(Cnil);
}

@(defun macroexpand (form &optional (env Cnil))
	cl_object new_form = OBJNULL;
	cl_object done = Cnil;
@
	new_form = macro_expand1(form, env);
	while (new_form != form) {
		done = Ct;
		form = new_form;
		new_form = macro_expand(form, env);
	}
	@(return new_form done)
@)

@(defun macroexpand_1 (form &optional (env Cnil))
	cl_object new_form;
@
	new_form = macro_expand1(form, env);
	@(return new_form (new_form == form? Cnil : Ct))
@)

/*
	MACRO_EXPAND1 is an internal function which simply applies the
	function EXP_FUN to FORM.  On return, the expanded form is stored
	in VALUES(0).
*/
cl_object
macro_expand1(cl_object form, cl_object env)
{
	cl_object hook, lex;
	cl_object exp_fun;

	exp_fun = macro_def(form, env);
	if (Null(exp_fun))
		return form;
	hook = symbol_value(Vmacroexpand_hook);
	if (hook == Sfuncall)
		return funcall(3, exp_fun, form, env);
	else
		return funcall(4, hook, exp_fun, form, env);
}

/*
	MACRO_EXPAND expands a form as many times as possible and returns
	the finally expanded form.
*/
cl_object
macro_expand(cl_object form, cl_object env)
{
	cl_object new_form;

	for (new_form = OBJNULL; new_form != form; form = new_form) {
		new_form = macro_expand1(form, env);
	}
	return new_form;
}

void
init_macros(void)
{
	SYM_VAL(Vmacroexpand_hook) = Sfuncall;
	SYM_VAL(siVinhibit_macro_special) = Cnil;
}
