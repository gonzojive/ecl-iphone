/*
    eval.c -- Eval.
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

/* Calling conventions:
   Compiled C code calls lisp function supplying #args, and args.
   Linking function performs check_args, gets jmp_buf with _setjmp, then
    if cfun then stores C code address into function link location
	    and transfers to jmp_buf at cf_self
    if cclosure then replaces #args with cc_env and calls cc_self
    otherwise, it emulates funcall.
 */

/*
 *----------------------------------------------------------------------
 *
 *     apply --
 *	    applies a Lisp function to the arguments in array args.
 *	    narg is their count.
 *
 *     Results:
 *	    number of values
 *
 *     Side Effect:
 *	    values are placed into the array Values
 *----------------------------------------------------------------------
 */
cl_object
apply(int narg, cl_object fun, cl_object *args)
{
      AGAIN:
	switch (type_of(fun)) {
	case t_cfun:
		return APPLY(narg, fun->cfun.entry, args);
	case t_cclosure:
		return APPLY_closure(narg, fun->cclosure.entry, fun->cclosure.env,
				     args);
#ifdef CLOS
	case t_gfun:
		fun = compute_method(narg, fun, args);
		goto AGAIN;
#endif
	case t_symbol: {
		cl_object x = SYM_FUN(fun);
		if (x == OBJNULL)
			FEundefined_function(fun);
		fun = x;
		goto AGAIN;
	}
	case t_bytecodes:
		return lambda_apply(narg, fun, args);
	}
	FEinvalid_function(fun);
}

/*----------------------------------------------------------------------*
 *	Linking mechanism						*
 *----------------------------------------------------------------------*/

cl_object
link_call(cl_object sym, cl_objectfn *pLK, int narg, va_list args)
{
	cl_object fun = symbol_function(sym);

	if (fun == OBJNULL) FEerror("Undefined function.", 0);
 AGAIN:
	switch (type_of(fun)) {
	case t_cfun:
		if (pLK) {
			putprop(sym, CONS(CONS(make_unsigned_integer((cl_index)pLK),
					       make_unsigned_integer((cl_index)*pLK)),
					  getf(sym->symbol.plist, @'si::link-from', Cnil)),
				@'si::link-from');
			*pLK = fun->cfun.entry;
		}
		return va_APPLY(narg, fun->cfun.entry, args);
#ifdef CLOS
#ifndef va_copy
#define va_copy(x) (x)
#endif
	case t_gfun: {
		va_list aux = va_copy(args);
		fun = va_compute_method(narg, fun, aux);
		pLK = NULL;
		goto AGAIN;
	}
#endif /* CLOS */
	case t_cclosure:
		return va_APPLY_closure(narg, fun->cclosure.entry,
					fun->cclosure.env, args);
	case t_bytecodes:
		return va_lambda_apply(narg, fun, args);
	default:
		FEinvalid_function(fun);
	}
}

@(defun si::unlink_symbol (s)
	cl_object pl;
@
	if (!SYMBOLP(s))
		FEtype_error_symbol(s);
	pl = getf(s->symbol.plist, @'si::link-from', Cnil);
	if (!endp(pl)) {
		for (; !endp(pl); pl = CDR(pl))
			*(void **)(fixnnint(CAAR(pl))) = (void *)fixnnint(CDAR(pl));
		remf(&s->symbol.plist, @'si::link-from');
	}
	@(return)
@)

@(defun funcall (function &rest funargs)
	cl_object fun = function, x;
@
      AGAIN:
	switch (type_of(fun)) {
	case t_cfun:
		return va_APPLY(narg-1, fun->cfun.entry, funargs);
	case t_cclosure:
		return va_APPLY_closure(narg-1, fun->cclosure.entry,
					fun->cclosure.env, funargs);
#ifdef CLOS
	case t_gfun: {
		va_list aux = va_copy(funargs);
		fun = va_compute_method(narg-1, fun, aux);
		goto AGAIN;
	}
#endif
	case t_symbol:
		fun = SYM_FUN(fun);
		if (fun == OBJNULL)
			FEundefined_function(function);
		goto AGAIN;
	case t_bytecodes:
		return va_lambda_apply(narg-1, fun, funargs);
	}
	FEinvalid_function(fun);
@)

@(defun eval (form)
	cl_object output;
@
	output = eval(form, NULL, Cnil);
	returnn(output);
@)

@(defun si::eval-with-env (form env)
	cl_object output;
@
	output = eval(form, NULL, env);
	returnn(output);
@)

cl_object
cl_safe_eval(cl_object form, cl_object *new_bytecodes, cl_object env, cl_object err_value)
{
	cl_object output;

	if (frs_push(FRS_CATCHALL, Cnil)) {
		output = err_value;
	} else {
		bds_bind(@'si::*ignore-errors*', Ct);
		output = eval(form, new_bytecodes, env);
		bds_unwind1;
	}
	frs_pop();
	return output;
}

@(defun si::safe-eval (form &optional (err_value @'error') env)
	cl_object output;
@
	returnn(cl_safe_eval(form, NULL, env, err_value));
@)

@(defun constantp (arg)
	cl_object flag;
@
	switch (type_of(arg)) {
	case t_cons:
		flag = (CAR(arg) == @'quote') ? Ct : Cnil;
		break;
	case t_symbol:
		flag = (arg->symbol.stype == stp_constant) ? Ct : Cnil;
		break;
	default:
		flag = Ct;
	}
	@(return flag)
@)

void
init_eval(void)
{
	SYM_VAL(@'si::*ignore-errors*') = Cnil;
	SYM_VAL(@'call-arguments-limit') = MAKE_FIXNUM(64);
}
