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

void
cl__va_start(cl_va_list args, int narg_before)
{
	if (args[0].narg > C_ARGUMENTS_LIMIT) {
		args[0].sp = cl_stack_index() - args[0].narg;
	} else {
		args[0].sp = 0;
	}
	if (args[0].narg < narg_before)
		FEtoo_few_arguments(args[0].narg);
	args[0].narg -= narg_before;
}

cl_object
cl_va_arg(cl_va_list args)
{
	if (args[0].narg <= 0)
		FEerror("Too few arguments", 0);
	args[0].narg--;
	if (args[0].sp)
		return cl_stack[args[0].sp++];
	return va_arg(args[0].args, cl_object);
}


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
cl_apply_from_stack(cl_index narg, cl_object fun)
{
      AGAIN:
	switch (type_of(fun)) {
	case t_cfun:
		return APPLY(narg, fun->cfun.entry, cl_stack_top - narg);
	case t_cclosure:
		return APPLY_closure(narg, fun->cclosure.entry,
				     fun->cclosure.env, cl_stack_top - narg);
#ifdef CLOS
	case t_gfun:
		fun = compute_method(narg, fun, cl_stack_top - narg);
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
		return lambda_apply(narg, fun);
	}
	FEinvalid_function(fun);
}

/*----------------------------------------------------------------------*
 *	Linking mechanism						*
 *----------------------------------------------------------------------*/

cl_object
link_call(cl_object sym, cl_objectfn *pLK, int narg, cl_va_list args)
{
	cl_index sp;
	cl_object out, fun = symbol_function(sym);

	if (fun == OBJNULL)
		FEerror("Undefined function.", 0);
	if (args[0].sp)
		sp = args[0].sp;
	else
		sp = cl_stack_push_va_list(args);
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
		out = APPLY(narg, fun->cfun.entry, cl_stack + sp);
		break;
#ifdef CLOS
	case t_gfun: {
		fun = compute_method(narg, fun, cl_stack + sp);
		pLK = NULL;
		goto AGAIN;
	}
#endif /* CLOS */
	case t_cclosure:
		out = APPLY_closure(narg, fun->cclosure.entry,
				    fun->cclosure.env, cl_stack + sp);
		break;
	case t_bytecodes:
		out = lambda_apply(narg, fun);
		break;
	default:
		FEinvalid_function(fun);
	}
	if (!args[0].sp)
		cl_stack_set_index(sp);
	return out;
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
	cl_index sp;
	cl_object fun = function, out;
@
	narg--;
	if (funargs[0].sp)
		sp = funargs[0].sp;
	else
		sp = cl_stack_push_va_list(funargs);
      AGAIN:
	switch (type_of(fun)) {
	case t_cfun:
		out = APPLY(narg, fun->cfun.entry, cl_stack + sp);
		break;
	case t_cclosure:
		out = APPLY_closure(narg, fun->cclosure.entry,
				    fun->cclosure.env, cl_stack + sp);
		break;
#ifdef CLOS
	case t_gfun:
		fun = compute_method(narg, fun, cl_stack + sp);
		goto AGAIN;
#endif
	case t_symbol:
		fun = SYM_FUN(fun);
		if (fun == OBJNULL)
			FEundefined_function(function);
		goto AGAIN;
	case t_bytecodes:
		out = lambda_apply(narg, fun);
		break;
	default:
		FEinvalid_function(fun);
	}
	if (!funargs[0].sp)
		cl_stack_set_index(sp);
	return out;
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
	SYM_VAL(@'call-arguments-limit') = MAKE_FIXNUM(CALL_ARGUMENTS_LIMIT);
}
