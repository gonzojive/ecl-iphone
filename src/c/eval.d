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
	if (args[0].narg < narg_before)
		FEwrong_num_arguments_anonym();
	if (args[0].narg > C_ARGUMENTS_LIMIT) {
		args[0].narg -= narg_before;
		args[0].sp = cl_stack_index() - args[0].narg;
	} else {
		args[0].narg -= narg_before;
		args[0].sp = 0;
	}
}

cl_object
cl_va_arg(cl_va_list args)
{
	if (args[0].narg <= 0)
		FEwrong_num_arguments_anonym();
	args[0].narg--;
	if (args[0].sp)
		return cl_env.stack[args[0].sp++];
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
cl_apply_from_stack(cl_index narg, cl_object x)
{
	cl_object fun = x;
      AGAIN:
	if (fun == OBJNULL || fun == Cnil)
		FEundefined_function(x);
	switch (type_of(fun)) {
	case t_cfun:
		if (fun->cfun.narg >= 0) {
			if (narg != (cl_index)fun->cfun.narg)
				FEwrong_num_arguments(fun);
			return APPLY_fixed(narg, (cl_objectfn_fixed)fun->cfun.entry,
					   cl_env.stack_top - narg);
		}
		return APPLY(narg, fun->cfun.entry, cl_env.stack_top - narg);
	case t_cclosure:
		return APPLY_closure(narg, fun->cclosure.entry,
				     fun->cclosure.env, cl_env.stack_top - narg);
#ifdef CLOS
	case t_instance:
		if (!fun->instance.isgf)
			goto ERROR;
		fun = compute_method(narg, fun, cl_env.stack_top - narg);
		goto AGAIN;
#endif
	case t_symbol:
		if (fun->symbol.mflag)
			FEundefined_function(x);
		fun = SYM_FUN(fun);
		goto AGAIN;
	case t_bytecodes:
		return lambda_apply(narg, fun);
	default:
	ERROR:
		FEinvalid_function(x);
	}
}

/*----------------------------------------------------------------------*
 *	Linking mechanism						*
 *----------------------------------------------------------------------*/

cl_object
link_call(cl_object sym, cl_objectfn *pLK, cl_object cblock, int narg, cl_va_list args)
{
	cl_index sp;
	cl_object out, fun = ecl_fdefinition(sym);

	if (fun == OBJNULL)
		FEerror("Undefined function.", 0);
	if (args[0].sp)
		sp = args[0].sp;
	else
		sp = cl_stack_push_va_list(args);
 AGAIN:
	if (fun == OBJNULL)
		goto ERROR;
	switch (type_of(fun)) {
	case t_cfun:
		if (fun->cfun.narg >= 0) {
			if (narg != fun->cfun.narg)
				FEwrong_num_arguments(fun);
			out = APPLY_fixed(narg, (cl_objectfn_fixed)fun->cfun.entry,
					  cl_env.stack_top - narg);
		} else {
			if (pLK) {
				si_put_sysprop(sym, @'si::link-from',
					       CONS(CONS(make_unsigned_integer((cl_index)pLK),
							 make_unsigned_integer((cl_index)*pLK)),
						    si_get_sysprop(sym, @'si::link-from')));
				*pLK = fun->cfun.entry;
				cblock->cblock.links =
				    CONS(sym, cblock->cblock.links);
			}
			out = APPLY(narg, fun->cfun.entry, cl_env.stack + sp);
		}
		break;
#ifdef CLOS
	case t_instance: {
		if (!fun->instance.isgf)
			goto ERROR;
		fun = compute_method(narg, fun, cl_env.stack + sp);
		pLK = NULL;
		goto AGAIN;
	}
#endif /* CLOS */
	case t_cclosure:
		out = APPLY_closure(narg, fun->cclosure.entry,
				    fun->cclosure.env, cl_env.stack + sp);
		break;
	case t_bytecodes:
		out = lambda_apply(narg, fun);
		break;
	default:
	ERROR:
		FEinvalid_function(fun);
	}
	if (!args[0].sp)
		cl_stack_set_index(sp);
	return out;
}

cl_object
si_unlink_symbol(cl_object s)
{
	cl_object pl;

	if (!SYMBOLP(s))
		FEtype_error_symbol(s);
	pl = si_get_sysprop(s, @'si::link-from');
	if (!endp(pl)) {
		for (; !endp(pl); pl = CDR(pl)) {
			cl_object record = CAR(pl);
			void **location = (void **)fixnnint(CAR(record));
			void *original = (void *)fixnnint(CDR(record));
			*location = original;
		}
		si_rem_sysprop(s, @'si::link-from');
	}
	@(return)
}

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
	if (fun == OBJNULL || fun == Cnil)
		FEundefined_function(function);
	switch (type_of(fun)) {
	case t_cfun:
		if (fun->cfun.narg >= 0) {
			if (narg != fun->cfun.narg)
				FEwrong_num_arguments(fun);
			out = APPLY_fixed(narg, (cl_objectfn_fixed)fun->cfun.entry,
					  cl_env.stack_top - narg);
		} else {
			out = APPLY(narg, fun->cfun.entry, cl_env.stack + sp);
		}
		break;
	case t_cclosure:
		out = APPLY_closure(narg, fun->cclosure.entry,
				    fun->cclosure.env, cl_env.stack + sp);
		break;
#ifdef CLOS
	case t_instance:
		if (!fun->instance.isgf)
			goto ERROR;
		fun = compute_method(narg, fun, cl_env.stack + sp);
		goto AGAIN;
#endif
	case t_symbol:
		if (fun->symbol.mflag)
			FEundefined_function(fun);
		fun = SYM_FUN(fun);
		goto AGAIN;
	case t_bytecodes:
		out = lambda_apply(narg, fun);
		break;
	default:
	ERROR:
		FEinvalid_function(fun);
	}
	if (!funargs[0].sp)
		cl_stack_set_index(sp);
	return out;
@)

cl_object
cl_eval(cl_object form)
{
	return si_eval_with_env(1, form);
}

cl_object
cl_safe_eval(cl_object form, cl_object env, cl_object err_value)
{
	cl_object output;

	CL_CATCH_ALL_BEGIN
		bds_bind(@'si::*ignore-errors*', Ct);
		output = si_eval_with_env(2, form, env);
		bds_unwind1();
	CL_CATCH_ALL_IF_CAUGHT
		output = err_value;
	CL_CATCH_ALL_END;
	return output;
}

@(defun si::safe-eval (form &optional (err_value @'error') env)
@
	return cl_safe_eval(form, env, err_value);
@)

@(defun constantp (arg &optional env)
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
