/* -*- mode: c; c-basic-offset: 8 -*- */
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


#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>

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

void
cl_va_copy(cl_va_list dest, cl_va_list orig)
{
	dest[0].narg = orig[0].narg;
	dest[0].sp = orig[0].sp;
	va_copy(dest[0].args, orig[0].args);
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

cl_object
ecl_apply_from_stack_frame(cl_object frame, cl_object x)
{
	cl_index narg = frame->frame.narg;
	cl_object *sp = frame->frame.sp + cl_env.stack;
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
					   sp);
		}
		return APPLY(narg, fun->cfun.entry, sp);
	case t_cclosure:
		return APPLY_closure(narg, fun->cclosure.entry,
				     fun->cclosure.env, sp);
#ifdef CLOS
	case t_instance:
		switch (fun->instance.isgf) {
		case ECL_STANDARD_DISPATCH:
			return _ecl_standard_dispatch(frame, fun);
		case ECL_USER_DISPATCH:
			fun = fun->instance.slots[fun->instance.length - 1];
		default:
			FEinvalid_function(fun);
		}
		goto AGAIN;
#endif
	case t_symbol:
		if (fun->symbol.stype & stp_macro)
			FEundefined_function(x);
		fun = SYM_FUN(fun);
		goto AGAIN;
	case t_bytecodes:
		return ecl_apply_lambda(frame, fun);
	default:
	ERROR:
		FEinvalid_function(x);
	}
}

/*----------------------------------------------------------------------*
 *	Linking mechanism						*
 *----------------------------------------------------------------------*/

cl_object
_ecl_link_call(cl_object sym, cl_objectfn *pLK, cl_object cblock, int narg, cl_va_list args)
{
	cl_object out, fun = ecl_fdefinition(sym);
	struct ecl_stack_frame frame_aux;
	cl_object frame;

	if (fun == OBJNULL)
		FEerror("Undefined function.", 0);
	frame = (cl_object)&frame_aux;
	frame->frame.t = t_frame;
	frame->frame.narg = narg;
	if (args[0].sp)
		frame->frame.sp = args[0].sp;
	else
		frame->frame.sp = cl_stack_push_va_list(args);
 AGAIN:
	if (fun == OBJNULL)
		goto ERROR;
	switch (type_of(fun)) {
	case t_cfun:
		if (fun->cfun.narg >= 0) {
			if (narg != fun->cfun.narg)
				FEwrong_num_arguments(fun);
			out = APPLY_fixed(narg, (cl_objectfn_fixed)fun->cfun.entry,
					  cl_env.stack + frame->frame.sp);
		} else {
			if (pLK) {
				si_put_sysprop(sym, @'si::link-from',
					       CONS(CONS(ecl_make_unsigned_integer((cl_index)pLK),
							 ecl_make_unsigned_integer((cl_index)*pLK)),
						    si_get_sysprop(sym, @'si::link-from')));
				*pLK = fun->cfun.entry;
				cblock->cblock.links =
				    CONS(sym, cblock->cblock.links);
			}
			out = APPLY(narg, fun->cfun.entry, cl_env.stack + frame->frame.sp);
		}
		break;
#ifdef CLOS
	case t_instance:
		switch (fun->instance.isgf) {
		case ECL_STANDARD_DISPATCH:
			return _ecl_standard_dispatch(frame, fun);
		case ECL_USER_DISPATCH:
			fun = fun->instance.slots[fun->instance.length - 1];
		default:
			FEinvalid_function(fun);
		}
		goto AGAIN;
#endif /* CLOS */
	case t_cclosure:
		out = APPLY_closure(narg, fun->cclosure.entry,
				    fun->cclosure.env, cl_env.stack + frame->frame.sp);
		break;
	case t_bytecodes:
		out = ecl_apply_lambda(frame, fun);
		break;
	default:
	ERROR:
		FEinvalid_function(fun);
	}
	if (!args[0].sp)
		ecl_stack_frame_close(frame);
	return out;
}

cl_object
si_unlink_symbol(cl_object s)
{
	cl_object pl;

	if (!SYMBOLP(s))
		FEtype_error_symbol(s);
	pl = si_get_sysprop(s, @'si::link-from');
	if (!ecl_endp(pl)) {
		for (; !ecl_endp(pl); pl = CDR(pl)) {
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
	struct ecl_stack_frame frame_aux;
	cl_object frame;
	cl_object out;
@
	frame = (cl_object)&frame_aux;
	frame->frame.t = t_frame;
	frame->frame.narg = narg-1;
	if (funargs[0].sp)
		frame->frame.sp = funargs[0].sp;
	else
		frame->frame.sp = cl_stack_push_va_list(funargs);
        out = ecl_apply_from_stack_frame(frame, function);
	if (!funargs[0].sp) {
		/* Closing a frame implies popping out all arguments.
		 * If the arguments had been previously pushed, we must
		 * avoid this and leave that task to the caller */
		ecl_stack_frame_close(frame);
	}
	return out;
@)

@(defun apply (fun lastarg &rest args)
@
	if (narg == 2 && type_of(lastarg) == t_frame) {
		return ecl_apply_from_stack_frame(lastarg, fun);
	} else {
		cl_object out;
		cl_index i;
		struct ecl_stack_frame frame_aux;
		const cl_object frame = (cl_object)&frame_aux;
		frame->frame.t = t_frame;
		frame->frame.narg = frame->frame.sp = 0;
		narg -= 2;
		for (i = 0; narg; i++,narg--) {
			ecl_stack_frame_push(frame, lastarg);
			lastarg = cl_va_arg(args);
		}
		if (type_of(lastarg) == t_frame) {
			ecl_stack_frame_reserve(frame, lastarg->frame.narg);
			/* This could be replaced with a memcpy() */
			for (i = 0; i < lastarg->frame.narg; i++) {
				cl_object o = ecl_stack_frame_elt(lastarg, i);
				ecl_stack_frame_elt_set(frame, i, o);
			}
		} else loop_for_in (lastarg) {
			if (i >= CALL_ARGUMENTS_LIMIT) {
				ecl_stack_frame_close(frame);
				FEprogram_error("CALL-ARGUMENTS-LIMIT exceeded",0);
			}
			ecl_stack_frame_push(frame, CAR(lastarg));
			i++;
		} end_loop_for_in;
		out = ecl_apply_from_stack_frame(frame, fun);
		ecl_stack_frame_close(frame);
		return out;
	}
@)

cl_object
cl_eval(cl_object form)
{
	return si_eval_with_env(1, form);
}

@(defun constantp (arg &optional env)
	cl_object flag;
@
	switch (type_of(arg)) {
	case t_list:
		if (Null(arg)) {
			flag = Ct;
		} else if (CAR(arg) == @'quote') {
			flag = Ct;
		} else {
			flag = Cnil;
		}
		break;
	case t_symbol:
		flag = (arg->symbol.stype & stp_constant) ? Ct : Cnil;
		break;
	default:
		flag = Ct;
	}
	@(return flag)
@)
