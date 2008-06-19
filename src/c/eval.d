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

cl_object *
_ecl_va_sp(cl_narg narg)
{
	return cl_env.stack_top - narg;
}

static cl_object
build_funcall_frame(cl_object f, cl_va_list args)
{
	cl_index n = args[0].narg;
	cl_object *p = args[0].sp;
	f->frame.stack = 0;
	if (!p) {
#ifdef ECL_USE_VARARG_AS_POINTER
		p = (cl_object*)(args[0].args);
#else
		cl_index i;
		p = cl_env.values;
		for (i = 0; i < n; i++) {
			p[i] = va_arg(args[0].args, cl_object);
		}
		f->frame.stack = (void*)0x1;
#endif
	}
	f->frame.bottom = p;
	f->frame.top = p + n;
	f->frame.t = t_frame;
	return f;
}

/* Calling conventions:
   Compiled C code calls lisp function supplying #args, and args.
   Linking function performs check_args, gets jmp_buf with _setjmp, then
    if cfun then stores C code address into function link location
	    and transfers to jmp_buf at cf_self
    if cclosure then replaces #args with cc_env and calls cc_self
    otherwise, it emulates funcall.
 */

cl_object
ecl_apply_from_stack_frame(cl_object frame, cl_object x)
{
	cl_object *sp = frame->frame.bottom;
	cl_index narg = frame->frame.top - sp;
	cl_object fun = x;
      AGAIN:
	if (fun == OBJNULL || fun == Cnil)
		FEundefined_function(x);
	switch (type_of(fun)) {
	case t_cfunfixed:
		if (narg != (cl_index)fun->cfun.narg)
			FEwrong_num_arguments(fun);
		return APPLY_fixed(narg, (cl_objectfn_fixed)fun->cfun.entry, sp);
	case t_cfun:
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
		return ecl_interpret(frame, Cnil, fun, 0);
	case t_bclosure:
		return ecl_interpret(frame, fun->bclosure.lex, fun->bclosure.code, 0);
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
 AGAIN:
	if (fun == OBJNULL)
		goto ERROR;
	switch (type_of(fun)) {
	case t_cfunfixed:
		if (narg != fun->cfun.narg)
			FEwrong_num_arguments(fun);
		frame = build_funcall_frame((cl_object)&frame_aux, args);
		out = APPLY_fixed(narg, (cl_objectfn_fixed)fun->cfun.entry,
				  frame->frame.bottom);
		break;
	case t_cfun:
		if (pLK) {
			si_put_sysprop(sym, @'si::link-from',
				       CONS(CONS(ecl_make_unsigned_integer((cl_index)pLK),
						 ecl_make_unsigned_integer((cl_index)*pLK)),
					    si_get_sysprop(sym, @'si::link-from')));
			*pLK = fun->cfun.entry;
			cblock->cblock.links =
				CONS(sym, cblock->cblock.links);
		}
		frame = build_funcall_frame((cl_object)&frame_aux, args);
		out = APPLY(narg, fun->cfun.entry, frame->frame.bottom);
		break;
#ifdef CLOS
	case t_instance:
		switch (fun->instance.isgf) {
		case ECL_STANDARD_DISPATCH:
			frame = build_funcall_frame((cl_object)&frame_aux, args);
			out = _ecl_standard_dispatch(frame, fun);
			break;
		case ECL_USER_DISPATCH:
			fun = fun->instance.slots[fun->instance.length - 1];
			goto AGAIN;
		default:
			FEinvalid_function(fun);
		}
		break;
#endif /* CLOS */
	case t_cclosure:
		frame = build_funcall_frame((cl_object)&frame_aux, args);
		out = APPLY_closure(narg, fun->cclosure.entry,
				    fun->cclosure.env, frame->frame.bottom);
		break;
	case t_bytecodes:
		frame = build_funcall_frame((cl_object)&frame_aux, args);
		out = ecl_interpret(frame, Cnil, fun, 0);
		break;
	case t_bclosure:
		frame = build_funcall_frame((cl_object)&frame_aux, args);
		out = ecl_interpret(frame, fun->bclosure.lex, fun->bclosure.code, 0);
		break;
	default:
	ERROR:
		FEinvalid_function(fun);
	}
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
@
	return ecl_apply_from_stack_frame(build_funcall_frame((cl_object)&frame_aux, funargs), function);
@)

@(defun apply (fun lastarg &rest args)
@
	if (narg == 2 && type_of(lastarg) == t_frame) {
		return ecl_apply_from_stack_frame(lastarg, fun);
	} else {
		cl_object out;
		cl_index i;
		struct ecl_stack_frame frame_aux;
		const cl_object frame = ecl_stack_frame_open((cl_object)&frame_aux,
							     narg -= 2);
		for (i = 0; i < narg; i++) {
			ecl_stack_frame_elt_set(frame, i, lastarg);
			lastarg = cl_va_arg(args);
		}
		if (type_of(lastarg) == t_frame) {
			/* This could be replaced with a memcpy() */
			cl_object *p = lastarg->frame.bottom;
			while (p != lastarg->frame.top) {
				ecl_stack_frame_push(frame, *(p++));
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
