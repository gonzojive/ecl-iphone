/*
    eval.c -- Eval.
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
#include "ecls-inl.h"

/******************************* EXPORTS ******************************/

cl_object @'apply';
cl_object @'funcall';

/******************************* ------- ******************************/

static struct nil3 { cl_object nil3_self[3]; } three_nils;

#define SYMBOL_FUNCTION(sym)    (SYM_FUN(sym) == OBJNULL ? \
				 (FEundefined_function(sym),Cnil) : SYM_FUN(sym))

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
		return gcall(narg, fun, args);
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
	default:
	}
	FEinvalid_function(fun);
}

/*----------------------------------------------------------------------*
 *	Linking mechanism						*
 *----------------------------------------------------------------------*/

static cl_object @'si::link-to';
static cl_object @'si::link-from';

cl_object
#ifdef CLOS
link_call(cl_object sym, cl_object (**pLK)(), cl_object *gfun,
	  int narg, va_list args)
#else
link_call(cl_object sym, cl_object (**pLK)(), int narg, va_list args)
#endif CLOS
{
	cl_object fun = symbol_function(sym);

	if (fun == OBJNULL) FEerror("Undefined function.", 0);

	switch (type_of(fun)) {
	case t_cfun:
		putprop(sym, CONS(CONS(make_unsigned_integer((cl_index)pLK),
				       make_unsigned_integer((cl_index)*pLK)),
				  getf(sym->symbol.plist, @'si::link-from', Cnil)),
			@'si::link-from');
		*pLK = fun->cfun.entry;
		return va_APPLY(narg, fun->cfun.entry, args);
#ifdef CLOS
	case t_gfun:
		putprop(sym, CONS(CONS(make_unsigned_integer((cl_index)gfun),
				       make_unsigned_integer((cl_index)OBJNULL)),
				  getf(sym->symbol.plist, @'si::link-from', Cnil)),
			@'si::link-from');
		*gfun = fun;
		return va_gcall(narg, fun, args);
#endif CLOS
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
	case t_gfun:
		return va_gcall(narg-1, fun, funargs);
#endif
	case t_symbol:
		fun = SYM_FUN(fun);
		if (fun == OBJNULL)
			FEundefined_function(function);
		goto AGAIN;
	case t_bytecodes:
		return va_lambda_apply(narg-1, fun, funargs);
	default:
	}
	FEinvalid_function(fun);
@)

@(defun apply (fun lastarg &rest args)
	int i;
@
	narg -= 2;
	for (i = 0; narg; narg--) {
		VALUES(i++) = lastarg;
		lastarg = va_arg(args, cl_object);
	}
	loop_for_in (lastarg) {
		if (i >= CALL_ARGUMENTS_LIMIT)
			FEprogram_error("CALL-ARGUMENTS-LIMIT exceeded",0);
		VALUES(i++) = CAR(lastarg);
	} end_loop_for_in;
	{
		cl_object savargs[i];
		memcpy(savargs, &VALUES(0), i * sizeof(cl_object));
		return apply(i, fun, savargs);
	}
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
	make_constant("CALL-ARGUMENTS-LIMIT", MAKE_FIXNUM(64));

	three_nils.nil3_self[0] = Cnil;
	three_nils.nil3_self[1] = Cnil;
	three_nils.nil3_self[2] = Cnil;

	@'si::link-from' = make_si_ordinary("LINK-FROM");
	register_root(&@'si::link-from');
	@'si::link-to' = make_si_ordinary("LINK-TO");
	register_root(&@'si::link-to');
}
