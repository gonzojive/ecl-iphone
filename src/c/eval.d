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

cl_object Sapply;
cl_object Sfuncall;

/******************************* ------- ******************************/

static struct nil3 { cl_object nil3_self[3]; } three_nils;

#define SYMBOL_FUNCTION(sym)    (SYM_FUN(sym) == OBJNULL ? \
				 (FEundefined_function(sym),Cnil) : SYM_FUN(sym))

#ifdef THREADS
#define eval1            clwp->lwp_eval1
#else
static int eval1 = 0;          /*  = 1 during one-shot bypass of evalhook/applyhook  */
#endif THREADS

cl_object Vevalhook;
cl_object Vapplyhook;

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
	cl_object x = fun;

      AGAIN:
	if (fun == OBJNULL)
		FEundefined_function(x);

	switch (type_of(fun)) {
	case t_cfun:
		return APPLY(narg, fun->cfun.entry, args);

	case t_cclosure:
		return APPLY_closure(narg, fun->cclosure.entry, fun->cclosure.env, args);
		
#ifdef CLOS
	case t_gfun:
		return gcall(narg, fun, args);
#endif

	case t_symbol:
		fun = SYM_FUN(fun);
		goto AGAIN;

	case t_bytecodes:
		{
		  cl_object mv_values[narg]; /* __GNUC__ */
		  /* move args out of VALUES, or macroexpand of fun's body
		     will clobber them */
		  memcpy(mv_values, args, narg * sizeof(cl_object));
		  return lambda_apply(narg, fun, mv_values);
		}
		break;
	default:
	}
	FEinvalid_function(fun);
}

cl_return
funcall(int narg, ...)
{
	cl_object fun, x;
	va_list funargs;
	va_start(funargs, narg);
	fun = va_arg(funargs, cl_object);

      AGAIN:
	if (fun == OBJNULL) {
		va_start(funargs, narg);
		FEundefined_function(va_arg(funargs, cl_object));
	      }
	switch (type_of(fun)) {
	case t_cfun:
		return APPLY(narg-1, fun->cfun.entry, funargs);

	case t_cclosure:
		return APPLY_closure(narg-1, fun->cclosure.entry, fun->cclosure.env, funargs);

#ifdef CLOS
	case t_gfun:
		return gcall(narg-1, fun, funargs);
#endif

	case t_symbol:
		fun = SYM_FUN(fun);
		goto AGAIN;

	case t_bytecodes:
		return lambda_apply(narg-1, fun, (cl_object *)funargs);

	default:
	}
	FEinvalid_function(fun);
}

/*----------------------------------------------------------------------*
 *	Linking mechanism						*
 *----------------------------------------------------------------------*/

static cl_object siSlink_to;
static cl_object siSlink_from;

cl_object
#ifdef CLOS
link_call(cl_object sym, cl_object (**pLK)(), cl_object *gfun, cl_object *args)
#else
link_call(cl_object sym, cl_object (**pLK)(), cl_object *args)
#endif CLOS
{       int narg = (int)args[0];
	cl_object fun = symbol_function(sym);

	if (fun == OBJNULL) FEerror("Undefined function.", 0);

	switch (type_of(fun)) {
	case t_cfun:
	  putprop(sym, CONS(CONS(MAKE_FIXNUM((int)pLK),
				 MAKE_FIXNUM((int)*pLK)),
			    getf(sym->symbol.plist, siSlink_from, Cnil)),
		  siSlink_from);
	  *pLK = fun->cfun.entry;
	  return APPLY(narg, fun->cfun.entry, &args[1]);
#ifdef CLOS
	case t_gfun:
	  putprop(sym, CONS(CONS(MAKE_FIXNUM((int)gfun),
				 MAKE_FIXNUM((int)OBJNULL)),
			    getf(sym->symbol.plist, siSlink_from, Cnil)),
		  siSlink_from);
	  *gfun = fun;
	  return gcall(narg, fun, &args[1]);
#endif CLOS
	case t_cclosure:
		args[0] = (cl_object)fun->cclosure.env;
		return APPLY(narg+1, fun->cclosure.entry, args);

	case t_bytecodes:
		return lambda_apply(narg, fun, &args[1]);

	default:
		FEinvalid_function(fun);
	}
}

@(defun si::unlink_symbol (s)
	cl_object pl;
@
	if (!SYMBOLP(s))
		FEtype_error_symbol(s);
	pl = getf(s->symbol.plist, siSlink_from, Cnil);
	if (!endp(pl)) {
		for (; !endp(pl); pl = CDR(pl))
			*(int *)(fix(CAAR(pl))) = fix(CDAR(pl));
		remf(&s->symbol.plist, siSlink_from);
	}
	@(return)
@)

@(defun funcall (fun &rest args)
@
	return(apply(narg-1, fun, (cl_object *)args));
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
		VALUES(i++) = CAR(lastarg);
	} end_loop_for_in;
 	{
 	cl_object savargs[i];
 	memcpy(savargs, &VALUES(0), i*sizeof(cl_object));
 	return apply(i, fun, savargs);
 	}
@)

@(defun eval (form)
	cl_object output, lex_old = lex_env;
@
	lex_new();
	output = eval(form, NULL);
	lex_env = lex_old;
	returnn(output);
@)

@(defun evalhook (form evalhookfn applyhookfn &optional (env Cnil))
	cl_object output, lex_old = lex_env;
	bds_ptr old_bds_top = bds_top;
@
	lex_env = env;
	lex_copy();
	bds_bind(Vevalhook, evalhookfn);
	bds_bind(Vapplyhook, applyhookfn);
	eval1 = 1;
	output = eval(form, NULL);
	bds_unwind(old_bds_top);
	lex_env = lex_old;
	returnn(output);
@)

@(defun applyhook (fun args evalhookfn applyhookfn)
	bds_ptr old_bds_top = bds_top;
@
	bds_bind(Vevalhook, evalhookfn);
	bds_bind(Vapplyhook, applyhookfn);
	VALUES(0) = Lapply(2, fun, args);
	bds_unwind(old_bds_top);
	returnn(VALUES(0));
@)

@(defun constantp (arg)
	cl_object flag;
@
	switch (type_of(arg)) {
	case t_cons:
		flag = (CAR(arg) == Squote) ? Ct : Cnil;
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

	SYM_VAL(Vevalhook) = Cnil;
	SYM_VAL(Vapplyhook) = Cnil;

	eval1 = 0;

	three_nils.nil3_self[0] = Cnil;
	three_nils.nil3_self[1] = Cnil;
	three_nils.nil3_self[2] = Cnil;

	siSlink_from = make_si_ordinary("LINK-FROM");
	register_root(&siSlink_from);
	siSlink_to = make_si_ordinary("LINK-TO");
	register_root(&siSlink_to);
}
