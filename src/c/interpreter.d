/*
    interpreter.c -- Bytecode interpreter.
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecls.h"
#include "ecls-inl.h"
#include "bytecodes.h"

#define next_code(v) *(v++)
#undef frs_pop
#define frs_pop() { cl_stack_top = cl_stack + frs_top->frs_sp; frs_top--; }

/* -------------------- INTERPRETER STACK -------------------- */

cl_index cl_stack_size = 0;
cl_object *cl_stack = NULL;
cl_object *cl_stack_top = NULL;
cl_object *cl_stack_limit = NULL;

static void
cl_stack_set_size(cl_index new_size)
{
	cl_index top = cl_stack_top - cl_stack;
	cl_object *new_stack;

	printf("*+*+*+\n");

	if (top > new_size)
		FEerror("Internal error: cannot shrink stack that much.",0);

	start_critical_section();

	new_stack = alloc(new_size * sizeof(cl_object));
	memcpy(new_stack, cl_stack, cl_stack_size * sizeof(cl_object));
	cl_stack_size = new_size;
	cl_stack = new_stack;
	cl_stack_top = cl_stack + top;
	cl_stack_limit = cl_stack + (new_size - 2);

	end_critical_section();
}

void
cl_stack_grow(void)
{
	cl_stack_set_size(cl_stack_size + LISP_PAGESIZE);
}

void
cl_stack_push(cl_object x) {
	if (cl_stack_top >= cl_stack_limit)
		cl_stack_grow();
	*(cl_stack_top++) = x;
}

cl_object
cl_stack_pop() {
	if (cl_stack_top == cl_stack)
		FEerror("Internal error: stack underflow.",0);
	return *(--cl_stack_top);
}

cl_index
cl_stack_index() {
	return cl_stack_top - cl_stack;
}

void
cl_stack_set_index(cl_index index) {
	cl_object *new_top = cl_stack + index;
	if (new_top > cl_stack_top)
		FEerror("Internal error: tried to advance stack.",0);
	cl_stack_top = new_top;
}

void
cl_stack_insert(cl_index where, cl_index n) {
	if (cl_stack_top + n > cl_stack_limit) {
		cl_index delta = (n + (LISP_PAGESIZE-1))/LISP_PAGESIZE;
		cl_stack_set_size(cl_stack_size + delta * LISP_PAGESIZE);
	}
	cl_stack_top += n;
	memmove(&cl_stack[where+n], &cl_stack[where],
		(cl_stack_top - cl_stack) * sizeof(*cl_stack));
}


void
cl_stack_pop_n(cl_index index) {
	cl_object *new_top = cl_stack_top - index;
	if (new_top < cl_stack)
		FEerror("Internal error: stack underflow.",0);
	cl_stack_top = new_top;
}

/* -------------------- LAMBDA FUNCTIONS -------------------- */

static void
bind_var(register cl_object var, register cl_object val)
{
	CAR(lex_env) = CONS(var, CONS(val, CAR(lex_env)));
}

static void
bind_special(register cl_object var, register cl_object val)
{
	bds_bind(var, val);
}

static void
lambda_bind_var(cl_object var, cl_object val, cl_object specials)
{
	if (!member_eq(var, specials))
		bind_var(var, val);
	else
		bind_special(var, val);
}

static cl_object *
lambda_bind(int narg, cl_object lambda_list, cl_object *args)
{
	cl_object *data = &lambda_list->bytecodes.data[2];
	cl_object specials = lambda_list->bytecodes.data[1];
	cl_object aux;
	int i, n;
	bool other_keys = FALSE;
	bool check_remaining = TRUE;
	bool allow_other_keys_found = FALSE;

	/* 1) REQUIRED ARGUMENTS:  N var1 ... varN */
	n = fix(next_code(data));
	if (narg < n)
	  check_arg_failed(narg, n);
	for (; n; n--, narg--)
	  lambda_bind_var(next_code(data), next_code(args), specials);

	/* 2) OPTIONAL ARGUMENTS:  N var1 value1 flag1 ... varN valueN flagN */
	for (n = fix(next_code(data)); n; n--, data+=3) {
	  if (narg) {
	    lambda_bind_var(data[0], args[0], specials);
	    args++; narg--;
	    if (!Null(data[2]))
	      lambda_bind_var(data[2], Ct, specials);
	  } else {
	    cl_object defaults = data[1];
	    if (FIXNUMP(defaults)) {
	      interpret(&data[1] + fix(defaults));
	      defaults = VALUES(0);
	    }
	    lambda_bind_var(data[0], defaults, specials);
	    if (!Null(data[2]))
	      lambda_bind_var(data[2], Cnil, specials);
	  }
	}

	/* 3) REST ARGUMENT: {rest-var | NIL} */
	if (!Null(data[0])) {
	  cl_object rest = Cnil;
	  check_remaining = FALSE;
	  for (i=narg; i; )
	    rest = CONS(args[--i], rest);
	  lambda_bind_var(data[0], rest, specials);
	}
	data++;

	/* 4) ALLOW-OTHER-KEYS: { T | NIL } */
	other_keys = !Null(next_code(data));

	/* 5) KEYWORDS: N key1 var1 value1 flag1 ... keyN varN valueN flagN */
	n = fix(next_code(data));
	if (n != 0 || other_keys) {
	  cl_object *keys;
	  cl_object spp[n];
	  bool other_found = FALSE;
	  for (i=0; i<n; i++)
	    spp[i] = OBJNULL;
	  for (; narg; args+=2, narg-=2) {
	    if (!SYMBOLP(args[0]))
	      FEprogram_error("LAMBDA: Keyword expected, got ~S.", 1, args[0]);
	    keys = data;
	    for (i = 0; i < n; i++, keys += 4) {
	      if (args[0] == keys[0]) {
		if (spp[i] == OBJNULL)
		  spp[i] = args[1];
		goto FOUND;
	      }
	    }
	    if (args[0] != @':allow-other-keys')
	      other_found = TRUE;
	    else if (!allow_other_keys_found) {
	      allow_other_keys_found = TRUE;
	      other_keys = !Null(args[1]);
	    }
	  FOUND:
	  }
	  if (other_found && !other_keys)
	    FEprogram_error("LAMBDA: Unknown keys found in function ~S.",
			    1, lambda_list->bytecodes.data[0]);
	  for (i=0; i<n; i++, data+=4) {
	    if (spp[i] != OBJNULL)
	      lambda_bind_var(data[1],spp[i],specials);
	    else {
	      cl_object defaults = data[2];
	      if (FIXNUMP(defaults)) {
		      interpret(&data[2] + fix(defaults));
		      defaults = VALUES(0);
	      }
	      lambda_bind_var(data[1],defaults,specials);
	    }
	    if (!Null(data[3]))
	      lambda_bind_var(data[3],(spp[i] != OBJNULL)? Ct : Cnil,specials);
	  }
	}
	if (narg && !other_keys && check_remaining)
	  FEprogram_error("LAMBDA: Too many arguments to function ~S.", 1,
			  lambda_list->bytecodes.data[0]);

	return &data[2];
}

cl_object
lambda_apply(int narg, cl_object fun, cl_object *args)
{
	cl_object output, name, *body;
	bds_ptr old_bds_top;
	volatile bool block, closure;

	if (type_of(fun) != t_bytecodes)
		FEinvalid_function(fun);

	/* 1) Save the lexical environment and set up a new one */
	cl_stack_push(lex_env);
	if (Null(fun->bytecodes.lex))
		lex_env = CONS(Cnil, Cnil);
	else
		lex_env = CONS(CAR(fun->bytecodes.lex),CDR(fun->bytecodes.lex));
	ihs_push(fun, lex_env);
	old_bds_top = bds_top;

	/* Establish bindings */
	body = lambda_bind(narg, fun, args);

	/* If it is a named lambda, set a block for RETURN-FROM */
	block = FALSE;
	name = fun->bytecodes.data[0];
	if (Null(fun->bytecodes.data[0]))
		block = FALSE;
	else {
		block = TRUE;
		fun = new_frame_id();
		lex_block_bind(name, fun);
		if (frs_push(FRS_CATCH, fun)) {
			output = VALUES(0);
			goto END;
		}
	}

	/* Process statements */
	VALUES(0) = Cnil;
	NValues = 0;
	interpret(body);

END:    if (block) frs_pop();
	bds_unwind(old_bds_top);
	ihs_pop();
	lex_env = cl_stack_pop();
	returnn(VALUES(0));
}


#ifdef NO_ARGS_ARRAY
cl_object
va_lambda_apply(int narg, cl_object fun, va_list args)
{
	cl_object out;
	int i;
	for (i=narg; i; i--)
		cl_stack_push(cl_nextarg(args));
	out = lambda_apply(narg, fun, cl_stack_top-narg);
	cl_stack_pop_n(narg);
	return out;
}

#ifdef CLOS
cl_object
va_gcall(int narg, cl_object fun, va_list args)
{
	cl_object out;
	int i;
	for (i=narg; i; i--)
		cl_stack_push(cl_nextarg(args));
	out = gcall(narg, fun, cl_stack_top-narg);
	cl_stack_pop_n(narg);
	return out;
}
#endif
#endif

/* -------------------- AIDS TO THE INTERPRETER -------------------- */

static inline cl_fixnum
get_oparg(cl_object o) {
	return GET_OPARG(o);
}

static inline cl_object *
packed_label(cl_object *v) {
	return v + GET_OPARG(v[0]);
}

static inline cl_object *
simple_label(cl_object *v) {
	return v + fix(v[0]);
}

static cl_object
search_symbol_function(register cl_object fun) {
	cl_object output = lex_fun_sch(fun);
	if (!Null(output))
		return output;
	output = SYM_FUN(fun);
	if (output == OBJNULL || fun->symbol.mflag)
		FEundefined_function(fun);
	return output;
}

static cl_object
search_local(register cl_object s) {
	cl_object x;

	for (x = CAR(lex_env);  CONSP(x);  x = CDDR(x))
		if (CAR(x) == s) {
			return CADR(x);
		}
	FEerror("Internal error: local ~S not found.", 1, s);
}

static cl_object
setq_local(register cl_object s, register cl_object v) {
	cl_object x;
	for (x = CAR(lex_env); CONSP(x); x = CDDR(x))
		if (CAR(x) == s) {
			CADR(x) = v;
			return;
		}
	FEerror("Internal error: local ~S not found.", 1, s);
}

static cl_object
search_global(register cl_object s) {
	cl_object x = SYM_VAL(s);
	if (x == OBJNULL)
		FEunbound_variable(s);
	return x;
}
		
static cl_object
interpret_call(int narg, cl_object fun) {
	cl_object *args;
	cl_object x;

	args = cl_stack_top - narg;
 AGAIN:
	switch (type_of(fun)) {
	case t_cfun:
		ihs_push(fun->cfun.name, Cnil);
		x = APPLY(narg, fun->cfun.entry, args);
		ihs_pop();
		break;
	case t_cclosure:
		/* FIXME! Shouldn't we register this call somehow? */
		x = APPLY_closure(narg, fun->cclosure.entry, fun->cclosure.env, args);
		break;
#ifdef CLOS
	case t_gfun:
		ihs_push(fun->gfun.name, Cnil);
		x = gcall(narg, fun, args);
		ihs_pop();
		break;
#endif
	case t_bytecodes:
		x = lambda_apply(narg, fun, args);
		break;
	case t_symbol:
		fun = search_symbol_function(fun);
		goto AGAIN;
	default:
		FEinvalid_function(fun);
	}
	cl_stack_pop_n(narg);
	return x;
}

/* Similar to interpret_call(), but looks for symbol functions in the
   global environment. */

static cl_object
interpret_funcall(int narg, cl_object fun) {
	cl_object *args;
	cl_object x;

	args = cl_stack_top - narg;
 AGAIN:
	switch (type_of(fun)) {
	case t_cfun:
		ihs_push(fun->cfun.name, Cnil);
		x = APPLY(narg, fun->cfun.entry, args);
		ihs_pop();
		break;
	case t_cclosure:
		/* FIXME! Shouldn't we register this call somehow? */
		x = APPLY_closure(narg, fun->cclosure.entry, fun->cclosure.env, args);
		break;
#ifdef CLOS
	case t_gfun:
		ihs_push(fun->gfun.name, Cnil);
		x = gcall(narg, fun, args);
		ihs_pop();
		break;
#endif
	case t_bytecodes:
		x = lambda_apply(narg, fun, args);
		break;
	case t_symbol: {
		cl_object function = SYM_FUN(fun);
		if (function == OBJNULL)
			FEundefined_function(fun);
		fun = function;
		goto AGAIN;
	}
	default:
		FEinvalid_function(fun);
	}
	cl_stack_pop_n(narg);
	return x;
}

/* -------------------- THE INTERPRETER -------------------- */

static cl_object *
interpret_block(cl_object *vector) {
	cl_object * volatile exit, name;
	cl_object id = new_frame_id();

	/* 1) Save current environment */
	cl_stack_push(CDR(lex_env));

	/* 2) Set up a block with given name */
	exit = packed_label(vector - 1);
	lex_block_bind(next_code(vector), id);
	if (frs_push(FRS_CATCH,id) == 0)
		vector = interpret(vector);
	frs_pop();

	/* 3) Restore environment */
	CDR(lex_env) = cl_stack_pop();
	return exit;
}

static cl_object *
interpret_catch(cl_object *vector) {
	cl_object * volatile exit;
	exit = packed_label(vector - 1);
	if (frs_push(FRS_CATCH,VALUES(0)) == 0)
		interpret(vector);
	frs_pop();
	return exit;
}

static cl_object *
interpret_tagbody(cl_object *vector) {
	cl_index i, ntags = get_oparg(vector[-1]);
	cl_object id = new_frame_id();
	cl_object *aux, *tag_list = vector;

	/* 1) Save current environment */
	cl_stack_push(CDR(lex_env));

	/* 2) Bind tags */
	aux = vector;
	for (i=0; i<ntags; i++, aux+=2)
		lex_tag_bind(*aux, id);

	/* 3) Wait here for gotos */
	if (frs_push(FRS_CATCH, id) != 0) {
		for (aux = vector, i=0; i<ntags; i++, aux+=2)
			if (eql(aux[0], nlj_tag)) {
				aux++;
				break;
			}
		if (i >= ntags)
			FEerror("Internal error: TAGBODY id used for RETURN-FROM.",0);
		else
			aux = simple_label(aux);
	}
	vector = interpret(aux);
	frs_pop();

	/* 4) Restore environment */
	CDR(lex_env) = cl_stack_pop();
	VALUES(0) = Cnil;
	NValues = 0;
	return vector;
}

static cl_object *
interpret_unwind_protect(cl_object *vector) {
	bool unwinding;
	int nr;
	cl_object * volatile exit;

	exit = packed_label(vector-1);
	if (frs_push(FRS_PROTECT, Cnil))
		unwinding = TRUE;
	else {
		interpret(vector);
		unwinding = FALSE;
	}
	frs_pop();
	nr = NValues;
	MV_SAVE(nr);
	exit = interpret(exit);
	MV_RESTORE(nr);
	if (unwinding)
		unwind(nlj_fr, nlj_tag);
	return exit;
}

static cl_object *
interpret_do(cl_object *vector) {
	cl_object *volatile exit;
	cl_object id = new_frame_id();

	/* 1) Save all environment */
	bds_ptr old_bds_top = bds_top;
	cl_stack_push(CAR(lex_env));
	cl_stack_push(CDR(lex_env));

	/* 2) Set up new block name */
	lex_block_bind(Cnil, id);
	exit = packed_label(vector-1);
	if (frs_push(FRS_CATCH,id) == 0)
		interpret(vector);
	frs_pop();

	/* 3) Restore all environment */
	bds_unwind(old_bds_top);
	CDR(lex_env) = cl_stack_pop();
	CAR(lex_env) = cl_stack_pop();
	return exit;
}

static cl_object *
interpret_dolist(cl_object *vector) {
	cl_object *output, *volatile exit;
	cl_object list, var;
	cl_object id = new_frame_id();

	/* 1) Save all environment */
	bds_ptr old_bds_top = bds_top;
	cl_stack_push(CAR(lex_env));
	cl_stack_push(CDR(lex_env));

	/* 2) Set up a nil block */
	lex_block_bind(Cnil, id);
	if (frs_push(FRS_CATCH,id) == 0) {
		list = VALUES(0);
		exit = packed_label(vector - 1);

		/* 3) Build list & bind variable*/
		vector = interpret(vector);
		output = packed_label(vector-1);

		/* 4) Repeat until list is exahusted */
		while (!endp(list)) {
			NValues = 1;
			VALUES(0) = CAR(list);
			interpret(vector);
			list = CDR(list);
		}
		VALUES(0) = Cnil;
		NValues = 1;
		interpret(output);
	}
	frs_pop();

	/* 5) Restore environment */
	CDR(lex_env) = cl_stack_pop();
	CAR(lex_env) = cl_stack_pop();
	bds_unwind(old_bds_top);
	return exit;
}

static cl_object *
interpret_dotimes(cl_object *vector) {
	cl_object *output, *volatile exit;
	cl_fixnum length, i;
	cl_object var;
	cl_object id = new_frame_id();

	/* 1) Save all environment */
	bds_ptr old_bds_top = bds_top;
	cl_stack_push(CAR(lex_env));
	cl_stack_push(CDR(lex_env));

	/* 2) Set up a nil block */
	lex_block_bind(Cnil, id);
	if (frs_push(FRS_CATCH,id) == 0) {
		/* 3) Retrieve number and bind variables */
		length = fix(VALUES(0));
		exit = packed_label(vector - 1);
		vector = interpret(vector);
		output = packed_label(vector-1);

		/* 4) Loop while needed */
		for (i = 0; i < length;) {
			interpret(vector);
			NValues = 1;
			VALUES(0) = MAKE_FIXNUM(++i);
		}
		interpret(output);
	}
	frs_pop();

	/* 5) Restore environment */
	CDR(lex_env) = cl_stack_pop();
	CAR(lex_env) = cl_stack_pop();
	bds_unwind(old_bds_top);
	return exit;
}

static cl_object
close_around(cl_object fun, cl_object lex) {
	cl_object v = alloc_object(t_bytecodes);
	v->bytecodes.size = fun->bytecodes.size;
	v->bytecodes.data = fun->bytecodes.data;
	if (!Null(CAR(lex)) || !Null(CDR(lex)))
		v->bytecodes.lex = CONS(CAR(lex),CDR(lex));
	else
		v->bytecodes.lex = Cnil;
	return v;
}

static cl_object *
interpret_flet(cl_object *vector) {
	cl_index nfun = get_oparg(vector[-1]);

	/* 1) Copy the environment so that functions get it */
	cl_object lex = CONS(CAR(lex_env), CDR(lex_env));

	/* 2) Save current environment */
	cl_stack_push(CDR(lex_env));

	/* 3) Add new closures to environment */
	while (nfun--) {
		cl_object fun = next_code(vector);
		cl_object f = close_around(fun,lex);
		lex_fun_bind(f->bytecodes.data[0], f);
	}
	vector = interpret(vector);

	/* 4) Restore environment */
	CDR(lex_env) = cl_stack_pop();
	return vector;
}

static cl_object *
interpret_labels(cl_object *vector) {
	cl_index i, nfun = get_oparg(vector[-1]);
	cl_object l, lex;

	/* 1) Save current environment */
	cl_stack_push(CDR(lex_env));

	/* 2) Build up a new environment with all functions */
	for (i=0; i<nfun; i++) {
		cl_object f = next_code(vector);
		lex_fun_bind(f->bytecodes.data[0], f);
	}
	lex = CONS(CAR(lex_env), CDR(lex_env));

	/* 3) Update the closures so that all functions can call each other */
	for (i=0, l=CDR(lex_env); i<nfun; i++) {
		cl_object f = CADDAR(l);
		CADDAR(l) = close_around(f, lex);
		l = CDR(l);
	}
	vector = interpret(vector);

	/* 4) Restore environment */
	CDR(lex_env) = cl_stack_pop();
	return vector;
}

static cl_object *
interpret_mbind(cl_object *vector)
{
	int i = get_oparg(vector[-1]);
	while (i--) {
		cl_object var = next_code(vector);
		cl_object value = (i < NValues) ? VALUES(i) : Cnil;
		if (var == MAKE_FIXNUM(1))
			bind_special(next_code(vector), value);
		else
			bind_var(var, value);
	}
	return vector;
}

static cl_object *
interpret_mcall(cl_object *vector) {
	cl_index sp = cl_stack_index();
	vector = interpret(vector);
	VALUES(0) = interpret_call(cl_stack_index()-sp, VALUES(0));
	return vector;
}

static cl_object *
interpret_mprog1(cl_object *vector) {
	cl_index i,n = NValues;
	for (i=0; i<n; i++) {
		cl_stack_push(VALUES(i));
	}
	vector = interpret(vector);
	for (i=n; i;) {
		VALUES(--i) = cl_stack_pop();
	}
	NValues = n;
	return vector;
}

static cl_object *
interpret_msetq(cl_object *vector)
{
	cl_object var, value;
	int i = get_oparg(vector[-1]);
	while (i--) {
		var = next_code(vector);
		value = (i < NValues) ? VALUES(i) : Cnil;
		if (var != MAKE_FIXNUM(1))
			setq_local(var, value);
		else {
			var = next_code(vector);
			if (var->symbol.stype == stp_constant)
				FEassignment_to_constant(var);
			else
				SYM_VAL(var) = value;
		}
	}
	if (NValues > 1) NValues = 1;
	return vector;
}

static cl_object *
interpret_progv(cl_object *vector) {
	cl_object values = VALUES(0);
	cl_object vars = cl_stack_pop();

	/* 1) Save current environment */
	bds_ptr old_bds_top = bds_top;
	cl_stack_push(CAR(lex_env));

	/* 2) Add new bindings */
	while (!endp(vars)) {
		if (values == Cnil)
			bds_bind(CAR(vars), OBJNULL);
		else {
			bds_bind(CAR(vars), car(values));
			values = CDR(values);
		}
		vars = CDR(vars);
	}
	vector = interpret(vector);

	/* 3) Restore environment */
	CAR(lex_env) = cl_stack_pop();
	bds_unwind(old_bds_top);
	return vector;
}

static cl_object *
interpret_pushenv(cl_object *vector) {
	/* 1) Save environment */
	bds_ptr old_bds_top = bds_top;
	cl_stack_push(CAR(lex_env));

	/* 2) Execute */
	vector = interpret(vector);

	/* 3) Restore environment */
	CAR(lex_env) = cl_stack_pop();
	bds_unwind(old_bds_top);
	return vector;
}

cl_object *
interpret(cl_object *vector) {
	cl_type t;
	cl_object s;
	cl_fixnum n;

 BEGIN:
	s = next_code(vector);
	t = type_of(s);
	if (t != t_fixnum) {
		VALUES(0) = s;
		NValues = 1;
		goto BEGIN;
	}
	switch (GET_OP(s)) {
	case OP_PUSHQ:
		cl_stack_push(next_code(vector));
		break;
	case OP_PUSH:
		cl_stack_push(VALUES(0));
		break;
	case OP_PUSHV:
		cl_stack_push(search_local(next_code(vector)));
		break;
	case OP_PUSHVS:
		cl_stack_push(search_global(next_code(vector)));
		break;
	case OP_VAR:
		VALUES(0) = search_local(next_code(vector));
		NValues = 1;
		break;
	case OP_VARS:
		VALUES(0) = search_global(next_code(vector));
		NValues = 1;
		break;
	case OP_QUOTE:
		VALUES(0) = next_code(vector);
		NValues = 1;
		break;
	case OP_NOP:
		VALUES(0) = Cnil;
		NValues = 0;
		break;
	case OP_BLOCK:
		vector = interpret_block(vector);
		break;
	case OP_PUSHVALUES: {
		int i;
		for (i=0; i<NValues; i++)
			cl_stack_push(VALUES(i));
		break;
	}
	case OP_MCALL:
		vector = interpret_mcall(vector);
		break;
	case OP_CALL: {
		cl_fixnum n = get_oparg(s);
		cl_object name = next_code(vector);
		VALUES(0) = interpret_call(n, name);
		break;
	}
	case OP_PCALL: {
		cl_fixnum n = get_oparg(s);
		cl_object name = next_code(vector);
		VALUES(0) = interpret_call(n, name);
		cl_stack_push(VALUES(0));
		break;
	}
	case OP_FCALL: {
		cl_fixnum n = get_oparg(s);
		cl_object fun = VALUES(0);
		VALUES(0) = interpret_funcall(n, fun);
		break;
	}
	case OP_PFCALL: {
		cl_fixnum n = get_oparg(s);
		cl_object fun = VALUES(0);
		VALUES(0) = interpret_funcall(n, fun);
		cl_stack_push(VALUES(0));
		break;
	}
	case OP_CATCH:
		vector = interpret_catch(vector);
		break;
	case OP_EXIT:
		return vector;
	case OP_HALT:
		return vector-1;
	case OP_FLET:
		vector = interpret_flet(vector);
		break;
	case OP_LABELS:
		vector = interpret_labels(vector);
		break;
	case OP_FUNCTION:
		VALUES(0) = search_symbol_function(next_code(vector));
		NValues = 1;
		break;
	case OP_CLOSE:
		VALUES(0) = close_around(next_code(vector), lex_env);
		NValues = 1;
		break;
	case OP_GO: {
		cl_object tag = next_code(vector);
		cl_object id = lex_tag_sch(tag);
		if (Null(id))
			FEcontrol_error("GO: Undefined tag ~S.", 1, tag);
		VALUES(0) = Cnil;
		NValues = 0;
		go(id, tag);
		break;
	}
	case OP_RETURN: {
		cl_object tag = next_code(vector);
		cl_object id = lex_block_sch(tag);
		if (Null(id))
			FEcontrol_error("RETURN-FROM: Unknown block ~S.", 1, tag);
		return_from(id, tag);
		break;
	}
	case OP_THROW:
		throw(cl_stack_pop());
		break;
	case OP_JMP:
		vector = vector - 1 + get_oparg(s);
		break;
	case OP_JNIL:
		NValues = 1;
		if (Null(VALUES(0))) vector = vector - 1 + get_oparg(s);
		break;
	case OP_JT:
		NValues = 1;
		if (!Null(VALUES(0))) vector = vector - 1 + get_oparg(s);
		break;
	case OP_JEQ:
		if (VALUES(0) == next_code(vector))
			vector = vector + get_oparg(s) - 2;
		break;
	case OP_JNEQ:
		if (VALUES(0) != next_code(vector))
			vector = vector + get_oparg(s) - 2;
		break;
	case OP_BIND:
		bind_var(next_code(vector), VALUES(0));
		break;
	case OP_BINDS:
		bind_special(next_code(vector), VALUES(0));
		break;
	case OP_SETQ:
		setq_local(next_code(vector), VALUES(0));
		break;
	case OP_SETQS: {
		cl_object var = next_code(vector);
		if (var->symbol.stype == stp_constant)
			FEassignment_to_constant(var);
		else
			SYM_VAL(var) = VALUES(0);
		break;
	}
	case OP_PBIND:
		bind_var(next_code(vector), cl_stack_pop());
		break;
	case OP_PBINDS:
		bind_special(next_code(vector), cl_stack_pop());
		break;
	case OP_PSETQ:
		setq_local(next_code(vector), cl_stack_pop());
		Values[0] = Cnil;
		NValues = 1;
		break;
	case OP_PSETQS: {
		cl_object var = next_code(vector);
		if (var->symbol.stype == stp_constant)
			FEassignment_to_constant(var);
		else
			SYM_VAL(var) = cl_stack_pop();
		Values[0] = Cnil;
		NValues = 1;
		break;
	}
	case OP_MSETQ:
		vector = interpret_msetq(vector);
		break;
	case OP_MBIND:
		vector = interpret_mbind(vector);
		break;
	case OP_MPROG1:
		vector = interpret_mprog1(vector);
		break;
	case OP_PROGV:
		vector = interpret_progv(vector);
		break;
	case OP_PUSHENV:
		vector = interpret_pushenv(vector);
		break;
	case OP_VALUES: {
		cl_fixnum n = get_oparg(s);
		NValues = n;
		while (n)
			VALUES(--n) = cl_stack_pop();
		break;
	}
	case OP_NTHVAL: {
		cl_index n = fix(cl_stack_pop());
		if (n < 0 || n >= NValues)
			VALUES(0) = Cnil;
		else
			VALUES(0) = VALUES(n);
		NValues = 1;
		break;
	}
	case OP_DOLIST:
		vector = interpret_dolist(vector);
		break;
	case OP_DOTIMES:
		vector = interpret_dotimes(vector);
		break;
	case OP_DO:
		vector = interpret_do(vector);
		break;
	case OP_TAGBODY:
		vector = interpret_tagbody(vector);
		break;
	case OP_UNWIND:
		vector = interpret_unwind_protect(vector);
		break;
	default:
		FEerror("Internal error: Unknown code ~S",
			1, MAKE_FIXNUM(*(vector-1)));
	}
	goto BEGIN;
}

@(defun si::interpreter_stack ()
@
	@(return Cnil)
@)

void
init_interpreter(void)
{
	cl_stack = NULL;
	cl_stack_size = 0;
	cl_stack_set_size(8*LISP_PAGESIZE);
}
