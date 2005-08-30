/*
    interpreter.c -- Bytecode interpreter.
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <string.h>
#include "ecl.h"
#include "ecl-inl.h"
#include "bytecodes.h"

#undef frs_pop
#define frs_pop() { \
	cl_env.stack_top = cl_env.stack + cl_env.frs_top->frs_sp; \
	cl_env.frs_top--; }

/* -------------------- INTERPRETER STACK -------------------- */

void
cl_stack_set_size(cl_index new_size)
{
	cl_index top = cl_env.stack_top - cl_env.stack;
	cl_object *new_stack;

	/*printf("*+*+*+\n");*/

	if (top > new_size)
		FEerror("Internal error: cannot shrink stack that much.",0);

	start_critical_section();

	new_stack = (cl_object *)cl_alloc_atomic(new_size * sizeof(cl_object));
	memcpy(new_stack, cl_env.stack, cl_env.stack_size * sizeof(cl_object));

#ifdef BOEHM_GBC
	GC_free(cl_env.stack);
#else
	cl_dealloc(cl_env.stack, cl_env.stack_size);
#endif
	cl_env.stack_size = new_size;
	cl_env.stack = new_stack;
	cl_env.stack_top = cl_env.stack + top;
	cl_env.stack_limit = cl_env.stack + (new_size - 2);

	end_critical_section();
}

static void
cl_stack_grow(void)
{
	cl_stack_set_size(cl_env.stack_size + LISP_PAGESIZE);
}

void
cl_stack_push(cl_object x) {
	if (cl_env.stack_top >= cl_env.stack_limit)
		cl_stack_grow();
	*(cl_env.stack_top++) = x;
}

cl_object
cl_stack_pop() {
	if (cl_env.stack_top == cl_env.stack)
		FEerror("Internal error: stack underflow.",0);
	return *(--cl_env.stack_top);
}

cl_index
cl_stack_index() {
	return cl_env.stack_top - cl_env.stack;
}

void
cl_stack_set_index(cl_index index) {
	cl_object *new_top = cl_env.stack + index;
	if (new_top > cl_env.stack_top)
		FEerror("Internal error: tried to advance stack.",0);
	cl_env.stack_top = new_top;
}

void
cl_stack_insert(cl_index where, cl_index n) {
	if (cl_env.stack_top + n > cl_env.stack_limit) {
		cl_index delta = (n + (LISP_PAGESIZE-1))/LISP_PAGESIZE;
		cl_stack_set_size(cl_env.stack_size + delta * LISP_PAGESIZE);
	}
	cl_env.stack_top += n;
	memmove(&cl_env.stack[where+n], &cl_env.stack[where],
		(cl_env.stack_top - cl_env.stack) * sizeof(cl_object));
}

void
cl_stack_pop_n(cl_index index) {
	cl_object *new_top = cl_env.stack_top - index;
	if (new_top < cl_env.stack)
		FEerror("Internal error: stack underflow.",0);
	cl_env.stack_top = new_top;
}

cl_index
cl_stack_push_values(void) {
	cl_index i;
	for (i=0; i<NVALUES; i++)
		cl_stack_push(VALUES(i));
	return i;
}

void
cl_stack_pop_values(cl_index n) {
	NVALUES = n;
	while (n > 0)
		VALUES(--n) = cl_stack_pop();
}

cl_index
cl_stack_push_va_list(cl_va_list args) {
	cl_index sp;

	sp = cl_env.stack_top - cl_env.stack;
	while (cl_env.stack_top + args[0].narg > cl_env.stack_limit)
		cl_stack_grow();
	while (args[0].narg > 0) {
		*(cl_env.stack_top++) = cl_va_arg(args);
	}
	return sp;
}

cl_index
cl_stack_push_list(cl_object list)
{
	cl_index n;
	cl_object fast, slow;

	/* INV: A list's length always fits in a fixnum */
	fast = slow = list;
	for (n = 0; CONSP(fast); n++, fast = CDR(fast)) {
		*cl_env.stack_top = CAR(fast);
		if (++cl_env.stack_top >= cl_env.stack_limit)
			cl_stack_grow();
		if (n & 1) {
			/* Circular list? */
			if (slow == fast) break;
			slow = CDR(slow);
		}
	}
	if (fast != Cnil)
		FEtype_error_proper_list(list);
	return n;
}

/* ------------------------------ LEXICAL ENV. ------------------------------ */

static void
bind_var(register cl_object var, register cl_object val)
{
	cl_env.lex_env = CONS(var, CONS(val, cl_env.lex_env));
}

static void
bind_function(cl_object name, cl_object fun)
{
	cl_env.lex_env = CONS(@':function', CONS(CONS(name, fun), cl_env.lex_env));
}

static cl_object
bind_tagbody()
{
	cl_object id = new_frame_id();
	cl_env.lex_env = CONS(@':tag', CONS(id, cl_env.lex_env));
	return id;
}

static void
bind_block(cl_object name, cl_object id)
{
	cl_env.lex_env = CONS(@':block', CONS(CONS(name, id), cl_env.lex_env));
}

static void
bind_special(register cl_object var, register cl_object val)
{
	bds_bind(var, val);
}

static cl_object
search_local(register int s) {
	cl_object x;
	for (x = cl_env.lex_env; s-- > 0 && !Null(x); x = CDDR(x));
	if (Null(x))
		FEerror("Internal error: local not found.", 0);
	return CADR(x);
}

static void
setq_local(register int s, register cl_object v) {
	cl_object x;
	for (x = cl_env.lex_env; s-- > 0 && !Null(x); x = CDDR(x));
	if (Null(x))
		FEerror("Internal error: local ~S not found.", 1, s);
	CADR(x) = v;
}

/* -------------------- LAMBDA FUNCTIONS -------------------- */

static void
lambda_bind_var(cl_object var, cl_object val, cl_object specials)
{
	if (!member_eq(var, specials))
		bind_var(var, val);
	else
		bind_special(var, val);
}

static void
lambda_bind(cl_narg narg, cl_object lambda, cl_index sp)
{
	cl_object *data = lambda->bytecodes.data;
	cl_object specials = lambda->bytecodes.specials;
	int i, n;
	bool check_remaining = TRUE;

	/* 1) REQUIRED ARGUMENTS:  N var1 ... varN */
	n = fix(*(data++));
	if (narg < n)
	  FEwrong_num_arguments(lambda->bytecodes.name);
	for (; n; n--, narg--)
	  lambda_bind_var(*(data++), cl_env.stack[sp++], specials);

	/* 2) OPTIONAL ARGUMENTS:  N var1 value1 flag1 ... varN valueN flagN */
	for (n = fix(*(data++)); n; n--, data+=3) {
	  if (narg) {
	    lambda_bind_var(data[0], cl_env.stack[sp], specials);
	    sp++; narg--;
	    if (!Null(data[2]))
	      lambda_bind_var(data[2], Ct, specials);
	  } else {
	    cl_object defaults = data[1];
	    if (FIXNUMP(defaults)) {
	      interpret(lambda, (cl_opcode*)lambda->bytecodes.code + fix(defaults));
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
	    rest = CONS(cl_env.stack[sp+(--i)], rest);
	  lambda_bind_var(data[0], rest, specials);
	}
	data++;

	/* 4) ALLOW-OTHER-KEYS: { T | NIL | 0} */
	if (data[0] == MAKE_FIXNUM(0)) {
	  data++;
	  if (narg && check_remaining)
	    FEprogram_error("LAMBDA: Too many arguments to function ~S.", 1,
			    lambda->bytecodes.name);
	} else {
	  /*
	   * Only when ALLOW-OTHER-KEYS /= 0, we process this:
	   * 5) KEYWORDS: N key1 var1 value1 flag1 ... keyN varN valueN flagN
	   */
	  bool allow_other_keys = !Null(*(data++));
	  bool allow_other_keys_found = allow_other_keys;
	  int n = fix(*(data++));
	  cl_object *keys;
#ifdef __GNUC__
	  cl_object spp[n];
#else
#define SPP_MAX 64
	  cl_object spp[SPP_MAX];
#endif
	  bool other_found = FALSE;
	  void *unbound = spp; /* not a valid lisp object */
	  if ((narg & 1) != 0)
	    FEprogram_error("Function called with odd number of keyword arguments.", 0);
	  for (i=0; i<n; i++)
#ifdef __GNUC__
	    spp[i] = unbound;
#else
	    if (i >= SPP_MAX)
	      FEerror("lambda_bind: Too many keyword arguments, limited to ~A.", 1, MAKE_FIXNUM(SPP_MAX));
	    else
	    spp[i] = unbound;
#endif
	  for (; narg; narg-=2) {
	    cl_object key = cl_env.stack[sp++];
	    cl_object value = cl_env.stack[sp++];
	    if (!SYMBOLP(key))
	      FEprogram_error("LAMBDA: Keyword expected, got ~S.", 1, key);
	    keys = data;
	    if (key == @':allow-other-keys') {
	      if (!allow_other_keys_found) {
		allow_other_keys_found = TRUE;
		allow_other_keys = !Null(value);
	      }
	    }
	    for (i = 0; i < n; i++, keys += 4) {
	      if (key == keys[0]) {
		if (spp[i] == unbound)
		  spp[i] = value;
		goto FOUND;
	      }
	    }
	    if (key != @':allow-other-keys')
	      other_found = TRUE;
	  FOUND:
	    (void)0;
	  }
	  if (other_found && !allow_other_keys)
	    FEprogram_error("LAMBDA: Unknown keys found in function ~S.",
			    1, lambda->bytecodes.name);
	  for (i=0; i<n; i++, data+=4) {
	    if (spp[i] != unbound)
	      lambda_bind_var(data[1],spp[i],specials);
	    else {
	      cl_object defaults = data[2];
	      if (FIXNUMP(defaults)) {
		      interpret(lambda, (cl_opcode*)lambda->bytecodes.code + fix(defaults));
		      defaults = VALUES(0);
	      }
	      lambda_bind_var(data[1],defaults,specials);
	    }
	    if (!Null(data[3]))
	      lambda_bind_var(data[3],(spp[i] != unbound)? Ct : Cnil,specials);
	  }
	}
}

cl_object
lambda_apply(cl_narg narg, cl_object fun)
{
	cl_index args = cl_stack_index() - narg;
	cl_object name;
	bds_ptr old_bds_top;
	struct ihs_frame ihs;

	if (type_of(fun) != t_bytecodes)
		FEinvalid_function(fun);

	/* Save the lexical environment and set up a new one */
	ihs_push(&ihs, fun);
	cl_env.lex_env = fun->bytecodes.lex;
	old_bds_top = cl_env.bds_top;

	/* Establish bindings */
	lambda_bind(narg, fun, args);

	/* If it is a named lambda, set a block for RETURN-FROM */
	VALUES(0) = Cnil;
	NVALUES = 0;
	name = fun->bytecodes.name;
	if (Null(name))
		interpret(fun, fun->bytecodes.code);
	else {
		/* Accept (SETF name) */
		if (CONSP(name)) name = CADR(name);
		CL_BLOCK_BEGIN(id) {
			bind_block(name, id);
			interpret(fun, fun->bytecodes.code);
		} CL_BLOCK_END;
	}
	bds_unwind(old_bds_top);
	ihs_pop();
	returnn(VALUES(0));
}


/* -------------------- AIDS TO THE INTERPRETER -------------------- */

static cl_object
search_global(register cl_object s) {
	cl_object x = SYM_VAL(s);
	if (x == OBJNULL)
		FEunbound_variable(s);
	return x;
}

/*
 * INTERPRET-FUNCALL is one of the few ways to "exit" the interpreted
 * environment and get into the C/lisp world. Since almost all data from the
 * interpreter is kept in local variables, and frame stacks, binding stacks,
 * etc, are already handled by the C core, only the lexical environment
 * (cl_env.lex_env) needs to be saved.
 */
static cl_object
interpret_funcall(cl_narg narg, cl_object fun) {
	cl_object lex_env = cl_env.lex_env;
	cl_object *args;
	cl_object x;
	args = cl_env.stack_top - narg;
	if (fun == OBJNULL || fun == Cnil)
		goto ERROR;
 AGAIN:
	switch (type_of(fun)) {
	case t_cfun: {
		struct ihs_frame ihs;
		ihs_push(&ihs, fun->cfun.name);
		if (fun->cfun.narg >= 0) {
			if (narg != fun->cfun.narg)
				FEwrong_num_arguments(fun);
			x = APPLY_fixed(narg, (cl_objectfn_fixed)fun->cfun.entry, args);
		} else {
			x = APPLY(narg, fun->cfun.entry, args);
		}
		ihs_pop();
		break;
	}
	case t_cclosure:{
		struct ihs_frame ihs;
		ihs_push(&ihs, fun);
		x = APPLY_closure(narg, fun->cclosure.entry, fun->cclosure.env, args);
		ihs_pop();
		break;
	}
#ifdef CLOS
	case t_instance:
		if (!fun->instance.isgf)
			goto ERROR;
		fun = compute_method(narg, fun, args);
		goto AGAIN;
#endif
	case t_bytecodes:
		x = lambda_apply(narg, fun);
		break;
	case t_symbol: {
		cl_object function = SYM_FUN(fun);
		if (function == Cnil || fun->symbol.mflag)
			FEundefined_function(fun);
		fun = function;
		goto AGAIN;
	}
	default: ERROR:
		FEinvalid_function(fun);
	}
	cl_env.lex_env = lex_env;
	cl_stack_pop_n(narg);
	return x;
}

@(defun apply (fun lastarg &rest args)
	cl_index i;
@
	narg -= 2;
	for (i = 0; narg; i++,narg--) {
		cl_stack_push(lastarg);
		lastarg = cl_va_arg(args);
	}
	loop_for_in (lastarg) {
		if (i >= CALL_ARGUMENTS_LIMIT) {
			cl_stack_pop_n(i);
			FEprogram_error("CALL-ARGUMENTS-LIMIT exceeded",0);
		}
		cl_stack_push(CAR(lastarg));
		i++;
	} end_loop_for_in;
	returnn(interpret_funcall(i, fun));
@)

/* -------------------- THE INTERPRETER -------------------- */

/* OP_DOLIST	labelz, labelo
   ...		; code to bind the local variable
   OP_EXIT
   ...		; code executed on each iteration
   OP_EXIT
   labelo:
   ...		; code executed at the end
   OP_EXIT
   labelz:

	High level construct for the DOLIST iterator. The list over which
	we iterate is stored in VALUES(0).
*/
static cl_opcode *
interpret_dolist(cl_object bytecodes, cl_opcode *vector) {
	cl_opcode *volatile exit;
	cl_opcode *output;

	GET_LABEL(exit, vector);
	GET_LABEL(output, vector);

	/* 1) Set NIL block */
	CL_BLOCK_BEGIN(id) {
		cl_object list = VALUES(0);

		bind_block(Cnil, id);

		/* 2) Build list & bind variable*/
		vector = interpret(bytecodes, vector);

		/* 3) Repeat until list is exahusted */
		while (!endp(list)) {
			NVALUES = 1;
			VALUES(0) = CAR(list);
			interpret(bytecodes, vector);
			list = CDR(list);
		}
		VALUES(0) = Cnil;
		NVALUES = 1;
		interpret(bytecodes, output);

		/* 4) Restore environment */
		cl_env.lex_env = cl_env.frs_top->frs_lex;
		bds_unwind(cl_env.frs_top->frs_bds_top);
	} CL_BLOCK_END;
	return exit;
}

/* OP_TIMES	labelz, labelo
   ...		; code to bind the local variable
   OP_EXIT
   ...		; code executed on each iteration
   OP_EXIT
   labelo:
   ...		; code executed at the end
   OP_EXIT
   labelz:

	High level construct for the DOTIMES iterator. The number of times
	we iterate is stored in VALUES(0).
*/
static cl_opcode *
interpret_dotimes(cl_object bytecodes, cl_opcode *vector) {
	cl_opcode *volatile exit;
	cl_opcode *output;

	GET_LABEL(exit, vector);
	GET_LABEL(output, vector);

	CL_BLOCK_BEGIN(id) {
		cl_object length = VALUES(0);

		/* 1) Set up a nil block */
		bind_block(Cnil, id);

		/* 2) Retrieve number and bind variables */
		vector = interpret(bytecodes, vector);

		if (FIXNUMP(length)) {
		    cl_fixnum i, l = fix(length);
		    /* 3) Loop while needed */
		    for (i = 0; i < l; i++) {
			NVALUES = 1;
			VALUES(0) = MAKE_FIXNUM(i);
			interpret(bytecodes, vector);
		    }
		    length = MAKE_FIXNUM(i);
		} else {
		    cl_object i;
		    for (i = MAKE_FIXNUM(0);
			 number_compare(i, length) < 0;
			 i = one_plus(i))
		    {
			NVALUES = 1;
			VALUES(0) = i;
			interpret(bytecodes, vector);
		    }
		    length = i;
		}
		NVALUES = 1;
		VALUES(0) = length;
		interpret(bytecodes, output);

		/* 4) Restore environment */
		cl_env.lex_env = cl_env.frs_top->frs_lex;
		bds_unwind(cl_env.frs_top->frs_bds_top);
	} CL_BLOCK_END;
	return exit;
}

static cl_object
close_around(cl_object fun, cl_object lex) {
	cl_object v = cl_alloc_object(t_bytecodes);
	v->bytecodes = fun->bytecodes;
	v->bytecodes.lex = lex;
	return v;
}

/* OP_FLET	nfun{arg}
   fun1{object}
   ...
   funn{object}
   ...
   OP_EXIT

	Executes the enclosed code in a lexical enviroment extended with
	the functions "fun1" ... "funn".
*/
static cl_opcode *
interpret_flet(cl_object bytecodes, cl_opcode *vector) {
	cl_index nfun = GET_OPARG(vector);

	/* 1) Copy the environment so that functions get it without references
	      to themselves. */
	cl_object lex = cl_env.lex_env;

	/* 3) Add new closures to environment */
	while (nfun--) {
		cl_object fun = GET_DATA(vector, bytecodes);
		cl_object f = close_around(fun,lex);
		bind_function(f->bytecodes.name, f);
	}
	return vector;
}

/* OP_FLET	nfun{arg}
   fun1{object}
   ...
   funn{object}
   ...
   OP_EXIT

	Executes the enclosed code in a lexical enviroment extended with
	the functions "fun1" ... "funn".
*/
static cl_opcode *
interpret_labels(cl_object bytecodes, cl_opcode *vector) {
	cl_index i, nfun = GET_OPARG(vector);
	cl_object l;

	/* 1) Build up a new environment with all functions */
	for (i=0; i<nfun; i++) {
		cl_object f = GET_DATA(vector, bytecodes);
		bind_function(f->bytecodes.name, f);
	}

	/* 2) Update the closures so that all functions can call each other */
	for (i=0, l=cl_env.lex_env; i<nfun; i++) {
		cl_object record = CADR(l);
		CDR(record) = close_around(CDR(record), cl_env.lex_env);
		l = CDDR(l);
	}
	return vector;
}

/* OP_MSETQ	n{arg}
   {fixnumn}
   ...
   {fixnum1}

	Sets N variables to the N values in VALUES(), filling with
	NIL when there are values missing. Local variables are denoted
	with an integer which points a position in the lexical environment,
	while special variables are denoted with a negative index X, which
	denotes the value -1-X in the table of constants.
*/
static cl_opcode *
interpret_msetq(cl_object bytecodes, cl_opcode *vector)
{
	cl_object value;
	cl_index i, n = GET_OPARG(vector);
	for (i=0; i<n; i++) {
		cl_fixnum var = GET_OPARG(vector);
		value = (i < NVALUES) ? VALUES(i) : Cnil;
		if (var >= 0)
			setq_local(var, value);
		else {
			cl_object name = bytecodes->bytecodes.data[-1-var];
			if (name->symbol.stype == stp_constant)
				FEassignment_to_constant(name);
			else
				ECL_SETQ(name, value);
		}
	}
	if (NVALUES > 1) NVALUES = 1;
	return vector;
}

/* OP_PROGV	bindings{list}
   ...
   OP_EXIT
	Execute the code enclosed with the special variables in BINDINGS
	set to the values in the list which was passed in VALUES(0).
*/
static cl_opcode *
interpret_progv(cl_object bytecodes, cl_opcode *vector) {
	cl_object values = VALUES(0);
	cl_object vars = cl_stack_pop();

	/* 1) Save current environment */
	bds_ptr old_bds_top = cl_env.bds_top;
	cl_object old_lex_env = cl_env.lex_env;

	/* 2) Add new bindings */
	while (!endp(vars)) {
		if (values == Cnil)
			bds_bind(CAR(vars), OBJNULL);
		else {
			bds_bind(CAR(vars), cl_car(values));
			values = CDR(values);
		}
		vars = CDR(vars);
	}
	vector = interpret(bytecodes, vector);

	/* 3) Restore environment */
	cl_env.lex_env = old_lex_env;
	bds_unwind(old_bds_top);
	return vector;
}

void *
interpret(cl_object bytecodes, void *pc) {
	cl_opcode *vector = pc;
	cl_object reg0 = VALUES(0);
	static int i = 0;
	i++;
 BEGIN:
	switch (GET_OPCODE(vector)) {
	/* OP_QUOTE
		Sets REG0 to an immediate value.
	*/
	case OP_QUOTE:
		reg0 = GET_DATA(vector, bytecodes);
		break;

	/* OP_VAR	n{arg}, var{symbol}
		Sets REG0 to the value of the n-th local.
		VAR is the name of the variable for readability purposes.
	*/
	case OP_VAR: {
		int lex_env_index = GET_OPARG(vector);
		reg0 = search_local(lex_env_index);
		break;
	}

	/* OP_VARS	var{symbol}
		Sets REG0 to the value of the symbol VAR.
		VAR should be either a special variable or a constant.
	*/
	case OP_VARS: {
		cl_object var_name = GET_DATA(vector, bytecodes);
		reg0 = search_global(var_name);
		break;
	}

	/* OP_PUSH
		Pushes the object in VALUES(0).
	*/
	case OP_PUSH:
		cl_stack_push(reg0);
		break;

	/* OP_PUSHV	n{arg}
		Pushes the value of the n-th local onto the stack.
	*/
	case OP_PUSHV: {
		int lex_env_index = GET_OPARG(vector);
		cl_stack_push(search_local(lex_env_index));
		break;
	}

	/* OP_PUSHVS	var{symbol}
		Pushes the value of the symbol VAR onto the stack.
		VAR should be either a special variable or a constant.
	*/
	case OP_PUSHVS: {
		cl_object var_name = GET_DATA(vector, bytecodes);
		cl_stack_push(search_global(var_name));
		break;
	}

	/* OP_PUSHQ	value{object}
		Pushes "value" onto the stack.
	*/
	case OP_PUSHQ:
		cl_stack_push(GET_DATA(vector, bytecodes));
		break;

	/* OP_CALL	n{arg}
		Calls the function in REG0 with N arguments which
		have been deposited in the stack. The output values
		are left in VALUES(...)
	*/
	case OP_CALL: {
		cl_fixnum n = GET_OPARG(vector);
		VALUES(0) = reg0 = interpret_funcall(n, reg0);
		break;
	}

	/* OP_CALLG	n{arg}, name{arg}
		Calls the function NAME with N arguments which have been
		deposited in the stack. The output values are left in VALUES.
	*/
	case OP_CALLG: {
		cl_fixnum n = GET_OPARG(vector);
		cl_object f = GET_DATA(vector, bytecodes);
		VALUES(0) = reg0 = interpret_funcall(n, f);
		break;
	}

	/* OP_FCALL	n{arg}
		Calls a function in the stack with N arguments which
		have been also deposited in the stack. The output values
		are left in VALUES(...)
	*/
	case OP_FCALL: {
		cl_fixnum n = GET_OPARG(vector);
		cl_object fun = cl_env.stack_top[-n-1];
		VALUES(0) = reg0 = interpret_funcall(n, fun);
		cl_stack_pop();
		break;
	}

	/* OP_MCALL
		Similar to FCALL, but gets the number of arguments from
		the stack (They all have been deposited by OP_PUSHVALUES)
	*/
	case OP_MCALL: {
		cl_fixnum n = fix(cl_stack_pop());
		cl_object fun = cl_env.stack_top[-n-1];
		VALUES(0) = reg0 = interpret_funcall(n, fun);
		cl_stack_pop();
		break;
	}

	/* OP_PCALL	n{arg}
		Calls the function in REG0 with N arguments which
		have been deposited in the stack. The first output value
		is pushed on the stack.
	*/
	case OP_PCALL: {
		cl_fixnum n = GET_OPARG(vector);
		cl_stack_push(interpret_funcall(n, reg0));
		break;
	}

	/* OP_PCALLG	n{arg}, name{arg}
		Calls the function NAME with N arguments which have been
		deposited in the stack. The first output value is pushed on
		the stack.
	*/
	case OP_PCALLG: {
		cl_fixnum n = GET_OPARG(vector);
		cl_object f = GET_DATA(vector, bytecodes);
		cl_stack_push(interpret_funcall(n, f));
		break;
	}

	/* OP_PFCALL	n{arg}
		Calls the function in the stack with N arguments which
		have been also deposited in the stack. The first output value
		is pushed on the stack.
	*/
	case OP_PFCALL: {
		cl_fixnum n = GET_OPARG(vector);
		cl_object fun = cl_env.stack_top[-n-1];
		cl_object reg0 = interpret_funcall(n, fun);
		cl_env.stack_top[-1] = reg0;
		break;
	}

	/* OP_EXIT
		Marks the end of a high level construct (BLOCK, CATCH...)
		or a function.
	*/
	case OP_EXIT:
		return (char *)vector;

	case OP_FLET:
		vector = interpret_flet(bytecodes, vector);
		break;
	case OP_LABELS:
		vector = interpret_labels(bytecodes, vector);
		break;

	/* OP_LFUNCTION	n{arg}, function-name{symbol}
		Calls the local or global function with N arguments
		which have been deposited in the stack.
	*/
	case OP_LFUNCTION: {
		int lex_env_index = GET_OPARG(vector);
		cl_object fun_record = search_local(lex_env_index);
		reg0 = CDR(fun_record);
		break;
	}

	/* OP_FUNCTION	name{symbol}
		Extracts the function associated to a symbol. The function
		may be defined in the global environment or in the local
		environment. This last value takes precedence.
	*/
	case OP_FUNCTION:
		reg0 = ecl_fdefinition(GET_DATA(vector, bytecodes));
		break;

	/* OP_CLOSE	name{symbol}
		Extracts the function associated to a symbol. The function
		may be defined in the global environment or in the local
		environment. This last value takes precedence.
	*/
	case OP_CLOSE: {
		cl_object function_object = GET_DATA(vector, bytecodes);
		reg0 = close_around(function_object, cl_env.lex_env);
		break;
	}
	/* OP_GO	n{arg}
	   OP_QUOTE	tag-name{symbol}
		Jumps to the tag which is defined at the n-th position in
		the lexical environment. TAG-NAME is kept for debugging
		purposes.
	*/
	case OP_GO: {
		cl_object id = search_local(GET_OPARG(vector));
		cl_object tag_name = GET_DATA(vector, bytecodes);
		cl_go(id, tag_name);
		break;
	}
	/* OP_RETURN	n{arg}
		Returns from the block whose record in the lexical environment
		occuppies the n-th position.
	*/
	case OP_RETURN: {
		int lex_env_index = GET_OPARG(vector);
		cl_object block_record = search_local(lex_env_index);
		cl_object block_name = CAR(block_record);
		cl_object id = CDR(block_record);
		cl_return_from(id, block_name);
		break;
	}
	/* OP_THROW
		Jumps to an enclosing CATCH form whose tag matches the one
		of the THROW. The tag is taken from the stack, while the
		output values are left in VALUES(...).
	*/
	case OP_THROW: {
		cl_object tag_name = cl_stack_pop();
		cl_throw(tag_name);
		break;
	}
	/* OP_JMP	label{arg}
	   OP_JNIL	label{arg}
	   OP_JT	label{arg}
	   OP_JEQ	value{object}, label{arg}
	   OP_JNEQ	value{object}, label{arg}
		Direct or conditional jumps. The conditional jumps are made
		comparing with the value of REG0.
	*/
	case OP_JMP: {
		cl_oparg jump = GET_OPARG(vector);
		vector += jump - OPARG_SIZE;
		break;
	}
	case OP_JNIL: {
		cl_oparg jump = GET_OPARG(vector);
		NVALUES = 1;
		if (Null(VALUES(0)))
			vector += jump - OPARG_SIZE;
		break;
	}
	case OP_JT: {
		cl_oparg jump = GET_OPARG(vector);
		NVALUES = 1;
		if (!Null(VALUES(0)))
			vector += jump - OPARG_SIZE;
		break;
	}
	case OP_JEQL: {
		cl_oparg value = GET_OPARG(vector);
		cl_oparg jump = GET_OPARG(vector);
		if (eql(reg0, bytecodes->bytecodes.data[value]))
			vector += jump - OPARG_SIZE;
		break;
	}
	case OP_JNEQL: {
		cl_oparg value = GET_OPARG(vector);
		cl_oparg jump = GET_OPARG(vector);
		if (!eql(reg0, bytecodes->bytecodes.data[value]))
			vector += jump - OPARG_SIZE;
		break;
	}
	case OP_NOT:
		reg0 = (reg0 == Cnil)? Ct : Cnil;
		break;
	/* OP_UNBIND	n{arg}
		Undo "n" local bindings.
	*/
	case OP_UNBIND: {
		cl_index n = GET_OPARG(vector);
		while (n--)
			cl_env.lex_env = CDDR(cl_env.lex_env);
		break;
	}
	/* OP_UNBINDS	n{arg}
		Undo "n" bindings of special variables.
	*/
	case OP_UNBINDS: {
		cl_index n = GET_OPARG(vector);
		bds_unwind_n(n);
		break;
	}
	/* OP_BIND	name{symbol}
	   OP_PBIND	name{symbol}
	   OP_BINDS	name{symbol}
	   OP_PBINDS	name{symbol}
		Binds a lexical or special variable to the either the
		value of REG0 or the first value of the stack.
	*/
	case OP_BIND: {
		cl_object var_name = GET_DATA(vector, bytecodes);
		bind_var(var_name, reg0);
		break;
	}
	case OP_PBIND: {
		cl_object var_name = GET_DATA(vector, bytecodes);
		cl_object value = cl_stack_pop();
		bind_var(var_name, value);
		break;
	}
	case OP_VBIND: {
		cl_index n = GET_OPARG(vector);
		cl_object var_name = GET_DATA(vector, bytecodes);
		cl_object value = (n < NVALUES) ? VALUES(n) : Cnil;
		bind_var(var_name, value);
		break;
	}
	case OP_BINDS: {
		cl_object var_name = GET_DATA(vector, bytecodes);
		bind_special(var_name, reg0);
		break;
	}
	case OP_PBINDS: {
		cl_object var_name = GET_DATA(vector, bytecodes);
		cl_object value = cl_stack_pop();
		bind_special(var_name, value);
		break;
	}
	case OP_VBINDS: {
		cl_index n = GET_OPARG(vector);
		cl_object var_name = GET_DATA(vector, bytecodes);
		cl_object value = (n < NVALUES) ? VALUES(n) : Cnil;
		bind_special(var_name, value);
		break;
	}
	/* OP_SETQ	n{arg}
	   OP_PSETQ	n{arg}
	   OP_SETQS	var-name{symbol}
	   OP_PSETQS	var-name{symbol}
		Sets either the n-th local or a special variable VAR-NAME,
		to either the value in REG0 (OP_SETQ[S]) or to the 
		first value on the stack (OP_PSETQ[S]).
	*/
	case OP_SETQ: {
		int lex_env_index = GET_OPARG(vector);
		setq_local(lex_env_index, reg0);
		break;
	}
	case OP_SETQS: {
		cl_object var = GET_DATA(vector, bytecodes);
		if (var->symbol.stype == stp_constant)
			FEassignment_to_constant(var);
		ECL_SETQ(var, reg0);
		break;
	}
	case OP_PSETQ: {
		int lex_env_index = GET_OPARG(vector);
		setq_local(lex_env_index, cl_stack_pop());
		break;
	}
	case OP_PSETQS: {
		cl_object var = GET_DATA(vector, bytecodes);
		if (var->symbol.stype == stp_constant)
			FEassignment_to_constant(var);
		ECL_SETQ(var, cl_stack_pop());
		break;
	}

	/* OP_BLOCK	label{arg}
	   ...
	   OP_EXIT
	 label:

	   Executes the enclosed code in a named block.
	   LABEL points to the first instruction after OP_EXIT.
	*/
	case OP_BLOCK: {
		cl_object name;
		cl_object id = new_frame_id();
		cl_opcode *exit;
		/* FIXME! */
		name = GET_DATA(vector, bytecodes);
		GET_LABEL(exit, vector);
		cl_stack_push((cl_object)exit);
		if (frs_push(id) == 0) {
			bind_block(name, id);
		} else {
			reg0 = VALUES(0);
			cl_env.lex_env = cl_env.frs_top->frs_lex;
			frs_pop();
			vector = (cl_opcode *)cl_stack_pop();
		}
		break;
	}
	/* OP_DO	label
	     ...	; code executed within a NIL block
	   OP_EXIT_FRAME
	   label:

	   High level construct for the DO and BLOCK forms.
	*/
	case OP_DO: {
		cl_object name = Cnil;
		cl_object id = new_frame_id();
		cl_opcode *exit;
		/* FIXME! */
		GET_LABEL(exit, vector);
		cl_stack_push((cl_object)exit);
		if (frs_push(id) == 0) {
			bind_block(name, id);
		} else {
			reg0 = VALUES(0);
			cl_env.lex_env = cl_env.frs_top->frs_lex;
			frs_pop();
			vector = (cl_opcode *)cl_stack_pop(); /* FIXME! */
		}
		break;
	}
	/* OP_CATCH	label{arg}
	   ...
	   OP_EXIT_FRAME
	   label:

	   Sets a catch point using the tag in VALUES(0). LABEL points to the
	   first instruction after the end (OP_EXIT) of the block
	*/
	case OP_CATCH: {
		cl_opcode *exit;
		GET_LABEL(exit, vector);
		cl_stack_push((cl_object)exit);
		if (frs_push(reg0) != 0) {
			reg0 = VALUES(0);
			cl_env.lex_env = cl_env.frs_top->frs_lex;
			frs_pop();
			vector = (cl_opcode *)cl_stack_pop(); /* FIXME! */
		}
		break;
	}
	/* OP_TAGBODY	n{arg}
	     label1
	     ...
	     labeln
	   label1:
	     ...
	   labeln:
	     ...
	   OP_EXIT

	   High level construct for the TAGBODY form.
	*/
	case OP_TAGBODY: {
		int n = GET_OPARG(vector);
		/* Here we save the location of the jump table */
		cl_stack_push((cl_object)vector); /* FIXME! */
		if (frs_push(bind_tagbody()) == 0) {
			/* The first time, we "name" the tagbody and
			 * skip the jump table */
			vector += n * OPARG_SIZE;
		} else {
			/* Wait here for gotos. Each goto sets
			   VALUES(0) to an integer which ranges from 0
			   to ntags-1, depending on the tag. These
			   numbers are indices into the jump table and
			   are computed at compile time. */
			cl_opcode *table = (cl_opcode *)cl_env.stack_top[-1];
			table = table + fix(VALUES(0)) * OPARG_SIZE;
			vector = table + *(cl_oparg *)table;
			cl_env.lex_env = cl_env.frs_top->frs_lex;
		}
		break;
	}
	case OP_EXIT_TAGBODY:
		cl_env.lex_env = CDDR(cl_env.frs_top->frs_lex);
		frs_pop();
		cl_stack_pop();
	case OP_NIL:
		reg0 = Cnil;
		break;
	case OP_PUSHNIL:
		cl_stack_push(Cnil);
		break;
	case OP_VALUEREG0:
		VALUES(0) = reg0;
		NVALUES = 1;
		break;
	case OP_NOP:
		VALUES(0) = reg0 = Cnil;
		NVALUES = 0;
		break;
	case OP_EXIT_FRAME:
		bds_unwind(cl_env.frs_top->frs_bds_top);
		cl_env.lex_env = cl_env.frs_top->frs_lex;
		frs_pop();
		cl_stack_pop();
		break;
	case OP_DOLIST:
		vector = interpret_dolist(bytecodes, vector);
		reg0 = VALUES(0);
		break;
	case OP_DOTIMES:
		vector = interpret_dotimes(bytecodes, vector);
		reg0 = VALUES(0);
		break;
	case OP_MSETQ:
		vector = interpret_msetq(bytecodes, vector);
		reg0 = VALUES(0);
		break;
	case OP_PROGV:
		vector = interpret_progv(bytecodes, vector);
		reg0 = VALUES(0);
		break;
	/* OP_PUSHVALUES
		Pushes the values output by the last form, plus the number
		of values.
	*/
	PUSH_VALUES:
	case OP_PUSHVALUES: {
		cl_index i;
		for (i=0; i<NVALUES; i++)
			cl_stack_push(VALUES(i));
		cl_stack_push(MAKE_FIXNUM(NVALUES));
		break;
	}
	/* OP_PUSHMOREVALUES
		Adds more values to the ones pushed by OP_PUSHVALUES.
	*/
	case OP_PUSHMOREVALUES: {
		cl_index i, n = fix(cl_stack_pop());
		for (i=0; i<NVALUES; i++)
			cl_stack_push(VALUES(i));
		cl_stack_push(MAKE_FIXNUM(n + NVALUES));
		break;
	}
	/* OP_POP
		Pops a singe value pushed by a OP_PUSH* operator.
	*/
	case OP_POP:
		VALUES(0) = reg0 = cl_stack_pop();
		NVALUES = 1;
		break;
	/* OP_POPVALUES
		Pops all values pushed by a OP_PUSHVALUES operator.
	*/
	case OP_POPVALUES: {
		int n = NVALUES = fix(cl_stack_pop());
		if (n == 0) {
			VALUES(0) = Cnil;
		} else do {
			VALUES(--n) = cl_stack_pop();
		} while (n);
		reg0 = VALUES(0);
		break;
	}
	/* OP_VALUES	n{arg}
		Pop N values from the stack and store them in VALUES(...)
	*/
	case OP_VALUES: {
		cl_fixnum n = GET_OPARG(vector);
		NVALUES = n;
		while (--n)
			VALUES(n) = cl_stack_pop();
		VALUES(0) = reg0 = cl_stack_pop();
		break;
	}
	/* OP_NTHVAL
		Set VALUES(0) to the N-th value of the VALUES(...) list.
		The index N-th is extracted from the top of the stack.
	*/
	case OP_NTHVAL: {
		cl_fixnum n = fix(cl_stack_pop());
		if (n < 0) {
			FEerror("Wrong index passed to NTH-VAL", 1, MAKE_FIXNUM(n));
		} else if ((cl_index)n >= NVALUES) {
			VALUES(0) = reg0 = Cnil;
		} else {
			VALUES(0) = reg0 = VALUES(n);
		}
		NVALUES = 1;
		break;
	}
	/* OP_PROTECT	label
	     ...	; code to be protected and whose value is output
	   OP_EXIT
	   label:
	     ...	; code executed at exit
	   OP_EXIT

	  High level construct for UNWIND-PROTECT. The first piece of code is
	  executed and its output value is saved. Then the second piece of code
	  is executed and the output values restored. The second piece of code
	  is always executed, even if a THROW, RETURN or GO happen within the
	  first piece of code.
	*/
	case OP_PROTECT: {
		cl_opcode *exit;
		GET_LABEL(exit, vector);
		cl_stack_push((cl_object)exit);
		if (frs_push(ECL_PROTECT_TAG) != 0) {
			cl_env.lex_env = cl_env.frs_top->frs_lex;
			frs_pop();
			vector = (cl_opcode *)cl_stack_pop();
			cl_stack_push(MAKE_FIXNUM(cl_env.nlj_fr - cl_env.frs_top));
			goto PUSH_VALUES;
		}
		break;
	}
	case OP_PROTECT_NORMAL:
		bds_unwind(cl_env.frs_top->frs_bds_top);
		cl_env.lex_env = cl_env.frs_top->frs_lex;
		frs_pop();
		cl_stack_pop();
		cl_stack_push(MAKE_FIXNUM(1));
		goto PUSH_VALUES;
	case OP_PROTECT_EXIT: {
		volatile cl_fixnum n = NVALUES = fix(cl_stack_pop());
		while (n--)
			VALUES(n) = cl_stack_pop();
		reg0 = VALUES(0);
		n = fix(cl_stack_pop());
		if (n <= 0)
			unwind(cl_env.frs_top + n);
		break;
	}
	case OP_STEPIN: {
		cl_object form = GET_DATA(vector, bytecodes);
		cl_object a = SYM_VAL(@'si::*step-action*');
		cl_index n = cl_stack_push_values();
		if (a == Ct) {
			/* We are stepping in, but must first ask the user
			 * what to do. */
			ECL_SETQ(@'si::*step-level*',
				 cl_1P(SYM_VAL(@'si::*step-level*')));
			cl_stack_push(form);
			interpret_funcall(1, @'si::stepper');
		} else if (a != Cnil) {
			/* The user told us to step over. *step-level* contains
			 * an integer number that, when it becomes 0, means
			 * that we have finished stepping over. */
			ECL_SETQ(@'si::*step-action*', cl_1P(a));
		} else {
			/* We are not inside a STEP form. This should
			 * actually never happen. */
		}
		cl_stack_pop_values(n);
		break;
	}
	case OP_STEPCALL: {
		/* We are going to call a function. However, we would
		 * like to step _in_ the function. STEPPER takes care of
		 * that. */
		cl_fixnum n = GET_OPARG(vector);
		if (SYM_VAL(@'si::*step-action*') == Ct) {
			cl_stack_push(reg0);
			reg0 = interpret_funcall(1, @'si::stepper');
		}
		reg0 = interpret_funcall(n, reg0);
	}
	case OP_STEPOUT: {
		cl_object a = SYM_VAL(@'si::*step-action*');
		cl_index n = cl_stack_push_values();
		if (a == Ct) {
			/* We exit one stepping level */
			ECL_SETQ(@'si::*step-level*',
				 cl_1M(SYM_VAL(@'si::*step-level*')));
		} else if (a == MAKE_FIXNUM(0)) {
			/* We are back to the level in which the user
			 * selected to step over. */
			ECL_SETQ(@'si::*step-action*', Ct);
		} else if (a != Cnil) {
			ECL_SETQ(@'si::*step-action*', cl_1M(a));
		} else {
			/* Not stepping, nothing to be done. */
		}
		cl_stack_pop_values(n);
		break;
	}
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
