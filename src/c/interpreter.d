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

	/*printf("*+*+*+\n");*/

	if (top > new_size)
		FEerror("Internal error: cannot shrink stack that much.",0);

	start_critical_section();

	new_stack = (cl_object *)cl_alloc(new_size * sizeof(cl_object));
	memcpy(new_stack, cl_stack, cl_stack_size * sizeof(cl_object));

	GC_free(cl_stack);
	cl_stack_size = new_size;
	cl_stack = new_stack;
	cl_stack_top = cl_stack + top;
	cl_stack_limit = cl_stack + (new_size - 2);

	end_critical_section();
}

static void
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

int
cl_stack_push_values(void) {
	int i;
	for (i=0; i<NValues; i++)
		cl_stack_push(VALUES(i));
	return i;
}

void
cl_stack_pop_values(int n) {
	NValues = n;
	while (n > 0)
		VALUES(--n) = cl_stack_pop();
}

cl_index
cl_stack_push_va_list(cl_va_list args) {
	cl_index sp;

	sp = cl_stack_top - cl_stack;
	while (cl_stack_top + args[0].narg > cl_stack_limit)
		cl_stack_grow();
	while (args[0].narg > 0) {
		args[0].narg--;
		*(cl_stack_top++) = va_arg(args[0].args,cl_object);
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
		*cl_stack_top = CAR(fast);
		if (++cl_stack_top >= cl_stack_limit)
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

cl_object lex_env;

static void
bind_var(register cl_object var, register cl_object val)
{
	lex_env = CONS(var, CONS(val, lex_env));
}

static void
bind_function(cl_object name, cl_object fun)
{
	lex_env = CONS(@':function', CONS(CONS(name, fun), lex_env));
}

static void
bind_tagbody(cl_object id)
{
	lex_env = CONS(@':tag', CONS(id, lex_env));
}

static void
bind_block(cl_object name, cl_object id)
{
	lex_env = CONS(@':block', CONS(CONS(name, id), lex_env));
}

static void
bind_special(register cl_object var, register cl_object val)
{
	bds_bind(var, val);
}

static cl_object
search_local(register int s) {
	cl_object x;
	for (x = lex_env; s-- > 0 && !Null(x); x = CDDR(x));
	if (Null(x))
		FEerror("Internal error: local not found.", 0);
	return CADR(x);
}

static void
setq_local(register int s, register cl_object v) {
	cl_object x;
	for (x = lex_env; s-- > 0 && !Null(x); x = CDDR(x));
	if (Null(x))
		FEerror("Internal error: local ~S not found.", 1, s);
	CADR(x) = v;
}

static cl_object
search_tag(cl_object name, cl_object type)
{
	cl_object x;

	for (x = lex_env;  CONSP(x);  x = CDDR(x))
		if (CAR(x) == type) {
			cl_object record = CADR(x);
			cl_object the_name = CAR(record);
			cl_object the_value = CDR(record);
			if (name == the_name)
				return the_value;
		}
	return Cnil;
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

static cl_object *
lambda_bind(int narg, cl_object lambda_list, cl_index sp)
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
	  lambda_bind_var(next_code(data), cl_stack[sp++], specials);

	/* 2) OPTIONAL ARGUMENTS:  N var1 value1 flag1 ... varN valueN flagN */
	for (n = fix(next_code(data)); n; n--, data+=3) {
	  if (narg) {
	    lambda_bind_var(data[0], cl_stack[sp], specials);
	    sp++; narg--;
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
	    rest = CONS(cl_stack[sp+(--i)], rest);
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
	  for (; narg; narg-=2) {
	    cl_object key = cl_stack[sp++];
	    cl_object value = cl_stack[sp++];
	    if (!SYMBOLP(key))
	      FEprogram_error("LAMBDA: Keyword expected, got ~S.", 1, key);
	    keys = data;
	    for (i = 0; i < n; i++, keys += 4) {
	      if (key == keys[0]) {
		if (spp[i] == OBJNULL)
		  spp[i] = value;
		goto FOUND;
	      }
	    }
	    if (key != @':allow-other-keys')
	      other_found = TRUE;
	    else if (!allow_other_keys_found) {
	      allow_other_keys_found = TRUE;
	      other_keys = !Null(value);
	    }
	  FOUND:
	    (void)0;
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
lambda_apply(int narg, cl_object fun)
{
	cl_index args = cl_stack_index() - narg;
	cl_object output, name, *body;
	bds_ptr old_bds_top;
	struct ihs_frame ihs;

	if (type_of(fun) != t_bytecodes)
		FEinvalid_function(fun);

	/* 1) Save the lexical environment and set up a new one */
	ihs_push(&ihs, fun);
	lex_env = fun->bytecodes.lex;
	old_bds_top = bds_top;

	/* Establish bindings */
	body = lambda_bind(narg, fun, args);

	/* If it is a named lambda, set a block for RETURN-FROM */
	VALUES(0) = Cnil;
	NValues = 0;
	name = fun->bytecodes.data[0];
	if (Null(name))
		interpret(body);
	else {
		/* Accept (SETF name) */
		if (CONSP(name)) name = CADR(name);
		CL_BLOCK_BEGIN(id) {
			bind_block(name, id);
			interpret(body);
		} CL_BLOCK_END;
	}
	bds_unwind(old_bds_top);
	ihs_pop();
	returnn(VALUES(0));
}


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
search_global(register cl_object s) {
	cl_object x = SYM_VAL(s);
	if (x == OBJNULL)
		FEunbound_variable(s);
	return x;
}
		
/* Similar to funcall(), but registers calls in the IHS stack. */

static cl_object
interpret_funcall(int narg, cl_object fun) {
	cl_object *args;
	cl_object x;

	args = cl_stack_top - narg;
 AGAIN:
	switch (type_of(fun)) {
	case t_cfun: {
		struct ihs_frame ihs;
		ihs_push(&ihs, fun->cfun.name);
		if (fun->cfun.narg >= 0) {
			if (narg != fun->cfun.narg)
				check_arg_failed(narg, fun->cfun.narg);
			x = APPLY_fixed(narg, fun->cfun.entry, args);
		} else {
			x = APPLY(narg, fun->cfun.entry, args);
		}
		ihs_pop();
		break;
	}
	case t_cclosure:
		/* FIXME! Shouldn't we register this call somehow? */
		x = APPLY_closure(narg, fun->cclosure.entry, fun->cclosure.env, args);
		break;
#ifdef CLOS
	case t_gfun:
		fun = compute_method(narg, fun, args);
		goto AGAIN;
#endif
	case t_bytecodes:
		x = lambda_apply(narg, fun);
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

@(defun apply (fun lastarg &rest args)
	int i;
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

/* OP_BLOCK	label{arg}, block-name{symbol}
	...
   OP_EXIT
   label:

	Executes the enclosed code in a named block.
	LABEL points to the first instruction after OP_EXIT.
*/
static cl_object *
interpret_block(cl_object *vector) {
	cl_object * volatile exit = packed_label(vector - 1);
	CL_BLOCK_BEGIN(id) {
		bind_block(next_code(vector), id);
		vector = interpret(vector);
		lex_env = frs_top->frs_lex;
	} CL_BLOCK_END;
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

/* OP_TAGBODY	n{arg}
   tag1
   label1
   ...
   tagn
   labeln
label1:
   ...
labeln:
   ...
   OP_EXIT

	High level construct for the TAGBODY form.
*/
static cl_object *
interpret_tagbody(cl_object *vector) {
	cl_index i, ntags = get_oparg(vector[-1]);
	cl_object id = new_frame_id();
	cl_object *aux, *tag_list = vector;

	/* 1) Save current environment */
	cl_object old_lex_env = lex_env;

	/* 2) Bind tags */
	bind_tagbody(id);

	/* 3) Wait here for gotos. Each goto sets VALUES(0) to a integer
	      which ranges from 0 to ntags-1, depending on the tag. These
	      numbers are indices into the jump table and are computed
	      at compile time.
	*/
	aux = vector + ntags;
	if (frs_push(FRS_CATCH, id) != 0)
		aux = simple_label(vector + fix(VALUES(0)));
	vector = interpret(aux);
	frs_pop();

	/* 4) Restore environment */
	lex_env = old_lex_env;
	VALUES(0) = Cnil;
	NValues = 0;
	return vector;
}

/* OP_UNWIND	label
   ...		; code to be protected and whose value is output
   OP_EXIT
label:
   ...		; code executed at exit
   OP_EXIT
	High level construct for UNWIND-PROTECT. The first piece of code
	is executed and its output value is saved. Then the second piece
	of code is executed and the output values restored. The second
	piece of code is always executed, even if a THROW, RETURN or GO
	happen within the first piece of code.
*/
static cl_object *
interpret_unwind_protect(cl_object *vector) {
	cl_object * volatile exit = packed_label(vector-1);
	CL_UNWIND_PROTECT_BEGIN {
		interpret(vector);
	} CL_UNWIND_PROTECT_EXIT {
		exit = interpret(exit);
	} CL_UNWIND_PROTECT_END;
	return exit;
}

/* OP_DO	label
   ...		; code executed within a NIL block
   OP_EXIT
   label:

	High level construct for the DO and BLOCK forms.
*/
static cl_object *
interpret_do(cl_object *vector) {
	cl_object *volatile exit = packed_label(vector-1);
	CL_BLOCK_BEGIN(id) {
		bind_block(Cnil, id);
		interpret(vector);
		bds_unwind(frs_top->frs_bds_top);
		lex_env = frs_top->frs_lex;
	} CL_BLOCK_END;
	return exit;
}

/* OP_DOLIST	label
   ...		; code to bind the local variable
   OP_EXIT
   ...		; code executed on each iteration
   OP_EXIT
   ...		; code executed at the end
   OP_EXIT
   label:

	High level construct for the DOLIST iterator. The list over which
	we iterate is stored in VALUES(0).
*/
static cl_object *
interpret_dolist(cl_object *vector) {
	cl_object *volatile exit = packed_label(vector - 1);

	/* 1) Set NIL block */
	CL_BLOCK_BEGIN(id) {
		cl_object *output;
		cl_object list, var;

		bind_block(Cnil, id);
		list = VALUES(0);
		exit = packed_label(vector - 1);

		/* 2) Build list & bind variable*/
		vector = interpret(vector);
		output = packed_label(vector-1);

		/* 3) Repeat until list is exahusted */
		while (!endp(list)) {
			NValues = 1;
			VALUES(0) = CAR(list);
			interpret(vector);
			list = CDR(list);
		}
		VALUES(0) = Cnil;
		NValues = 1;
		interpret(output);

		/* 4) Restore environment */
		lex_env = frs_top->frs_lex;
		bds_unwind(frs_top->frs_bds_top);
	} CL_BLOCK_END;
	return exit;
}

/* OP_TIMES	label
   ...		; code to bind the local variable
   OP_EXIT
   ...		; code executed on each iteration
   OP_EXIT
   ...		; code executed at the end
   OP_EXIT
   label:

	High level construct for the DOTIMES iterator. The number of times
	we iterate is stored in VALUES(0).
*/
static cl_object *
interpret_dotimes(cl_object *vector) {
	cl_object *volatile exit = packed_label(vector - 1);
	CL_BLOCK_BEGIN(id) {
		cl_object *output;
		cl_fixnum length, i;
		cl_object var;

		/* 1) Set up a nil block */
		bind_block(Cnil, id);

		/* 2) Retrieve number and bind variables */
		length = fix(VALUES(0));
		exit = packed_label(vector - 1);
		vector = interpret(vector);
		output = packed_label(vector-1);

		/* 3) Loop while needed */
		for (i = 0; i < length;) {
			interpret(vector);
			NValues = 1;
			VALUES(0) = MAKE_FIXNUM(++i);
		}
		interpret(output);

		/* 4) Restore environment */
		lex_env = frs_top->frs_lex;
		bds_unwind(frs_top->frs_bds_top);
	} CL_BLOCK_END;
	return exit;
}

static cl_object
close_around(cl_object fun, cl_object lex) {
	cl_object v = cl_alloc_object(t_bytecodes);
	v->bytecodes.size = fun->bytecodes.size;
	v->bytecodes.data = fun->bytecodes.data;
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
static cl_object *
interpret_flet(cl_object *vector) {
	cl_index nfun = get_oparg(vector[-1]);

	/* 1) Copy the environment so that functions get it without references
	      to themselves. */
	cl_object lex = lex_env;

	/* 3) Add new closures to environment */
	while (nfun--) {
		cl_object fun = next_code(vector);
		cl_object f = close_around(fun,lex);
		bind_function(f->bytecodes.data[0], f);
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
static cl_object *
interpret_labels(cl_object *vector) {
	cl_index i, nfun = get_oparg(vector[-1]);
	cl_object l;

	/* 1) Build up a new environment with all functions */
	for (i=0; i<nfun; i++) {
		cl_object f = next_code(vector);
		bind_function(f->bytecodes.data[0], f);
	}

	/* 2) Update the closures so that all functions can call each other */
	for (i=0, l=lex_env; i<nfun; i++) {
		cl_object record = CADR(l);
		CDR(record) = close_around(CDR(record), lex_env);
		l = CDDR(l);
	}
	return vector;
}

/* OP_MCALL
   ...
   OP_EXIT

	Saves the stack pointer, executes the enclosed code and
	funcalls VALUE(0) using the content of the stack.
*/
static cl_object *
interpret_mcall(cl_object *vector) {
	cl_index sp = cl_stack_index();
	vector = interpret(vector);
	VALUES(0) = interpret_funcall(cl_stack_index()-sp, VALUES(0));
	return vector;
}

/* OP_MPROG1
   ...
   OP_EXIT

	Save the values in VALUES(..), execute the code enclosed, and
	restore the values.
*/
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

/* OP_MSETQ	n{arg}
   {fixnumn}|{symboln}
   ...
   {fixnum1}|{symbol1}

	Sets N variables to the N values in VALUES(), filling with
	NIL when there are values missing. Local variables are denoted
	with an integer which points a position in the lexical environment,
	while special variables are denoted just with the name.
*/
static cl_object *
interpret_msetq(cl_object *vector)
{
	cl_object var, value;
	int i = get_oparg(vector[-1]);
	while (i--) {
		var = next_code(vector);
		value = (i < NValues) ? VALUES(i) : Cnil;
		if (FIXNUMP(var))
			setq_local(fix(var), value);
		else {
			if (var->symbol.stype == stp_constant)
				FEassignment_to_constant(var);
			else
				SYM_VAL(var) = value;
		}
	}
	if (NValues > 1) NValues = 1;
	return vector;
}

/* OP_PROGV	bindings{list}
   ...
   OP_EXIT
	Execute the code enclosed with the special variables in BINDINGS
	set to the values in the list which was passed in VALUES(0).
*/
static cl_object *
interpret_progv(cl_object *vector) {
	cl_object values = VALUES(0);
	cl_object vars = cl_stack_pop();

	/* 1) Save current environment */
	bds_ptr old_bds_top = bds_top;
	cl_object old_lex_env = lex_env;

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
	vector = interpret(vector);

	/* 3) Restore environment */
	lex_env = old_lex_env;
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
	/* OP_QUOTE
		Sets VALUES(0) to an immediate value.
	*/
	case OP_QUOTE:
		VALUES(0) = next_code(vector);
		NValues = 1;
		break;

	/* OP_VAR	n{arg}, var{symbol}
		Sets NValues=1 and VALUES(0) to the value of the n-th local.
		VAR is the name of the variable for readability purposes.
	*/
	case OP_VAR: {
		int lex_env_index = get_oparg(s);
		VALUES(0) = search_local(lex_env_index);
		NValues = 1;
		break;
	}

	/* OP_VARS	var{symbol}
		Sets NValues=1 and VALUES(0) to the value of the symbol VAR.
		VAR should be either a special variable or a constant.
	*/
	case OP_VARS: {
		cl_object var_name = next_code(vector);
		VALUES(0) = search_global(var_name);
		NValues = 1;
		break;
	}

	/* OP_PUSH
		Pushes the object in VALUES(0).
	*/
	case OP_PUSH:
		cl_stack_push(VALUES(0));
		break;

	/* OP_PUSHV	n{arg}
		Pushes the value of the n-th local onto the stack.
	*/
	case OP_PUSHV: {
		int lex_env_index = get_oparg(s);
		cl_stack_push(search_local(lex_env_index));
		break;
	}

	/* OP_PUSHVS	var{symbol}
		Pushes the value of the symbol VAR onto the stack.
		VAR should be either a special variable or a constant.
	*/
	case OP_PUSHVS: {
		cl_object var_name = next_code(vector);
		cl_stack_push(search_global(var_name));
		break;
	}

	/* OP_PUSHQ	value{object}
		Pushes "value" onto the stack.
	*/
	case OP_PUSHQ:
		cl_stack_push(next_code(vector));
		break;

	/* OP_CALLG	n{arg}, function-name{symbol}
		Calls the global function with N arguments which have
		been deposited in the stack. The output values are
		left in VALUES(...)
	*/
	case OP_CALLG: {
		cl_fixnum n = get_oparg(s);
		cl_object fun = next_code(vector);
		if (fun->symbol.gfdef == OBJNULL)
			FEundefined_function(fun);
		VALUES(0) = interpret_funcall(n, fun->symbol.gfdef);
		break;
	}

	/* OP_FCALL	n{arg}
		Calls the function in VALUES(0) with N arguments which
		have been deposited in the stack. The output values
		are left in VALUES(...)
	*/
	case OP_FCALL: {
		cl_fixnum n = get_oparg(s);
		cl_object fun = VALUES(0);
		VALUES(0) = interpret_funcall(n, fun);
		break;
	}

	/* OP_PCALLG	n{arg}, function-name{symbol}
		Calls the global function with N arguments which have
		been deposited in the stack. The first output value is
		left on the stack.
	*/
	case OP_PCALLG: {
		cl_fixnum n = get_oparg(s);
		cl_object fun = next_code(vector);
		if (fun->symbol.gfdef == OBJNULL)
			FEundefined_function(fun);
		VALUES(0) = interpret_funcall(n, fun->symbol.gfdef);
		cl_stack_push(VALUES(0));
		break;
	}

	/* OP_PFCALL	n{arg}
		Calls the function in VALUES(0) with N arguments which
		have been deposited in the stack. The first output value
		is pushed on the stack.
	*/
	case OP_PFCALL: {
		cl_fixnum n = get_oparg(s);
		cl_object fun = VALUES(0);
		VALUES(0) = interpret_funcall(n, fun);
		cl_stack_push(VALUES(0));
		break;
	}

	/* OP_EXIT
		Marks the end of a high level construct (BLOCK, CATCH...)
	*/
	case OP_EXIT:
		return vector;

	/* OP_NOP
		Sets VALUES(0) = NIL and NValues = 1
	*/   		
	case OP_NOP:
		VALUES(0) = Cnil;
		NValues = 0;
		break;

	/* OP_HALT
		Marks the end of a function.
	*/
	case OP_HALT:
		return vector-1;
	case OP_FLET:
		vector = interpret_flet(vector);
		break;
	case OP_LABELS:
		vector = interpret_labels(vector);
		break;

	/* OP_LFUNCTION	n{arg}, function-name{symbol}
		Calls the local or global function with N arguments
		which have been deposited in the stack.
	*/
	case OP_LFUNCTION: {
		int lex_env_index = get_oparg(s);
		cl_object fun_record = search_local(lex_env_index);
		cl_object fun_object = CDR(fun_record);
		VALUES(0) = fun_object;
		NValues = 1;
		break;
	}

	/* OP_FUNCTION	name{symbol}
		Extracts the function associated to a symbol. The function
		may be defined in the global environment or in the local
		environment. This last value takes precedence.
	*/
	case OP_FUNCTION: {
		cl_object fun_name = next_code(vector);
		cl_object fun_object = SYM_FUN(fun_name);
		if (fun_object == OBJNULL || fun_name->symbol.mflag)
			FEundefined_function(fun_name);
		VALUES(0) = fun_object;
		NValues = 1;
		break;
	}
	/* OP_CLOSE	name{symbol}
		Extracts the function associated to a symbol. The function
		may be defined in the global environment or in the local
		environment. This last value takes precedence.
	*/
	case OP_CLOSE: {
		cl_object function_object = next_code(vector);
		VALUES(0) = close_around(function_object, lex_env);
		NValues = 1;
		break;
	}
	/* OP_GO	n{arg}, tag-name{symbol}
		Jumps to the tag which is defined at the n-th position in
		the lexical environment. TAG-NAME is kept for debugging
		purposes.
	*/
	case OP_GO: {
		cl_object tag_name = next_code(vector);
		cl_object id = search_local(get_oparg(s));
		VALUES(0) = Cnil;
		NValues = 0;
		cl_go(id, tag_name);
		break;
	}
	/* OP_RETURN	n{arg}
		Returns from the block whose record in the lexical environment
		occuppies the n-th position.
	*/
	case OP_RETURN: {
		int lex_env_index = get_oparg(s);
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
	   OP_JEQ	label{arg}, value{object}
	   OP_JNEQ	label{arg}, value{object}
		Direct or conditional jumps. The conditional jumps are made
		comparing with the value of VALUES(0).
	*/
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
	/* OP_UNBIND	n{arg}
		Undo "n" local bindings.
	*/
	case OP_UNBIND: {
		cl_index n = get_oparg(s);
		while (n--)
			lex_env = CDDR(lex_env);
		break;
	}
	/* OP_UNBINDS	n{arg}
		Undo "n" bindings of special variables.
	*/
	case OP_UNBINDS: {
		cl_index n = get_oparg(s);
		bds_unwind_n(n);
		break;
	}
	/* OP_BIND	name{symbol}
	   OP_PBIND	name{symbol}
	   OP_BINDS	name{symbol}
	   OP_PBINDS	name{symbol}
		Binds a lexical or special variable to the either the
		value of VALUES(0) or the first value of the stack.
	*/
	case OP_BIND: {
		cl_object var_name = next_code(vector);
		cl_object value = VALUES(0);
		bind_var(var_name, value);
		break;
	}
	case OP_PBIND: {
		cl_object var_name = next_code(vector);
		cl_object value = cl_stack_pop();
		bind_var(var_name, value);
		break;
	}
	case OP_VBIND: {
		int n = get_oparg(s);
		cl_object var_name = next_code(vector);
		cl_object value = (--n < NValues) ? VALUES(n) : Cnil;
		bind_var(var_name, value);
		break;
	}
	case OP_BINDS: {
		cl_object var_name = next_code(vector);
		cl_object value = VALUES(0);
		bind_special(var_name, value);
		break;
	}
	case OP_PBINDS: {
		cl_object var_name = next_code(vector);
		cl_object value = cl_stack_pop();
		bind_special(var_name, value);
		break;
	}
	case OP_VBINDS: {
		int n = get_oparg(s);
		cl_object var_name = next_code(vector);
		cl_object value = (--n < NValues) ? VALUES(n) : Cnil;
		bind_special(var_name, value);
		break;
	}
	/* OP_SETQ	n{arg}
	   OP_PSETQ	n{arg}
	   OP_SETQS	var-name{symbol}
	   OP_PSETQS	var-name{symbol}
		Sets either the n-th local or a special variable VAR-NAME,
		to either the value in VALUES(0) (OP_SETQ[S]) or to the 
		first value on the stack (OP_PSETQ[S]).
	*/
	case OP_SETQ: {
		int lex_env_index = get_oparg(s);
		setq_local(lex_env_index, VALUES(0));
		break;
	}
	case OP_SETQS: {
		cl_object var = next_code(vector);
		if (var->symbol.stype == stp_constant)
			FEassignment_to_constant(var);
		else
			SYM_VAL(var) = VALUES(0);
		break;
	}
	case OP_PSETQ: {
		int lex_env_index = get_oparg(s);
		setq_local(lex_env_index, cl_stack_pop());
		Values[0] = Cnil;
		NValues = 1;
		break;
	}
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

	case OP_BLOCK:
		vector = interpret_block(vector);
		break;
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
	case OP_MCALL:
		vector = interpret_mcall(vector);
		break;
	case OP_CATCH:
		vector = interpret_catch(vector);
		break;
	case OP_MSETQ:
		vector = interpret_msetq(vector);
		break;
	case OP_MPROG1:
		vector = interpret_mprog1(vector);
		break;
	case OP_PROGV:
		vector = interpret_progv(vector);
		break;
	/* OP_PUSHVALUES
		Pushes the values output by the last form.
	*/
	case OP_PUSHVALUES: {
		int i;
		for (i=0; i<NValues; i++)
			cl_stack_push(VALUES(i));
		break;
	}

	/* OP_VALUES	n{arg}
		Pop N values from the stack and store them in VALUES(...)
	*/
	case OP_VALUES: {
		cl_fixnum n = get_oparg(s);
		NValues = n;
		while (n)
			VALUES(--n) = cl_stack_pop();
		break;
	}
	/* OP_NTHVAL
		Set VALUES(0) to the N-th value of the VALUES(...) list.
		The index N-th is extracted from the top of the stack.
	*/
	case OP_NTHVAL: {
		cl_fixnum n = fix(cl_stack_pop());
		if (n < 0 || n >= NValues)
			VALUES(0) = Cnil;
		else
			VALUES(0) = VALUES(n);
		NValues = 1;
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

void
init_interpreter(void)
{
	cl_stack = NULL;
	cl_stack_size = 0;
	cl_stack_set_size(16*LISP_PAGESIZE);
}
