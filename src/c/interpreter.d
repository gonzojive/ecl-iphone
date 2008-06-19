/* -*- mode: c; c-basic-offset: 8 -*- */
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

#include <ecl/ecl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ecl/ecl-inl.h>
#include <ecl/bytecodes.h>

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
	cl_dealloc(cl_env.stack);
#endif
	cl_env.stack_size = new_size;
	cl_env.stack = new_stack;
	cl_env.stack_top = cl_env.stack + top;
	cl_env.stack_limit = cl_env.stack + (new_size - 2);

	/* A stack always has at least one element. This is assumed by cl__va_start
	 * and friends, which take a sp=0 to have no arguments.
	 */
	if (top == 0)
		cl_stack_push(MAKE_FIXNUM(0));

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

cl_object
ecl_stack_frame_open(cl_object f, cl_index size)
{
	cl_object *top = cl_env.stack_top;
	if (size) {
		if (cl_env.stack_limit - top < size) {
			cl_index delta = (size + (LISP_PAGESIZE-1))/LISP_PAGESIZE;
			cl_stack_set_size(cl_env.stack_size + delta * LISP_PAGESIZE);
			top = cl_env.stack_top;
		}
	}
	f->frame.t = t_frame;
	f->frame.stack = cl_env.stack;
	f->frame.bottom = top;
	cl_env.stack_top = f->frame.top = (top + size);
	return f;
}

void
ecl_stack_frame_enlarge(cl_object f, cl_index size)
{
	cl_object *top;
	if (f->frame.stack == 0) {
		ecl_internal_error("Inconsistency in interpreter stack frame");
	}
	top = cl_env.stack_top;
	if ((cl_env.stack_limit - top) < size) {
		cl_index delta = (size + (LISP_PAGESIZE-1))/LISP_PAGESIZE;
		cl_stack_set_size(cl_env.stack_size + delta * LISP_PAGESIZE);
		f->frame.bottom = (f->frame.bottom - f->frame.stack) + cl_env.stack;
		f->frame.stack = cl_env.stack;
		top = cl_env.stack_top;
	} else if (top != f->frame.top) {
		f->frame.bottom = (f->frame.bottom - f->frame.stack) + cl_env.stack;
		f->frame.stack = cl_env.stack;
		top = cl_env.stack_top;
	}
	cl_env.stack_top = f->frame.top = (top + size);
}

void
ecl_stack_frame_push(cl_object f, cl_object o)
{
	cl_object *top;
	if (f->frame.stack == 0) {
		ecl_internal_error("Inconsistency in interpreter stack frame");
	}
	top = cl_env.stack_top;
	if (top >= cl_env.stack_limit) {
		cl_stack_grow();
		f->frame.bottom = (f->frame.bottom - f->frame.stack) + cl_env.stack;
		f->frame.stack = cl_env.stack;
		top = cl_env.stack_top;
	} else if (top != f->frame.top) {
		f->frame.bottom = (f->frame.bottom - f->frame.stack) + cl_env.stack;
		f->frame.stack = cl_env.stack;
		top = cl_env.stack_top;
	}
	*(top++) = o;
	cl_env.stack_top = f->frame.top = top;
}

void
ecl_stack_frame_push_values(cl_object f)
{
	if (f->frame.stack == 0) {
		ecl_internal_error("Inconsistency in interpreter stack frame");
	}
	cl_stack_push_values();
	f->frame.bottom = (f->frame.bottom - f->frame.stack) + cl_env.stack;
	f->frame.stack = cl_env.stack;
	f->frame.top = cl_env.stack_top;
}

cl_object
ecl_stack_frame_pop_values(cl_object f)
{
	cl_index n = f->frame.top - f->frame.bottom;
	NVALUES = n;
	VALUES(0) = Cnil;
	while (n--) {
		VALUES(n) = f->frame.bottom[n];
	}
	return VALUES(0);
}

cl_object
ecl_stack_frame_elt(cl_object f, cl_index ndx)
{
	if (ndx >= (f->frame.top - f->frame.bottom)) {
		FEtype_error_index(f, ecl_make_unsigned_integer(ndx));
	}
	return f->frame.bottom[ndx];
}

void
ecl_stack_frame_elt_set(cl_object f, cl_index ndx, cl_object o)
{
	if (ndx >= (f->frame.top - f->frame.bottom)) {
		FEtype_error_index(f, ecl_make_unsigned_integer(ndx));
	}
	f->frame.bottom[ndx] = o;
}

cl_object
ecl_stack_frame_from_va_list(cl_object frame, cl_va_list args)
{
	cl_index nargs = args[0].narg;
	ecl_stack_frame_open(frame, nargs);
	while (nargs) {
		*(frame->frame.top-nargs) = cl_va_arg(args);
		nargs--;
	}
	return frame;
}

void
ecl_stack_frame_close(cl_object f)
{
	if (f->frame.stack) {
		cl_stack_set_index(f->frame.bottom - f->frame.stack);
	}
}

cl_object
ecl_stack_frame_copy(cl_object dest, cl_object orig)
{
	cl_index size = orig->frame.top - orig->frame.bottom;
	dest = ecl_stack_frame_open(dest, size);
	memcpy(dest->frame.bottom, orig->frame.bottom, size * sizeof(cl_object));
	return dest;
}


/* ------------------------------ LEXICAL ENV. ------------------------------ */

#define bind_var(env, var, val)		CONS(CONS(var, val), (env))
#define bind_function(env, name, fun) 	CONS(CONS(fun, name), (env))
#define bind_tagbody(env, id) 		CONS(CONS(id, MAKE_FIXNUM(0)), (env))

static cl_object
ecl_lex_env_get_record(register cl_object env, register int s)
{
	do {
		if (s-- == 0) return ECL_CONS_CAR(env);
		env = ECL_CONS_CDR(env);
	} while(1);
}

#define ecl_lex_env_get_var(env,x) ECL_CONS_CDR(ecl_lex_env_get_record(env,x))
#define ecl_lex_env_set_var(env,x,v) ECL_RPLACD(ecl_lex_env_get_record(env,x),(v))
#define ecl_lex_env_get_fun(env,x) ECL_CONS_CAR(ecl_lex_env_get_record(env,x))
#define ecl_lex_env_get_tag(env,x) ECL_CONS_CAR(ecl_lex_env_get_record(env,x))

/* -------------------- LAMBDA FUNCTIONS -------------------- */

static cl_object
lambda_bind_var(cl_object env, cl_object var, cl_object val, cl_object specials)
{
	if (!ecl_member_eq(var, specials))
		env = bind_var(env, var, val);
	else
		bds_bind(var, val);
	return env;
}

static cl_object
lambda_bind(cl_object env, cl_narg narg, cl_object lambda, cl_object *sp)
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
		env = lambda_bind_var(env, *(data++), *(sp++), specials);

	/* 2) OPTIONAL ARGUMENTS:  N var1 value1 flag1 ... varN valueN flagN */
	for (n = fix(*(data++)); n; n--, data+=3) {
		if (narg) {
			env = lambda_bind_var(env, data[0], *sp, specials);
			sp++; narg--;
			if (!Null(data[2])) {
				env = lambda_bind_var(env, data[2], Ct, specials);
			}
		} else {
			cl_object defaults = data[1];
			if (FIXNUMP(defaults)) {
				defaults = ecl_interpret(env, lambda, (cl_opcode*)lambda->bytecodes.code + fix(defaults));
			}
			env = lambda_bind_var(env, data[0], defaults, specials);
			if (!Null(data[2])) {
				env = lambda_bind_var(env, data[2], Cnil, specials);
			}
		}
	}
	
	/* 3) REST ARGUMENT: {rest-var | NIL} */
	if (!Null(data[0])) {
		cl_object rest = Cnil;
		check_remaining = FALSE;
		for (i=narg; i; ) {
			rest = CONS(sp[--i], rest);
		}
		env = lambda_bind_var(env, data[0], rest, specials);
	}
	data++;

	/* 4) ALLOW-OTHER-KEYS: { T | NIL | 0} */
	if (data[0] == MAKE_FIXNUM(0)) {
		data++;
		if (narg && check_remaining) {
			FEprogram_error("LAMBDA: Too many arguments to function ~S.", 1,
					lambda->bytecodes.name);
		}
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
			cl_object key = *(sp++);
			cl_object value = *(sp++);
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
		if (other_found && !allow_other_keys) {
			FEprogram_error("LAMBDA: Unknown keys found in function ~S.",
					1, lambda->bytecodes.name);
		}
		for (i=0; i<n; i++, data+=4) {
			if (spp[i] != unbound) {
				env = lambda_bind_var(env, data[1],spp[i],specials);
			} else {
				cl_object defaults = data[2];
				if (FIXNUMP(defaults)) {
					defaults = ecl_interpret(env, lambda, (cl_opcode*)lambda->bytecodes.code + fix(defaults));
				}
				env = lambda_bind_var(env, data[1],defaults,specials);
			}
			if (!Null(data[3])) {
				env = lambda_bind_var(env, data[3],(spp[i] != unbound)? Ct : Cnil,specials);
			}
		}
	}
	return env;
}

cl_object
ecl_apply_lambda(cl_object frame, cl_object fun)
{
	cl_object name, env;
	bds_ptr old_bds_top;

	if (type_of(fun) != t_bytecodes)
		FEinvalid_function(fun);

	/* Save the lexical environment and set up a new one */
	old_bds_top = cl_env.bds_top;

	/* Establish bindings */
	env = lambda_bind(Cnil, frame->frame.top - frame->frame.bottom, fun, frame->frame.bottom);

	VALUES(0) = Cnil;
	NVALUES = 0;
	name = fun->bytecodes.name;
	fun = ecl_interpret(env, fun, fun->bytecodes.code);
	bds_unwind(old_bds_top);
	return fun;
}


cl_object
ecl_apply_bclosure(cl_object frame, cl_object fun)
{
	cl_object name, env;
	bds_ptr old_bds_top;

	if (type_of(fun) != t_bclosure)
		FEinvalid_function(fun);

	/* Save the lexical environment and set up a new one */
	env = fun->bclosure.lex;
	fun = fun->bclosure.code;
	old_bds_top = cl_env.bds_top;

	/* Establish bindings */
	env = lambda_bind(env, frame->frame.top - frame->frame.bottom, fun, frame->frame.bottom);

	VALUES(0) = Cnil;
	NVALUES = 0;
	name = fun->bytecodes.name;
	fun = ecl_interpret(env, fun, fun->bytecodes.code);
	bds_unwind(old_bds_top);
	return fun;
}


/* -------------------- AIDS TO THE INTERPRETER -------------------- */

static cl_object
search_global(register cl_object s) {
	cl_object x = SYM_VAL(s);
	if (x == OBJNULL)
		FEunbound_variable(s);
	return x;
}

static cl_object
close_around(cl_object fun, cl_object lex) {
	cl_object v = cl_alloc_object(t_bclosure);
	v->bclosure.code = fun;
	v->bclosure.lex = lex;
	return v;
}

#undef frs_pop
#define frs_pop(the_env) { \
	the_env->stack_top = the_env->stack + the_env->frs_top->frs_sp; \
	the_env->frs_top--; }

/*
 * Manipulation of the interpreter stack. As shown here, we omit may
 * security checks, assuming that the interpreted code is consistent.
 * This is done for performance reasons, but could probably be undone
 * using a configuration flag.
 */

#define STACK_PUSH(the_env,x) { \
	cl_object __aux = (x); \
	if (the_env->stack_top == the_env->stack_limit) { \
		cl_stack_grow(); \
	} \
	*(the_env->stack_top++) = __aux; }

/*
 * INTERPRET-FUNCALL is one of the few ways to "exit" the interpreted
 * environment and get into the C/lisp world. Since almost all data
 * from the interpreter is kept in local variables, and frame stacks,
 * binding stacks, etc, are already handled by the C core, only the
 * lexical environment needs to be saved.
 */

#define INTERPRET_FUNCALL(reg0, the_env, frame, narg, fun) {	\
	cl_index __n = narg; \
	frame.stack = the_env->stack; \
	frame.top = the_env->stack_top; \
	frame.bottom = frame.top - __n; \
	reg0 = ecl_apply_from_stack_frame((cl_object)&frame, fun); \
	the_env->stack_top -= __n; }

/* -------------------- THE INTERPRETER -------------------- */

cl_object
ecl_interpret(cl_object env, cl_object bytecodes, void *pc)
{
	ECL_OFFSET_TABLE;
	typedef struct cl_env_struct *cl_env_ptr;
	const cl_env_ptr the_env = &cl_env;
	cl_opcode *vector = pc;
	cl_object reg0 = the_env->values[0], reg1;
	struct ecl_stack_frame frame_aux;
	struct ihs_frame ihs;
	ihs_push(&ihs, bytecodes, env);
#define lex_env ihs.lex_env
	frame_aux.t = t_frame;
	frame_aux.stack = frame_aux.top = frame_aux.bottom = 0;
 BEGIN:
	BEGIN_SWITCH {
	CASE(OP_NOP); {
		the_env->values[0] = reg0 = Cnil;
		the_env->nvalues = 0;
		THREAD_NEXT;
	}
	/* OP_QUOTE
		Sets REG0 to an immediate value.
	*/
	CASE(OP_QUOTE); {
		reg0 = GET_DATA(vector, bytecodes);
		THREAD_NEXT;
	}
	/* OP_VAR	n{arg}, var{symbol}
		Sets REG0 to the value of the n-th local.
		VAR is the name of the variable for readability purposes.
	*/
	CASE(OP_VAR); {
		int lex_env_index = GET_OPARG(vector);
		reg0 = ecl_lex_env_get_var(lex_env, lex_env_index);
		THREAD_NEXT;
	}

	/* OP_VARS	var{symbol}
		Sets REG0 to the value of the symbol VAR.
		VAR should be either a special variable or a constant.
	*/
	CASE(OP_VARS); {
		cl_object var_name = GET_DATA(vector, bytecodes);
		reg0 = search_global(var_name);
		THREAD_NEXT;
	}

	/* OP_PUSH
		Pushes the object in VALUES(0).
	*/
	CASE(OP_PUSH); {
		STACK_PUSH(the_env, reg0);
		THREAD_NEXT;
	}
	/* OP_PUSHV	n{arg}
		Pushes the value of the n-th local onto the stack.
	*/
	CASE(OP_PUSHV); {
		int lex_env_index = GET_OPARG(vector);
		STACK_PUSH(the_env, ecl_lex_env_get_var(lex_env, lex_env_index));
		THREAD_NEXT;
	}

	/* OP_PUSHVS	var{symbol}
		Pushes the value of the symbol VAR onto the stack.
		VAR should be either a special variable or a constant.
	*/
	CASE(OP_PUSHVS); {
		cl_object var_name = GET_DATA(vector, bytecodes);
		STACK_PUSH(the_env, search_global(var_name));
		THREAD_NEXT;
	}

	/* OP_PUSHQ	value{object}
		Pushes "value" onto the stack.
	*/
	CASE(OP_PUSHQ); {
		STACK_PUSH(the_env, GET_DATA(vector, bytecodes));
		THREAD_NEXT;
	}
	/* OP_CALL	n{arg}
		Calls the function in REG0 with N arguments which
		have been deposited in the stack. The output values
		are left in VALUES(...)
	*/
	CASE(OP_CALL); {
		cl_fixnum n = GET_OPARG(vector);
		INTERPRET_FUNCALL(reg0, the_env, frame_aux, n, reg0);
		the_env->values[0] = reg0;
		THREAD_NEXT;
	}

	/* OP_CALLG	n{arg}, name{arg}
		Calls the function NAME with N arguments which have been
		deposited in the stack. The output values are left in VALUES.
	*/
	CASE(OP_CALLG); {
		cl_fixnum n = GET_OPARG(vector);
		cl_object f = GET_DATA(vector, bytecodes);
		INTERPRET_FUNCALL(reg0, the_env, frame_aux, n, f);
		the_env->values[0] = reg0;
		THREAD_NEXT;
	}

	/* OP_FCALL	n{arg}
		Calls a function in the stack with N arguments which
		have been also deposited in the stack. The output values
		are left in VALUES(...)
	*/
	CASE(OP_FCALL); {
		cl_fixnum n = GET_OPARG(vector);
		cl_object fun = the_env->stack_top[-n-1];
		INTERPRET_FUNCALL(reg0, the_env, frame_aux, n, fun);
		the_env->values[0] = reg0;
		cl_stack_pop();
		THREAD_NEXT;
	}

	/* OP_MCALL
		Similar to FCALL, but gets the number of arguments from
		the stack (They all have been deposited by OP_PUSHVALUES)
	*/
	CASE(OP_MCALL); {
		cl_fixnum n = fix(cl_stack_pop());
		cl_object fun = the_env->stack_top[-n-1];
		INTERPRET_FUNCALL(reg0, the_env, frame_aux, n, fun);
		the_env->values[0] = reg0;
		cl_stack_pop();
		THREAD_NEXT;
	}

	/* OP_PCALL	n{arg}
		Calls the function in REG0 with N arguments which
		have been deposited in the stack. The first output value
		is pushed on the stack.
	*/
	CASE(OP_PCALL); {
		cl_fixnum n = GET_OPARG(vector);
		INTERPRET_FUNCALL(reg0, the_env, frame_aux, n, reg0);
		STACK_PUSH(the_env, reg0);
		THREAD_NEXT;
	}

	/* OP_PCALLG	n{arg}, name{arg}
		Calls the function NAME with N arguments which have been
		deposited in the stack. The first output value is pushed on
		the stack.
	*/
	CASE(OP_PCALLG); {
		cl_fixnum n = GET_OPARG(vector);
		cl_object f = GET_DATA(vector, bytecodes);
		INTERPRET_FUNCALL(f, the_env, frame_aux, n, f);
		STACK_PUSH(the_env, f);
		THREAD_NEXT;
	}

	/* OP_PFCALL	n{arg}
		Calls the function in the stack with N arguments which
		have been also deposited in the stack. The first output value
		is pushed on the stack.
	*/
	CASE(OP_PFCALL); {
		cl_fixnum n = GET_OPARG(vector);
		cl_object fun = the_env->stack_top[-n-1];
		INTERPRET_FUNCALL(fun, the_env, frame_aux, n, fun);
		the_env->stack_top[-1] = fun;
		THREAD_NEXT;
	}

	/* OP_EXIT
		Marks the end of a high level construct (BLOCK, CATCH...)
		or a function.
	*/
	CASE(OP_EXIT); {
		ihs_pop();
		return VALUES(0);
	}
	/* OP_FLET	nfun{arg}, fun1{object}
	   ...
	   OP_UNBIND nfun
	   
	   Executes the enclosed code in a lexical enviroment extended with
	   the functions "fun1" ... "funn". Note that we only record the
	   index of the first function: the others are after this one.
	*/
	CASE(OP_FLET); {
		cl_index nfun = GET_OPARG(vector);
		cl_index first = GET_OPARG(vector);
		cl_object *fun = bytecodes->bytecodes.data + first;
		/* Copy the environment so that functions get it without references
		   to themselves, and then add new closures to the environment. */
		cl_object old_lex = lex_env;
		cl_object new_lex = old_lex;
		while (nfun--) {
			cl_object f = close_around(*(fun++), old_lex);
			new_lex = bind_function(new_lex, f->bytecodes.name, f);
		}
		lex_env = new_lex;
		THREAD_NEXT;
	}
	/* OP_LABELS	nfun{arg}
	   fun1{object}
	   ...
	   funn{object}
	   ...
	   OP_UNBIND n

	   Executes the enclosed code in a lexical enviroment extended with
	   the functions "fun1" ... "funn".
	*/
	CASE(OP_LABELS); {
		cl_index i, nfun = GET_OPARG(vector);
		cl_index first = GET_OPARG(vector);
		cl_object *fun = bytecodes->bytecodes.data + first;
		cl_object l, new_lex;
		/* Build up a new environment with all functions */
		for (new_lex = lex_env, i = nfun; i; i--) {
			cl_object f = *(fun++);
			new_lex = bind_function(new_lex, f->bytecodes.name, f);
		}
		/* Update the closures so that all functions can call each other */
		;
		for (l = new_lex, i = nfun; i; i--) {
			cl_object record = ECL_CONS_CAR(l);
			ECL_RPLACA(record, close_around(ECL_CONS_CAR(record), new_lex));
			l = ECL_CONS_CDR(l);
		}
		lex_env = new_lex;
		THREAD_NEXT;
	}
	/* OP_LFUNCTION	n{arg}, function-name{symbol}
		Calls the local or global function with N arguments
		which have been deposited in the stack.
	*/
	CASE(OP_LFUNCTION); {
		int lex_env_index = GET_OPARG(vector);
		cl_object fun_record = ecl_lex_env_get_record(lex_env, lex_env_index);
		reg0 = CAR(fun_record);
		THREAD_NEXT;
	}

	/* OP_FUNCTION	name{symbol}
		Extracts the function associated to a symbol. The function
		may be defined in the global environment or in the local
		environment. This last value takes precedence.
	*/
	CASE(OP_FUNCTION);
		reg0 = ecl_fdefinition(GET_DATA(vector, bytecodes));
		THREAD_NEXT;

	/* OP_CLOSE	name{symbol}
		Extracts the function associated to a symbol. The function
		may be defined in the global environment or in the local
		environment. This last value takes precedence.
	*/
	CASE(OP_CLOSE); {
		cl_object function_object = GET_DATA(vector, bytecodes);
		reg0 = close_around(function_object, lex_env);
		THREAD_NEXT;
	}
	/* OP_GO	n{arg}
	   OP_QUOTE	tag-name{symbol}
		Jumps to the tag which is defined at the n-th position in
		the lexical environment. TAG-NAME is kept for debugging
		purposes.
	*/
	CASE(OP_GO); {
		cl_object id = ecl_lex_env_get_tag(lex_env, GET_OPARG(vector));
		cl_object tag_name = GET_DATA(vector, bytecodes);
		cl_go(id, tag_name);
		THREAD_NEXT;
	}
	/* OP_RETURN	n{arg}
		Returns from the block whose record in the lexical environment
		occuppies the n-th position.
	*/
	CASE(OP_RETURN); {
		int lex_env_index = GET_OPARG(vector);
		cl_object block_record = ecl_lex_env_get_record(lex_env, lex_env_index);
		cl_object id = CAR(block_record);
		cl_object block_name = CDR(block_record);
		cl_return_from(id, block_name);
		THREAD_NEXT;
	}
	/* OP_THROW
		Jumps to an enclosing CATCH form whose tag matches the one
		of the THROW. The tag is taken from the stack, while the
		output values are left in VALUES(...).
	*/
	CASE(OP_THROW); {
		cl_object tag_name = cl_stack_pop();
		cl_throw(tag_name);
		THREAD_NEXT;
	}
	/* OP_JMP	label{arg}
	   OP_JNIL	label{arg}
	   OP_JT	label{arg}
	   OP_JEQ	value{object}, label{arg}
	   OP_JNEQ	value{object}, label{arg}
		Direct or conditional jumps. The conditional jumps are made
		comparing with the value of REG0.
	*/
	CASE(OP_JMP); {
		cl_oparg jump = GET_OPARG(vector);
		vector += jump - OPARG_SIZE;
		THREAD_NEXT;
	}
	CASE(OP_JNIL); {
		cl_oparg jump = GET_OPARG(vector);
		if (Null(reg0))
			vector += jump - OPARG_SIZE;
		THREAD_NEXT;
	}
	CASE(OP_JT); {
		cl_oparg jump = GET_OPARG(vector);
		if (!Null(reg0))
			vector += jump - OPARG_SIZE;
		THREAD_NEXT;
	}
	CASE(OP_JEQL); {
		cl_oparg value = GET_OPARG(vector);
		cl_oparg jump = GET_OPARG(vector);
		if (ecl_eql(reg0, bytecodes->bytecodes.data[value]))
			vector += jump - OPARG_SIZE;
		THREAD_NEXT;
	}
	CASE(OP_JNEQL); {
		cl_oparg value = GET_OPARG(vector);
		cl_oparg jump = GET_OPARG(vector);
		if (!ecl_eql(reg0, bytecodes->bytecodes.data[value]))
			vector += jump - OPARG_SIZE;
		THREAD_NEXT;
	}
	CASE(OP_NOT); {
		reg0 = (reg0 == Cnil)? Ct : Cnil;
		THREAD_NEXT;
	}
	/* OP_UNBIND	n{arg}
		Undo "n" local bindings.
	*/
	CASE(OP_UNBIND); {
		cl_index n = GET_OPARG(vector);
		while (n--)
			lex_env = ECL_CONS_CDR(lex_env);
		THREAD_NEXT;
	}
	/* OP_UNBINDS	n{arg}
		Undo "n" bindings of special variables.
	*/
	CASE(OP_UNBINDS); {
		cl_index n = GET_OPARG(vector);
		bds_unwind_n(n);
		THREAD_NEXT;
	}
	/* OP_BIND	name{symbol}
	   OP_PBIND	name{symbol}
	   OP_BINDS	name{symbol}
	   OP_PBINDS	name{symbol}
		Binds a lexical or special variable to the either the
		value of REG0 or the first value of the stack.
	*/
	CASE(OP_BIND); {
		cl_object var_name = GET_DATA(vector, bytecodes);
		lex_env = bind_var(lex_env, var_name, reg0);
		THREAD_NEXT;
	}
	CASE(OP_PBIND); {
		cl_object var_name = GET_DATA(vector, bytecodes);
		cl_object value = cl_stack_pop();
		lex_env = bind_var(lex_env, var_name, value);
		THREAD_NEXT;
	}
	CASE(OP_VBIND); {
		cl_index n = GET_OPARG(vector);
		cl_object var_name = GET_DATA(vector, bytecodes);
		cl_object value = (n < the_env->nvalues) ? the_env->values[n] : Cnil;
		lex_env = bind_var(lex_env, var_name, value);
		THREAD_NEXT;
	}
	CASE(OP_BINDS); {
		cl_object var_name = GET_DATA(vector, bytecodes);
		bds_bind(var_name, reg0);
		THREAD_NEXT;
	}
	CASE(OP_PBINDS); {
		cl_object var_name = GET_DATA(vector, bytecodes);
		cl_object value = cl_stack_pop();
		bds_bind(var_name, value);
		THREAD_NEXT;
	}
	CASE(OP_VBINDS); {
		cl_index n = GET_OPARG(vector);
		cl_object var_name = GET_DATA(vector, bytecodes);
		cl_object value = (n < the_env->nvalues) ? the_env->values[n] : Cnil;
		bds_bind(var_name, value);
		THREAD_NEXT;
	}
	/* OP_SETQ	n{arg}
	   OP_PSETQ	n{arg}
	   OP_SETQS	var-name{symbol}
	   OP_PSETQS	var-name{symbol}
		Sets either the n-th local or a special variable VAR-NAME,
		to either the value in REG0 (OP_SETQ[S]) or to the 
		first value on the stack (OP_PSETQ[S]).
	*/
	CASE(OP_SETQ); {
		int lex_env_index = GET_OPARG(vector);
		ecl_lex_env_set_var(lex_env, lex_env_index, reg0);
		THREAD_NEXT;
	}
	CASE(OP_SETQS); {
		cl_object var = GET_DATA(vector, bytecodes);
		/* INV: Not NIL, and of type t_symbol */
		if (var->symbol.stype & stp_constant)
			FEassignment_to_constant(var);
		ECL_SETQ(var, reg0);
		THREAD_NEXT;
	}
	CASE(OP_PSETQ); {
		int lex_env_index = GET_OPARG(vector);
		ecl_lex_env_set_var(lex_env, lex_env_index, cl_stack_pop());
		THREAD_NEXT;
	}
	CASE(OP_PSETQS); {
		cl_object var = GET_DATA(vector, bytecodes);
		/* INV: Not NIL, and of type t_symbol */
		ECL_SETQ(var, cl_stack_pop());
		THREAD_NEXT;
	}

	/* OP_BLOCK	label{arg}
	   ...
	   OP_EXIT_FRAME
	 label:

	   Executes the enclosed code in a named block.
	   LABEL points to the first instruction after OP_EXIT.
	*/
	CASE(OP_BLOCK); {
		reg0 = GET_DATA(vector, bytecodes);
		reg1 = new_frame_id();
		goto DO_BLOCK;
	}
	/* OP_CATCH	label{arg}
	   ...
	   OP_EXIT_FRAME
	   label:

	   Sets a catch point using the tag in VALUES(0). LABEL points to the
	   first instruction after the end (OP_EXIT) of the block
	*/
	CASE(OP_CATCH); {
		reg1 = reg0;
		goto DO_BLOCK;
	}
	/* OP_DO	label
	     ...	; code executed within a NIL block
	   OP_EXIT_FRAME
	   label:

	   High level construct for the DO and BLOCK forms.
	*/
	CASE(OP_DO); {
		reg0 = Cnil;
		reg1 = new_frame_id();
	}
	DO_BLOCK: {
		cl_opcode *exit;
		GET_LABEL(exit, vector);
		STACK_PUSH(the_env, lex_env);
		STACK_PUSH(the_env, (cl_object)exit);
		if (frs_push(reg1) == 0) {
			lex_env = CONS(CONS(reg1, reg0), lex_env);
		} else {
			reg0 = the_env->values[0];
			frs_pop(the_env);
			vector = (cl_opcode *)cl_stack_pop(); /* FIXME! */
			lex_env = cl_stack_pop();
		}
		THREAD_NEXT;
	}
	CASE(OP_EXIT_FRAME); {
		bds_unwind(the_env->frs_top->frs_bds_top);
		frs_pop(the_env);
		cl_stack_pop();
		lex_env = cl_stack_pop();
		THREAD_NEXT;
	}
	/* OP_TAGBODY	n{arg}
	     label1
	     ...
	     labeln
	   label1:
	     ...
	   labeln:
	     ...
	   OP_EXIT_TAGBODY

	   High level construct for the TAGBODY form.
	*/
	CASE(OP_TAGBODY); {
		cl_object id = new_frame_id();
		int n = GET_OPARG(vector);
		/* Here we save the location of the jump table and the env. */
		lex_env = bind_tagbody(lex_env, id);
		STACK_PUSH(the_env, lex_env);
		STACK_PUSH(the_env, (cl_object)vector); /* FIXME! */
		if (frs_push(id) == 0) {
			/* The first time, we "name" the tagbody and
			 * skip the jump table */
			vector += n * OPARG_SIZE;
		} else {
			/* Wait here for gotos. Each goto sets
			   VALUES(0) to an integer which ranges from 0
			   to ntags-1, depending on the tag. These
			   numbers are indices into the jump table and
			   are computed at compile time. */
			cl_opcode *table = (cl_opcode *)the_env->stack_top[-1];
			table = table + fix(the_env->values[0]) * OPARG_SIZE;
			vector = table + *(cl_oparg *)table;
			lex_env = the_env->stack_top[-2];
		}
		THREAD_NEXT;
	}
	CASE(OP_EXIT_TAGBODY); {
		frs_pop(the_env);
		cl_stack_pop();
		lex_env = ECL_CONS_CDR(cl_stack_pop());
	}
	CASE(OP_NIL); {
		reg0 = Cnil;
		THREAD_NEXT;
	}
	CASE(OP_PUSHNIL); {
		STACK_PUSH(the_env, Cnil);
		THREAD_NEXT;
	}
	CASE(OP_VALUEREG0); {
		the_env->values[0] = reg0;
		the_env->nvalues = 1;
		THREAD_NEXT;
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
	CASE(OP_MSETQ); {
		cl_object value;
		cl_index i, n = GET_OPARG(vector), nv = the_env->nvalues;
		for (i=0; i<n; i++) {
			cl_fixnum var = GET_OPARG(vector);
			value = (i < nv) ? the_env->values[i] : Cnil;
			if (var >= 0) {
				ecl_lex_env_set_var(lex_env, var, value);
			} else {
				cl_object name = bytecodes->bytecodes.data[-1-var];
				if (Null(name) || (name->symbol.stype & stp_constant)) {
					FEassignment_to_constant(name);
				}
				ECL_SETQ(name, value);
			}
		}
		if (nv == 0) {
			reg0 = Cnil;
		} else {
			reg0 = the_env->values[0];
		}
		THREAD_NEXT;
	}

	/* OP_PUSHVALUES
		Pushes the values output by the last form, plus the number
		of values.
	*/
	PUSH_VALUES:
	CASE(OP_PUSHVALUES); {
		cl_index i;
		for (i=0; i<the_env->nvalues; i++)
			STACK_PUSH(the_env, the_env->values[i]);
		STACK_PUSH(the_env, MAKE_FIXNUM(the_env->nvalues));
		THREAD_NEXT;
	}
	/* OP_PUSHMOREVALUES
		Adds more values to the ones pushed by OP_PUSHVALUES.
	*/
	CASE(OP_PUSHMOREVALUES); {
		cl_index i, n = fix(cl_stack_pop());
		for (i=0; i<the_env->nvalues; i++)
			STACK_PUSH(the_env, the_env->values[i]);
		STACK_PUSH(the_env, MAKE_FIXNUM(n + the_env->nvalues));
		THREAD_NEXT;
	}
	/* OP_POP
		Pops a singe value pushed by a OP_PUSH* operator.
	*/
	CASE(OP_POP); {
		reg0 = cl_stack_pop();
		THREAD_NEXT;
	}
	/* OP_POPVALUES
		Pops all values pushed by a OP_PUSHVALUES operator.
	*/
	CASE(OP_POPVALUES); {
		cl_object *dest = the_env->values;
		cl_object *sp = the_env->stack_top;
		int n = the_env->nvalues = fix(*(--sp));
		if (n == 0) {
			*dest = reg0 = Cnil;
			the_env->stack_top = sp;
			THREAD_NEXT;
		} else if (n == 1) {
			*dest = reg0 = *(--sp);
			the_env->stack_top = sp;
			THREAD_NEXT;
		} else {
			sp -= n;
			memcpy(dest, sp, n * sizeof(cl_object));
			reg0 = *dest;
			the_env->stack_top = sp;
			THREAD_NEXT;
		}
	}
	/* OP_VALUES	n{arg}
		Pop N values from the stack and store them in VALUES(...)
		Note that N is strictly > 0.
	*/
	CASE(OP_VALUES); {
		cl_fixnum n = GET_OPARG(vector);
		cl_object *sp = the_env->stack_top - n;
		cl_object *dest = the_env->values;
		the_env->nvalues = n;
		memcpy(dest, sp, n * sizeof(cl_object));
		reg0 = *dest;
		the_env->stack_top = sp;
		THREAD_NEXT;
	}
	/* OP_NTHVAL
		Set VALUES(0) to the N-th value of the VALUES(...) list.
		The index N-th is extracted from the top of the stack.
	*/
	CASE(OP_NTHVAL); {
		cl_fixnum n = fix(cl_stack_pop());
		if (n < 0) {
			FEerror("Wrong index passed to NTH-VAL", 1, MAKE_FIXNUM(n));
		} else if ((cl_index)n >= the_env->nvalues) {
			reg0 = Cnil;
		} else {
			reg0 = the_env->values[n];
		}
		THREAD_NEXT;
	}
	/* OP_PROTECT	label
	     ...	; code to be protected and whose value is output
	   OP_PROTECT_NORMAL
	   label:
	     ...	; code executed at exit
	   OP_PROTECT_EXIT

	  High level construct for UNWIND-PROTECT. The first piece of code is
	  executed and its output value is saved. Then the second piece of code
	  is executed and the output values restored. The second piece of code
	  is always executed, even if a THROW, RETURN or GO happen within the
	  first piece of code.
	*/
	CASE(OP_PROTECT); {
		cl_opcode *exit;
		GET_LABEL(exit, vector);
		STACK_PUSH(the_env, lex_env);
		STACK_PUSH(the_env, (cl_object)exit);
		if (frs_push(ECL_PROTECT_TAG) != 0) {
			frs_pop(the_env);
			vector = (cl_opcode *)cl_stack_pop();
			lex_env = cl_stack_pop();
			STACK_PUSH(the_env, MAKE_FIXNUM(the_env->nlj_fr - the_env->frs_top));
			goto PUSH_VALUES;
		}
		THREAD_NEXT;
	}
	CASE(OP_PROTECT_NORMAL); {
		bds_unwind(the_env->frs_top->frs_bds_top);
		frs_pop(the_env);
		cl_stack_pop();
		lex_env = cl_stack_pop();
		STACK_PUSH(the_env, MAKE_FIXNUM(1));
		goto PUSH_VALUES;
	}
	CASE(OP_PROTECT_EXIT); {
		volatile cl_fixnum n = the_env->nvalues = fix(cl_stack_pop());
		while (n--)
			the_env->values[n] = cl_stack_pop();
		reg0 = the_env->values[0];
		n = fix(cl_stack_pop());
		if (n <= 0)
			ecl_unwind(the_env->frs_top + n);
		THREAD_NEXT;
	}

	/* OP_PROGV	bindings{list}
	   ...
	   OP_EXIT
	   Execute the code enclosed with the special variables in BINDINGS
	   set to the values in the list which was passed in VALUES(0).
	*/
	CASE(OP_PROGV); {
		cl_object values = reg0;
		cl_object vars = cl_stack_pop();
		cl_index n;
		for (n = 0; !ecl_endp(vars); n++, vars = ECL_CONS_CDR(vars)) {
			cl_object var = ECL_CONS_CAR(vars);
			if (values == Cnil) {
				bds_bind(var, OBJNULL);
			} else {
				bds_bind(var, cl_car(values));
				values = ECL_CONS_CDR(values);
			}
		}
		STACK_PUSH(the_env, MAKE_FIXNUM(n));
		THREAD_NEXT;
	}
	CASE(OP_EXIT_PROGV); {
		cl_index n = fix(cl_stack_pop());
		bds_unwind_n(n);
		THREAD_NEXT;
	}

	CASE(OP_STEPIN); {
		cl_object form = GET_DATA(vector, bytecodes);
		cl_object a = SYM_VAL(@'si::*step-action*');
		cl_index n = cl_stack_push_values();
		if (a == Ct) {
			/* We are stepping in, but must first ask the user
			 * what to do. */
			ECL_SETQ(@'si::*step-level*',
				 cl_1P(SYM_VAL(@'si::*step-level*')));
			STACK_PUSH(the_env, form);
			INTERPRET_FUNCALL(form, the_env, frame_aux, 1, @'si::stepper');
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
		THREAD_NEXT;
	}
	CASE(OP_STEPCALL); {
		/* We are going to call a function. However, we would
		 * like to step _in_ the function. STEPPER takes care of
		 * that. */
		cl_fixnum n = GET_OPARG(vector);
		if (SYM_VAL(@'si::*step-action*') == Ct) {
			STACK_PUSH(the_env, reg0);
			INTERPRET_FUNCALL(reg0, the_env, frame_aux, 1, @'si::stepper');
		}
		INTERPRET_FUNCALL(reg0, the_env, frame_aux, n, reg0);
	}
	CASE(OP_STEPOUT); {
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
		THREAD_NEXT;
	}
	}
}

@(defun si::interpreter_stack ()
@
	@(return Cnil)
@)
