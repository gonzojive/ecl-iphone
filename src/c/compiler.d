/*
    compiler.c -- Bytecode compiler
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

/********************* EXPORTS *********************/

cl_object @'lambda-block';
cl_object @'declare';
cl_object @'defun';
cl_object @'compile', @'load', @'eval', @'progn', @'warn', @'typep', @'otherwise';
cl_object @':execute', @':compile-toplevel', @':load-toplevel';
cl_object @'si::*inhibit-macro-special*';

cl_object @'&optional';
cl_object @'&rest';
cl_object @'&key';
cl_object @'&allow-other-keys';
cl_object @'&aux';

cl_object @':allow-other-keys';

cl_object bytecodes;
int lexical_level;

/********************* PRIVATE ********************/

static cl_index asm_begin(void);
static cl_object asm_end(cl_index);
static void asm_clear(cl_index);
static void asm_grow(void);
static void asm1(register cl_object op);
static void asm_op(register int n);
static void asm_list(register cl_object l);
static void asmn(int narg, ...);
static void asm_at(register cl_index where, register cl_object what);
static cl_index asm_jmp(register int op);
static void asm_complete(register int op, register cl_index original);
static cl_index current_pc();
static void set_pc(cl_index pc);
static cl_object asm_ref(register cl_index where);

static void c_and(cl_object args);
static void c_block(cl_object args);
static void c_case(cl_object args);
static void c_catch(cl_object args);
static void c_compiler_let(cl_object args);
static void c_cond(cl_object args);
static void c_do(cl_object args);
static void c_doa(cl_object args);
static void c_dolist(cl_object args);
static void c_dotimes(cl_object args);
static void c_eval_when(cl_object args);
static void c_flet(cl_object args);
static void c_function(cl_object args);
static void c_go(cl_object args);
static void c_if(cl_object args);
static void c_labels(cl_object args);
static void c_let(cl_object args);
static void c_leta(cl_object args);
static void c_locally(cl_object args);
static void c_macrolet(cl_object args);
static void c_multiple_value_bind(cl_object args);
static void c_multiple_value_call(cl_object args);
static void c_multiple_value_prog1(cl_object args);
static void c_multiple_value_setq(cl_object args);
static void c_nth_value(cl_object args);
static void c_or(cl_object args);
static void c_progv(cl_object args);
static void c_psetq(cl_object args);
static void c_values(cl_object args);
static void c_setq(cl_object args);
static void c_return(cl_object args);
static void c_return_from(cl_object args);
static void c_symbol_macrolet(cl_object args);
static void c_tagbody(cl_object args);
static void c_throw(cl_object args);
static void c_unless(cl_object args);
static void c_unwind_protect(cl_object args);
static void c_when(cl_object args);
static void compile_body(cl_object args);
static void compile_form(cl_object args, bool push);

static void FEillegal_variable_name(cl_object) __attribute__((noreturn));
static void FEill_formed_input() __attribute__((noreturn));

/* -------------------- SAFE LIST HANDLING -------------------- */

static cl_object
pop(cl_object *l) {
	cl_object head, list = *l;
	if (ATOM(list))
		FEill_formed_input();
	head = CAR(list);
	*l = CDR(list);
	return head;
}

static cl_object
pop_maybe_nil(cl_object *l) {
	cl_object head, list = *l;
	if (list == Cnil)
		return Cnil;
	if (ATOM(list))
		FEill_formed_input();
	head = CAR(list);
	*l = CDR(list);
	return head;
}

/* ------------------------------ ASSEMBLER ------------------------------ */

static cl_index
asm_begin(void) {
	/* Save beginning of bytecodes for this session */
	return current_pc();
}

static void
asm_clear(cl_index beginning) {
	cl_index i;
	/* Remove data from this session */
	bytecodes->vector.fillp = beginning;
}

static cl_object
asm_end(cl_index beginning) {
	cl_object new_bytecodes;
	cl_index length, bytes, i;

	/* Save bytecodes from this session in a new vector */
	length = current_pc() - beginning;
	bytes = length * sizeof(cl_object);
	new_bytecodes = alloc_object(t_bytecodes);
	new_bytecodes->bytecodes.lex = Cnil;
	new_bytecodes->bytecodes.data = alloc(bytes);
	new_bytecodes->bytecodes.size = length;
	memcpy(new_bytecodes->bytecodes.data,
	       &bytecodes->vector.self.t[beginning],
	       bytes);

	asm_clear(beginning);
	return new_bytecodes;
}

static void
asm_grow(void) {
	cl_object *old_data = bytecodes->vector.self.t;
	cl_index old_size = bytecodes->vector.fillp;
	bytecodes->vector.dim += 128;
	array_allocself(bytecodes);
	memcpy(bytecodes->vector.self.t, old_data, old_size*sizeof(cl_object));
}

static void
asm1(register cl_object op) {
	int where = bytecodes->vector.fillp;
	if (where >= bytecodes->vector.dim)
		asm_grow();
	bytecodes->vector.self.t[where] = op;
	bytecodes->vector.fillp++;
}

static void
asm_op(register int n) {
	asm1(MAKE_FIXNUM(n));
}

static void
asm_op2(register int code, register cl_fixnum n) {
	cl_object op = MAKE_FIXNUM(code);
	cl_object new_op = SET_OPARG(op, n);
	if (n < -MAX_OPARG || MAX_OPARG < n)
		FEprogram_error("Argument to bytecode is too large", 0);
	else
		asm1(new_op);
}

static inline cl_object
make_op(int code) {
	return MAKE_FIXNUM(code);
}

static cl_object
make_op2(int code, cl_fixnum n) {
	cl_object volatile op = MAKE_FIXNUM(code);
	cl_object new_op = SET_OPARG(op, n);
	if (n < -MAX_OPARG || MAX_OPARG < n)
		FEprogram_error("Argument to bytecode is too large", 0);
	return new_op;
}

static void
asm_insert(cl_fixnum where, cl_object op) {
	cl_fixnum end = bytecodes->vector.fillp;
	if (where > end)
		FEprogram_error("asm1_insert: position out of range", 0);
	if (end >= bytecodes->vector.dim)
		asm_grow();
	memmove(&bytecodes->vector.self.t[where+1],
		&bytecodes->vector.self.t[where],
		(end - where) * sizeof(cl_object));
	bytecodes->vector.fillp++;
	bytecodes->vector.self.t[where] = op;
}

static void
asm_list(register cl_object l) {
	if (ATOM(l))
		asm1(l);
	while(!endp(l)) {
		asm1(CAR(l));
		l = CDR(l);
	}
}

static void
asmn(int narg, ...) {
	va_list args;

	va_start(args, narg);
	while (narg-- > 0)
		asm1(va_arg(args, cl_object));
}

static void
asm_at(register cl_index where, register cl_object what) {
	if (where > bytecodes->vector.fillp)
		FEprogram_error("Internal error at asm_at()",0);
	bytecodes->vector.self.t[where] = what;
}

static cl_index
asm_block(void) {
	cl_index output;
	output = current_pc();
	asm1(MAKE_FIXNUM(0));
	return output;
}	

static cl_index
asm_jmp(register int op) {
	cl_index output = current_pc();
	asm_op(op);
	return output;
}

static void
asm_complete(register int op, register cl_index original) {
	cl_fixnum delta = current_pc() - original;
	cl_object code = asm_ref(original);
	cl_object new_code = SET_OPARG(code, delta);
	if (code != MAKE_FIXNUM(op))
		FEprogram_error("Non matching codes in ASM-COMPLETE2", 0);
	else if (delta < -MAX_OPARG || delta > MAX_OPARG)
		FEprogram_error("Too large jump", 0);
	else
		asm_at(original, new_code);
}

static cl_index
current_pc(void) {
	return bytecodes->vector.fillp;
}

static void
set_pc(cl_index pc) {
	bytecodes->vector.fillp = pc;
}

static cl_object
asm_ref(register cl_index n) {
	return bytecodes->vector.self.t[n];
}

/* ------------------------------ COMPILER ------------------------------ */

typedef struct {
  cl_object symbol;
  const char *const name;
  void (*compiler)(cl_object);
  int lexical_increment;
} compiler_record;

static compiler_record database[] = {
  {OBJNULL, "AND", c_and, 1},
  {OBJNULL, "BLOCK", c_block, 1},
  {OBJNULL, "CASE", c_case, 1},
  {OBJNULL, "CATCH", c_catch, 1},
  {OBJNULL, "COMPILER-LET", c_compiler_let, 0},
  {OBJNULL, "COND", c_cond, 1},
  {OBJNULL, "DO", c_do, 1},
  {OBJNULL, "DO*", c_doa, 1},
  {OBJNULL, "DOLIST", c_dolist, 1},
  {OBJNULL, "DOTIMES", c_dotimes, 1},
  {OBJNULL, "EVAL-WHEN", c_eval_when, 0},
  {OBJNULL, "FLET", c_flet, 1},
  {OBJNULL, "FUNCTION", c_function, 1},
  {OBJNULL, "GO", c_go, 1},
  {OBJNULL, "IF", c_if, 1},
  {OBJNULL, "LABELS", c_labels, 1},
  {OBJNULL, "LET", c_let, 1},
  {OBJNULL, "LET*", c_leta, 1},
  {OBJNULL, "LOCALLY", c_locally, 0},
  {OBJNULL, "MACROLET", c_macrolet, 0},
  {OBJNULL, "MULTIPLE-VALUE-BIND", c_multiple_value_bind, 1},
  {OBJNULL, "MULTIPLE-VALUE-CALL", c_multiple_value_call, 1},
  {OBJNULL, "MULTIPLE-VALUE-PROG1", c_multiple_value_prog1, 1},
  {OBJNULL, "MULTIPLE-VALUE-SETQ", c_multiple_value_setq, 1},
  {OBJNULL, "NTH-VALUE", c_nth_value, 1},
  {OBJNULL, "OR", c_or, 1},
  {OBJNULL, "PROGN", compile_body, 0},
  {OBJNULL, "PROGV", c_progv, 1},
  {OBJNULL, "PSETQ", c_psetq, 1},
  {OBJNULL, "RETURN", c_return, 1},
  {OBJNULL, "RETURN-FROM", c_return_from, 1},
  {OBJNULL, "SETQ", c_setq, 1},
  {OBJNULL, "SYMBOL-MACROLET", c_symbol_macrolet, 0},
  {OBJNULL, "TAGBODY", c_tagbody, 1},
  {OBJNULL, "THROW", c_throw, 1},
  {OBJNULL, "UNWIND-PROTECT", c_unwind_protect, 1},
  {OBJNULL, "UNLESS", c_unless, 1},
  {OBJNULL, "VALUES", c_values, 1},
  {OBJNULL, "WHEN", c_when, 1},
  {OBJNULL, "", c_when, 1}
};

/* ----------------- LEXICAL ENVIRONMENT HANDLING -------------------- */

static void
FEillegal_variable_name(cl_object v)
{
	FEprogram_error("Not a valid variable name ~S.", 1, v);
}

static void
FEill_formed_input()
{
	FEprogram_error("Unproper list handled to the compiler.", 0);
}

static void
c_register_var(register cl_object var, bool special)
{
	CAR(lex_env) = CONS(CONS(var, special? @'special' : Cnil), CAR(lex_env));
}

static bool
special_variablep(register cl_object var, register cl_object specials)
{
	return ((var->symbol.stype == stp_special) || member_eq(var, specials));
}

static void
c_pbind(cl_object var, cl_object specials)
{
	if (!SYMBOLP(var))
		FEillegal_variable_name(var);
	else if (special_variablep(var, specials)) {
		c_register_var(var, TRUE);
		asm_op(OP_PBINDS);
	} else {
		c_register_var(var, FALSE);
		asm_op(OP_PBIND);
	}
	asm1(var);
}

static void
c_bind(cl_object var, cl_object specials)
{
	if (!SYMBOLP(var))
		FEillegal_variable_name(var);
	else if (special_variablep(var, specials)) {
		c_register_var(var, TRUE);
		asm_op(OP_BINDS);
	} else {
		c_register_var(var, FALSE);
		asm_op(OP_BIND);
	}
	asm1(var);
}

static void
compile_setq(int op, cl_object var)
{
	cl_object ndx;

	if (!SYMBOLP(var))
		FEillegal_variable_name(var);
	ndx = lex_var_sch(var);
	if (!Null(ndx) && CDR(ndx) != @'special')
		asm_op(op); /* Lexical variable */
	else if (var->symbol.stype == stp_constant)
		FEassignment_to_constant(var);
	else if (op == OP_SETQ)
		asm_op(OP_SETQS); /* Special variable */
	else
		asm_op(OP_PSETQS); /* Special variable */
	asm1(var);
}

/* -------------------- THE COMPILER -------------------- */

static void
c_and(cl_object args) {
	if (Null(args)) {
		asm1(Ct);
		return;
	} else if (ATOM(args)) {
		FEill_formed_input();
	} else {
		compile_form(pop(&args),FALSE);
		if (!endp(args)) {
			cl_index label = asm_jmp(OP_JNIL);
			c_and(args);
			asm_complete(OP_JNIL, label);
		}
	}
}

/*
	The OP_BLOCK operator encloses several forms within a block
	named BLOCK_NAME, thus catching any OP_RETFROM whose argument
	matches BLOCK_NAME. The end of this block is marked both by
	the OP_EXIT operator and the LABELZ which is packed within
	the OP_BLOCK operator.
	
		[OP_BLOCK + labelz]
		block_name
		....
		OP_EXIT
	labelz:	...
*/

static void
c_block(cl_object body) {
	cl_object name = pop(&body);
	cl_index labelz = asm_jmp(OP_BLOCK);
	if (!SYMBOLP(name))
		FEprogram_error("BLOCK: Not a valid block name, ~S", 1, name);
	asm1(name);
	compile_body(body);
	asm_op(OP_EXIT);
	asm_complete(OP_BLOCK, labelz);
}

/*
	There are several ways to invoke functions and to handle the
	output arguments. These are

		[OP_CALL + nargs]
		function_name

		[OP_PCALL + nargs]
		function_name

		[OP_FCALL + nargs]

		[OP_PFCALL + nargs]

	 OP_CALL and OP_FCALL leave all arguments in the VALUES() array,
	 while OP_PCALL and OP_PFCALL leave the first argument in the
	 stack.

	 OP_CALL and OP_PCALL use the following symbol to retrieve the
	 function, while OP_FCALL and OP_PFCALL use the value in VALUES(0).
 */
static void
c_call(cl_object args, bool push) {
	cl_object name;
	cl_index nargs;

	name = pop(&args);
	for (nargs = 0; !endp(args); nargs++) {
		compile_form(pop(&args),TRUE);
	}
	if (ATOM(name)) {
		asm_op2(push? OP_PCALL : OP_CALL, nargs);
		asm1(name);
	} else if (CAR(name) == @'lambda') {
		asm_op(OP_CLOSE);
		asm1(make_lambda(Cnil, CDR(name)));
		asm_op2(push? OP_PFCALL : OP_FCALL, nargs);
	} else {
		cl_object aux = setf_namep(name);
		if (aux == OBJNULL)
			FEprogram_error("FUNCALL: Invalid function name ~S.",
					1, name);
		asm_op2(push? OP_PCALL : OP_CALL, nargs);
		asm1(aux);
	}
}

static void
c_funcall(cl_object args, bool push) {
	cl_object name;
	cl_index nargs;

	name = pop(&args);
	for (nargs = 0; !endp(args); nargs++) {
		compile_form(pop(&args),TRUE);
	}
	compile_form(name, FALSE);
	asm_op2(push? OP_PFCALL : OP_FCALL, nargs);
}

static void
perform_c_case(cl_object args) {
	cl_object test, clause, conseq;
	cl_fixnum label1, label2;

	if (Null(args)) {
		asm_op(OP_NOP);
		return;
	}

	clause = pop(&args);
	if (ATOM(clause))
		FEprogram_error("CASE: Illegal clause ~S.",1,clause);
	test = pop(&clause);
	if (@'otherwise' == test || test == Ct) {
		compile_body(clause);
	} else {
		cl_index labeln, labelz;
		if (CONSP(test)) {
			cl_index n = length(test);
			while (n > 1) {
				cl_object v = pop(&test);
				cl_fixnum jump = (n--) * 2;
				asm_op2(OP_JEQ, jump);
				asm1(v);
			}
			test = CAR(test);
		}
		labeln = asm_jmp(OP_JNEQ);
		asm1(test);
		compile_body(clause);
		labelz = asm_jmp(OP_JMP);
		asm_complete(OP_JNEQ, labeln);
		perform_c_case(args);
		asm_complete(OP_JMP, labelz);
	}
}

static void
c_case(cl_object clause) {
	compile_form(pop(&clause), FALSE);
	perform_c_case(clause);
}

/*
	The OP_CATCH takes the object in VALUES(0) and uses it to catch
	any OP_THROW operation which uses that value as argument. If a
	catch occurs, or when all forms have been properly executed, it
	jumps to LABELZ. LABELZ is packed within the OP_CATCH operator.
		[OP_CATCH + labelz]
		...
		"forms to be caught"
		...
	       	OP_EXIT
	labelz:	...
*/

static void
c_catch(cl_object args) {
	cl_index labelz;

	/* Compile evaluation of tag */
	compile_form(pop(&args), FALSE);

	/* Compile jump point */
	labelz = asm_jmp(OP_CATCH);

	/* Compile body of CATCH */
	compile_body(args);
	asm_op(OP_EXIT);
	asm_complete(OP_CATCH, labelz);
}

static void
c_compiler_let(cl_object args) {
	cl_object bindings;
	bds_ptr old_bds_top = bds_top;

	for (bindings = pop(&args); !endp(bindings); ) {
		cl_object form = pop(&bindings);
		cl_object var = pop(&form);
		cl_object value = pop_maybe_nil(&form);
		bds_bind(var, value);
	}
	compile_body(args);
	bds_unwind(old_bds_top);
}

/*
	There are three operators which perform explicit jumps, but
	almost all other operators use labels in one way or
	another.

	1) Jumps are always relative to the place where the jump label
	is retrieved so that if the label is in vector[0], then the
	destination is roughly vector + vector[0].

	2) There are two types of labels, "packed labels" and "simple
	labels". The first ones are packed in the upper bits of an
	operator so that
		destination = vector + vector[0]>>16
	Simple labels take the whole word and thus
		destination = vector + fix(vector[0])

	3) The three jump forms are

		[OP_JMP + label]	; Unconditional jump
		[OP_JNIL + label]	; Jump if VALUES(0) == Cnil
		[OP_JT + label]		; Jump if VALUES(0) != Cnil

	It is important to remark that both OP_JNIL and OP_JT truncate
	the values stack, so that always NValues = 1 after performing
	any of these operations.
*/
static void
c_cond(cl_object args) {
	cl_object test, clause, conseq;
	cl_fixnum label_nil, label_exit;

	clause = pop(&args);
	if (ATOM(clause))
		FEprogram_error("COND: Illegal clause ~S.",1,clause);
	test = pop(&clause);
	if (Ct == test) {
		/* Default sentence. If no forms, just output T. */
		if (Null(clause))
			compile_form(Ct, FALSE);
		else
			compile_body(clause);
	} else {
		/* Compile the test. If no more forms, just output
		   the first value (this is guaranteed by OP_JNIL */
		compile_form(test, FALSE);
		label_nil = asm_jmp(OP_JNIL);
		if (!Null(clause))
			compile_body(clause);
		if (Null(args))
			asm_complete(OP_JNIL, label_nil);
		else {
			label_exit = asm_jmp(OP_JMP);
			asm_complete(OP_JNIL, label_nil);
			c_cond(args);
			asm_complete(OP_JMP, label_exit);
		}
	}
}

/*	The OP_DO operator saves the lexical environment and establishes
	a NIL block to execute the enclosed forms, which are typically
	like the ones shown below. At the exit of the block, either by
	means of a OP_RETFROM jump or because of normal termination,
	the lexical environment is restored, and all bindings undone.

		[OP_DO + labelz]
		labelz
		...	; bindings
	labelb:	...	; body
		...	; stepping forms
	labelt:	...	; test form
		[JNIL + label]
		...	; output form
		OP_EXIT
	labelz:

*/
static void
c_do_doa(int op, cl_object args) {
	cl_object bindings, test, specials, body, l;
	cl_object stepping = Cnil, vars = Cnil;
	cl_index labelb, labelt, labelz;
	cl_object lex_old = lex_env;
	lex_copy();

	bindings = pop(&args);
	test = pop(&args);

	@si::process-declarations(1, args);
	body = VALUES(1);
	specials = VALUES(3);

	labelz = asm_jmp(OP_DO);

	/* Compile initial bindings */
	if (length(bindings) == 1)
		op = OP_BIND;
	for (l=bindings; !endp(l); ) {
		cl_object aux = pop(&l);
		cl_object var, value;
		if (ATOM(aux)) {
			var = aux;
			value = Cnil;
		} else {
			var = pop(&aux);
			value = pop_maybe_nil(&aux);
			if (!endp(aux))
				stepping = CONS(CONS(var,pop(&aux)),stepping);
			if (!Null(aux))
				FEprogram_error("LET: Ill formed declaration.", 0);
		}
		if (!SYMBOLP(var))
			FEillegal_variable_name(var);
		if (op == OP_PBIND) {
			compile_form(value, TRUE);
			vars = CONS(var, vars);
		} else {
			compile_form(value, FALSE);
			c_bind(var, specials);
		}
	}
	while (!endp(vars))
		c_pbind(pop(&vars), specials);

	/* Jump to test */
	labelt = asm_jmp(OP_JMP);

	/* Compile body */
	labelb = current_pc();
	c_tagbody(body);

	/* Compile stepping clauses */
	if (length(stepping) == 1)
		op = OP_BIND;
	for (vars = Cnil, stepping=nreverse(stepping); !endp(stepping); ) {
		cl_object pair = pop(&stepping);
		cl_object var = CAR(pair);
		cl_object value = CDR(pair);
		if (op == OP_PBIND) {
			compile_form(value, TRUE);
			vars = CONS(var, vars);
		} else {
			compile_form(value, FALSE);
			compile_setq(OP_SETQ, var);
		}
	}
	while (!endp(vars))
		compile_setq(OP_PSETQ, pop(&vars));

	/* Compile test */
	asm_complete(OP_JMP, labelt);
	compile_form(pop(&test), FALSE);
	asm_op2(OP_JNIL, labelb - current_pc());

	/* Compile output clauses */
	compile_body(test);
	asm_op(OP_EXIT);

	/* Compile return point of block */
	asm_complete(OP_DO, labelz);

	lex_env = lex_old;
}


static void
c_doa(cl_object args) {
	c_do_doa(OP_BIND, args);
}

static void
c_do(cl_object args) {
	c_do_doa(OP_PBIND, args);
}

/*
	The OP_DOLIST & OP_DOTIMES operators save the lexical
	environment and establishes a NIL block to execute the
	enclosed forms, which iterate over the elements in a list or
	over a range of integer numbers. At the exit of the block,
	either by means of a OP_RETFROM jump or because of normal
	termination, the lexical environment is restored, and all
	bindings undone.

		[OP_DOTIMES/OP_DOLIST + labelz]
		...	; bindings
		[OP_EXIT + labelo]
		...	; body
		...	; stepping forms
		OP_EXIT
	labelo:	...	; output form
		OP_EXIT
	labelz:

 */

static void
c_dolist_dotimes(int op, cl_object args) {
	cl_object head = pop(&args);
	cl_object var = pop(&head);
	cl_object list = pop(&head);
	cl_object specials, body;
	cl_index labelz, labelo;
	cl_object lex_old = lex_env;
	lex_copy();

	@si::process-declarations(1, args);
	body = VALUES(1);
	specials = VALUES(3);

	if (!SYMBOLP(var))
		FEillegal_variable_name(var);

	/* Compute list and enter loop */
	compile_form(list, FALSE);
	labelz = asm_jmp(op);

	/* Initialize the variable */
	compile_form((op == OP_DOLIST)? Cnil : MAKE_FIXNUM(0), FALSE);
	c_bind(var, specials);
	labelo = asm_jmp(OP_EXIT);

	/* Variable assignment and iterated body */
	compile_setq(OP_SETQ, var);
	c_tagbody(body);
	asm_op(OP_EXIT);

	/* Output */
	asm_complete(OP_EXIT, labelo);
	if (CDR(head) != Cnil)
		FEprogram_error("DOLIST: Too many output forms.", 0);
	if (Null(head))
		compile_body(Cnil);
	else {
		compile_setq(OP_SETQ, var);
		compile_form(pop(&head), FALSE);
	}
	asm_op(OP_EXIT);

	/* Exit point for block */
	asm_complete(op, labelz);

	lex_env = lex_old;
}


static void
c_dolist(cl_object args) {
	c_dolist_dotimes(OP_DOLIST, args);
}

static void
c_dotimes(cl_object args) {
	c_dolist_dotimes(OP_DOTIMES, args);
}

static void
c_eval_when(cl_object args) {
	cl_object situation = pop(&args);

	if (member_eq(@'eval', situation) || member_eq(@':execute', situation))
		compile_body(args);
	else
		compile_body(Cnil);
}


/*
	The OP_FLET/OP_FLABELS operators change the lexical environment
	to add a few local functions.

		[OP_FLET/OP_FLABELS + nfun]
		fun1
		...
		funn
		...
		OP_EXIT
	labelz:
*/
static void
c_labels_flet(int op, cl_object args) {
	cl_object def_list = pop(&args);
	int nfun = length(def_list);
	cl_object lex_old = lex_env;
	lex_copy();

	/* Remove declarations */
	@si::process-declarations(1, args);
	args = VALUES(1);
	if (nfun == 0) {
		compile_body(args);
		return;
	}
	asm_op2(op, nfun);
	do {
		cl_object definition = pop(&def_list);
		cl_object name = pop(&definition);
		asm1(make_lambda(name, definition));
	} while (!endp(def_list));
	compile_body(args);
	asm_op(OP_EXIT);

	lex_env = lex_old;
}


static void
c_flet(cl_object args) {
	c_labels_flet(OP_FLET, args);
}


/*
	There are two operators that produce functions. The first one
	is
		OP_FUNCTION
		symbol
	which takes the function binding of SYMBOL. The second one is
		OP_CLOSE
		interpreted
	which encloses the INTERPRETED function in the current lexical
	environment.
*/
static void
c_function(cl_object args) {
	cl_object function = pop(&args);
	if (!endp(args))
		FEprogram_error("FUNCTION: Too many arguments.", 0);
	if (SYMBOLP(function)) {
		asm_op(OP_FUNCTION);
		asm1(function);
	} else if (CONSP(function) && CAR(function) == @'lambda') {
		asm_op(OP_CLOSE);
		asm1(make_lambda(Cnil, CDR(function)));
	} else if (CONSP(function) && CAR(function) == @'lambda-block') {
		cl_object name = CADR(function);
		cl_object body = CDDR(function);
		asm_op(OP_CLOSE);
		asm1(make_lambda(name, body));
	} else
		FEprogram_error("FUNCTION: Not a valid argument ~S.", 1, function);
}


static void
c_go(cl_object args) {
	asm_op(OP_GO);
	asm1(pop(&args));
	if (!Null(args))
		FEprogram_error("GO: Too many arguments.",0);
}


/*
	To get an idea of what goes on

		...		; test form
		JNIL	labeln
		...		; form for true case
		JMP	labelz
		...		; form fro nil case
	labelz:
*/
static void
c_if(cl_object form) {
	cl_fixnum label_nil, label_true;

	/* Compile test */
	compile_form(pop(&form), FALSE);
	label_nil = asm_jmp(OP_JNIL);

	/* Compile THEN clause */
	compile_form(pop(&form), FALSE);
	label_true = asm_jmp(OP_JMP);

	/* Compile ELSE clause */
	asm_complete(OP_JNIL, label_nil);
	if (!endp(form))
		compile_form(pop(&form), FALSE);
	asm_complete(OP_JMP, label_true);

	if (!Null(form))
		FEprogram_error("IF: Too many arguments.", 0);
}


static void
c_labels(cl_object args) {
	c_labels_flet(OP_LABELS, args);
}


/*
	The OP_PUSHENV saves the current lexical environment to allow
	several bindings.
		OP_PUSHENV
		...		; binding forms
		...		; body
		OP_EXIT

	There are four forms which perform bindings
		OP_PBIND	; Bind NAME in the lexical env. using
		name		; a value from the stack
		OP_PBINDS	; Bind NAME as special variable using
		name		; a value from the stack
		OP_BIND		; Bind NAME in the lexical env. using
		name		; VALUES(0)
		OP_BINDS	; Bind NAME as special variable using
		name		; VALUES(0)

	After a variable has been bound, there are several ways to
	refer to it.

	1) Refer to the n-th variable in the lexical environment
		[SYMVAL + n]

	2) Refer to the value of a special variable or constant
		SYMVALS
		name

        3) Push the value of the n-th variable of the lexical environment
		[PUSHV + n]

	4) Push the value of a special variable or constant
		PUSHVS
		name
*/

static void
c_let_leta(int op, cl_object args) {
	cl_object bindings, specials, body, l, vars;
	cl_object lex_old = lex_env;
	lex_copy();

	bindings = car(args);
	@si::process-declarations(1, CDR(args));
	body = VALUES(1);
	specials = VALUES(3);

	/* Optimize some common cases */
	switch(length(bindings)) {
	case 0:		compile_body(body); return;
	case 1:		op = OP_BIND; break;
	default:
	}

	asm_op(OP_PUSHENV);
	for (vars=Cnil, l=bindings; !endp(l); ) {
		cl_object aux = pop(&l);
		cl_object var, value;
		if (ATOM(aux)) {
			var = aux;
			value = Cnil;
		} else {
			var = pop(&aux);
			value = pop_maybe_nil(&aux);
			if (!Null(aux))
				FEprogram_error("LET: Ill formed declaration.",0);
		}
		if (!SYMBOLP(var))
			FEillegal_variable_name(var);
		if (op == OP_PBIND) {
			compile_form(value, TRUE);
			vars = CONS(var, vars);
		} else {
			compile_form(value, FALSE);
			c_bind(var, specials);
		}
	}
	while (!endp(vars))
		c_pbind(pop(&vars), specials);
	compile_body(body);
	asm_op(OP_EXIT);

	lex_env = lex_old;
}

static void
c_let(cl_object args) {
	c_let_leta(OP_PBIND, args);
}

static void
c_leta(cl_object args) {
	c_let_leta(OP_BIND, args);
}

static void
c_locally(cl_object args) {
	/* Forget about declarations... */
	@si::process-declarations(1, args);

	/* ...and only process body */
	compile_body(VALUES(1));
}

/*
	MACROLET

	The current lexical environment is saved. A new one is prepared with
	the definitions of these macros, and this environment is used to
	compile the body.
 */
static void
c_macrolet(cl_object args)
{
	cl_object def_list, def, name;
	int nfun = 0;
	cl_object lex_old = lex_env;
	lex_copy();

	/* Pop the list of definitions */
	for (def_list = pop(&args); !endp(def_list); ) {
		cl_object definition = pop(&def_list);
		cl_object name = pop(&definition);
		cl_object arglist = pop(&definition);
		cl_object macro, function;
		macro = funcall(4, @'si::expand-defmacro', name, arglist,
				definition);
		function = make_lambda(name, CDR(macro));
		lex_macro_bind(name, function);
	}
	compile_body(args);
	lex_env = lex_old;
}


static void
c_multiple_value_bind(cl_object args)
{
	cl_object vars, value, body, specials;
	cl_index save_pc, n;
	cl_object lex_old = lex_env;
	lex_copy();

	vars = pop(&args);
	value = pop(&args);
	@si::process-declarations(1,args);
	body = VALUES(1);
	specials = VALUES(3);

	compile_form(value, FALSE);
	n = length(vars);
	if (n == 0) {
		compile_body(body);
	} else {
		asm_op(OP_PUSHENV);
		asm_op2(OP_MBIND, n);
		for (vars=reverse(vars); n; n--){
			cl_object var = pop(&vars);
			if (!SYMBOLP(var))
				FEillegal_variable_name(var);
			if (special_variablep(var, specials)) {
				asm1(MAKE_FIXNUM(1));
				c_register_var(var, TRUE);
			} else
				c_register_var(var, FALSE);
			asm1(var);
		}
		compile_body(body);
		asm_op(OP_EXIT);
	}
	lex_env = lex_old;
}


static void
c_multiple_value_call(cl_object args) {
	cl_object name;

	name = pop(&args);
	if (endp(args)) {
		/* If no arguments, just use ordinary call */
		c_call(list(1, name), FALSE);
		return;
	}
	asm_op(OP_MCALL);
	do {
		compile_form(pop(&args), FALSE);
		asm_op(OP_PUSHVALUES);
	} while (!endp(args));
	compile_form(name, FALSE);
	asm_op(OP_EXIT);
}


static void
c_multiple_value_prog1(cl_object args) {
	compile_form(pop(&args), FALSE);
	if (!endp(args)) {
		asm_op(OP_MPROG1);
		compile_body(args);
		asm_op(OP_EXIT);
	}
}


static void
c_multiple_value_setq(cl_object args) {
	cl_object orig_vars;
	cl_object vars = Cnil;
	cl_object temp_vars = Cnil;
	cl_object late_assignment = Cnil;
	cl_index nvars = 0;

	/* Look for symbol macros, building the list of variables
	   and the list of late assignments. */
	for (orig_vars = reverse(pop(&args)); !endp(orig_vars); ) {
		cl_object aux, v = pop(&orig_vars);
		if (!SYMBOLP(v))
			FEillegal_variable_name(v);
		v = macro_expand1(v, lex_env);
		if (!SYMBOLP(v)) {
			aux = v;
			v = @gensym(0);
			temp_vars = CONS(v, temp_vars);
			late_assignment = CONS(list(3, @'setf', aux, v),
					       late_assignment);
		}
		vars = CONS(v, vars);
		nvars++;
	}

	if (!Null(temp_vars)) {
		asm_op(OP_PUSHENV);
		do {
			compile_form(Cnil, FALSE);
			c_bind(CAR(temp_vars), Cnil);
			temp_vars = CDR(temp_vars);
		} while (!Null(temp_vars));
	}

	/* Compile values */
	compile_form(pop(&args), FALSE);
	if (args != Cnil)
		FEprogram_error("MULTIPLE-VALUE-SETQ: Too many arguments.", 0);
	if (nvars == 0)
		/* No variables */
		return;

	/* Compile variables */
	asm_op2(OP_MSETQ, nvars);
	vars = reverse(vars);
	while (nvars--) {
		cl_object ndx, var = pop(&vars);
		if (!SYMBOLP(var))
			FEillegal_variable_name(var);
		ndx = lex_var_sch(var);
		if (!Null(ndx) && CDR(ndx) != @'special')
			asm1(var); /* Lexical variable */
		else if (var->symbol.stype == stp_constant)
			FEassignment_to_constant(var);
		else {
			asm1(MAKE_FIXNUM(1));
			asm1(var);
		}
	}

	/* Assign to symbol-macros */
	if (!Null(late_assignment)) {
		compile_body(late_assignment);
		asm_op(OP_EXIT);
	}
}


/*
	The OP_NTHVAL operator moves a value from VALUES(ndx) to
	VALUES(0). The index NDX is taken from the stack.

		OP_NTHVAL
*/
static void
c_nth_value(cl_object args) {
	compile_form(pop(&args), TRUE);		/* INDEX */
	compile_form(pop(&args), FALSE);	/* VALUES */
	if (args != Cnil)
		FEprogram_error("NTH-VALUE: Too many arguments.",0);
	asm_op(OP_NTHVAL);
}


static void
c_or(cl_object args) {
	if (Null(args)) {
		asm1(Cnil);
		return;
	} else if (ATOM(args)) {
		FEill_formed_input();
	} else {
		compile_form(pop(&args), FALSE);
		if (!endp(args)) {
			cl_index label = asm_jmp(OP_JT);
			c_or(args);
			asm_complete(OP_JT, label);
		}
	}
}


/*
	The OP_PROGV operator exectures a set of statements in a lexical
	environment that has been extended with special variables. The
	list of special variables is taken from the top of the stack,
	while the list of values is in VALUES(0).

		...		; list of variables
		OP_PUSH
		...		; list of values
		OP_PROGV
		...		; body of progv
		OP_EXIT
*/
static void
c_progv(cl_object args) {
	cl_object vars = pop(&args);
	cl_object values = pop(&args);

	/* The list of variables is in the stack */
	compile_form(vars, TRUE);

	/* The list of values is in VALUES(0) */
	compile_form(values, FALSE);

	/* The body is interpreted within an extended lexical
	   environment. However, as all the new variables are
	   special, the compiler need not take care of them
	*/
	asm_op(OP_PROGV);
	compile_body(args);
	asm_op(OP_EXIT);
}


/*
	There are four assignment operators. They are

	1) Assign VALUES(0) to the lexical variable which occupies the
	   N-th position
		[OP_SETQ + n]

	2) Assign VALUES(0) to the special variable NAME
		OP_SETQS
		name

	3) Pop a value from the stack and assign it to the lexical
	   variable in the N-th position.
		[OP_PSETQ + n]

	4) Pop a value from the stack and assign it to the special
	   variable denoted by NAME
		OP_PSETQS
		name
*/
static void
c_psetq(cl_object old_args) {
	cl_object args = Cnil, vars = Cnil;
	bool use_psetf = FALSE;
	cl_index nvars = 0;

	/* We have to make sure that non of the variables which
	   are to be assigned is actually a symbol macro. If that
	   is the case, we invoke (PSETF ...) to handle the
	   macro expansions.
	*/
	while (!endp(old_args)) {
		cl_object var = pop(&old_args);
		cl_object value = pop(&old_args);
		if (!SYMBOLP(var))
			FEillegal_variable_name(var);
		var = macro_expand1(var, lex_env);
		if (!SYMBOLP(var))
			use_psetf = TRUE;
		args = CONS(var, CONS(value, args));
		nvars++;
	}
	if (use_psetf) {
		compile_form(CONS(@'psetf', args), FALSE);
		return;
	}
	while (!endp(args)) {
		cl_object var = pop(&args);
		cl_object value = pop(&args);
		vars = CONS(var, vars);
		compile_form(value, TRUE);
	}
	while (!endp(vars))
		compile_setq(OP_PSETQ, pop(&vars));
}


/*
	The OP_RETFROM operator returns from a block using the objects
	in VALUES() as output values.

		...		; output form
		OP_RETFROM
		tag		; object which names the block
*/
static void
c_return(cl_object stmt) {
	cl_object output = pop_maybe_nil(&stmt);

	compile_form(output, FALSE);
	asm_op(OP_RETURN);
	asm1(Cnil);
	if (stmt != Cnil)
		FEprogram_error("RETURN: Too many arguments.", 0);
}


static void
c_return_from(cl_object stmt) {
	cl_object name = pop(&stmt);
	cl_object output = pop_maybe_nil(&stmt);

	compile_form(output, FALSE);
	asm_op(OP_RETURN);
	if (!SYMBOLP(name))
		FEprogram_error("RETURN-FROM: Not a valid tag ~S.", 1, name);
	asm1(name);
	if (stmt != Cnil)
		FEprogram_error("RETURN-FROM: Too many arguments.", 0);
}


static void
c_setq(cl_object args) {
	while (!endp(args)) {
		cl_object var = pop(&args);
		cl_object value = pop(&args);
		if (!SYMBOLP(var))
			FEillegal_variable_name(var);
		var = macro_expand1(var, lex_env);
		if (SYMBOLP(var)) {
			compile_form(value, FALSE);
			compile_setq(OP_SETQ, var);
		} else {
			compile_form(list(3, @'setf', var, value), FALSE);
		}
	}
}


static void
c_symbol_macrolet(cl_object args)
{
	cl_object def_list, def, name, specials, body;
	cl_object lex_old = lex_env;
	int nfun = 0;

	/* Set a new lexical environment where we will bind
	   our macrology */
	lex_copy();

	def_list = pop(&args);
	@si::process-declarations(1,args);
	body = VALUES(1);
	specials = VALUES(3);

	/* Scan the list of definitions */
	for (; !endp(def_list); ) {
		cl_object definition = pop(&def_list);
		cl_object name = pop(&definition);
		cl_object expansion = pop(&definition);
		cl_object arglist = list(2, @gensym(0), @gensym(0));
		cl_object function;
		if (special_variablep(name, specials))
			FEprogram_error("SYMBOL-MACROLET: Symbol ~A cannot be \
declared special and appear in a symbol-macrolet.", 1, name);
		definition = list(2, arglist, list(2, @'quote', expansion));
		function = make_lambda(name, definition);
		lex_symbol_macro_bind(name, function);
	}
	compile_body(body);
	lex_env = lex_old;
}

static void
c_tagbody(cl_object args)
{
	cl_fixnum tag_base;
	cl_object label, body;
	enum type item_type;
	int nt;

	/* count the tags */
	for (nt = 0, body = args; !endp(body); body = CDR(body)) {
		label = CAR(body);
		item_type = type_of(CAR(body));
		if (item_type == t_symbol || item_type == t_fixnum ||
	            item_type == t_bignum) {
			nt += 1;
		}
	}
	if (nt == 0) {
		compile_body(args);
		compile_form(Cnil, FALSE);
		return;
	}
	asm_op2(OP_TAGBODY, nt);
	tag_base = current_pc();
	set_pc(tag_base + 2 * nt);

	for (body = args; !endp(body); body = CDR(body)) {
		label = CAR(body);
		item_type = type_of(label);
		if (item_type == t_symbol || item_type == t_fixnum ||
	            item_type == t_bignum) {
			asm_at(tag_base, label);
			tag_base++;
			asm_at(tag_base, MAKE_FIXNUM(current_pc()-tag_base));
			tag_base++;
		} else {
			compile_form(label, FALSE);
		}
	}
	asm_op(OP_EXIT);
}


/*
	The OP_THROW jumps to an enclosing OP_CATCH whose tag
	matches the one of the throw. The tag is taken from the
	stack, while the output values are left in VALUES().
*/
static void
c_throw(cl_object stmt) {
	/* FIXME! Do we apply the right protocol here? */
	cl_object tag = pop(&stmt);
	cl_object form = pop(&stmt);
	if (stmt != Cnil)
		FEprogram_error("THROW: Too many arguments.",0);
	compile_form(tag, TRUE);
	compile_form(form, FALSE);
	asm_op(OP_THROW);
}


static void
c_unless(cl_object form) {
	cl_fixnum label_true, label_false;

	/* Compile test */
	compile_form(pop(&form), FALSE);
	label_true = asm_jmp(OP_JT);

	/* Compile body */
	compile_body(form);
	label_false = asm_jmp(OP_JMP);
	asm_complete(OP_JT, label_true);

	/* When test failed, output NIL */
	asm1(Cnil);
	asm_complete(OP_JMP, label_false);
}


static void
c_unwind_protect(cl_object args) {
	cl_index label = asm_jmp(OP_UNWIND);

	/* Compile form to be protected */
	compile_form(pop(&args), FALSE);
	asm_op(OP_EXIT);

	/* Compile exit clause */
	asm_complete(OP_UNWIND, label);
	compile_body(args);
	asm_op(OP_EXIT);
}


/*
	The OP_VALUES moves N values from the stack to VALUES().

		[OP_VALUES + n]
*/
static void
c_values(cl_object args) {
	int n = 0;

	while (!endp(args)) {
		compile_form(pop_maybe_nil(&args), TRUE);
		n++;
	}
	asm_op2(OP_VALUES, n);
}


static void
c_when(cl_object form) {
	cl_fixnum label;

	/* Compile test */
	compile_form(pop(&form), FALSE);
	label = asm_jmp(OP_JNIL);

	/* Compile body */
	compile_body(form);
	asm_complete(OP_JNIL, label);
}


static void
compile_form(cl_object stmt, bool push) {
	compiler_record *l;
	cl_object function;
	cl_object macro;

	/* FIXME! We should protect this region with error handling */
 BEGIN:
	/*
	 * First try with variable references and quoted constants
	 */
	if (ATOM(stmt)) {
		if (SYMBOLP(stmt)) {
			cl_object stmt1 = macro_expand1(stmt, lex_env);
			if (stmt1 != stmt) {
				stmt = stmt1;
				goto BEGIN;
			}
			if (push) asm_op(OP_PUSHV);
			asm1(stmt);
			goto OUTPUT;
		}
	QUOTED:
		if (push)
			asm_op(OP_PUSHQ);
		else if (FIXNUMP(stmt) || SYMBOLP(stmt))
			asm_op(OP_QUOTE);
		asm1(stmt);
		goto OUTPUT;
	}
 LIST:
	/*
	 * Next try with special forms.
	 */
	function = CAR(stmt);
	if (!SYMBOLP(function))
		goto ORDINARY_CALL;
	if (function == @'quote') {
		stmt = CDR(stmt);
		if (CDR(stmt) != Cnil)
			FEprogram_error("QUOTE: Too many arguments.",0);
		stmt = CAR(stmt);
		goto QUOTED;
	}
	if (function == @'funcall') {
		c_funcall(CDR(stmt), push);
		goto OUTPUT;
	}
	for (l = database; l->symbol != OBJNULL; l++)
		if (l->symbol == function) {
			lexical_level += l->lexical_increment;
			(*(l->compiler))(CDR(stmt));
			if (push) asm_op(OP_PUSH);
			goto OUTPUT;
		}
	/*
	 * Next try to macroexpand
	 */
	{
		cl_object new_stmt = macro_expand1(stmt, lex_env);
		if (new_stmt != stmt){
			stmt = new_stmt;
			goto BEGIN;
		}
	}
	if (function->symbol.isform)
		FEprogram_error("BYTECOMPILE-FORM: Found no macroexpander \
for special form ~S.", 1, function);
 ORDINARY_CALL:
	/*
	 * Finally resort to ordinary function calls.
	 */
	c_call(stmt, push);
 OUTPUT:
}


static void
compile_body(cl_object body) {
	if (lexical_level == 0 && !endp(body)) {
		while (!endp(CDR(body))) {
			cl_index handle = asm_begin();
			compile_form(CAR(body), FALSE);
			asm_op(OP_EXIT);
			asm_op(OP_HALT);
			VALUES(0) = Cnil;
			NValues = 0;
			interpret(&bytecodes->vector.self.t[handle]);
			asm_clear(handle);
			body = CDR(body);
		}
	}
	if (endp(body))
		asm_op(OP_NOP);
	else do {
		compile_form(CAR(body), FALSE);
		body = CDR(body);
	} while (!endp(body));
}

/* ----------------------------- PUBLIC INTERFACE ---------------------------- */

/* ------------------------------------------------------------
   LAMBDA OBJECTS: An interpreted function is a vector made of
	the following components

      #(LAMBDA
	{block-name | NIL}
	{variable-env | NIL}
	{function-env | NIL}
	{block-env | NIL}
	(list of variables declared special)
	Nreq {var}*			; required arguments
	Nopt {var value flag}*		; optional arguments
	{rest-var NIL}			; rest variable
	{T | NIL}			; allow other keys?
	Nkey {key var value flag}*	; keyword arguments
	Naux {var init}			; auxiliary variables
	documentation-string
	list-of-declarations
	{form}*				; body)

   ------------------------------------------------------------ */

#define push(v,l) l = CONS(v, l)
#define push_var(v, list) \
	check_symbol(v); \
	if (v->symbol.stype == stp_constant) \
		FEillegal_variable_name(v); \
	push(v, list);

/*
  Handles special declarations, removes declarations from body
 */
@(defun si::process_declarations (body &optional doc)
	cl_object documentation = Cnil, declarations = Cnil, form, specials = Cnil;
	cl_object decls, vars, v;
@
	/* BEGIN: SEARCH DECLARE */
	for (; !endp(body); body = CDR(body)) {
	  form = CAR(body);

	  if (!Null(doc) && type_of(form) == t_string) {
	    if (documentation == Cnil)
	      documentation = form;
	    else
	      break;
	    continue;
	  }

	  if (ATOM(form) || (CAR(form) != @'declare'))
	    break;

	  for (decls = CDR(form); !endp(decls); decls = CDR(decls)) {
	    cl_object sentence = CAR(decls);
	    if (ATOM(sentence))
	      FEill_formed_input();
	    push(sentence, declarations);
	    if (CAR(sentence) == @'special')
	      for (vars = CDR(sentence); !endp(vars); vars = CDR(vars)) {
		v = CAR(vars);
		check_symbol(v);
		push(v,specials);
	      }
	  }
	}
	/* END: SEARCH DECLARE */

	@(return declarations body documentation specials)
@)

@(defun si::process_lambda_list (lambda)
	cl_object documentation, declarations, specials;
	cl_object lambda_list, body, form;
	cl_object x, v, key, init, spp;
	cl_object reqs = Cnil, opts = Cnil, keys = Cnil, rest = Cnil, auxs = Cnil;
	int nreq = 0, nopt = 0, nkey = 0, naux = 0;
	cl_object allow_other_keys = Cnil;
@
	bds_check;
	if (ATOM(lambda))
		FEprogram_error("LAMBDA: No lambda list.", 0);
	lambda_list = CAR(lambda);

	declarations = @si::process-declarations(2, CDR(lambda), Ct);
	body = VALUES(1);
	documentation = VALUES(2);
	specials = VALUES(3);

REQUIRED:
	while (1) {
		if (endp(lambda_list))
			goto OUTPUT;
		v = CAR(lambda_list);
		lambda_list = CDR(lambda_list);
		if (v == @'&allow-other-keys')
			goto ILLEGAL_LAMBDA;
		if (v == @'&optional')
			goto OPTIONAL;
		if (v == @'&rest')
			goto REST;
		if (v == @'&key')
			goto KEYWORD;
		if (v == @'&aux')
			goto AUX;
		nreq++;
		push_var(v, reqs);
	}
OPTIONAL:
	while (1) {
		if (endp(lambda_list))
			goto OUTPUT;
		x = CAR(lambda_list);
		lambda_list = CDR(lambda_list);
		spp = Cnil;
		init = Cnil;
		if (ATOM(x)) {
			if (x == @'&optional' || x == @'&allow-other-keys')
				goto ILLEGAL_LAMBDA;
			if (x == @'&rest')
				goto REST;
			if (x == @'&key')
				goto KEYWORD;
			if (x == @'&aux')
				goto AUX;
			v = x;
		} else {
			v = CAR(x);
			if (!endp(x = CDR(x))) {
				init = CAR(x);
				if (!endp(x = CDR(x))) {
					spp = CAR(x);
					if (!endp(CDR(x)))
						goto ILLEGAL_LAMBDA;
				}
			}
		}
		nopt++;
		push_var(v, opts);
		push(init, opts);
		if (spp != Cnil) {
			push_var(spp, opts);
		} else {
			push(Cnil, opts);
		}
	}

REST:
	if (endp(lambda_list))
		goto ILLEGAL_LAMBDA;
	v = CAR(lambda_list);
	push_var(v, rest);

	lambda_list = CDR(lambda_list);
	if (endp(lambda_list))
		goto OUTPUT;
	v = CAR(lambda_list);
	lambda_list = CDR(lambda_list);
	if (v == @'&optional' || v == @'&rest' || v == @'&allow-other-keys')
		goto ILLEGAL_LAMBDA;
	if (v == @'&key')
		goto KEYWORD;
	if (v == @'&aux')
		goto AUX;
	goto ILLEGAL_LAMBDA;

KEYWORD:
	while (1) {
		if (endp(lambda_list))
			goto OUTPUT;
		init = Cnil;
		spp = Cnil;
		x = CAR(lambda_list);
		lambda_list = CDR(lambda_list);
		if (ATOM(x)) {
			if (x == @'&allow-other-keys') {
				if (!Null(allow_other_keys))
					goto ILLEGAL_LAMBDA;
				allow_other_keys = Ct;
				if (endp(lambda_list))
					goto OUTPUT;
				x = CAR(lambda_list);
				lambda_list = CDR(lambda_list);
				if (key != @'&aux')
					goto ILLEGAL_LAMBDA;
				goto AUX;
			} else if (x == @'&optional' || x == @'&rest' || x == @'&key')
				goto ILLEGAL_LAMBDA;
			else if (x == @'&aux')
				goto AUX;
			v = x;
		} else {
			v = CAR(x);
			if (!endp(x = CDR(x))) {
				init = CAR(x);
				if (!endp(x = CDR(x))) {
					spp = CAR(x);
					if (!endp(CDR(x)))
						goto ILLEGAL_LAMBDA;
				}
			}
		}
		if (CONSP(v)) {
			key = CAR(v);
			if (endp(CDR(v)) || !endp(CDDR(v)))
				goto ILLEGAL_LAMBDA;
			v = CADR(v);
			check_symbol(v);
			check_symbol(key);
		} else {
			check_symbol(v);
			key = intern(v->symbol.name, keyword_package);
		}
		nkey++;
		push(key, keys);
		push_var(v, keys);
		push(init, keys);
		if (Null(spp)) {
			push(Cnil, keys);
		} else {
			push_var(spp, keys);
		}
	}

AUX:
	while (1) {
		if (endp(lambda_list))
			goto OUTPUT;
		x = CAR(lambda_list);
		lambda_list = CDR(lambda_list);
		if (ATOM(x)) {
			if (x == @'&optional' || x == @'&rest' ||
			    x == @'&key' || x == @'&allow-other-keys' ||
			    x == @'&aux')
				goto ILLEGAL_LAMBDA;
			v = x;
			init = Cnil;
		} else if (endp(CDDR(x))) {
			v = CAR(x);
			init = CADR(x);
		} else
			goto ILLEGAL_LAMBDA;
		naux++;
		push_var(v, auxs);
		push(init, auxs);
	}

OUTPUT:
	@(return CONS(MAKE_FIXNUM(nreq), nreverse(reqs))
		 CONS(MAKE_FIXNUM(nopt), nreverse(opts))
		 nreverse(rest)
		 allow_other_keys
		 CONS(MAKE_FIXNUM(nkey), nreverse(keys))
		 nreverse(auxs)
		 documentation
		 specials
		 declarations
		 body)

ILLEGAL_LAMBDA:
	FEprogram_error("LAMBDA: Illegal lambda list ~S.", 0);
@)

static void
c_default(cl_index deflt_pc) {
	cl_object deflt = asm_ref(deflt_pc);
	enum cl_type t = type_of(deflt);
	if ((t == t_symbol) && (deflt->symbol.stype == stp_constant))
		/* FIXME! Shouldn't this happen only in unsafe mode */
		asm_at(deflt_pc, SYM_VAL(deflt));
	else if ((t == t_symbol) || (t == t_cons) || (t == t_fixnum)) {
		cl_index pc = current_pc();
		asm_at(deflt_pc, MAKE_FIXNUM(pc-deflt_pc));
		compile_form(deflt, FALSE);
		asm_op(OP_EXIT);
	}
}

static void
c_register_var2(register cl_object var, register cl_object *specials)
{
	if (Null(var))
		return;
	if (member_eq(var, *specials))
		c_register_var(var, TRUE);
	else if (var->symbol.stype == stp_special) {
		*specials = CONS(var, *specials);
		c_register_var(var, TRUE);
	} else if (var->symbol.stype == stp_constant)
		FEassignment_to_constant(var);
	else
		c_register_var(var, FALSE);
}

cl_object
make_lambda(cl_object name, cl_object lambda) {
	cl_object reqs, opts, rest, keys, auxs, allow_other_keys;
	cl_object specials, doc, decl, body, l;
	cl_index specials_pc, opts_pc, keys_pc, label;
	int nopts, nkeys;
	cl_index handle;
	cl_object lex_old = lex_env;
	int old_lexical_level = lexical_level;

	lex_copy();
	lexical_level++;

	reqs = @si::process-lambda-list(1,lambda);
	opts = VALUES(1);
	rest = VALUES(2);
	allow_other_keys = VALUES(3);
	keys = VALUES(4);
	auxs = VALUES(5);
	doc  = VALUES(6);
	specials = VALUES(7);
	decl = VALUES(8);
	body = VALUES(9);

	handle = asm_begin();

	asm1(name);			/* Name of the function */
	specials_pc = current_pc();	/* Which variables are declared special */
	asm1(specials);

	asm_list(reqs);			/* Special arguments */
	reqs = CDR(reqs);
	while (!endp(reqs)) {
		cl_object v = pop(&reqs);
		c_register_var2(v, &specials);
	}

	opts_pc = current_pc()+1;	/* Optional arguments */
	nopts = fix(CAR(opts));
	asm_list(opts);

	asm_list(rest);			/* Name of &rest argument */

	asm1(allow_other_keys);		/* Value of &allow-other-keys */

	keys_pc = current_pc()+1;	/* Keyword arguments */
	nkeys = fix(CAR(keys));
	asm_list(keys);
	asmn(2, doc, decl);

	label = asm_jmp(OP_JMP);

	while (nopts--) {
		c_default(opts_pc+1);
		c_register_var2(asm_ref(opts_pc), &specials);
		c_register_var2(asm_ref(opts_pc+2), &specials);
		opts_pc+=3;
	}
	c_register_var2(car(rest), &specials);
	while (nkeys--) {
		c_default(keys_pc+2);
		c_register_var2(asm_ref(keys_pc+1), &specials);
		c_register_var2(asm_ref(keys_pc+3), &specials);
		keys_pc+=4;
	}
	
	if ((current_pc() - label) == 1)
		set_pc(label);
	else
		asm_complete(OP_JMP, label);
	while (!endp(auxs)) {		/* Local bindings */
		cl_object var = pop(&auxs);
		cl_object value = pop(&auxs);
		
		compile_form(value, FALSE);
		c_bind(var, specials);
	}
	asm_at(specials_pc, specials);
	compile_body(body);
	asm_op(OP_HALT);

	lexical_level = old_lexical_level;
	lex_env = lex_old;

	return asm_end(handle);
}

static cl_object
alloc_bytecodes()
{
	cl_object vector = alloc_simple_vector(128, aet_object);
	array_allocself(vector);
	vector->vector.hasfillp = TRUE;
	vector->vector.fillp = 0;
	return vector;
}

@(defun si::make_lambda (name rest)
	cl_object lambda, old_bytecodes = bytecodes;
	cl_object lex_old = lex_env;
@
	lex_new();
	if (frs_push(FRS_PROTECT, Cnil)) {
		lex_env = lex_old;
		bytecodes = old_bytecodes;
		frs_pop();
		unwind(nlj_fr, nlj_tag);
	}
	bytecodes = alloc_bytecodes();
	lambda = make_lambda(name,rest);
	frs_pop();
	bytecodes = old_bytecodes;
	lex_env = lex_old;
	@(return lambda)
@)

cl_object
eval(cl_object form, cl_object *new_bytecodes, cl_object env)
{
	cl_object old_bytecodes = bytecodes;
	int old_lexical_level = lexical_level;
	cl_object lex_old = lex_env;
	cl_index handle;
	bool unwinding;

	if (new_bytecodes == NULL)
		bytecodes = alloc_bytecodes();
	else if (*new_bytecodes != Cnil) {
		bytecodes = *new_bytecodes;
	} else {
		bytecodes = *new_bytecodes = alloc_bytecodes();
	}
	if (Null(env)) {
		lex_new();
		lexical_level = 0;
	} else {
		lexical_level = 1;
		lex_env = env;
		lex_copy();
	}
	if (frs_push(FRS_PROTECT, Cnil)) {
		lex_env = lex_old;
		bytecodes = old_bytecodes;
		lexical_level = old_lexical_level;
		frs_pop();
		unwind(nlj_fr, nlj_tag);
	}
	handle = asm_begin();
	compile_form(form, FALSE);
	asm_op(OP_EXIT);
	asm_op(OP_HALT);
	VALUES(0) = Cnil;
	NValues = 0;
	interpret(&bytecodes->vector.self.t[handle]);
	asm_clear(handle);
	frs_pop();
	lex_env = lex_old;
	bytecodes = old_bytecodes;
	lexical_level = old_lexical_level;
	return VALUES(0);
}

void
init_compiler(void)
{
	compiler_record *l;

	register_root(&bytecodes);

	for (l = database; l->name[0] != 0; l++)
	  l->symbol = _intern(l->name, lisp_package);
}
