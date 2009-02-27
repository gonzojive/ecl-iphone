/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    compiler.c -- Bytecode compiler
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*  Remarks:

    [1] The virtual machine has a word size of 16 bits. Operands and arguments
    have this very size, so that for instance, a jump

		OP_JMP increment

    takes two words of memory: one for the operator and one for the argument.
    The interpreter is written with this assumption in mind, but it should be
    easily modifed, because arguments are retrieved with "next_arg" and
    operators with "next_op".  Parts which will require a careful modification
    are marked with flag [1].
*/
#include <string.h>
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/bytecodes.h>

/********************* EXPORTS *********************/

#define REGISTER_SPECIALS	1
#define IGNORE_DECLARATIONS	0

/* Flags for the compilation routines: */
/* + Push the output of this form */
#define FLAG_PUSH		1
/* + Set the output of this form in VALUES */
#define FLAG_VALUES		2
/* + Set the output of this form in REG0 */
#define FLAG_REG0		4
/* + Search function binding in the global environment */
#define FLAG_GLOBAL		8
/* + Ignore this form */
#define FLAG_IGNORE		0
#define FLAG_USEFUL		(FLAG_PUSH | FLAG_VALUES | FLAG_REG0)

#define ENV_RECORD_LOCATION(r)	CADDDR(r)

#define ECL_SPECIAL_VAR_REF	-2
#define ECL_UNDEFINED_VAR_REF	-1

/********************* PRIVATE ********************/

typedef struct cl_compiler_env *cl_compiler_ptr;

#define asm_begin(env) current_pc(env)
#define current_pc(env) ECL_STACK_INDEX(env)
#define set_pc(env,n) asm_clear(env,n)
#define asm_ref(env,n) (cl_fixnum)((env)->stack[n])
static void asm_clear(cl_env_ptr env, cl_index h);
static void asm_op(cl_env_ptr env, cl_fixnum op);
static void asm_op2(cl_env_ptr env, int op, int arg);
static cl_object asm_end(cl_env_ptr env, cl_index handle);
static cl_index asm_jmp(cl_env_ptr env, register int op);
static void asm_complete(cl_env_ptr env, register int op, register cl_index original);

static cl_fixnum c_var_ref(cl_env_ptr env, cl_object var, int allow_symbol_macro, bool ensure_defined);

static int c_block(cl_env_ptr env, cl_object args, int flags);
static int c_case(cl_env_ptr env, cl_object args, int flags);
static int c_catch(cl_env_ptr env, cl_object args, int flags);
static int c_compiler_let(cl_env_ptr env, cl_object args, int flags);
static int c_cond(cl_env_ptr env, cl_object args, int flags);
static int c_eval_when(cl_env_ptr env, cl_object args, int flags);
static int c_flet(cl_env_ptr env, cl_object args, int flags);
static int c_funcall(cl_env_ptr env, cl_object args, int flags);
static int c_function(cl_env_ptr env, cl_object args, int flags);
static int c_go(cl_env_ptr env, cl_object args, int flags);
static int c_if(cl_env_ptr env, cl_object args, int flags);
static int c_labels(cl_env_ptr env, cl_object args, int flags);
static int c_let(cl_env_ptr env, cl_object args, int flags);
static int c_leta(cl_env_ptr env, cl_object args, int flags);
static int c_load_time_value(cl_env_ptr env, cl_object args, int flags);
static int c_locally(cl_env_ptr env, cl_object args, int flags);
static int c_macrolet(cl_env_ptr env, cl_object args, int flags);
static int c_multiple_value_bind(cl_env_ptr env, cl_object args, int flags);
static int c_multiple_value_call(cl_env_ptr env, cl_object args, int flags);
static int c_multiple_value_prog1(cl_env_ptr env, cl_object args, int flags);
static int c_multiple_value_setq(cl_env_ptr env, cl_object args, int flags);
static int c_not(cl_env_ptr env, cl_object args, int flags);
static int c_nth_value(cl_env_ptr env, cl_object args, int flags);
static int c_prog1(cl_env_ptr env, cl_object args, int flags);
static int c_progv(cl_env_ptr env, cl_object args, int flags);
static int c_psetq(cl_env_ptr env, cl_object args, int flags);
static int c_values(cl_env_ptr env, cl_object args, int flags);
static int c_setq(cl_env_ptr env, cl_object args, int flags);
static int c_return(cl_env_ptr env, cl_object args, int flags);
static int c_return_from(cl_env_ptr env, cl_object args, int flags);
static int c_symbol_macrolet(cl_env_ptr env, cl_object args, int flags);
static int c_tagbody(cl_env_ptr env, cl_object args, int flags);
static int c_throw(cl_env_ptr env, cl_object args, int flags);
static int c_unwind_protect(cl_env_ptr env, cl_object args, int flags);
static int c_while(cl_env_ptr env, cl_object args, int flags);
static int c_until(cl_env_ptr env, cl_object args, int flags);
static int compile_body(cl_env_ptr env, cl_object args, int flags);
static int compile_form(cl_env_ptr env, cl_object args, int push);

static int c_cons(cl_env_ptr env, cl_object args, int push);
static int c_endp(cl_env_ptr env, cl_object args, int push);
static int c_car(cl_env_ptr env, cl_object args, int push);
static int c_cdr(cl_env_ptr env, cl_object args, int push);
static int c_list(cl_env_ptr env, cl_object args, int push);
static int c_listA(cl_env_ptr env, cl_object args, int push);

static cl_object ecl_make_lambda(cl_env_ptr env, cl_object name, cl_object lambda);

static void FEillegal_variable_name(cl_object) /*__attribute__((noreturn))*/;
static void FEill_formed_input(void) /*__attribute__((noreturn))*/;

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

static cl_object
asm_end(cl_env_ptr env, cl_index beginning) {
        const cl_compiler_ptr c_env = env->c_env;
	cl_object bytecodes;
	cl_index code_size, data_size, i;
	cl_opcode *code;
	cl_object file = ECL_SYM_VAL(env,@'*load-truename*');
	cl_object position = cl_cdr(ECL_SYM_VAL(env,@'ext::*source-location*'));

	/* Save bytecodes from this session in a new vector */
	code_size = current_pc(env) - beginning;
	data_size = ecl_length(c_env->constants);
	bytecodes = ecl_alloc_object(t_bytecodes);
	bytecodes->bytecodes.name = @'si::bytecodes';
	bytecodes->bytecodes.code_size = code_size;
	bytecodes->bytecodes.data_size = data_size;
	bytecodes->bytecodes.code = ecl_alloc_atomic(code_size * sizeof(cl_opcode));
	bytecodes->bytecodes.data = (cl_object*)ecl_alloc(data_size * sizeof(cl_object));
	bytecodes->bytecodes.file = (file == OBJNULL)? Cnil : file;
	bytecodes->bytecodes.file_position = (position == OBJNULL)? Cnil : position;
	for (i = 0, code = (cl_opcode *)bytecodes->bytecodes.code; i < code_size; i++) {
		code[i] = (cl_opcode)(cl_fixnum)(env->stack[beginning+i]);
	}
	for (i=0; i < data_size; i++) {
		bytecodes->bytecodes.data[i] = CAR(c_env->constants);
		c_env->constants = CDR(c_env->constants);
	}
        bytecodes->bytecodes.entry =  _ecl_bytecodes_dispatch_vararg;
	asm_clear(env, beginning);
	return bytecodes;
}

#if defined(ECL_SMALL_BYTECODES)
static void
asm_arg(cl_env_ptr env, int n) {
#ifdef WORDS_BIGENDIAN
	asm_op(env, (n >> 8) & 0xFF);
	asm_op(env, n & 0xFF);
#else
	asm_op(env, n & 0xFF);
	asm_op(env, (n >> 8) & 0xFF);
#endif
}
#else
#define asm_arg(env,n) asm_op(env,n)
#endif

static void
asm_op(cl_env_ptr env, cl_fixnum code) {
        cl_object v = (cl_object)code;
        ECL_STACK_PUSH(env,v);
}

static void
asm_clear(cl_env_ptr env, cl_index h) {
        ECL_STACK_SET_INDEX(env, h);
}

static void
asm_op2(cl_env_ptr env, int code, int n) {
	if (n < -MAX_OPARG || MAX_OPARG < n)
		FEprogram_error("Argument to bytecode is too large", 0);
	asm_op(env, code);
	asm_arg(env, n);
}

static void
asm_constant(cl_env_ptr env, cl_object c)
{
        const cl_compiler_ptr c_env = env->c_env;
	c_env->constants = ecl_nconc(c_env->constants, ecl_list1(c));
}

static cl_index
asm_jmp(cl_env_ptr env, int op) {
	cl_index output;
	asm_op(env, op);
	output = current_pc(env);
	asm_arg(env, 0);
	return output;
}

static void
asm_complete(cl_env_ptr env, int op, cl_index pc) {
	cl_fixnum delta = current_pc(env) - pc;  /* [1] */
	if (op && (asm_ref(env, pc-1) != op))
		FEprogram_error("Non matching codes in ASM-COMPLETE2", 0);
	else if (delta < -MAX_OPARG || delta > MAX_OPARG)
		FEprogram_error("Too large jump", 0);
	else {
#ifdef ECL_SMALL_BYTECODES
		unsigned char low = delta & 0xFF;
		char high = delta >> 8;
# ifdef WORDS_BIGENDIAN
		env->stack[pc] = (cl_object)(cl_fixnum)high;
		env->stack[pc+1] = (cl_object)(cl_fixnum)low;
# else
		env->stack[pc] = (cl_object)(cl_fixnum)low;
		env->stack[pc+1] = (cl_object)(cl_fixnum)high;
# endif
#else
		env->stack[pc] = (cl_object)(cl_fixnum)delta;
#endif
	}
}

/* ------------------------------ COMPILER ------------------------------ */

typedef struct {
  void *symbol;
  int (*compiler)(cl_env_ptr, cl_object, int);
  int lexical_increment;
} compiler_record;

static compiler_record database[] = {
  {@'block', c_block, 1},
  {@'case', c_case, 1},
  {@'catch', c_catch, 1},
  {@'ext::compiler-let', c_compiler_let, 0},
  {@'cond', c_cond, 1},
  {@'eval-when', c_eval_when, 0},
  {@'flet', c_flet, 1},
  {@'function', c_function, 1},
  {@'funcall', c_funcall, 0},
  {@'go', c_go, 1},
  {@'if', c_if, 1},
  {@'labels', c_labels, 1},
  {@'let', c_let, 1},
  {@'let*', c_leta, 1},
  {@'locally', c_locally, 0},
  {@'load-time-value', c_load_time_value, 1},
  {@'macrolet', c_macrolet, 0},
  {@'multiple-value-bind', c_multiple_value_bind, 1},
  {@'multiple-value-call', c_multiple_value_call, 1},
  {@'multiple-value-prog1', c_multiple_value_prog1, 1},
  {@'multiple-value-setq', c_multiple_value_setq, 1},
  {@'not', c_not, 1},
  {@'nth-value', c_nth_value, 1},
  {@'null', c_not, 1},
  {@'progn', compile_body, 0},
  {@'prog1', c_prog1, 1},
  {@'progv', c_progv, 1},
  {@'psetq', c_psetq, 1},
  {@'return', c_return, 1},
  {@'return-from', c_return_from, 1},
  {@'setq', c_setq, 1},
  {@'symbol-macrolet', c_symbol_macrolet, 0},
  {@'tagbody', c_tagbody, 1},
  {@'throw', c_throw, 1},
  {@'unwind-protect', c_unwind_protect, 1},
  {@'values', c_values, 1},
  {@'si::while', c_while, 0},
  {@'si::until', c_until, 0},

  /* Extras */

  {@'cons', c_cons, 0},
  {@'car', c_car, 0},
  {@'cdr', c_cdr, 0},
  {@'first', c_car, 0},
  {@'rest', c_cdr, 0},
  {@'list', c_list, 0},
  {@'list*', c_listA, 0},
  {@'endp', c_endp, 0},
  {NULL, NULL, 1}
};

/* ----------------- LEXICAL ENVIRONMENT HANDLING -------------------- */

static void
assert_type_symbol(cl_object v)
{
	if (type_of(v) != t_symbol)
		FEprogram_error("Expected a symbol, found ~S.", 1, v);
}

static void
FEillegal_variable_name(cl_object v)
{
	FEprogram_error("Not a valid variable name ~S.", 1, v);
}

static void
FEill_formed_input()
{
	FEprogram_error("Syntax error: list with too few elements or improperly terminated.", 0);
}

static int
c_register_constant(cl_env_ptr env, cl_object c)
{
        const cl_compiler_ptr c_env = env->c_env;
	cl_object p = c_env->constants;
	int n;
	for (n = 0; !Null(p); n++, p=CDR(p)) {
		if (c_env->coalesce && ecl_eql(CAR(p), c)) {
			return n;
		}
	}
	asm_constant(env, c);
	return n;
}

static void
asm_c(cl_env_ptr env, cl_object o) {
	asm_arg(env, c_register_constant(env, o));
}

static void
asm_op2c(cl_env_ptr env, int code, cl_object o) {
	asm_op2(env, code, c_register_constant(env, o));
}

/*
 * Note: the following should match the definitions in cmp/cmpenv.lsp, as
 * well as CMP-ENV-REGISTER-MACROLET (lsp/defmacro.lsp)
 *
 * The compiler environment consists of two lists, one stored in
 * env->variables, the other one stored in env->macros.
 *
 * variable-record =	(:block block-name [used-p | block-object] location) |
 *			(:tag ({tag-name}*) [NIL | tag-object] location) |
 *			(:function function-name used-p [location]) |
 *			(var-name {:special | nil} bound-p [location]) |
 *			(symbol si::symbol-macro macro-function) |
 *			CB | LB | UNWIND-PROTECT |
 *			(:declare declaration-arguments*)
 * macro-record =	(function-name FUNCTION [| function-object]) |
 *			(macro-name si::macro macro-function)
 *			CB | LB | UNWIND-PROTECT
 *
 * A *-NAME is a symbol. A TAG-ID is either a symbol or a number. A
 * MACRO-FUNCTION is a function that provides us with the expansion
 * for that local macro or symbol macro. BOUND-P is true when the
 * variable has been bound by an enclosing form, while it is NIL if
 * the variable-record corresponds just to a special declaration.
 * CB, LB and UNWIND-PROTECT are only used by the C compiler and they
 * denote closure, lexical environment and unwind-protect boundaries.
 *
 * The brackets [] denote differences between the bytecodes and C
 * compiler environments, with the first option belonging to the
 * interpreter and the second alternative to the compiler.
 *
 * A LOCATION object is proper to the bytecodes compiler and denotes
 * the position of this variable, block, tag or function, in the
 * lexical environment. Currently, it is a CONS with two integers
 * (DEPTH . ORDER), denoting the depth of the nested environments and
 * the position in the environment (from the beginning, not from the
 * tail).
 *
 * The BLOCK-, TAG- and FUNCTION- objects are proper of the compiler
 * and carry further information.
 *
 * The last variable records are devoted to declarations and are only
 * used by the C compiler. Read cmpenv.lsp for more details on the
 * structure of these declaration forms, as they do not completely
 * match those of Common-Lisp.
 */

#if 0
#define new_location(env,x) MAKE_FIXNUM(0)
#else
static cl_object
new_location(cl_env_ptr env, cl_object name)
{
        const cl_compiler_ptr c_env = env->c_env;
	return CONS(MAKE_FIXNUM(c_env->env_depth),
                    MAKE_FIXNUM(c_env->env_size++));
}
#endif

static cl_index
c_register_block(cl_env_ptr env, cl_object name)
{
	cl_object loc = new_location(env, name);
        const cl_compiler_ptr c_env = env->c_env;
	c_env->variables = CONS(cl_list(4, @':block', name, Cnil, loc),
                                c_env->variables);
	return fix(ECL_CONS_CDR(loc));
}

static cl_index
c_register_tags(cl_env_ptr env, cl_object all_tags)
{
	cl_object loc = new_location(env, @':tag');
        const cl_compiler_ptr c_env = env->c_env;
	c_env->variables = CONS(cl_list(4, @':tag', all_tags, Cnil, loc),
                                c_env->variables);
	return fix(ECL_CONS_CDR(loc));
}

static void
c_register_function(cl_env_ptr env, cl_object name)
{
        const cl_compiler_ptr c_env = env->c_env;
	c_env->variables = CONS(cl_list(4, @':function', name, Cnil,
                                        new_location(env, name)),
                                c_env->variables);
	c_env->macros = CONS(cl_list(2, name, @'function'), c_env->macros);
}

static cl_object
c_macro_expand1(cl_env_ptr env, cl_object stmt)
{
        const cl_compiler_ptr c_env = env->c_env;
	return cl_macroexpand_1(2, stmt, CONS(c_env->variables, c_env->macros));
}

static void
c_register_symbol_macro(cl_env_ptr env, cl_object name, cl_object exp_fun)
{
        const cl_compiler_ptr c_env = env->c_env;
	c_env->variables = CONS(cl_list(3, name, @'si::symbol-macro', exp_fun),
                                c_env->variables);
}

/* UNUSED
static void
c_register_macro(cl_env_ptr env, cl_object name, cl_object exp_fun)
{
        const cl_compiler_ptr c_env = env->c_env;
	c_env->macros = CONS(cl_list(3, name, @'si::macro', exp_fun), c_env->macros);
}
*/

static void
c_register_var(cl_env_ptr env, cl_object var, bool special, bool bound)
{
	/* If this is just a declaration, ensure that the variable was not
	 * declared before as special, to save memory. */
	if (bound || (c_var_ref(env, var, 0, FALSE) >= ECL_UNDEFINED_VAR_REF)) {
                const cl_compiler_ptr c_env = env->c_env;
		c_env->variables = CONS(cl_list(4, var,
                                                special? @'special' : Cnil,
                                                bound? Ct : Cnil,
                                                new_location(env, var)),
                                        c_env->variables);
	}
}

static cl_object
guess_environment(cl_env_ptr env, cl_object interpreter_env)
{
	/*
	 * Given the environment of an interpreted function, we guess a
	 * suitable compiler enviroment to compile forms that access the
	 * variables and local functions of this interpreted code.
	 */
	for (interpreter_env = @revappend(interpreter_env, Cnil);
	     !Null(interpreter_env);
	     interpreter_env = CDR(interpreter_env))
	{
		cl_object record = CAR(interpreter_env);
		cl_object record0 = CAR(record);
		cl_object record1 = CDR(record);
		if (SYMBOLP(record0)) {
			c_register_var(env, record0, FALSE, TRUE);
		} else if (!FIXNUMP(record0)) {
			c_register_function(env, record1);
		} else if (record1 == MAKE_FIXNUM(0)) {
			c_register_tags(env, Cnil);
		} else {
			c_register_block(env, record1);
		}
	}
}

static void
c_new_env(cl_env_ptr the_env, cl_compiler_env_ptr new, cl_object env,
          cl_compiler_env_ptr old)
{
	the_env->c_env = new;
	new->stepping = 0;
	new->coalesce = TRUE;
	new->lexical_level = 0;
	new->constants = Cnil;
	new->env_depth = 0;
	new->env_size = 0;
	if (old) {
		if (!Null(env))
			ecl_internal_error("c_new_env with both ENV and OLD");
		new->variables = old->variables;
		new->macros = old->macros;
		new->lexical_level = old->lexical_level;
		new->constants = old->constants;
		new->lex_env = old->lex_env;
		new->env_depth = old->env_depth + 1;
		new->coalesce = old->coalesce;
		new->stepping = old->stepping;
	} else {
		new->variables = CAR(env);
		new->macros = CDR(env);
		for (env = new->variables; !Null(env); env = CDR(env)) {
			cl_object record = CAR(env);
			if (ATOM(record))
				continue;
			if (SYMBOLP(CAR(record)) && CADR(record) != @'si::symbol-macro') {
				continue;
			} else {
				new->lexical_level = 1;
				break;
			}
		}
	}
}

static cl_object
c_tag_ref(cl_env_ptr env, cl_object the_tag, cl_object the_type)
{
	cl_fixnum n = 0;
	cl_object l, type, name;
        const cl_compiler_ptr c_env = env->c_env;
	for (l = c_env->variables; CONSP(l); l = ECL_CONS_CDR(l)) {
		cl_object type, name, record = ECL_CONS_CAR(l);
		if (ATOM(record))
			continue;
		type = ECL_CONS_CAR(record);
                record = ECL_CONS_CDR(record);
		name = ECL_CONS_CAR(record);
		if (type == @':tag') {
			if (type == the_type) {
				cl_object label = ecl_assql(the_tag, name);
				if (!Null(label)) {
					return CONS(MAKE_FIXNUM(n), ECL_CONS_CDR(label));
				}
			}
			n++;
		} else if (type == @':block' || type == @':function') {
			/* We compare with EQUAL, because of (SETF fname) */
			if (type == the_type && ecl_equal(name, the_tag)) {
				/* Mark as used */
                                record = ECL_CONS_CDR(record);
				ECL_RPLACA(record, Ct);
				return MAKE_FIXNUM(n);
			}
			n++;
		} else if (Null(name)) {
			n++;
		} else {
			/* We are counting only locals and ignore specials
			 * and other declarations */
		}
	}
	return Cnil;
}

static cl_fixnum
c_var_ref(cl_env_ptr env, cl_object var, int allow_symbol_macro, bool ensure_defined)
{
	cl_fixnum n = 0;
	cl_object l, record, special, name;
        const cl_compiler_ptr c_env = env->c_env;
	for (l = c_env->variables; CONSP(l); l = ECL_CONS_CDR(l)) {
		record = ECL_CONS_CAR(l);
		if (ATOM(record))
			continue;
		name = ECL_CONS_CAR(record);
                record = ECL_CONS_CDR(record);
		special = ECL_CONS_CAR(record);
		if (name == @':block' || name == @':tag' || name == @':function') {
			n++;
		} else if (name == @':declare') {
			/* Ignored */
		} else if (name != var) {
			/* Symbol not yet found. Only count locals. */
			if (Null(special)) n++;
		} else if (special == @'si::symbol-macro') {
			/* We can only get here when we try to redefine a
			   symbol macro */
			if (allow_symbol_macro)
				return -1;
			FEprogram_error("Internal error: symbol macro ~S used as variable",
					1, var);
		} else if (Null(special)) {
			return n;
		} else {
			return ECL_SPECIAL_VAR_REF;
		}
	}
	if (ensure_defined) {
		l = ecl_symbol_value(@'si::*action-on-undefined-variable*');
		if (l != Cnil) {
			funcall(3, l, make_simple_base_string("Undefined variable referenced in interpreted code.~%Name: ~A"),
				var);
		}
	}
	return ECL_UNDEFINED_VAR_REF;
}

static bool
c_declared_special(register cl_object var, register cl_object specials)
{
	return ((ecl_symbol_type(var) & stp_special) || ecl_member_eq(var, specials));
}

static void
c_declare_specials(cl_env_ptr env, cl_object specials)
{
	while (!Null(specials)) {
		int ndx;
		cl_object var = pop(&specials);
		ndx = c_var_ref(env, var,0,FALSE);
		if (ndx >= 0 || ndx == ECL_UNDEFINED_VAR_REF)
			c_register_var(env, var, TRUE, FALSE);
	}
}

static cl_object
c_process_declarations(cl_object body)
{
	@si::process-declarations(1, body);
	body = VALUES(1);
	return body;
}

static bool
c_pbind(cl_env_ptr env, cl_object var, cl_object specials)
{
	bool special;
	if (!SYMBOLP(var))
		FEillegal_variable_name(var);
	else if ((special = c_declared_special(var, specials))) {
		c_register_var(env, var, TRUE, TRUE);
		asm_op2c(env, OP_PBINDS, var);
	} else {
		c_register_var(env, var, FALSE, TRUE);
		asm_op2c(env, OP_PBIND, var);
	}
	return special;
}

static bool
c_bind(cl_env_ptr env, cl_object var, cl_object specials)
{
	bool special;
	if (!SYMBOLP(var))
		FEillegal_variable_name(var);
	else if ((special = c_declared_special(var, specials))) {
		c_register_var(env, var, TRUE, TRUE);
		asm_op2c(env, OP_BINDS, var);
	} else {
		c_register_var(env, var, FALSE, TRUE);
		asm_op2c(env, OP_BIND, var);
	}
	return special;
}

static void
c_undo_bindings(cl_env_ptr the_env, cl_object old_vars, int only_specials)
{
	cl_object env;
	cl_index num_lexical = 0;
	cl_index num_special = 0;
        const cl_compiler_ptr c_env = the_env->c_env;

	for (env = c_env->variables; env != old_vars && !Null(env); env = ECL_CONS_CDR(env))
	{
		cl_object record = ECL_CONS_CAR(env);
		cl_object name = CAR(record);
		cl_object special = CADR(record);
		if (name == @':block' || name == @':tag') {
			(void)0;
		} else if (name == @':function' || Null(special)) {
			only_specials || num_lexical++;
		} else if (name == @':declare') {
			/* Ignored */
		} else if (special != @'si::symbol-macro') {
			/* If (third special) = NIL, the variable was declared
			   special, but there is no binding! */
			if (!Null(CADDR(record))) {
				num_special++;
			}
		}
	}
	c_env->variables = env;
	if (num_lexical) asm_op2(the_env, OP_UNBIND, num_lexical);
	if (num_special) asm_op2(the_env, OP_UNBINDS, num_special);
}

static void
compile_setq(cl_env_ptr env, int op, cl_object var)
{
	cl_fixnum ndx;

	if (!SYMBOLP(var))
		FEillegal_variable_name(var);
	ndx = c_var_ref(env, var,0,TRUE);
	if (ndx < 0) { /* Not a lexical variable */
		if (ecl_symbol_type(var) & stp_constant) {
			FEassignment_to_constant(var);
		}
		ndx = c_register_constant(env, var);
		if (op == OP_SETQ)
			op = OP_SETQS;
		else if (op == OP_PSETQ)
			op = OP_PSETQS;
		else if (op == OP_VSETQ)
			op = OP_VSETQS;
	}
	asm_op2(env, op, ndx);
}

/*
 * This routine is used to change the compilation flags in optimizers
 * that do not want to push values onto the stack.  Its purpose is to
 * keep ignorable forms ignored, while preserving the value of useful
 * forms. Qualitative behavior:
 *	FLAG_PUSH		-> FLAG_VALUES
 *	FLAG_VALUES		-> FLAG_VALUES
 *	FLAG_REG0		-> FLAG_REG0
 *	FLAG_IGNORE		-> FLAG_IGNORE
 */
static int
maybe_values_or_reg0(int flags) {
	if (flags & FLAG_PUSH)
		return (flags | FLAG_VALUES) & ~FLAG_PUSH;
	else
		return flags;
}

/*
 * This routine is used to change the compilation flags in optimizers
 * that do not want to push values onto the stack, but also do not want
 * to use REG0 (maybe because the call a nested ecl_interpret()). Ignorable
 * forms are kept ignored:
 *	FLAG_PUSH		-> FLAG_VALUES
 *	FLAG_VALUES		-> FLAG_VALUES
 *	FLAG_REG0		-> FLAG_VALUES
 *	FLAG_IGNORE		-> FLAG_IGNORE
 */
static int
maybe_values(int flags) {
	if (flags & FLAG_USEFUL)
		return (flags & ~(FLAG_PUSH | FLAG_REG0)) | FLAG_VALUES;
	else
		return flags;
}

/*
 * This routine is used to change the compilation flags in optimizers
 * that do not want to push values onto the stack.  Its purpose is to
 * keep ignorable forms ignored, while preserving the value of useful
 * forms. Qualitative behavior:
 *	FLAG_PUSH		-> FLAG_REG0
 *	FLAG_VALUES		-> FLAG_REG0
 *	FLAG_REG0		-> FLAG_REG0
 *	FLAG_IGNORE		-> FLAG_IGNORE
 */
static int
maybe_reg0(int flags) {
	if (flags & FLAG_USEFUL)
		return (flags & ~(FLAG_VALUES | FLAG_PUSH)) | FLAG_REG0;
	else
		return flags;
}

/* -------------------- THE COMPILER -------------------- */

/*
	The OP_BLOCK operator encloses several forms within a block
	named BLOCK_NAME, thus catching any OP_RETFROM whose argument
	matches BLOCK_NAME. The end of this block is marked both by
	the OP_EXIT operator and the LABELZ which is packed within
	the OP_BLOCK operator.

		[OP_BLOCK + name + labelz]
		....
		OP_EXIT_FRAME
	labelz:	...
*/

static int
c_block(cl_env_ptr env, cl_object body, int old_flags) {
	struct cl_compiler_env old_env;
	cl_object name = pop(&body);
	cl_object block_record;
	cl_index labelz, pc, loc;
	int flags;

	if (!SYMBOLP(name))
		FEprogram_error("BLOCK: Not a valid block name, ~S", 1, name);

	old_env = *(env->c_env);
	pc = current_pc(env);

	flags = maybe_values_or_reg0(old_flags);
	loc = c_register_block(env, name);
	block_record = CAR(env->c_env->variables);
	if (Null(name)) {
		asm_op(env, OP_DO);
	} else {
		asm_op2c(env, OP_BLOCK, name);
	}
	labelz = asm_jmp(env, OP_FRAME);
	compile_body(env, body, flags);
	if (CADDR(block_record) == Cnil) {
		/* Block unused. We remove the enclosing OP_BLOCK/OP_DO */
		*(env->c_env) = old_env;
		set_pc(env, pc);
		return compile_body(env, body, old_flags);
	} else {
		c_undo_bindings(env, old_env.variables, 0);
		asm_op(env, OP_EXIT_FRAME);
		asm_complete(env, 0, labelz);
		return flags;
	}
}

/*
	There are several ways to invoke functions and to handle the
	output arguments. These are

		[OP_CALL + nargs]
		function_name

		[OP_FCALL + nargs]

	 OP_CALL and OP_FCALL leave all arguments in the VALUES() array,
	 while OP_PCALL and OP_PFCALL leave the first argument in the
	 stack.

	 OP_CALL and OP_PCALL use the value in VALUES(0) to retrieve the
	 function, while OP_FCALL and OP_PFCALL use a value from the
	 stack.
 */
static int
c_arguments(cl_env_ptr env, cl_object args) {
	cl_index nargs;
	for (nargs = 0; !ecl_endp(args); nargs++) {
		compile_form(env, pop(&args), FLAG_PUSH);
	}
	return nargs;
}

static int asm_function(cl_env_ptr env, cl_object args, int flags);

static int
c_call(cl_env_ptr env, cl_object args, int flags) {
	cl_object name;
	cl_index nargs;
	bool push = flags & FLAG_PUSH;

	name = pop(&args);
	nargs = c_arguments(env, args);
	if (env->c_env->stepping) {
		/* When stepping, we only have one opcode to do function
		 * calls: OP_STEPFCALL. */
		asm_function(env, name, (flags & FLAG_GLOBAL) | FLAG_REG0);
		asm_op2(env, OP_STEPCALL, nargs);
		asm_op(env, OP_POP1);
		flags = FLAG_VALUES;
	} else if (SYMBOLP(name) &&
		   ((flags & FLAG_GLOBAL) || Null(c_tag_ref(env, name, @':function'))))
	{
		asm_op2(env, OP_CALLG, nargs);
		asm_c(env, name);
		flags = FLAG_VALUES;
	} else {
		/* Fixme!! We can optimize the case of global functions! */
		asm_function(env, name, (flags & FLAG_GLOBAL) | FLAG_REG0);
		asm_op2(env, OP_CALL, nargs);
		flags = FLAG_VALUES;
	}
	return flags;
}

static int
c_funcall(cl_env_ptr env, cl_object args, int flags) {
	cl_object name;
	cl_index nargs;

	name = pop(&args);
	if (CONSP(name)) {
                cl_object kind = ECL_CONS_CAR(name);
		if (kind == @'function') {
			if (cl_list_length(name) != MAKE_FIXNUM(2))
				FEprogram_error("FUNCALL: Invalid function name ~S",
						1, name);
			return c_call(env, CONS(CADR(name), args), flags);
		}
		if (kind == @'quote') {
			if (cl_list_length(name) != MAKE_FIXNUM(2))
				FEprogram_error("FUNCALL: Invalid function name ~S",
						1, name);
			return c_call(env, CONS(CADR(name), args), flags | FLAG_GLOBAL);
		}
	}
	compile_form(env, name, FLAG_PUSH);
	nargs = c_arguments(env, args);
	if (env->c_env->stepping) {
		asm_op2(env, OP_STEPCALL, nargs);
		flags = FLAG_VALUES;
	} else {
		asm_op2(env, OP_FCALL, nargs);
		flags = FLAG_VALUES;
	}
	asm_op(env, OP_POP1);
	return flags;
}

static int
perform_c_case(cl_env_ptr env, cl_object args, int flags) {
	cl_object test, clause;

	do {
		if (Null(args))
			return compile_body(env, Cnil, flags);
		clause = pop(&args);
		if (ATOM(clause))
			FEprogram_error("CASE: Illegal clause ~S.",1,clause);
		test = pop(&clause);
	} while (test == Cnil);

	if (@'otherwise' == test || test == Ct) {
		compile_body(env, clause, flags);
	} else {
		cl_index labeln, labelz;
		if (CONSP(test)) {
			cl_index n = ecl_length(test);
			while (n-- > 1) {
				cl_object v = pop(&test);
				asm_op(env, OP_JEQL);
				asm_c(env, v);
				asm_arg(env, n * (OPCODE_SIZE + OPARG_SIZE * 2)
					+ OPARG_SIZE);
			}
			test = ECL_CONS_CAR(test);
		}
		asm_op(env, OP_JNEQL);
		asm_c(env, test);
		labeln = current_pc(env);
		asm_arg(env, 0);
		compile_body(env, clause, flags);
		if (ecl_endp(args) && !(flags & FLAG_USEFUL)) {
			/* Ther is no otherwise. The test has failed and
			   we need no output value. We simply close jumps. */
			asm_complete(env, 0 & OP_JNEQL, labeln);
		} else {
			labelz = asm_jmp(env, OP_JMP);
			asm_complete(env, 0 & OP_JNEQL, labeln);
			perform_c_case(env, args, flags);
			asm_complete(env, OP_JMP, labelz);
		}
	}
	return flags;
}

static int
c_case(cl_env_ptr env, cl_object clause, int flags) {
	compile_form(env, pop(&clause), FLAG_REG0);
	return perform_c_case(env, clause, maybe_values_or_reg0(flags));
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
	       	OP_EXIT_FRAME
	labelz:	...
*/

static int
c_catch(cl_env_ptr env, cl_object args, int flags) {
	cl_index labelz, loc;
	cl_object old_env;

	/* Compile evaluation of tag */
	compile_form(env, pop(&args), FLAG_REG0);

	/* Compile binding of tag */
	old_env = env->c_env->variables;
	loc = c_register_block(env, MAKE_FIXNUM(0));
	asm_op(env, OP_CATCH);

	/* Compile jump point */
	labelz = asm_jmp(env, OP_FRAME);

	/* Compile body of CATCH */
	compile_body(env, args, FLAG_VALUES);

	c_undo_bindings(env, old_env, 0);
	asm_op(env, OP_EXIT_FRAME);
	asm_complete(env, 0, labelz);

	return FLAG_VALUES;
}

static int
c_compiler_let(cl_env_ptr env, cl_object args, int flags) {
	cl_object bindings;
	cl_index old_bds_top_index = env->bds_top - env->bds_org;

	for (bindings = pop(&args); !ecl_endp(bindings); ) {
		cl_object form = pop(&bindings);
		cl_object var = pop(&form);
		cl_object value = pop_maybe_nil(&form);
		ecl_bds_bind(env, var, value);
	}
	flags = compile_body(env, args, flags);
	ecl_bds_unwind(env, old_bds_top_index);
	return flags;
}

/*
	There are three operators which perform explicit jumps, but
	almost all other operators use labels in one way or
	another.

	1) Jumps are always relative to the place where the jump label
	is retrieved so that if the label is in vector[0], then the
	destination is roughly vector + vector[0].

	2) The three jump forms are

		[OP_JMP + label]	; Unconditional jump
		[OP_JNIL + label]	; Jump if VALUES(0) == Cnil
		[OP_JT + label]		; Jump if VALUES(0) != Cnil

	It is important to remark that both OP_JNIL and OP_JT truncate
	the values stack, so that always NVALUES = 1 after performing
	any of these operations.
*/
static int
c_cond(cl_env_ptr env, cl_object args, int flags) {
	cl_object test, clause;
	cl_index label_nil, label_exit;

	if (Null(args))
		return compile_form(env, Cnil, flags);
	clause = pop(&args);
	if (ATOM(clause))
		FEprogram_error("COND: Illegal clause ~S.",1,clause);
	test = pop(&clause);
	flags = maybe_values_or_reg0(flags);
	if (Ct == test) {
		/* Default sentence. If no forms, just output T. */
		if (Null(clause))
			compile_form(env, Ct, flags);
		else
			compile_body(env, clause, flags);
	} else {
		/* Compile the test. If no more forms, just output
		   the first value (this is guaranteed by OP_JT), but make
		   sure it is stored in the appropriate place. */
		if (Null(args)) {
			if (Null(clause)) {
				c_values(env, cl_list(1,test), flags);
			} else {
				compile_form(env, test, FLAG_REG0);
				if (flags & FLAG_VALUES) asm_op(env, OP_VALUEREG0);
				label_nil = asm_jmp(env, OP_JNIL);
				compile_body(env, clause, flags);
				asm_complete(env, OP_JNIL, label_nil);
			}
		} else if (Null(clause)) {
			compile_form(env, test, FLAG_REG0);
			if (flags & FLAG_VALUES) asm_op(env, OP_VALUEREG0);
			label_exit = asm_jmp(env, OP_JT);
			c_cond(env, args, flags);
			asm_complete(env, OP_JT, label_exit);
		} else {
			compile_form(env, test, FLAG_REG0);
			label_nil = asm_jmp(env, OP_JNIL);
			compile_body(env, clause, flags);
			label_exit = asm_jmp(env, OP_JMP);
			asm_complete(env, OP_JNIL, label_nil);
			c_cond(env, args, flags);
			asm_complete(env, OP_JMP, label_exit);
		}
	}
	return flags;
}

/*	The OP_DO operator saves the lexical environment and establishes
	a NIL block to execute the enclosed forms, which are typically
	like the ones shown below. At the exit of the block, either by
	means of a OP_RETFROM jump or because of normal termination,
	the lexical environment is restored, and all bindings undone.

		[OP_DO + labelz]
		...	; bindings
		[JMP + labelt]
	labelb:	...	; body
		...	; stepping forms
	labelt:	...	; test form
		[JNIL + label]
		...	; output form
		OP_EXIT_FRAME
	labelz:

*/
static int
c_while_until(cl_env_ptr env, cl_object body, int flags, bool is_while) {
	cl_object test = pop(&body);
	cl_index labelt, labelb;

	flags = maybe_reg0(flags);

	/* Jump to test */
	labelt = asm_jmp(env, OP_JMP);

	/* Compile body */
	labelb = current_pc(env);
	c_tagbody(env, body, flags);

	/* Compile test */
	asm_complete(env, OP_JMP, labelt);
	compile_form(env, test, FLAG_REG0);
	asm_op(env, is_while? OP_JT : OP_JNIL);
	asm_arg(env, labelb - current_pc(env));

	return flags;
}

static int
c_while(cl_env_ptr env, cl_object body, int flags) {
	return c_while_until(env, body, flags, 1);
}

static int
c_until(cl_env_ptr env, cl_object body, int flags) {
	return c_while_until(env, body, flags, 0);
}

static int
c_eval_when(cl_env_ptr env, cl_object args, int flags) {
	cl_object situation = pop(&args);

	if (ecl_member_eq(@'eval', situation) || ecl_member_eq(@':execute', situation))
		return compile_body(env, args, flags);
	else
		return compile_body(env, Cnil, flags);
}


/*
	The OP_FLET/OP_FLABELS operators change the lexical environment
	to add a few local functions.

		[OP_FLET/OP_FLABELS + nfun + fun1]
		...
		OP_UNBIND nfun
	labelz:
*/
static cl_index
c_register_functions(cl_env_ptr env, cl_object l)
{
	cl_index nfun;
	for (nfun = 0; !ecl_endp(l); nfun++) {
		cl_object definition = pop(&l);
		cl_object name = pop(&definition);
		c_register_function(env, name);
	}
	return nfun;
}

static int
c_labels_flet(cl_env_ptr env, int op, cl_object args, int flags) {
	cl_object l, def_list = pop(&args);
	cl_object old_vars = env->c_env->variables;
	cl_object old_funs = env->c_env->macros;
	cl_index nfun, first = 0;

	if (ecl_length(def_list) == 0) {
		return c_locally(env, args, flags);
	}

	/* If compiling a LABELS form, add the function names to the lexical
	   environment before compiling the functions */
	if (op == OP_FLET)
		nfun = ecl_length(def_list);
	else
		nfun = c_register_functions(env, def_list);

	/* Push the operator (OP_LABELS/OP_FLET) with the number of functions */
	asm_op2(env, op, nfun);

	/* Compile the local functions now. */
	for (l = def_list; !ecl_endp(l); ) {
		cl_object definition = pop(&l);
		cl_object name = pop(&definition);
		cl_object lambda = ecl_make_lambda(env, name, definition);
		cl_index c = c_register_constant(env, lambda);
		if (first == 0) {
			asm_arg(env, c);
			first = 1;
		}
	}

	/* If compiling a FLET form, add the function names to the lexical
	   environment after compiling the functions */
	if (op == OP_FLET)
		c_register_functions(env, def_list);

	/* Compile the body of the form with the local functions in the lexical
	   environment. */
	flags = c_locally(env, args, flags);

	/* Restore and return */
	c_undo_bindings(env, old_vars, 0);
	env->c_env->macros = old_funs;

	return flags;
}


static int
c_flet(cl_env_ptr env, cl_object args, int flags) {
	return c_labels_flet(env, OP_FLET, args, flags);
}


/*
	There are two operators that produce functions. The first one
	is
		[OP_FUNCTION + name]
	which takes the function binding of SYMBOL. The second one is
		OP_CLOSE
		interpreted
	which encloses the INTERPRETED function in the current lexical
	environment.
*/
static int
c_function(cl_env_ptr env, cl_object args, int flags) {
	cl_object function = pop(&args);
	if (!ecl_endp(args))
		FEprogram_error("FUNCTION: Too many arguments.", 0);
	return asm_function(env, function, flags);
}

static int
asm_function(cl_env_ptr env, cl_object function, int flags) {
	if (!Null(si_valid_function_name_p(function))) {
		cl_object ndx = c_tag_ref(env, function, @':function');
		if (Null(ndx)) {
			/* Globally defined function */
			asm_op2c(env, OP_FUNCTION, function);
                        return FLAG_REG0;
		} else {
			/* Function from a FLET/LABELS form */
			asm_op2(env, OP_LFUNCTION, fix(ndx));
                        return FLAG_REG0;
		}
	}
        if (CONSP(function)) {
                cl_object kind = ECL_CONS_CAR(function);
                cl_object form = ECL_CONS_CDR(function);
                if (kind == @'lambda') {
                        asm_op2c(env, OP_CLOSE, ecl_make_lambda(env, Cnil, form));
                        return FLAG_REG0;
                } else if (kind == @'ext::lambda-block') {
                        cl_object name = ECL_CONS_CAR(form);
                        cl_object body = ECL_CONS_CDR(form);
                        asm_op2c(env, OP_CLOSE, ecl_make_lambda(env, name, body));
                        return FLAG_REG0;
                }
        }
        FEprogram_error("FUNCTION: Not a valid argument ~S.", 1, function);
	return FLAG_REG0;
}


static int
c_go(cl_env_ptr env, cl_object args, int flags) {
	cl_object tag = pop(&args);
	cl_object info = c_tag_ref(env, tag, @':tag');
	if (Null(info))
		FEprogram_error("GO: Unknown tag ~S.", 1, tag);
	if (!Null(args))
		FEprogram_error("GO: Too many arguments.",0);
	asm_op2(env, OP_GO, fix(CAR(info)));
	asm_arg(env, fix(CDR(info)));
	return flags;
}


/*
	(if a b) -> (cond (a b))
	(if a b c) -> (cond (a b) (t c))
*/
static int
c_if(cl_env_ptr env, cl_object form, int flags) {
	cl_object test = pop(&form);
	cl_object then = pop(&form);
	then = cl_list(2, test, then);
	if (Null(form)) {
		return c_cond(env, ecl_list1(then), flags);
	} else {
		return c_cond(env, cl_list(2, then, CONS(Ct, form)), flags);
	}
}


static int
c_labels(cl_env_ptr env, cl_object args, int flags) {
	return c_labels_flet(env, OP_LABELS, args, flags);
}


/*
	The OP_PUSHENV saves the current lexical environment to allow
	several bindings.
		OP_PUSHENV
		...		; binding forms
		...		; body
		OP_EXIT

	There are four forms which perform bindings
		OP_PBIND name	; Bind NAME in the lexical env. using
				; a value from the stack
		OP_PBINDS name	; Bind NAME as special variable using
				; a value from the stack
		OP_BIND name	; Bind NAME in the lexical env. using
				; VALUES(0)
		OP_BINDS name	; Bind NAME as special variable using
				; VALUES(0)

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

static int
c_let_leta(cl_env_ptr env, int op, cl_object args, int flags) {
	cl_object bindings, specials, body, l, vars;
	cl_object old_variables = env->c_env->variables;

	bindings = cl_car(args);
	body = c_process_declarations(CDR(args));
	specials = VALUES(3);

	/* Optimize some common cases */
	switch(ecl_length(bindings)) {
	case 0:		return c_locally(env, CDR(args), flags);
	case 1:		op = OP_BIND; break;
	}

	for (vars=Cnil, l=bindings; !ecl_endp(l); ) {
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
			compile_form(env, value, FLAG_PUSH);
			vars = CONS(var, vars);
		} else {
			compile_form(env, value, FLAG_REG0);
			c_bind(env, var, specials);
		}
	}
	while (!ecl_endp(vars))
		c_pbind(env, pop(&vars), specials);

	/* We have to register all specials, because in the list
	 * there might be some variable that is not bound by this LET form
	 */
	c_declare_specials(env, specials);

	flags = compile_body(env, body, flags);

	c_undo_bindings(env, old_variables, 0);
	return flags;
}

static int
c_let(cl_env_ptr env, cl_object args, int flags) {
	return c_let_leta(env, OP_PBIND, args, flags);
}

static int
c_leta(cl_env_ptr env, cl_object args, int flags) {
	return c_let_leta(env, OP_BIND, args, flags);
}

static int
c_load_time_value(cl_env_ptr env, cl_object args, int flags)
{
	if (cl_rest(args) != Cnil)
		FEprogram_error("LOAD-TIME-VALUE: Too many arguments.", 0);
	return c_values(env, args, flags);
}

static int
c_locally(cl_env_ptr env, cl_object args, int flags) {
	cl_object old_env = env->c_env->variables;

	/* First use declarations by declaring special variables... */
	args = c_process_declarations(args);
	c_declare_specials(env, VALUES(3));

	/* ...and then process body */
	flags = compile_body(env, args, flags);

	c_undo_bindings(env, old_env, 0);

	return flags;
}

/*
	MACROLET

	The current lexical environment is saved. A new one is prepared with
	the definitions of these macros, and this environment is used to
	compile the body.
 */
static int
c_macrolet(cl_env_ptr the_env, cl_object args, int flags)
{
        const cl_compiler_ptr c_env = the_env->c_env;
	cl_object old_env = c_env->macros;
	cl_object env = funcall(3, @'si::cmp-env-register-macrolet', pop(&args),
				CONS(c_env->variables, c_env->macros));
	c_env->macros = CDR(env);
	flags = c_locally(the_env, args, flags);
	c_env->macros = old_env;
	return flags;
}

static void
c_vbind(cl_env_ptr env, cl_object var, int n, cl_object specials)
{
        if (c_declared_special(var, specials)) {
                c_register_var(env, var, FLAG_PUSH, TRUE);
                if (n) {
                        asm_op2(env, OP_VBINDS, n);
                } else {
                        asm_op(env, OP_BINDS);
                }
        } else {
                c_register_var(env, var, FALSE, TRUE);
                if (n) {
                        asm_op2(env, OP_VBIND, n);
                } else {
                        asm_op(env, OP_BIND);
                }
        }
        asm_c(env, var);
}

static int
c_multiple_value_bind(cl_env_ptr env, cl_object args, int flags)
{
	cl_object old_env = env->c_env->variables;
	cl_object vars, value, body, specials;
	cl_index n;

	vars = pop(&args);
	value = pop(&args);
	body = c_process_declarations(args);
	specials = VALUES(3);

	compile_form(env, value, FLAG_VALUES);
	n = ecl_length(vars);
	if (n == 0) {
		c_declare_specials(env, specials);
		flags = compile_body(env, body, flags);
		c_undo_bindings(env, old_env, 0);
	} else {
		cl_object old_variables = env->c_env->variables;
		for (vars=cl_reverse(vars); n--; ) {
			cl_object var = pop(&vars);
			if (!SYMBOLP(var))
				FEillegal_variable_name(var);
                        c_vbind(env, var, n, specials);
		}
		c_declare_specials(env, specials);
		flags = compile_body(env, body, flags);
		c_undo_bindings(env, old_variables, 0);
	}
	return flags;
}


static int
c_multiple_value_call(cl_env_ptr env, cl_object args, int flags) {
	cl_object name;
	int op;

	name = pop(&args);
	if (ecl_endp(args)) {
		/* If no arguments, just use ordinary call */
		return c_funcall(env, cl_list(1, name), flags);
	}
	compile_form(env, name, FLAG_PUSH);
	for (op = OP_PUSHVALUES; !ecl_endp(args); op = OP_PUSHMOREVALUES) {
		compile_form(env, pop(&args), FLAG_VALUES);
		asm_op(env, op);
	}
	asm_op(env, OP_MCALL);
	asm_op(env, OP_POP1);

	return FLAG_VALUES;
}


static int
c_multiple_value_prog1(cl_env_ptr env, cl_object args, int flags) {
	compile_form(env, pop(&args), FLAG_VALUES);
	if (!ecl_endp(args)) {
		asm_op(env, OP_PUSHVALUES);
		compile_body(env, args, FLAG_IGNORE);
		asm_op(env, OP_POPVALUES);
	}
	return FLAG_VALUES;
}


static int
c_multiple_value_setq(cl_env_ptr env, cl_object orig_args, int flags) {
	cl_object args = orig_args;
	cl_object orig_vars;
	cl_object vars = Cnil, values;
	cl_object old_variables = env->c_env->variables;
	cl_index nvars = 0;

	/* Look for symbol macros, building the list of variables
	   and the list of late assignments. */
	for (orig_vars = pop(&args); !ecl_endp(orig_vars); ) {
		cl_object v = pop(&orig_vars);
		if (!SYMBOLP(v))
			FEillegal_variable_name(v);
		v = c_macro_expand1(env, v);
		if (!SYMBOLP(v)) {
			/* If any of the places to be set is not a variable,
			 * transform MULTIPLE-VALUE-SETQ into (SETF (VALUES ...))
			 */
			args = orig_args;
			return compile_form(env, cl_listX(3, @'setf',
                                                          CONS(@'values', CAR(args)),
                                                          CDR(args)),
					    flags);
		}
		vars = CONS(v, vars);
		nvars++;
	}

	/* Compile values */
	values = pop(&args);
	if (args != Cnil)
		FEprogram_error("MULTIPLE-VALUE-SETQ: Too many arguments.", 0);
	if (nvars == 0) {
		/* No variables */
		return compile_form(env, cl_list(2, @'values', values), flags);
	}
	compile_form(env, values, FLAG_VALUES);

	/* Compile variables */
	for (nvars = 0, vars = cl_nreverse(vars); vars != Cnil; nvars++, vars = ECL_CONS_CDR(vars)) {
		if (nvars) {
			compile_setq(env, OP_VSETQ, ECL_CONS_CAR(vars));
			asm_arg(env, nvars);
		} else {
			compile_setq(env, OP_SETQ, ECL_CONS_CAR(vars));
		}
	}

	c_undo_bindings(env, old_variables, 0);

	return FLAG_REG0;
}

/*
	The OP_NOT operator reverses the boolean value of VALUES(0).
*/
static int
c_not(cl_env_ptr env, cl_object args, int flags) {
	flags = maybe_reg0(flags);
	if (flags & FLAG_USEFUL) {
		/* The value is useful */
		compile_form(env, pop(&args), FLAG_REG0);
		asm_op(env, OP_NOT);
	} else {
		/* The value may be ignored. */
		flags = compile_form(env, pop(&args), flags);
	}
	if (!Null(args))
		FEprogram_error("NOT/NULL: Too many arguments.", 0);
	return flags;
}

/*
	The OP_NTHVAL operator moves a value from VALUES(ndx) to
	VALUES(0). The index NDX is taken from the stack.

		OP_NTHVAL
*/
static int
c_nth_value(cl_env_ptr env, cl_object args, int flags) {
	compile_form(env, pop(&args), FLAG_PUSH);	/* INDEX */
	compile_form(env, pop(&args), FLAG_VALUES);	/* VALUES */
	if (args != Cnil)
		FEprogram_error("NTH-VALUE: Too many arguments.",0);
	asm_op(env, OP_NTHVAL);
	return FLAG_REG0;
}


static int
c_prog1(cl_env_ptr env, cl_object args, int flags) {
	cl_object form = pop(&args);
	if (!(flags & FLAG_USEFUL) || (flags & FLAG_PUSH)) {
		flags = compile_form(env, form, flags);
		compile_body(env, args, FLAG_IGNORE);
	} else {
		flags = FLAG_REG0;
		compile_form(env, form, FLAG_PUSH);
		compile_body(env, args, FLAG_IGNORE);
		asm_op(env, OP_POP);
	}
	return flags;
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
static int
c_progv(cl_env_ptr env, cl_object args, int flags) {
	cl_object vars = pop(&args);
	cl_object values = pop(&args);

	/* The list of variables is in the stack */
	compile_form(env, vars, FLAG_PUSH);

	/* The list of values is in reg0 */
	compile_form(env, values, FLAG_REG0);

	/* The body is interpreted within an extended lexical
	   environment. However, as all the new variables are
	   special, the compiler need not take care of them
	*/
	asm_op(env, OP_PROGV);
	flags = compile_body(env, args, FLAG_VALUES);
	asm_op(env, OP_EXIT_PROGV);

	return flags;
}


/*
	There are four assignment operators. They are

	1) Assign VALUES(0) to the lexical variable which occupies the
	   N-th position
		[OP_SETQ + n]

	2) Assign VALUES(0) to the special variable NAME
		[OP_SETQS + name]

	3) Pop a value from the stack and assign it to the lexical
	   variable in the N-th position.
		[OP_PSETQ + n]

	4) Pop a value from the stack and assign it to the special
	   variable denoted by NAME
		[OP_PSETQS + name]
*/
static int
c_psetq(cl_env_ptr env, cl_object old_args, int flags) {
	cl_object args = Cnil, vars = Cnil;
	bool use_psetf = FALSE;
	cl_index nvars = 0;

	if (ecl_endp(old_args))
		return compile_body(env, Cnil, flags);
	/* We have to make sure that non of the variables which
	   are to be assigned is actually a symbol macro. If that
	   is the case, we invoke (PSETF ...) to handle the
	   macro expansions.
	*/
	while (!ecl_endp(old_args)) {
		cl_object var = pop(&old_args);
		cl_object value = pop(&old_args);
		if (!SYMBOLP(var))
			FEillegal_variable_name(var);
		var = c_macro_expand1(env, var);
		if (!SYMBOLP(var))
			use_psetf = TRUE;
		args = ecl_nconc(args, cl_list(2, var, value));
		nvars++;
	}
	if (use_psetf) {
		return compile_form(env, CONS(@'psetf', args), flags);
	}
	while (!ecl_endp(args)) {
		cl_object var = pop(&args);
		cl_object value = pop(&args);
		vars = CONS(var, vars);
		compile_form(env, value, FLAG_PUSH);
	}
	while (!ecl_endp(vars))
		compile_setq(env, OP_PSETQ, pop(&vars));
	return compile_form(env, Cnil, flags);
}


/*
	The OP_RETFROM operator returns from a block using the objects
	in VALUES() as output values.

		...		; output form
		OP_RETFROM
		tag		; object which names the block
*/
static int
c_return_aux(cl_env_ptr env, cl_object name, cl_object stmt, int flags)
{
	cl_object ndx = c_tag_ref(env, name, @':block');
	cl_object output = pop_maybe_nil(&stmt);

	if (!SYMBOLP(name) || Null(ndx))
		FEprogram_error("RETURN-FROM: Unknown block name ~S.", 1, name);
	if (stmt != Cnil)
		FEprogram_error("RETURN-FROM: Too many arguments.", 0);
	compile_form(env, output, FLAG_VALUES);
	asm_op2(env, OP_RETURN, fix(ndx));
	return FLAG_VALUES;
}

static int
c_return(cl_env_ptr env, cl_object stmt, int flags) {
	return c_return_aux(env, Cnil, stmt, flags);
}


static int
c_return_from(cl_env_ptr env, cl_object stmt, int flags) {
	cl_object name = pop(&stmt);
	return c_return_aux(env, name, stmt, flags);
}


static int
c_setq(cl_env_ptr env, cl_object args, int flags) {
	if (ecl_endp(args))
		return compile_form(env, Cnil, flags);
	do {
		cl_object var = pop(&args);
		cl_object value = pop(&args);
		if (!SYMBOLP(var))
			FEillegal_variable_name(var);
		var = c_macro_expand1(env, var);
		if (SYMBOLP(var)) {
			flags = FLAG_REG0;
			compile_form(env, value, FLAG_REG0);
			compile_setq(env, OP_SETQ, var);
		} else {
			flags = ecl_endp(args)? FLAG_VALUES : FLAG_REG0;
			compile_form(env, cl_list(3, @'setf', var, value), flags);
		}
	} while (!ecl_endp(args));
	return flags;
}


static int
c_symbol_macrolet(cl_env_ptr env, cl_object args, int flags)
{
	cl_object def_list, specials, body;
	cl_object old_variables = env->c_env->variables;

	def_list = pop(&args);
	body = c_process_declarations(args);
	specials = VALUES(3);

	/* Scan the list of definitions */
	for (; !ecl_endp(def_list); ) {
		cl_object definition = pop(&def_list);
		cl_object name = pop(&definition);
		cl_object expansion = pop(&definition);
		cl_object arglist = cl_list(2, @gensym(0), @gensym(0));
		cl_object function;
		if ((ecl_symbol_type(name) & (stp_special | stp_constant)) ||
		    c_var_ref(env, name,1,FALSE) == -2)
		{
			FEprogram_error("SYMBOL-MACROLET: Symbol ~A cannot be \
declared special and appear in a symbol-macrolet.", 1, name);
		}
		definition = cl_list(2, arglist, cl_list(2, @'quote', expansion));
		function = ecl_make_lambda(env, name, definition);
		c_register_symbol_macro(env, name, function);
	}
	c_declare_specials(env, specials);
	flags = compile_body(env, body, flags);
	c_undo_bindings(env, old_variables, 0);
	return flags;
}

static int
c_tagbody(cl_env_ptr env, cl_object args, int flags)
{
	cl_object old_env = env->c_env->variables;
	cl_index tag_base;
	cl_object labels = Cnil, label, body;
	cl_type item_type;
	int nt, i;

	/* count the tags */
	for (nt = 0, body = args; !ecl_endp(body); body = CDR(body)) {
		label = CAR(body);
		item_type = type_of(CAR(body));
		if (item_type == t_symbol || item_type == t_fixnum ||
	            item_type == t_bignum) {
			labels = CONS(CONS(label,MAKE_FIXNUM(nt)), labels);
			nt += 1;
		}
	}
	if (nt == 0) {
		compile_body(env, args, 0);
		return compile_form(env, Cnil, flags);
	}
	asm_op2c(env, OP_BLOCK, MAKE_FIXNUM(0));
	c_register_tags(env, labels);
	asm_op2(env, OP_TAGBODY, nt);
	tag_base = current_pc(env);
	for (i = nt; i; i--)
		asm_arg(env, 0);

	for (body = args; !ecl_endp(body); body = CDR(body)) {
		label = CAR(body);
		item_type = type_of(label);
		if (item_type == t_symbol || item_type == t_fixnum ||
	            item_type == t_bignum) {
			asm_complete(env, 0, tag_base);
			tag_base += OPARG_SIZE;
		} else {
			compile_form(env, label, FLAG_IGNORE);
		}
	}
	asm_op(env, OP_EXIT_TAGBODY);
	c_undo_bindings(env, old_env, 0);
	return FLAG_REG0;
}


/*
	The OP_THROW jumps to an enclosing OP_CATCH whose tag
	matches the one of the throw. The tag is taken from the
	stack, while the output values are left in VALUES().
*/
static int
c_throw(cl_env_ptr env, cl_object stmt, int flags) {
	cl_object tag = pop(&stmt);
	cl_object form = pop(&stmt);
	if (stmt != Cnil)
		FEprogram_error("THROW: Too many arguments.",0);
	compile_form(env, tag, FLAG_PUSH);
	compile_form(env, form, FLAG_VALUES);
	asm_op(env, OP_THROW);
	return flags;
}


static int
c_unwind_protect(cl_env_ptr env, cl_object args, int flags) {
	cl_index label = asm_jmp(env, OP_PROTECT);

	flags = maybe_values(flags);

	/* Compile form to be protected */
	flags = compile_form(env, pop(&args), flags);
	asm_op(env, OP_PROTECT_NORMAL);

	/* Compile exit clause */
	asm_complete(env, OP_PROTECT, label);
	compile_body(env, args, FLAG_IGNORE);
	asm_op(env, OP_PROTECT_EXIT);

	return flags;
}


/*
	The OP_VALUES moves N values from the stack to VALUES().

		[OP_VALUES + n]
*/
static int
c_values(cl_env_ptr env, cl_object args, int flags) {
	if (!(flags & FLAG_USEFUL)) {
		/* This value will be discarded. We do not care to
		   push it or to save it in VALUES */
		if (ecl_endp(args))
			return flags;
		return compile_body(env, args, flags);
	} else if (flags & FLAG_PUSH) {
		/* We only need the first value. However, the rest
		   of arguments HAVE to be be evaluated */
		if (ecl_endp(args))
			return compile_form(env, Cnil, flags);
		flags = compile_form(env, pop(&args), FLAG_PUSH);
		compile_body(env, args, FLAG_IGNORE);
		return flags;
	} else if (ecl_endp(args)) {
		asm_op(env, OP_NOP);
	} else {
		int n = 0;
		while (!ecl_endp(args)) {
			compile_form(env, pop_maybe_nil(&args), FLAG_PUSH);
			n++;
		}
		asm_op2(env, OP_VALUES, n);
	}
	return FLAG_VALUES;
}


static int
compile_form(cl_env_ptr env, cl_object stmt, int flags) {
        const cl_compiler_ptr c_env = env->c_env;
	cl_object code_walker = ECL_SYM_VAL(env, @'si::*code-walker*');
	compiler_record *l;
	cl_object function;
	bool push = flags & FLAG_PUSH;
	int new_flags;

	ecl_bds_bind(env, @'si::*current-form*', stmt);
 BEGIN:
	if (code_walker != OBJNULL) {
		stmt = funcall(3, ECL_SYM_VAL(env,@'si::*code-walker*'), stmt,
			       CONS(c_env->variables, c_env->macros));
	}
	/*
	 * First try with variable references and quoted constants
	 */
	if (ATOM(stmt)) {
		cl_fixnum index;
		if (SYMBOLP(stmt) && stmt != Cnil) {
			cl_object stmt1 = c_macro_expand1(env, stmt);
			if (stmt1 != stmt) {
				stmt = stmt1;
				goto BEGIN;
			}
			index = c_var_ref(env, stmt,0,FALSE);
			if (index >= 0) {
				asm_op2(env, push? OP_PUSHV : OP_VAR, index);
			} else {
				asm_op2c(env, push? OP_PUSHVS : OP_VARS, stmt);
			}
		} else
	QUOTED:
		if ((flags & FLAG_USEFUL)) {
			cl_fixnum n;
			if (stmt == Cnil) {
				asm_op(env, push? OP_PUSHNIL : OP_NIL);
			} else if (FIXNUMP(stmt) && (n = fix(stmt)) <= MAX_OPARG
                                   && n >= -MAX_OPARG) {
				asm_op2(env, push? OP_PINT : OP_INT, n);
			} else {
				asm_op2c(env, push? OP_PUSHQ : OP_QUOTE, stmt);
			}
		}

		if (flags & FLAG_VALUES)
			new_flags = (flags & ~FLAG_VALUES) | FLAG_REG0;
		else
			new_flags = flags;
		goto OUTPUT;
	}
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
	for (l = database; l->symbol != OBJNULL; l++) {
		/*cl_print(1, l->symbol);*/
		if (l->symbol == function) {
			c_env->lexical_level += l->lexical_increment;
			if (c_env->stepping && function != @'function' &&
			    c_env->lexical_level)
				asm_op2c(env, OP_STEPIN, stmt);
			new_flags = (*(l->compiler))(env, CDR(stmt), flags);
			if (c_env->stepping && function != @'function' &&
			    c_env->lexical_level)
				asm_op(env, OP_STEPOUT);
			goto OUTPUT;
		}
	}
	/*
	 * Next try to macroexpand
	 */
	{
		cl_object new_stmt = c_macro_expand1(env, stmt);
		if (new_stmt != stmt){
			stmt = new_stmt;
			goto BEGIN;
		}
	}
	if (ecl_symbol_type(function) & stp_special_form)
		FEprogram_error("BYTECOMPILE-FORM: Found no macroexpander \
for special form ~S.", 1, function);
 ORDINARY_CALL:
	/*
	 * Finally resort to ordinary function calls.
	 */
	if (c_env->stepping)
		asm_op2c(env, OP_STEPIN, stmt);
	if (function >= (cl_object)cl_symbols
	    && function < (cl_object)(cl_symbols + cl_num_symbols_in_core))
	{
		cl_object f = SYM_FUN(function);
		cl_type t = (f == OBJNULL)? t_other : type_of(f);
		if (t == t_cfunfixed) {
			cl_object args = ECL_CONS_CDR(stmt);
			cl_index n = ecl_length(args);
			if (f->cfun.narg == 1 && n == 1) {
				compile_form(env, ECL_CONS_CAR(args), FLAG_REG0);
				asm_op2c(env, OP_CALLG1, function);
				new_flags = FLAG_VALUES;
				goto OUTPUT;
			} else if (f->cfun.narg == 2 && n == 2) {
				compile_form(env, ECL_CONS_CAR(args), FLAG_PUSH);
				args = ECL_CONS_CDR(args);
				compile_form(env, ECL_CONS_CAR(args), FLAG_REG0);
				asm_op2c(env, OP_CALLG2, function);
				new_flags = FLAG_VALUES;
				goto OUTPUT;
			}
		}
	}
	new_flags = c_call(env, stmt, flags);
 OUTPUT:
	/*
		flags		new_flags		action
		PUSH		PUSH			---
		PUSH		VALUES			OP_PUSH
		PUSH		REG0			OP_PUSH
		VALUES		PUSH			Impossible
		VALUES		VALUES			---
		VALUES		REG0			OP_VALUEREG0
		REG0		PUSH			Impossible
		REG0		VALUES			---
		REG0		REG0			---
	*/
	if (push) {
		if (new_flags & (FLAG_REG0 | FLAG_VALUES))
			asm_op(env, OP_PUSH);
	} else if (flags & FLAG_VALUES) {
		if (new_flags & FLAG_REG0) {
			asm_op(env, OP_VALUEREG0);
		} else if (new_flags & FLAG_PUSH) {
			FEerror("Internal error in bytecodes compiler", 0);
		}
	} else if (new_flags & FLAG_PUSH) {
		FEerror("Internal error in bytecodes compiler", 0);
	}
	ecl_bds_unwind1(env);
	return flags;
}


static int
compile_body(cl_env_ptr env, cl_object body, int flags) {
        const cl_compiler_ptr old_c_env = env->c_env;
	if (old_c_env->lexical_level == 0 && !ecl_endp(body)) {
                struct ecl_stack_frame frame;
                frame.t = t_frame;
                frame.stack = frame.base = 0;
                frame.size = 0;
                frame.env = env;
		while (!ecl_endp(CDR(body))) {
			struct cl_compiler_env new_c_env = *old_c_env;
			cl_index handle;
			cl_object bytecodes;
			env->c_env = &new_c_env;
			handle = asm_begin(env);
			compile_form(env, CAR(body), FLAG_VALUES);
			asm_op(env, OP_EXIT);
			VALUES(0) = Cnil;
			NVALUES = 0;
			bytecodes = asm_end(env, handle);
			ecl_interpret((cl_object)&frame, new_c_env.lex_env, bytecodes);
			asm_clear(env, handle);
			env->c_env = old_c_env;
#ifdef GBC_BOEHM
			GC_free(bytecodes->bytecodes.code);
			GC_free(bytecodes->bytecodes.data);
			GC_free(bytecodes);
#endif
			body = CDR(body);
		}
	}
	if (ecl_endp(body)) {
		return compile_form(env, Cnil, flags);
	} else {
		do {
			if (ecl_endp(CDR(body)))
				return compile_form(env, CAR(body), flags);
			compile_form(env, CAR(body), FLAG_IGNORE);
			body = CDR(body);
		} while (1);
	}
}

/* ------------------------ INLINED FUNCTIONS -------------------------------- */

static int
c_cons(cl_env_ptr env, cl_object args, int flags)
{
	cl_object car, cdr;
	if (ecl_length(args) != 2) {
		FEprogram_error("CONS: Wrong number of arguments", 0);
	}
	compile_form(env, cl_first(args), FLAG_PUSH);
	compile_form(env, cl_second(args), FLAG_REG0);
	asm_op(env, OP_CONS);
	return FLAG_REG0;
}

static int
c_endp(cl_env_ptr env, cl_object args, int flags)
{
	cl_object list = pop(&args);
	if (args != Cnil) {
		FEprogram_error("ENDP: Too many arguments", 0);
	}
	compile_form(env, list, FLAG_REG0);
	asm_op(env, OP_ENDP);
	return FLAG_REG0;
}

static int
c_car(cl_env_ptr env, cl_object args, int flags)
{
	cl_object list = pop(&args);
	if (args != Cnil) {
		FEprogram_error("CAR: Too many arguments", 0);
	}
	compile_form(env, list, FLAG_REG0);
	asm_op(env, OP_CAR);
	return FLAG_REG0;
}

static int
c_cdr(cl_env_ptr env, cl_object args, int flags)
{
	cl_object list = pop(&args);
	if (args != Cnil) {
		FEprogram_error("CDR: Too many arguments", 0);
	}
	compile_form(env, list, FLAG_REG0);
	asm_op(env, OP_CDR);
	return FLAG_REG0;
}

static int
c_list_listA(cl_env_ptr env, cl_object args, int flags, int op)
{
	cl_index n = ecl_length(args);
	if (n == 0) {
		return compile_form(env, Cnil, flags);
	} else {
		while (ECL_CONS_CDR(args) != Cnil) {
			compile_form(env, ECL_CONS_CAR(args), FLAG_PUSH);
			args = ECL_CONS_CDR(args);
		}
		compile_form(env, ECL_CONS_CAR(args), FLAG_REG0);
		asm_op2(env, op, n);
		return FLAG_REG0;
	}
}

static int
c_list(cl_env_ptr env, cl_object args, int flags)
{
	return c_list_listA(env, args, flags, OP_LIST);
}

static int
c_listA(cl_env_ptr env, cl_object args, int flags)
{
	return c_list_listA(env, args, flags, OP_LISTA);
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
	if (context == @'function') { \
		if (ecl_symbol_type(v) & stp_constant)	\
			FEillegal_variable_name(v); } \
	push(v, list)

/*
  Handles special declarations, removes declarations from body
 */
@(defun si::process_declarations (body &optional doc)
	cl_object documentation = Cnil, declarations = Cnil, form, specials = Cnil;
	cl_object decls, vars, v;
@
	/* BEGIN: SEARCH DECLARE */
	for (; !ecl_endp(body); body = CDR(body)) {
	  form = CAR(body);

	  if (!Null(doc) && type_of(form) == t_base_string && !ecl_endp(CDR(body))) {
	    if (documentation == Cnil)
	      documentation = form;
	    else
	      break;
	    continue;
	  }

	  if (ATOM(form) || (CAR(form) != @'declare'))
	    break;

	  for (decls = CDR(form); !ecl_endp(decls); decls = CDR(decls)) {
	    cl_object sentence = CAR(decls);
	    if (ATOM(sentence))
	      FEill_formed_input();
	    push(sentence, declarations);
	    if (CAR(sentence) == @'special')
	      for (vars = CDR(sentence); !ecl_endp(vars); vars = CDR(vars)) {
		v = CAR(vars);
		assert_type_symbol(v);
		push(v,specials);
	      }
	  }
	}
	/* END: SEARCH DECLARE */

	@(return declarations body documentation specials)
@)

static size_t si_process_lambda_ctr = 0;

cl_object
si_process_lambda(cl_object lambda)
{
	cl_object documentation, declarations, specials;
	cl_object lambda_list, body;

	if (ATOM(lambda))
		FEprogram_error("LAMBDA: No lambda list.", 0);
	lambda_list = CAR(lambda);

	declarations = @si::process-declarations(2, CDR(lambda), Ct);
	body = VALUES(1);
	documentation = VALUES(2);
	specials = VALUES(3);

	si_process_lambda_ctr++;

	VALUES(0) = si_process_lambda_list(lambda_list, @'function');
	VALUES(NVALUES++) = documentation;
	VALUES(NVALUES++) = specials;
	VALUES(NVALUES++) = declarations;
	VALUES(NVALUES++) = body;
	return VALUES(0);
}

/*
 * (si::process-lambda-list lambda-list context)
 *
 * Parses different types of lambda lists. CONTEXT may be MACRO,
 * FTYPE, FUNCTION, METHOD or DESTRUCTURING-BIND, and determines the
 * valid sytax. The output is made of several values:
 *
 * VALUES(0) = (N req1 ... )			; required values
 * VALUES(1) = (N opt1 init1 flag1 ... )	; optional values
 * VALUES(2) = rest-var				; rest-variable, if any
 * VALUES(3) = key-flag				; T if &key was supplied
 * VALUES(4) = (N key1 var1 init1 flag1 ... )	; keyword arguments
 * VALUES(5) = allow-other-keys			; flag &allow-other-keys
 * VALUES(6) = (N aux1 init1 ... )		; auxiliary variables
 *
 * 1°) The prefix "N" is an integer value denoting the number of
 * variables which are declared within this section of the lambda
 * list.
 *
 * 2°) The INIT* arguments are lisp forms which are evaluated when
 * no value is provided.
 *
 * 3°) The FLAG* arguments is the name of a variable which holds a
 * boolean value in case an optional or keyword argument was
 * provided. If it is NIL, no such variable exists.
 */

cl_object
si_process_lambda_list(cl_object org_lambda_list, cl_object context)
{
#define AT_REQUIREDS	0
#define AT_OPTIONALS	1
#define AT_REST		2
#define AT_KEYS		3
#define AT_OTHER_KEYS	4
#define AT_AUXS		5
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v, key, init, spp, lambda_list = org_lambda_list;
	cl_object reqs = Cnil, opts = Cnil, keys = Cnil, rest = Cnil, auxs = Cnil;
	int nreq = 0, nopt = 0, nkey = 0, naux = 0, stage = 0;
	cl_object allow_other_keys = Cnil;
	cl_object key_flag = Cnil;

	if (!CONSP(lambda_list) && lambda_list != Cnil)
		goto ILLEGAL_LAMBDA;
LOOP:
	if (ATOM(lambda_list)) {
		if (lambda_list == Cnil)
			goto OUTPUT;
		else if (context == @'function')
			goto ILLEGAL_LAMBDA;
		else {
			v = lambda_list;
			lambda_list = Cnil;
			goto REST;
		}
	}
	v = CAR(lambda_list);
	lambda_list = CDR(lambda_list);
	if (v == @'&optional') {
		if (stage >= AT_OPTIONALS)
			goto ILLEGAL_LAMBDA;
		stage = AT_OPTIONALS;
		goto LOOP;
	}
	if (v == @'&rest' || (v == @'&body' && (context == @'si::macro' || context == @'destructuring-bind'))) {
		if (ATOM(lambda_list))
			goto ILLEGAL_LAMBDA;
		v = CAR(lambda_list);
		lambda_list = CDR(lambda_list);
REST:		if (stage >= AT_REST)
			goto ILLEGAL_LAMBDA;
		stage = AT_REST;
		rest = v;
		goto LOOP;
	}
	if (v == @'&key') {
		if (stage >= AT_KEYS)
			goto ILLEGAL_LAMBDA;
		key_flag = Ct;
		stage = AT_KEYS;
		goto LOOP;
	}
	if (v == @'&aux') {
		if (stage >= AT_AUXS)
			goto ILLEGAL_LAMBDA;
		stage = AT_AUXS;
		goto LOOP;
	}
	if (v == @'&allow-other-keys') {
		allow_other_keys = Ct;
		if (stage != AT_KEYS)
			goto ILLEGAL_LAMBDA;
		stage = AT_OTHER_KEYS;
		goto LOOP;
	}
	switch (stage) {
	case AT_REQUIREDS:
		nreq++;
		push_var(v, reqs);
		break;
	case AT_OPTIONALS:
		spp = Cnil;
		init = Cnil;
		if (!ATOM(v)) {
			cl_object x = v;
			v = CAR(x);
			if (!ecl_endp(x = CDR(x))) {
				init = CAR(x);
				if (!ecl_endp(x = CDR(x))) {
					spp = CAR(x);
					if (!ecl_endp(CDR(x)))
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
		break;
	case AT_REST:
		/* If we get here, the user has declared more than one
		 * &rest variable, as in (lambda (&rest x y) ...) */
		goto ILLEGAL_LAMBDA;
	case AT_KEYS:
		init = Cnil;
		spp = Cnil;
		if (!ATOM(v)) {
			cl_object x = v;
			v = CAR(x);
			if (!ecl_endp(x = CDR(x))) {
				init = CAR(x);
				if (!ecl_endp(x = CDR(x))) {
					spp = CAR(x);
					if (!ecl_endp(CDR(x)))
						goto ILLEGAL_LAMBDA;
				}
			}
		}
		if (CONSP(v)) {
			key = CAR(v);
			if (ecl_endp(CDR(v)) || !ecl_endp(CDDR(v)))
				goto ILLEGAL_LAMBDA;
			v = CADR(v);
			if (context == @'function')
				assert_type_symbol(v);
			assert_type_symbol(key);
		} else {
			int intern_flag;
			key = ecl_intern(ecl_symbol_name(v), cl_core.keyword_package,
					 &intern_flag);
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
		break;
	default:
		if (ATOM(v)) {
			init = Cnil;
		} else if (ecl_endp(CDDR(v))) {
			cl_object x = v;
			v = CAR(x);
			init = CADR(x);
		} else
			goto ILLEGAL_LAMBDA;
		naux++;
		push_var(v, auxs);
		push(init, auxs);
	}
	goto LOOP;

OUTPUT:
	if ((nreq+nopt+(!Null(rest))+nkey) >= CALL_ARGUMENTS_LIMIT)
		FEprogram_error("LAMBDA: Argument list ist too long, ~S.", 1,
				org_lambda_list);
	@(return CONS(MAKE_FIXNUM(nreq), cl_nreverse(reqs))
		 CONS(MAKE_FIXNUM(nopt), cl_nreverse(opts))
		 rest
		 key_flag
		 CONS(MAKE_FIXNUM(nkey), cl_nreverse(keys))
		 allow_other_keys
		 cl_nreverse(auxs))

ILLEGAL_LAMBDA:
	FEprogram_error("LAMBDA: Illegal lambda list ~S.", 1, org_lambda_list);
}

static void
c_default(cl_env_ptr env, cl_object var, cl_object stmt, cl_object flag, cl_object specials)
{
        /* Flag is in REG0, value, if it exists, in stack */
        cl_index label;
        label = asm_jmp(env, OP_JT);
        compile_form(env, stmt, FLAG_PUSH);
        if (Null(flag)) {
                asm_complete(env, OP_JT, label);
        } else {
                compile_form(env, Cnil, FLAG_REG0);
                asm_complete(env, OP_JT, label);
                c_bind(env, flag, specials);
        }
        c_pbind(env, var, specials);
}

cl_object
ecl_make_lambda(cl_env_ptr env, cl_object name, cl_object lambda) {
	cl_object reqs, opts, rest, key, keys, auxs, allow_other_keys;
	cl_object specials, doc, decl, body, output;
	int nopts, nkeys;
	cl_index handle;
	struct cl_compiler_env *old_c_env, new_c_env;

	ecl_bds_bind(env, @'si::*current-form*',
		     @list*(3, @'ext::lambda-block', name, lambda));

	old_c_env = env->c_env;
	c_new_env(env, &new_c_env, Cnil, old_c_env);

	new_c_env.lexical_level++;
	new_c_env.coalesce = 0;

	reqs = si_process_lambda(lambda);
	opts = VALUES(1);
	rest = VALUES(2);
	key  = VALUES(3);
	keys = VALUES(4);
	allow_other_keys = VALUES(5);
	auxs = VALUES(6);
	doc  = VALUES(7);
	specials = VALUES(8);
	decl = VALUES(9);
	body = VALUES(10);

	handle = asm_begin(env);

	/* Transform (SETF fname) => fname */
	if (!Null(name) && Null(si_valid_function_name_p(name)))
		FEprogram_error("LAMBDA: Not a valid function name ~S",1,name);

	/* We register as special variable a symbol which is not
	 * to be used. We use this to mark the boundary of a function
	 * environment and when code-walking */
	c_register_var(env, cl_make_symbol(make_constant_base_string("FUNCTION")),
		       TRUE, FALSE);

	new_c_env.constants = Cnil;
        new_c_env.coalesce = TRUE;
	asm_constant(env, doc);
	asm_constant(env, decl);

	reqs = ECL_CONS_CDR(reqs);		/* Required arguments */
	while (!ecl_endp(reqs)) {
		cl_object var = pop(&reqs);
                asm_op(env, OP_POPREQ);
                c_bind(env, var, specials);
	}
        opts = ECL_CONS_CDR(opts);
        while (!ecl_endp(opts)) {		/* Optional arguments */
                cl_object var = pop(&opts);
                cl_object stmt = pop(&opts);
                cl_object flag = pop(&opts);
                asm_op(env, OP_POPOPT);
                c_default(env, var, stmt, flag, specials);
        }
        if (Null(rest) && Null(key)) {		/* Check no excess arguments */
                asm_op(env, OP_NOMORE);
        }
        if (!Null(rest)) {			/* &rest argument */
                asm_op(env, OP_POPREST);
                c_bind(env, rest, specials);
        }
        if (!Null(key)) {
                cl_object aux = CONS(allow_other_keys,Cnil);
                cl_object names = Cnil;
                asm_op2c(env, OP_PUSHKEYS, aux);
                keys = ECL_CONS_CDR(keys);
                while (!ecl_endp(keys)) {
                        cl_object name = pop(&keys);
                        cl_object var = pop(&keys);
                        cl_object stmt = pop(&keys);
                        cl_object flag = pop(&keys);
                        names = CONS(name, names);
                        asm_op(env, OP_POP);
                        c_default(env, var, stmt, flag, specials);
                }
                ECL_RPLACD(aux, names);
        }

	while (!ecl_endp(auxs)) {		/* Local bindings */
		cl_object var = pop(&auxs);
		cl_object value = pop(&auxs);
		compile_form(env, value, FLAG_REG0);
		c_bind(env, var, specials);
	}
	c_declare_specials(env, specials);

	if (!Null(name)) {
		compile_form(env, @list*(3, @'block', si_function_block_name(name),
                                         body), FLAG_VALUES);
	} else {
		compile_body(env, body, FLAG_VALUES);
	}

        /* Only undo special bindings */
	c_undo_bindings(env, old_c_env->variables, 1);
	asm_op(env, OP_EXIT);

	output = asm_end(env, handle);
	output->bytecodes.name = name;
	output->bytecodes.definition = Null(ecl_symbol_value(@'si::*keep-definitions*'))?
		Cnil : lambda;

	env->c_env = old_c_env;

	ecl_bds_unwind1(env);

	return output;
}

static cl_object
ecl_function_block_name(cl_object name)
{
	if (SYMBOLP(name)) {
		return name;
	} else if (CONSP(name) && ECL_CONS_CAR(name) == @'setf') {
		name = ECL_CONS_CDR(name);
		if (CONSP(name)) {
                        cl_object output = ECL_CONS_CAR(name);
                        if (SYMBOLP(output) && Null(ECL_CONS_CDR(name)))
                                return output;
                }
        }
        return NULL;
}

cl_object
si_function_block_name(cl_object name)
{
        cl_object output = ecl_function_block_name(name);
        if (!output)
                FEinvalid_function_name(name);
        @(return output)
}

cl_object
si_valid_function_name_p(cl_object name)
{
        name = ecl_function_block_name(name);
        @(return (name? Ct : Cnil))
}

cl_object
si_make_lambda(cl_object name, cl_object rest)
{
	cl_object lambda;
        const cl_env_ptr the_env = ecl_process_env();
	volatile cl_compiler_env_ptr old_c_env = the_env->c_env;
	struct cl_compiler_env new_c_env;

	c_new_env(the_env, &new_c_env, Cnil, 0);
	CL_UNWIND_PROTECT_BEGIN(the_env) {
		lambda = ecl_make_lambda(the_env, name, rest);
	} CL_UNWIND_PROTECT_EXIT {
		the_env->c_env = old_c_env;
	} CL_UNWIND_PROTECT_END;
	@(return lambda)
}

@(defun si::eval-with-env (form &optional (env Cnil) (stepping Cnil) (compiler_env_p Cnil))
	volatile cl_compiler_env_ptr old_c_env;
	struct cl_compiler_env new_c_env;
	volatile cl_index handle;
	struct ihs_frame ihs;
	cl_object bytecodes, interpreter_env, compiler_env;
@
	/*
	 * Compile to bytecodes.
	 */
	if (compiler_env_p == Cnil) {
		interpreter_env = env;
		compiler_env = Cnil;
	} else {
		interpreter_env = Cnil;
		compiler_env = env;
	}
	old_c_env = the_env->c_env;
	c_new_env(the_env, &new_c_env, compiler_env, 0);
	guess_environment(the_env, interpreter_env);
	new_c_env.lex_env = env;
	new_c_env.stepping = stepping != Cnil;
	handle = asm_begin(the_env);
	CL_UNWIND_PROTECT_BEGIN(the_env) {
		compile_form(the_env, form, FLAG_VALUES);
		asm_op(the_env, OP_EXIT);
		bytecodes = asm_end(the_env, handle);
		bytecodes->bytecodes.definition = form;
	} CL_UNWIND_PROTECT_EXIT {
		/* Clear up */
		the_env->c_env = old_c_env;
		memset(&new_c_env, 0, sizeof(new_c_env));
	} CL_UNWIND_PROTECT_END;

	/*
	 * Interpret using the given lexical environment.
	 */
	ecl_ihs_push(the_env, &ihs, bytecodes, Cnil);
	VALUES(0) = Cnil;
	NVALUES = 0;
	{
                struct ecl_stack_frame frame;
                cl_object output;
                frame.t = t_frame;
                frame.stack = frame.base = 0;
                frame.size = 0;
                frame.env = the_env;
                output = ecl_interpret((cl_object)&frame, interpreter_env, bytecodes);
#ifdef GBC_BOEHM
                GC_free(bytecodes->bytecodes.code);
                GC_free(bytecodes->bytecodes.data);
                GC_free(bytecodes);
#endif
                ecl_ihs_pop(the_env);
                return output;
	}
@)
