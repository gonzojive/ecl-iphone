/*
    disassembler.c -- Byte compiler and function evaluator
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecl.h"
#include "ecl-inl.h"
#include "bytecodes.h"

#define next_code(v) (*(v++))

static cl_object *disassemble(cl_object *vector);

static cl_object *base = NULL;

static void
print_noarg(const char *s) {
	princ_str(s, Cnil);
}

static void
print_oparg(const char *s, cl_fixnum n) {
	princ_str(s, Cnil);
	princ(MAKE_FIXNUM(n), Cnil);
}

static void
print_arg(const char *s, cl_object x) {
	princ_str(s, Cnil);
	princ(x, Cnil);
}

static void
print_oparg_arg(const char *s, cl_fixnum n, cl_object x) {
	princ_str(s, Cnil);
	princ(MAKE_FIXNUM(n), Cnil);
	princ_str(",", Cnil);
	princ(x, Cnil);
}

static cl_object *
disassemble_vars(const char *message, cl_object *vector, cl_index step) {
	cl_index n = fix(next_code(vector));

	if (n) {
	  terpri(Cnil);
	  print_noarg(message);
	  for (; n; n--, vector+=step) {
	    prin1(vector[0], Cnil);
	    if (n > 1) print_noarg(", ");
	  }
	}
	return vector;
}

static void
disassemble_lambda(cl_object *vector) {
	cl_object specials;
	cl_index n;

	/* Name of LAMBDA */
	print_arg("\nName:\t\t", next_code(vector));

	/* Variables that have been declared special */
	specials = next_code(vector);

	/* Print required arguments */
	vector = disassemble_vars("Required:\t", vector, 1);

	/* Print optional arguments */
	vector = disassemble_vars("Optionals:\t", vector, 3);

	/* Print rest argument */
	if (vector[0] != Cnil) {
		print_arg("\nRest:\t\t", vector[0]);
	}
	vector++;

	/* Print keyword arguments */
	if (vector[0] != Cnil) {
		print_arg("\nOther keys:\t", vector[0]);
	}
	vector++;
	vector = disassemble_vars("Keywords:\t", vector, 4);

	/* Print aux arguments */
	print_arg("\nDocumentation:\t", next_code(vector));
	print_arg("\nDeclarations:\t", next_code(vector));

	base = vector;
	while (vector[0] != MAKE_FIXNUM(OP_HALT))
		vector = disassemble(vector);
}

/* -------------------- DISASSEMBLER AIDS -------------------- */

static inline cl_fixnum
get_oparg(cl_object o) {
	return GET_OPARG(o);
}

static inline cl_fixnum
packed_label(cl_object *v) {
	return v + get_oparg(v[0]) - base;
}

static inline cl_fixnum
simple_label(cl_object *v) {
	return v + fix(v[0]) - base;
}

static cl_object
search_symbol(register cl_object s) {
	return s;
}
		
/* -------------------- DISASSEMBLER CORE -------------------- */

/* OP_BLOCK	label{arg}, block-name{symbol}
	...
   OP_EXIT
   label:

	Executes the enclosed code in a named block.
	LABEL points to the first instruction after OP_EXIT.
*/
static cl_object *
disassemble_block(cl_object *vector) {
	cl_object lex_old = lex_env;
	cl_fixnum exit = packed_label(vector-1);
	cl_object block_name = next_code(vector);

	lex_env = cl_listX(3, @':block', CONS(block_name, Cnil), lex_env);

	print_oparg_arg("BLOCK\t", exit, block_name);
	vector = disassemble(vector);
	print_noarg("\t\t; block");

	lex_env = lex_old;
	return vector;
}

/* OP_CATCH	label{arg}
   ...
   OP_EXIT
   label:

	Sets a catch point using the tag in VALUES(0). LABEL points
	to the first instruction after the end (OP_EXIT) of the block
*/
static cl_object *
disassemble_catch(cl_object *vector) {
	print_oparg("CATCH\t", packed_label(vector - 1));
	vector = disassemble(vector);
	print_noarg("\t\t; catch");
	return vector;
}

/* OP_DO	label
   ...		; code executed within a NIL block
   OP_EXIT
   label:

	High level construct for the DO and BLOCK forms.
*/
static cl_object *
disassemble_do(cl_object *vector) {
	cl_fixnum exit;
	cl_object lex_old = lex_env;
	lex_copy();

	exit = packed_label(vector-1);
	print_oparg("DO\t", exit);
	vector = disassemble(vector);
	print_noarg("\t\t; do");

	lex_env = lex_old;
	return vector;
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
disassemble_dolist(cl_object *vector) {
	cl_fixnum exit;
	cl_object lex_old = lex_env;

	lex_copy();
	exit = packed_label(vector-1);
	print_oparg("DOLIST\t", exit);
	vector = disassemble(vector);
	print_noarg("\t\t; dolist binding");
	vector = disassemble(vector);
	print_noarg("\t\t; dolist body");
	vector = disassemble(vector);
	print_noarg("\t\t; dolist");

	lex_env = lex_old;
	return vector;
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
disassemble_dotimes(cl_object *vector) {
	cl_fixnum exit;
	cl_object lex_old = lex_env;

	lex_copy();
	exit = packed_label(vector-1);
	print_oparg("DOTIMES\t", exit);
	vector = disassemble(vector);
	print_noarg("\t\t; dotimes times");
	vector = disassemble(vector);
	print_noarg("\t\t; dotimes body");
	vector = disassemble(vector);
	print_noarg("\t\t; dotimes");

	lex_env = lex_old;
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
disassemble_flet(cl_object *vector) {
	cl_object lex_old = lex_env;
	cl_index nfun = get_oparg(vector[-1]);

	print_noarg("FLET");
	lex_copy();
	while (nfun--) {
		cl_object fun = next_code(vector);
		print_noarg("\n\tFLET\t");
		@prin1(1, fun->bytecodes.data[0]);
	}
	vector = disassemble(vector);
	print_noarg("\t\t; flet");

	lex_env = lex_old;
	return vector;
}

/* OP_LABELS	nfun{arg}
   fun1{object}
   ...
   funn{object}
   ...
   OP_EXIT

	Executes the enclosed code in a lexical enviroment extended with
	the functions "fun1" ... "funn".
*/
static cl_object *
disassemble_labels(cl_object *vector) {
	cl_object lex_old = lex_env;
	cl_index nfun = get_oparg(vector[-1]);

	print_noarg("LABELS");
	lex_copy();
	while (nfun--) {
		cl_object fun = next_code(vector);
		print_arg("\n\tLABELS\t", fun->bytecodes.data[0]);
	}
	vector = disassemble(vector);
	print_noarg("\t\t; labels");

	lex_env = lex_old;
	return vector;
}

/* OP_MCALL
   ...
   OP_EXIT

	Saves the stack pointer, executes the enclosed code and
	funcalls VALUE(0) using the content of the stack.
*/
static cl_object *
disassemble_mcall(cl_object *vector) {
	print_noarg("MCALL");
	vector = disassemble(vector);
	print_noarg("\t\t; mcall");
	return vector;
}

/* OP_PROG1
   ...
   OP_EXIT

	Save the values in VALUES(..), execute the code enclosed, and
	restore the values.
*/
static cl_object *
disassemble_mprog1(cl_object *vector) {
	print_noarg("MPROG1");
	vector = disassemble(vector);
	print_noarg("\t\t; mprog1");
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
disassemble_msetq(cl_object *vector)
{
	int i = get_oparg(vector[-1]);
	bool newline = FALSE;

	while (i--) {
		cl_object var = next_code(vector);
		if (newline) {
			print_noarg("\n\t");
		} else
			newline = TRUE;
		if (FIXNUMP(var)) {
			@format(4, Ct, make_constant_string("MSETQ\t~D,VALUES(~D)"),
				var, MAKE_FIXNUM(i));
		} else {
			@format(4, Ct, make_constant_string("MSETQS\t~A,VALUES(~D)"),
				var, MAKE_FIXNUM(i));
		}
	}
	return vector;
}


/* OP_PROGV	bindings{list}
   ...
   OP_EXIT
	Execute the code enclosed with the special variables in BINDINGS
	set to the values in the list which was passed in VALUES(0).
*/
static cl_object *
disassemble_progv(cl_object *vector) {
	print_noarg("PROGV");
	vector = disassemble(vector);
	print_noarg("\t\t; progv");
	return vector;
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
disassemble_tagbody(cl_object *vector) {
	cl_index i, ntags = get_oparg(vector[-1]);
	cl_object lex_old = lex_env;
	lex_copy();

	print_noarg("TAGBODY");
	for (i=0; i<ntags; i++, vector++) {
		@format(4, Ct, make_constant_string("\n\tTAG\t~D @@ ~D"),
			MAKE_FIXNUM(i), MAKE_FIXNUM(simple_label(vector)));
	}
	vector = disassemble(vector);
	print_noarg("\t\t; tagbody");

	lex_env = lex_old;
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
disassemble_unwind_protect(cl_object *vector) {
	cl_fixnum exit = packed_label(vector-1);

	print_oparg("PROTECT\t", exit);
	vector = disassemble(vector);
	vector = disassemble(vector);
	print_noarg("\t\t; protect");

	return vector;
}

static cl_object *
disassemble(cl_object *vector) {
	const char *string;
	cl_type t;
	cl_object s;
	cl_fixnum n;
	cl_object line_format = make_constant_string("~%~4d\t");

 BEGIN:
	@format(3, Ct, line_format, MAKE_FIXNUM(vector-base));
	s = next_code(vector);
	t = type_of(s);
	if (t == t_symbol) {
		@prin1(1, search_symbol(s));
		goto BEGIN;
	}
	if (t != t_fixnum) {
		@prin1(1, s);
		goto BEGIN;
	}
	switch (GET_OP(s)) {

	/* OP_NOP
		Sets VALUES(0) = NIL and NValues = 1
	*/   		
	case OP_NOP:		string = "NOP";	goto NOARG;

	/* OP_QUOTE
		Sets VALUES(0) to an immediate value.
	*/
	case OP_QUOTE:		string = "QUOTE\t";
				s = next_code(vector);
				goto ARG;

	/* OP_VAR	n{arg}
		Sets NValues=1 and VALUES(0) to the value of the n-th local.
	*/
	case OP_VAR:		string = "VAR\t";
				n = get_oparg(s);
				goto OPARG;

	/* OP_VARS	var{symbol}
		Sets NValues=1 and VALUES(0) to the value of the symbol VAR.
		VAR should be either a special variable or a constant.
	*/
	case OP_VARS:		string = "VARS\t";
				s = next_code(vector);
				goto ARG;

	/* OP_PUSH
		Pushes the object in VALUES(0).
	*/
	case OP_PUSH:		string = "PUSH\tVALUES(0)";
				goto NOARG;

	/* OP_PUSHV	n{arg}
		Pushes the value of the n-th local onto the stack.
	*/
	case OP_PUSHV:		string = "PUSHV\t";
				n = get_oparg(s);
				goto OPARG;

	/* OP_PUSHVS	var{symbol}
		Pushes the value of the symbol VAR onto the stack.
		VAR should be either a special variable or a constant.
	*/
	case OP_PUSHVS:		string = "PUSHVS\t";
				s = next_code(vector);
				goto ARG;

	/* OP_PUSHQ	value{object}
		Pushes "value" onto the stack.
	*/
	case OP_PUSHQ:		string = "PUSH\t'";
				s = next_code(vector);
				goto ARG;

	/* OP_PUSHVALUES
		Pushes the values output by the last form.
	*/
	case OP_PUSHVALUES:	string = "PUSH\tVALUES";
				goto NOARG;

	case OP_BLOCK:		vector = disassemble_block(vector);
				break;

	/* OP_CALLG	n{arg}, function-name{symbol}
		Calls the global function with N arguments which have
		been deposited in the stack. The output values are
		left in VALUES(...)
	*/
	case OP_CALLG:		string = "CALLG\t";
				n = get_oparg(s);
				s = next_code(vector);
				goto OPARG_ARG;

	/* OP_FCALL	n{arg}
		Calls the function in VALUES(0) with N arguments which
		have been deposited in the stack. The output values
		are left in VALUES(...)
	*/
	case OP_FCALL:		string = "FCALL\t";
				n = get_oparg(s);
				goto OPARG;

	/* OP_PCALLG	n{arg}, function-name{symbol}
		Calls the global function with N arguments which have
		been deposited in the stack. The first output value is
		left on the stack.
	*/
	case OP_PCALLG:		string = "PCALLG\t";
				n = get_oparg(s);
				s = next_code(vector);
				goto OPARG_ARG;

	/* OP_PFCALL	n{arg}
		Calls the function in VALUES(0) with N arguments which
		have been deposited in the stack. The first output value
		is pushed on the stack.
	*/
	case OP_PFCALL:		string = "PFCALL\t";
				n = get_oparg(s);
				goto OPARG;

	case OP_MCALL:		vector = disassemble_mcall(vector);
				break;
	case OP_CATCH:		vector = disassemble_catch(vector);
				break;

	/* OP_EXIT
		Marks the end of a high level construct (BLOCK, CATCH...)
	*/
	case OP_EXIT:		print_noarg("EXIT");
				return vector;

	/* OP_HALT
		Marks the end of a function.
	*/
	case OP_HALT:		print_noarg("HALT");
				return vector-1;

	case OP_FLET:		vector = disassemble_flet(vector);
				break;
	case OP_LABELS:		vector = disassemble_labels(vector);
				break;

	/* OP_LFUNCTION	name{symbol}
		Extracts the function associated to a symbol. The function
		may be defined in the global environment or in the local
		environment. This last value takes precedence.
	*/
	case OP_LFUNCTION:	string = "LOCFUNC\t";
				n = get_oparg(s);
				goto OPARG;

	/* OP_FUNCTION	name{symbol}
		Extracts the function associated to a symbol. The function
		may be defined in the global environment or in the local
		environment. This last value takes precedence.
	*/
	case OP_FUNCTION:	string = "SYMFUNC\t";
				s = next_code(vector);
				goto ARG;

	/* OP_CLOSE	name{symbol}
		Extracts the function associated to a symbol. The function
		may be defined in the global environment or in the local
		environment. This last value takes precedence.
	*/
	case OP_CLOSE:		string = "CLOSE\t";
				s = next_code(vector);
				goto ARG;

	/* OP_GO	n{arg}, tag-name{symbol}
		Jumps to the tag which is defined at the n-th position in
		the lexical environment. TAG-NAME is kept for debugging
		purposes.
	*/
	case OP_GO:		string = "GO\t";
				n = get_oparg(s);
				s = next_code(vector);
				goto OPARG_ARG;

	/* OP_RETURN	n{arg}
		Returns from the block whose record in the lexical environment
		occuppies the n-th position.
	*/
	case OP_RETURN:		string = "RETFROM";
				n = get_oparg(s);
				goto OPARG;

	/* OP_THROW
		Jumps to an enclosing CATCH form whose tag matches the one
		of the THROW. The tag is taken from the stack, while the
		output values are left in VALUES(...).
	*/
	case OP_THROW:		string = "THROW";
				goto NOARG;

	/* OP_JMP	label{arg}
	   OP_JNIL	label{arg}
	   OP_JT	label{arg}
	   OP_JEQ	label{arg}, value{object}
	   OP_JNEQ	label{arg}, value{object}
		Direct or conditional jumps. The conditional jumps are made
		comparing with the value of VALUES(0).
	*/
	case OP_JMP:		string = "JMP\t";
				n = packed_label(vector-1);
				goto OPARG;
	case OP_JNIL:		string = "JNIL\t";
				n = packed_label(vector-1);
				goto OPARG;
	case OP_JT:		string = "JT\t";
				n = packed_label(vector-1);
				goto OPARG;
	case OP_JEQ:		string = "JEQ\t";
				s = next_code(vector);
				n = packed_label(vector-2);
				goto OPARG_ARG;
	case OP_JNEQ:		string = "JNEQ\t";
				s = next_code(vector);
				n = packed_label(vector-2);
				goto OPARG_ARG;

	/* OP_UNBIND	n{arg}
		Undo "n" bindings of lexical variables.
	*/
	case OP_UNBIND:		string = "UNBIND\t";
				n = get_oparg(s);
				goto OPARG;
	/* OP_UNBINDS	n{arg}
		Undo "n" bindings of special variables.
	*/
	case OP_UNBINDS:	string = "UNBINDS\t";
				n = get_oparg(s);
				goto OPARG;
	/* OP_BIND	name{symbol}
	   OP_PBIND	name{symbol}
	   OP_BINDS	name{symbol}
	   OP_PBINDS	name{symbol}
		Binds a lexical or special variable to the either the
		value of VALUES(0), to the first value of the stack, or
		to the n-th value of VALUES(...).
	*/
	case OP_BIND:		string = "BIND\t";
				s = next_code(vector);
				goto ARG;
	case OP_PBIND:		string = "PBIND\t";
				s = next_code(vector);
				goto ARG;
	case OP_VBIND:		string = "VBIND\t";
				s = next_code(vector);
				goto ARG;
	case OP_BINDS:		string = "BINDS\t";
				s = next_code(vector);
				goto ARG;
	case OP_PBINDS:		string = "PBINDS\t";
				s = next_code(vector);
				goto ARG;
	case OP_VBINDS:		string = "VBINDS\t";
				s = next_code(vector);
				goto ARG;
	/* OP_SETQ	n{arg}
	   OP_PSETQ	n{arg}
	   OP_SETQS	var-name{symbol}
	   OP_PSETQS	var-name{symbol}
		Sets either the n-th local or a special variable VAR-NAME,
		to either the value in VALUES(0) (OP_SETQ[S]) or to the 
		first value on the stack (OP_PSETQ[S]).
	*/
	case OP_SETQ:		string = "SETQ\t";
				n = get_oparg(s);
				goto OPARG;
	case OP_PSETQ:		string = "PSETQ\t";
				n = get_oparg(s);
				goto OPARG;
	case OP_SETQS:		string = "SETQS";
				s = next_code(vector);
				goto ARG;
	case OP_PSETQS:		string = "PSETQS";
				s = next_code(vector);
				goto ARG;

	case OP_MSETQ:		vector = disassemble_msetq(vector);
				break;
	case OP_MPROG1:		vector = disassemble_mprog1(vector);
				break;
	case OP_PROGV:		vector = disassemble_progv(vector);
				break;

	/* OP_VALUES	n{arg}
		Pop N values from the stack and store them in VALUES(...)
	*/
	case OP_VALUES:		string = "VALUES\t";
				n = get_oparg(s);
				goto OPARG;
	/* OP_NTHVAL
		Set VALUES(0) to the N-th value of the VALUES(...) list.
		The index N-th is extracted from the top of the stack.
	*/
	case OP_NTHVAL:		string = "NTHVAL\t";
				goto NOARG;
	case OP_DOLIST:		vector = disassemble_dolist(vector);
				break;
	case OP_DOTIMES:	vector = disassemble_dotimes(vector);
				break;
	case OP_DO:		vector = disassemble_do(vector);
				break;
	case OP_TAGBODY:	vector = disassemble_tagbody(vector);
				break;
	case OP_UNWIND:		vector = disassemble_unwind_protect(vector);
				break;
	default:
		FEerror("Unknown code ~S", 1, MAKE_FIXNUM(*(vector-1)));
		return vector;
	NOARG:			print_noarg(string);
				break;
	ARG:			print_noarg(string);
				@prin1(1, s);
				break;
	OPARG:			print_oparg(string, n);
				break;
	OPARG_ARG:		print_oparg_arg(string, n, s);
				break;
	}
	goto BEGIN;
}

cl_object
si_bc_disassemble(cl_object v)
{
	if (type_of(v) == t_bytecodes) {
		disassemble_lambda(v->bytecodes.data);
		@(return v)
	}
	@(return Cnil)
}

cl_object
si_bc_split(cl_object b)
{
	cl_object vector;

	if (type_of(b) != t_bytecodes)
		@(return Cnil Cnil)
	vector = cl_alloc_simple_vector(b->bytecodes.size, aet_object);
	vector->vector.self.t = b->bytecodes.data;
	@(return b->bytecodes.lex vector)
}
