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

static cl_object *
disassemble_vars(const char *message, cl_object *vector, cl_index step) {
	cl_index n = fix(next_code(vector));

	if (n) {
	  @terpri(0);
	  printf(message);
	  for (; n; n--, vector+=step) {
	    @prin1(1,vector[0]);
	    if (n > 1) printf(", ");
	  }
	}
	return vector;
}

static void
disassemble_lambda(cl_object *vector) {
	cl_object specials;
	cl_index n;

	@terpri(0);
	/* Name of LAMBDA */
	printf("Name:\t\t");
	@prin1(1, next_code(vector));

	/* Variables that have been declared special */
	specials = next_code(vector);

	/* Print required arguments */
	vector = disassemble_vars("Required:\t", vector, 1);

	/* Print optional arguments */
	vector = disassemble_vars("Optionals:\t", vector, 3);

	/* Print rest argument */
	if (vector[0] != Cnil) {
		@terpri(0);
		printf("Rest:\t\t%s");
		@prin1(1, vector[0]);
	}
	vector++;

	/* Print keyword arguments */
	if (vector[0] != Cnil) {
		@terpri(0);
		printf("Other keys:\t");
		@prin1(1, vector[0]);
	}
	vector++;
	vector = disassemble_vars("Keywords:\t", vector, 4);

	/* Print aux arguments */
	@terpri(0);
	printf("\nDocumentation:\t");
	@prin1(1, next_code(vector));
	printf("\nDeclarations:\t");
	@prin1(1, next_code(vector));

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

	lex_env = listX(3, @':block', CONS(block_name, Cnil), lex_env);

	printf("BLOCK\t");
	@prin1(1, block_name);
	printf(",%d", exit);
	vector = disassemble(vector);
	printf("\t\t; block");

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
	printf("CATCH\t%d", packed_label(vector - 1));
	vector = disassemble(vector);
	printf("\t\t; catch");
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
	printf("DO\t%d", exit);
	vector = disassemble(vector);
	printf("\t\t; do");

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
	printf("DOLIST\t%d", exit);
	vector = disassemble(vector);
	printf("\t\t; dolist binding");
	vector = disassemble(vector);
	printf("\t\t; dolist body");
	vector = disassemble(vector);
	printf("\t\t; dolist");

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
	printf("DOTIMES\t%d", exit);
	vector = disassemble(vector);
	printf("\t\t; dotimes times");
	vector = disassemble(vector);
	printf("\t\t; dotimes body");
	vector = disassemble(vector);
	printf("\t\t; dotimes");

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

	printf("FLET");
	lex_copy();
	while (nfun--) {
		cl_object fun = next_code(vector);
		@terpri(0);
		printf("\tFLET\t");
		@prin1(1, fun->bytecodes.data[0]);
	}
	vector = disassemble(vector);
	printf("\t\t; flet");

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

	printf("LABELS");
	lex_copy();
	while (nfun--) {
		cl_object fun = next_code(vector);
		@terpri(0);
		printf("\tLABELS\t");
		@prin1(1, fun->bytecodes.data[0]);
	}
	vector = disassemble(vector);
	printf("\t\t; labels");

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
	printf("MCALL");
	vector = disassemble(vector);
	printf("\t\t; mcall");
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
	printf("MPROG1");
	vector = disassemble(vector);
	printf("\t\t; mprog1");
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
			@terpri(0);
			printf("\t");
		} else
			newline = TRUE;
		if (FIXNUMP(var)) {
			printf("MSETQ\t%d", fix(var));
		} else {
			printf("MSETQS\t");
			@prin1(1, var);
		}
		printf(", VALUES(%d)", i);
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
	printf("PROGV");
	vector = disassemble(vector);
	printf("\t\t; progv");
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

	printf("TAGBODY");
	for (i=0; i<ntags; i++, vector++) {
		@terpri(0);
		printf("\tTAG\t%d",i);
		printf(" @@ %d", simple_label(vector));
	}
	vector = disassemble(vector);
	printf("\t\t; tagbody");

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

	printf("PROTECT\t%d", exit);
	vector = disassemble(vector);
	vector = disassemble(vector);
	printf("\t\t; protect");

	return vector;
}

static cl_object *
disassemble(cl_object *vector) {
	const char *string;
	cl_type t;
	cl_object s;
	cl_fixnum n;

 BEGIN:
	@terpri(0);
	printf("%4d\t", vector - base);
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
	case OP_EXIT:		printf("EXIT");
				return vector;

	/* OP_HALT
		Marks the end of a function.
	*/
	case OP_HALT:		printf("HALT");
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
	NOARG:			printf(string);
				break;
	ARG:			printf(string);
				@prin1(1, s);
				break;
	OPARG:			printf("%s%d", string, n);
				break;
	OPARG_ARG:		printf("%s%d,", string, n);
				@prin1(1, s);
				break;
	}
	goto BEGIN;
}

@(defun si::bc_disassemble (v)
@
	if (type_of(v) == t_bytecodes)
		disassemble_lambda(v->bytecodes.data);
	@(return v)
@)

@(defun si::bc_split (b)
	cl_object vector;
@
	if (type_of(b) != t_bytecodes)
		@(return Cnil Cnil)
	vector = cl_alloc_simple_vector(b->bytecodes.size, aet_object);
	vector->vector.self.t = b->bytecodes.data;
	@(return b->bytecodes.lex vector)
@)
