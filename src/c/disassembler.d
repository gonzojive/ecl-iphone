/*
    disassembler.c -- Byte compiler and function evaluator
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

static cl_object *
disassemble_block(cl_object *vector) {
	cl_object lex_old = lex_env;
	cl_fixnum exit = packed_label(vector-1);

	printf("BLOCK\t");
	@prin1(1, next_code(vector));
	printf(",%d", exit);
	vector = disassemble(vector);
	printf("\t\t\t; block");

	lex_env = lex_old;
	return vector;
}

static cl_object *
disassemble_catch(cl_object *vector) {
	printf("CATCH\t%d", packed_label(vector - 1));
	vector = disassemble(vector);
	printf("\t\t\t; catch");
	return vector;
}

static cl_object *
disassemble_do(cl_object *vector) {
	cl_fixnum exit;
	cl_object lex_old = lex_env;
	lex_copy();

	exit = packed_label(vector-1);
	printf("DO\t%d", exit);
	vector = disassemble(vector);
	printf("\t\t\t; do");

	lex_env = lex_old;
	return vector;
}

static cl_object *
disassemble_dolist(cl_object *vector) {
	cl_fixnum exit;
	cl_object lex_old = lex_env;

	lex_copy();
	exit = packed_label(vector-1);
	printf("DOLIST\t%d", exit);
	vector = disassemble(vector);
	printf("\t\t\t; dolist binding");
	vector = disassemble(vector);
	printf("\t\t\t; dolist body");
	vector = disassemble(vector);
	printf("\t\t\t; dolist");

	lex_env = lex_old;
	return vector;
}

static cl_object *
disassemble_dotimes(cl_object *vector) {
	cl_fixnum exit;
	cl_object lex_old = lex_env;

	lex_copy();
	exit = packed_label(vector-1);
	printf("DOTIMES\t%d", exit);
	vector = disassemble(vector);
	printf("\t\t\t; dotimes times");
	vector = disassemble(vector);
	printf("\t\t\t; dotimes body");
	vector = disassemble(vector);
	printf("\t\t\t; dotimes");

	lex_env = lex_old;
	return vector;
}

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
	printf("\t\t\t; flet");

	lex_env = lex_old;
	return vector;
}

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
	printf("\t\t\t; labels");

	lex_env = lex_old;
	return vector;
}

static cl_object *
disassemble_mbind(cl_object *vector)
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
		if (var == MAKE_FIXNUM(1)) {
			printf("MBINDS\t");
			var = next_code(vector);
		} else {
			printf("MBIND\t");
		}
		@prin1(1, var);
		printf(", VALUES(%d)", i);
	}
	return vector;
}

static cl_object *
disassemble_mprog1(cl_object *vector) {
	printf("MPROG1");
	vector = disassemble(vector);
	printf("\t\t\t; mprog1");
	return vector;
}


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
		if (var == MAKE_FIXNUM(1)) {
			printf("MSETQS\t");
			var = next_code(vector);
		} else {
			printf("MSETQ\t");
		}
		@prin1(1, var);
		printf(", VALUES(%d)", i);
	}
	return vector;
}


static cl_object *
disassemble_progv(cl_object *vector) {
	printf("PROGV");
	vector = disassemble(vector);
	printf("\t\t\t; progv");
	return vector;
}

static cl_object *
disassemble_pushenv(cl_object *vector) {
	cl_object lex_old = lex_env;
	lex_copy();

	printf("PUSHENV");
	vector = disassemble(vector);
	printf("\t\t\t; pushenv");

	lex_env = lex_old;
	return vector;
}

/*	OP_TAGBODY n-tags
	tag1 addr1
	tag2 addr2
	...  ...
	tagn addrn
	{form}*
	OP_EXIT
*/

static cl_object *
disassemble_tagbody(cl_object *vector) {
	cl_index ntags = get_oparg(vector[-1]);
	cl_object lex_old = lex_env;
	lex_copy();

	printf("TAGBODY");
	while (ntags--) {
		@terpri(0);
		printf("\tTAG\t'");
		@prin1(1, vector[0]);
		printf(" @@ %d", simple_label(vector+1));
		vector+=2;
	}
	vector = disassemble(vector);
	printf("\t\t\t; tagbody");

	lex_env = lex_old;
	return vector;
}

static cl_object *
disassemble_unwind_protect(cl_object *vector) {
	cl_fixnum exit = packed_label(vector-1);

	printf("PROTECT\t%d", exit);
	vector = disassemble(vector);
	vector = disassemble(vector);
	printf("\t\t\t; protect");

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
	case OP_PUSHQ:		printf("PUSH\t'");
				@prin1(1,next_code(vector));
				break;
	case OP_PUSH:		string = "PUSH\tVALUES(0)"; goto NOARG;
	case OP_PUSHV:		string = "PUSHV"; goto SETQ;
	case OP_PUSHVS:		string = "PUSHVS"; goto QUOTE;
	case OP_VAR:		string = "VAR"; goto SETQ;
	case OP_VARS:		string = "VARS"; goto QUOTE;
	case OP_QUOTE:		string = "QUOTE";
	QUOTE:			s = next_code(vector);
				goto ARG;
	case OP_NOP:		string = "NOP";	goto NOARG;
	case OP_BLOCK:		vector = disassemble_block(vector);
				break;
	case OP_PUSHVALUES:	string = "PUSH\tVALUES"; goto NOARG;
	case OP_MCALL:		string = "MCALL"; goto NOARG;
	case OP_CALL:		string = "CALL";
				n = get_oparg(s);
				s = next_code(vector);
				goto OPARG_ARG;
	case OP_PCALL:		string = "PCALL";
				n = get_oparg(s);
				s = next_code(vector);
				goto OPARG_ARG;
	case OP_FCALL:		string = "FCALL";
				n = get_oparg(s);
				goto OPARG;
	case OP_PFCALL:		string = "PFCALL";
				n = get_oparg(s);
				goto OPARG;
	case OP_CATCH:		vector = disassemble_catch(vector);
				break;
	case OP_EXIT:		printf("EXIT");
				return vector;
	case OP_HALT:		printf("HALT");
				return vector-1;
	case OP_FLET:		vector = disassemble_flet(vector);
				break;
	case OP_LABELS:		vector = disassemble_labels(vector);
				break;
	case OP_FUNCTION:	string = "SYMFUNC";
				s = next_code(vector);
				goto ARG;
	case OP_CLOSE:		string = "CLOSE";
				s = next_code(vector);
				goto ARG;
	case OP_GO:		string = "GO";
				s = next_code(vector);
				goto ARG;
	case OP_RETURN:		string = "RETFROM";
				s = next_code(vector);
				goto ARG;
	case OP_THROW:		string = "THROW"; goto NOARG;
	case OP_JMP:		string = "JMP";
				n = packed_label(vector-1);
				goto OPARG;
	case OP_JNIL:		string = "JNIL";
				n = packed_label(vector-1);
				goto OPARG;
	case OP_JT:		string = "JT";
				n = packed_label(vector-1);
				goto OPARG;
	case OP_JEQ:		string = "JEQ";
				s = next_code(vector);
				n = packed_label(vector-2);
				goto OPARG_ARG;
	case OP_JNEQ:		string = "JNEQ";
				s = next_code(vector);
				n = packed_label(vector-2);
				goto OPARG_ARG;
	case OP_BIND:		string = "BIND"; goto QUOTE;
	case OP_BINDS:		string = "BINDS"; goto QUOTE;
	case OP_PBIND:		string = "PBIND"; goto QUOTE;
	case OP_PBINDS:		string = "PBINDS"; goto QUOTE;
	case OP_PSETQ:		string = "PSETQ"; goto SETQ;
	case OP_PSETQS:		string = "PSETQS"; goto QUOTE;
	case OP_SETQ:		string = "SETQ";
		SETQ:		s = next_code(vector);
				goto ARG;
	case OP_SETQS:		string = "SETQS"; goto QUOTE;
	case OP_MSETQ:		vector = disassemble_msetq(vector);
				break;
	case OP_MBIND:		vector = disassemble_mbind(vector);
				break;
	case OP_MPROG1:		vector = disassemble_mprog1(vector);
				break;
	case OP_PROGV:		vector = disassemble_progv(vector);
				break;
	case OP_PUSHENV:	vector = disassemble_pushenv(vector);
				break;
	case OP_VALUES:		string = "VALUES";
				n = get_oparg(s);
				goto OPARG;
	case OP_NTHVAL:		string = "NTHVAL"; goto NOARG;
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
		return;
	NOARG:			printf(string);
				break;
	ARG:			printf("%s\t", string);
				@prin1(1, s);
				break;
	OPARG:			printf("%s\t%d", string, n);
				break;
	OPARG_ARG:		printf("%s\t%d,", string, n);
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
	vector = alloc_simple_vector(b->bytecodes.size, aet_object);
	vector->vector.self.t = b->bytecodes.data;
	@(return b->bytecodes.lex vector)
@)
