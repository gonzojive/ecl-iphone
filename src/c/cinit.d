/*
    init.c  -- Lisp Initialization.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecl.h"

static cl_object si_simple_toplevel ()
{
	cl_object sentence;
	cl_object lex_old = lex_env;
	int i;

	/* Simple minded top level loop */
	printf(";*** Lisp core booted ****\nECLS (Embeddable Common Lisp)  %d pages\n", MAXPAGE);
	fflush(stdout);
#ifdef TK
	StdinResume();
#endif
	lex_new();
	for (i = 1; i<fix(si_argc()); i++) {
	  cl_object arg = si_argv(MAKE_FIXNUM(i));
	  cl_load(1, arg);
	}
	while (1) {
	  cl_object bytecodes = Cnil;
	  printf("\n> ");
	  sentence = @read(3, Cnil, Cnil, OBJNULL);
	  if (sentence == OBJNULL)
	    @(return);
	  prin1(eval(sentence, &bytecodes, Cnil), Cnil);
#ifdef TK
	  StdinResume();
#endif
	}
	lex_env = lex_old;
}

int
main(int argc, char **args)
{
	cl_object top_level;

	/* This should be always the first call */
	cl_boot(argc, args);

	SYM_VAL(@'*package*') = system_package;
	SYM_VAL(@'*features*') = CONS(make_keyword("ECL-MIN"), SYM_VAL(@'*features*'));
#ifdef CLOS
	SYM_VAL(@'*features*') = CONS(make_keyword("WANTS-CLOS"), SYM_VAL(@'*features*'));
#endif
#ifndef RUNTIME
	SYM_VAL(@'*features*') = CONS(make_keyword("WANTS-CMP"), SYM_VAL(@'*features*'));
#endif
#ifdef CLX
	SYM_VAL(@'*features*') = CONS(make_keyword("WANTS-CLX"), SYM_VAL(@'*features*'));
#endif
	top_level = _intern("TOP-LEVEL", system_package);
	cl_def_c_function(top_level, si_simple_toplevel, 0);
	funcall(1, top_level);
	return(0);
}

#ifdef __cplusplus
extern "C" void init_LSP(void);
extern "C" void init_CLOS(void);
#endif

void init_LSP(void) {}
void init_CLOS(void) {}
