/*
    init.c  -- Lisp Initialization.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecls.h"

static
@(defun si::simple_toplevel ()
	cl_object sentence;
	cl_object lex_old = lex_env;
@
	/* Simple minded top level loop */
	printf(";*** Lisp core booted ****\nECLS (Embeddable Common Lisp)  %d pages\n", MAXPAGE);
	fflush(stdout);
#ifdef TK
	StdinResume();
#endif
	lex_new();
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
@)

void
init_lisp_libs(void)
{
	SYM_VAL(@'*package*') = system_package;
	SYM_VAL(@'*features*') = CONS(make_keyword("ECLS-MIN"), SYM_VAL(@'*features*'));
	make_si_function("TOP-LEVEL", @si::simple-toplevel);
}
