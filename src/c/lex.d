/*
    lex.c -- Lexical environment.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecls.h"

/******** EXPORTS ********/

#ifndef THREADS
cl_object lex_env = OBJNULL;
#endif

cl_object @'si::symbol-macro';
cl_object @'macro';
cl_object @'block';
cl_object @'tag';

/******** ------- ********/

void
lex_fun_bind(cl_object name, cl_object fun)
{
	CDR(lex_env) = CONS(list(3, name, @'function', fun), CDR(lex_env));
}

void
lex_tag_bind(cl_object tag, cl_object id)
{
	CDR(lex_env) = CONS(list(3, tag, @'tag', id), CDR(lex_env));
}

void
lex_block_bind(cl_object name, cl_object id)
{
	CDR(lex_env) = CONS(list(3, name, @'block', id), CDR(lex_env));
}

cl_object
lex_sch(cl_object alist, cl_object name, cl_object type)
{
	while (!endp(alist)) {
		if (CAAR(alist) == name && CADAR(alist) == type)
			return(CADDAR(alist));
		alist = CDR(alist);
	}
	return(Cnil);
}

@(defun si::lex_env ()
@
	@(return lex_env)
@)
