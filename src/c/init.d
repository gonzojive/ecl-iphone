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

/******************************* ------- ******************************/

#ifdef __cplusplus
extern "C" void init_LSP(void);
extern "C" void init_CLOS(void);
#else
extern void init_LSP();
extern void init_CLOS();
#endif

void
init_lisp(void)
{
	init_symbol();
	init_package();

	/* These must come _after_ init_symbol() and init_package() */
	GC_disable();
	init_all_symbols();
	GC_enable();

#if !defined(GBC_BOEHM)
	/* We need this because a lot of stuff is to be created */
	init_GC();
#endif

	SYM_VAL(@'*package*') = lisp_package;
	SYM_VAL(@'*gensym_counter*') = MAKE_FIXNUM(0);

	init_typespec();
	init_number();
	init_character();
	init_file();
	init_read();
	init_print();
	init_pathname();
#ifdef unix
	init_load();
/*  	init_unixsys(); */
#endif /* unix */
	init_array();
/*  	init_list(); */
/*  	init_predicate(); */
/*  	init_cfun(); */
/*  	init_sequence(); */
/*  	init_structure(); */
/*  	init_string(); */
#if !defined(GBC_BOEHM)
	init_alloc_function();
#endif
#ifdef TCP
/*  	init_tcp(); */
#endif
#ifdef THREADS
	init_lwp();
#endif
#ifdef CLOS
/*  	init_instance(); */
	init_clos();
/*  	init_gfun(); */
#endif
#ifdef TK
	init_tk();
#endif
/*  	init_hash(); */
#ifdef unix
/*  	init_unixfsys(); */
	init_unixtime();
#endif
	init_compiler();
	init_interpreter();
	init_eval();
/*  	init_reference(); */
	init_assignment();
	init_error();
/*  	init_toplevel(); */
/*  	init_conditional(); */
/*  	init_catch(); */
	init_macros();
/*  	init_let(); */
/*  	init_prog(); */
/*  	init_block(); */
	init_multival();
/*  	init_mapfun(); */
/*  	init_iteration(); */
  	init_cmpaux();
	init_main();
	init_format();
	init_interrupt();
#ifdef RUNTIME
	SYM_VAL(@'*features*') = CONS(make_keyword("RUNTIME"), SYM_VAL(@'*features*'));
#endif
	lex_env = Cnil;
	ihs_push(_intern("TOP-LEVEL", system_package));
	init_LSP();
	init_CLOS();
}
