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

/******************************* ------- ******************************/

extern void init_lisp_libs(void);

void
init_lisp(void)
{
#ifndef RUNTIME
	function_entry_table = (void *)malloc(2 * function_entries_max * sizeof(void *));
#endif
	init_symbol();
	init_package();

#if !defined(GBC_BOEHM)
	/* We need this because a lot of stuff is to be created */
	init_GC();
#endif

	/* These must come _after_ init_symbol() and init_package() */
	init_all_keywords();
	init_all_symbols();
	init_all_functions();

	SYM_VAL(Vpackage) = lisp_package;
	SYM_VAL(Vgensym_counter) = MAKE_FIXNUM(0);

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
#endif unix
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
#endif TCP
#ifdef THREADS
	init_lwp();
#endif THREADS
#ifdef CLOS
/*  	init_instance(); */
	init_clos();
/*  	init_gfun(); */
#endif CLOS
#ifdef TK
	init_tk();
#endif TK
#ifdef LOCATIVE
	init_unify();
#endif LOCATIVE
/*  	init_hash(); */
#ifdef unix
/*  	init_unixfsys(); */
	init_unixtime();
#endif unix
	init_compiler();
	init_interpreter();
	init_eval();
/*  	init_lex(); */
/*  	init_reference(); */
	init_assignment();
/*  	init_stacks(); */
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
	SYM_VAL(Vfeatures) = CONS(make_keyword("RUNTIME"), SYM_VAL(Vfeatures));
#endif
	init_lisp_libs();
}
