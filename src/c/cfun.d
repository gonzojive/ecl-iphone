/*
    cfun.c -- Compiled functions.
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
#include <string.h>	/* for memmove() */

#ifdef PDE
cl_object @'defun', @'defmacro';
#endif PDE

static void record_fun_entry (cl_object sym, void *addr);

cl_object
make_cfun(cl_object (*self)(), cl_object name, cl_object cblock)
{
	cl_object cf;

	cf = alloc_object(t_cfun);
	cf->cfun.entry = self;
	cf->cfun.name = name;
	cf->cfun.block = cblock;
	return(cf);
}

cl_object
make_cclosure(cl_object (*self)(), cl_object env, cl_object block)
{
	cl_object cc;

	cc = alloc_object(t_cclosure);
	cc->cclosure.entry = self;
	cc->cclosure.env = env;
	cc->cclosure.block = block;
	return(cc);
}

void
MF(cl_object sym, cl_object (*self)(), cl_object block)
{
	cl_object cf;

	if (!SYMBOLP(sym))
		FEtype_error_symbol(sym);
	if (sym->symbol.isform && sym->symbol.mflag)
		sym->symbol.isform = FALSE;
	clear_compiler_properties(sym);
#ifndef RUNTIME
	record_fun_entry(sym, self);
#endif
#ifdef PDE
	record_source_pathname(sym, @'defun');
#endif PDE
	cf = alloc_object(t_cfun);
	cf->cfun.entry = self;
	cf->cfun.name = sym;
	cf->cfun.block = block;
	SYM_FUN(sym) = cf;
	sym->symbol.mflag = FALSE;
}

void
MM(cl_object sym, cl_object (*self)(), cl_object block)
{
	cl_object cf;

	if (!SYMBOLP(sym))
		FEtype_error_symbol(sym);
	if (sym->symbol.isform && sym->symbol.mflag)
		sym->symbol.isform = FALSE;
	clear_compiler_properties(sym);
#ifndef RUNTIME
	record_fun_entry(sym, self);
#endif
#ifdef PDE
	record_source_pathname(sym, @'defmacro');
#endif PDE
	cf = alloc_object(t_cfun);
	cf->cfun.entry = self;
	cf->cfun.name = sym;
	cf->cfun.block = block;
	SYM_FUN(sym) = cf;
	sym->symbol.mflag = TRUE;
}

cl_object
make_function(char *s, cl_object (*f)())
{
	cl_object x;

	x = make_ordinary(s);
	SYM_FUN(x) = make_cfun(f, x, NULL);
	x->symbol.mflag = FALSE;
#ifndef RUNTIME
	record_fun_entry(x, f);
#endif
	return(x);
}

cl_object
make_si_function(char *s, cl_object (*f)())
{
	cl_object x;

	x = make_si_ordinary(s);
	SYM_FUN(x) = make_cfun(f, x, NULL);
	x->symbol.mflag = FALSE;
#ifndef RUNTIME
	record_fun_entry(x, f);
#endif
	return(x);
}

@(defun si::compiled_function_name (fun)
	cl_object output;
@
	switch(type_of(fun)) {
	case t_bytecodes:
		output = fun->bytecodes.data[0]; break;
	case t_cfun:
		output = fun->cfun.name; break;
	case t_cclosure:
		output = Cnil; break;
	default:
		FEerror("~S is not a compiled-function.", 1, fun);
	}
	@(return output)
@)

@(defun si::compiled_function_block (fun)
       cl_object output;
@
       switch(type_of(fun)) {
	case t_cfun:
		output = fun->cfun.block; break;
	case t_cclosure:
		output = fun->cclosure.block; break;
	default:
		FEerror("~S is not a compiled-function.", 1, fun);
	}
	@(return output)
@)


#ifndef RUNTIME

#define FUN_TABLE_INC 256
void **function_entry_table;
int function_entries_max;
int function_entries;

/*----------------------------------------------------------------------
 * fun_entry_search --
 *	function_entry_table is an array containing alternated addr, sym values
 *	sorted in increasing addr value.
 * Result:
 *	  the index of the largest addr which is smaller than key
 *	  -2 if no such addr is present
 *----------------------------------------------------------------------
 */
static int
fun_entry_search(char *key)
{
  void **table = function_entry_table;
  int len = function_entries;
  int low = 0;
  int high = len;
  int mid, probe;
  char *entry;
  if (len == 0)
    return(-2);
  while (TRUE) {
    mid = (low + high) / 2;
    probe = mid * 2;
    entry = (char *)table[probe];
    if (entry == key)
      return(probe);
    if (entry < key) {
      if (mid + 1 == len || (char*)table[probe+2] > key)
	return(probe);
      else
	low = mid;
    } else {
      if (probe == 0)
	return(-2);
      else
	high = mid;
    }
  }
}

/*
 *----------------------------------------------------------------------
 * record_fun_entry --
 *	records the code start of function bound to symbol, so that
 *	one can determine which function is executing
 *
 *----------------------------------------------------------------------
 */
static void
record_fun_entry(cl_object sym, void *addr)
{
  cl_object def;
  register int i, end;

  end = 2*function_entries;
  def = SYM_FUN(sym);
  if (def != OBJNULL && type_of(def) == t_cfun) {
    /* clear previous definition */
    void *prevaddr = (void *)def->cfun.entry;
    i = fun_entry_search(prevaddr);
    if (i >= 0 && function_entry_table[i] == prevaddr) {
      function_entries--;
      end -= 2;
      memmove(&function_entry_table[i], &function_entry_table[i+2],
	      sizeof(void *) * (end - i));
    }
  }
  i = fun_entry_search(addr);
  if (i < 0 || function_entry_table[i] != (char*)addr) {
    if (2*function_entries_max == end) {
      function_entries_max += FUN_TABLE_INC;
      function_entry_table = realloc(function_entry_table,
				     2 * function_entries_max * sizeof(void *));
    }
    i += 2;
    memmove(&function_entry_table[i+2], &function_entry_table[i],
	   sizeof(void *) * (end - i));
    function_entries++;
  }
  function_entry_table[i++] = (char *)addr;
  function_entry_table[i++] = (char *)sym;
}

cl_object
get_function_entry(void *addr)
{
  int i;
  i = fun_entry_search(addr);
  if (i >= 0)
    return((cl_object)function_entry_table[i+1]);
  else
    return(OBJNULL);
}

#endif RUNTIME
