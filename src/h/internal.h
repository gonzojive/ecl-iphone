/*
    internal.h -- Structures and functions that are not meant for the end user
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifdef __cplusplus
extern "C" {
#endif

/* -------------------------------------------------------------------- *
 *	FUNCTIONS, VARIABLES AND TYPES NOT FOR GENERAL USE		*
 * -------------------------------------------------------------------- */

#include "machines.h"

/* all_symbols.d */

extern cl_index cl_num_symbols_in_core;

/* all_functions.d */

extern const struct {
	const char *name;
	cl_object (*f)(int, ...);
	short type;
} all_functions[];


/* print.d */

#define ECL_PPRINT_QUEUE_SIZE			128
#define ECL_PPRINT_INDENTATION_STACK_SIZE	256

#ifndef THREADS
extern bool PRINTescape;
extern bool PRINTpretty;
extern bool PRINTcircle;
extern int PRINTbase;
extern bool PRINTradix;
extern cl_object PRINTcase;
extern bool PRINTgensym;
extern int PRINTlevel;
extern int PRINTlength;
extern bool PRINTarray;
extern cl_object PRINTpackage;
extern bool PRINTstructure;
extern cl_object PRINTstream;
#endif

extern void edit_double(int n, double d, int *sp, char *s, int *ep);
extern void cl_setup_printer(cl_object strm);
extern void cl_write_object(cl_object x);

 /* read.d */
#define	RTABSIZE	CHAR_CODE_LIMIT	/*  read table size  */

#ifdef __cplusplus
}
#endif
