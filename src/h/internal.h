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

/* all_symbols.d */

extern cl_index cl_num_symbols_in_core;

/* all_functions.d */

extern const struct {
	const char *name;
	cl_object (*f)(int, ...);
	short type;
} all_functions[];

/* file.d */

/*
 * POSIX specifies that the "b" flag is ignored. This is good, because
 * under MSDOS and Apple's OS we need to open text files in binary mode,
 * so that we get both the carriage return and the linefeed characters.
 * Otherwise, it would be complicated to implement file-position and
 * seek operations.
 */
#define OPEN_R	"rb"
#define OPEN_W	"wb"
#define OPEN_RW	"w+b"
#define OPEN_A	"ab"
#define OPEN_RA	"a+b"

/* gfun.d, kernel.lsp */

#define GFUN_NAME(x) ((x)->instance.slots[0])
#define GFUN_HASH(x) ((x)->instance.slots[1])
#define GFUN_SPEC(x) ((x)->instance.slots[2])
#define GFUN_COMB(x) ((x)->instance.slots[3])

/* print.d */

#define ECL_PPRINT_QUEUE_SIZE			128
#define ECL_PPRINT_INDENTATION_STACK_SIZE	256

#ifndef THREADS
extern bool PRINTreadably;
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
