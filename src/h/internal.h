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

/* booting */
extern void init_all_symbols(void);
extern void init_alloc(void);
extern void init_assignment(void);
extern void init_backq(void);
extern void init_big(void);
extern void init_character(void);
#ifdef CLOS
extern void init_clos(void);
#endif
extern void init_cmpaux(void);
extern void init_compiler(void);
extern void init_error(void);
extern void init_eval(void);
extern void init_file(void);
#ifndef ECL_CMU_FORMAT
extern void init_format(void);
#endif
#ifndef GBC_BOEHM
extern void init_GC(void);
#endif
extern void init_hash(void);
extern void init_interpreter(void);
extern void init_load(void);
extern void init_macros(void);
extern void init_number(void);
extern void init_num_co(void);
extern void init_num_comp(void);
extern void init_num_rand(void);
extern void init_num_sfun(void);
extern void init_package(void);
extern void init_pathname(void);
extern void init_print(void);
extern void init_read(void);
extern void init_stacks(int *);
extern void init_symbol(void);
extern void init_unixtime(void);


/* all_symbols.d */

extern cl_index cl_num_symbols_in_core;

/* all_functions.d */

extern const struct {
	const char *name;
	cl_object (*f)(int, ...);
	short type;
} all_functions[];

/* alloc.d/alloc_2.d */

extern cl_object ecl_alloc_bytecodes(cl_index data_size, cl_index code_size);

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

/* num_log.d */

#define BOOLCLR		0
#define BOOLAND		01
#define BOOLANDC2	02
#define BOOL1		03
#define BOOLANDC1	04
#define BOOL2		05
#define BOOLXOR		06
#define BOOLIOR		07
#define BOOLNOR		010
#define BOOLEQV		011
#define BOOLC2		012
#define BOOLORC2	013
#define BOOLC1		014
#define BOOLORC1	015
#define BOOLNAND	016
#define BOOLSET		017

/* gfun.d, kernel.lsp */

#define GFUN_NAME(x) ((x)->instance.slots[0])
#define GFUN_HASH(x) ((x)->instance.slots[1])
#define GFUN_SPEC(x) ((x)->instance.slots[2])
#define GFUN_COMB(x) ((x)->instance.slots[3])

/* load.d */
#ifdef ENABLE_DLOPEN
cl_object ecl_library_open(cl_object filename);
void *ecl_library_symbol(cl_object block, const char *symbol);
cl_object ecl_library_error(cl_object block);
void ecl_library_close(cl_object block);
#endif

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

extern cl_object ecl_packages_to_be_created;
extern cl_object ecl_package_list;

#ifdef __cplusplus
}
#endif
