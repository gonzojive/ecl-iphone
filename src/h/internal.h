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
extern void init_backq(void);
extern void init_big(void);
#ifdef CLOS
extern void init_clos(void);
#endif
extern void init_error(void);
extern void init_eval(void);
extern void init_file(void);
#ifndef GBC_BOEHM
extern void init_GC(void);
#endif
extern void init_macros(void);
extern void init_number(void);
extern void init_read(void);
extern void init_stacks(int *);
extern void init_unixint(void);
extern void init_unixtime(void);
extern void ecl_init_env(struct cl_env_struct *);
extern void init_LSP(void);
extern void init_CLOS(void);

/* all_functions.d */

extern const struct {
	const char *name;
	cl_object (*f)(int, ...);
	short type;
} all_functions[];

/* alloc.d/alloc_2.d */

extern cl_object ecl_alloc_bytecodes(cl_index data_size, cl_index code_size);

/* interpreter.d */

#define cl_stack_ref(n) cl_env.stack[n]
#define cl_stack_index() (cl_env.stack_top-cl_env.stack)

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

/* format.d */

#ifndef ECL_CMU_FORMAT
extern cl_object si_formatter_aux _ARGS((cl_narg narg, cl_object strm, cl_object string, ...));
#endif

/* hash.d */
extern void ecl_extend_hashtable(cl_object hashtable);

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

/* package.d */

extern cl_object ecl_find_symbol_nolock(cl_object name, cl_object p, int *intern_flag);

/* print.d */

#define ECL_PPRINT_QUEUE_SIZE			128
#define ECL_PPRINT_INDENTATION_STACK_SIZE	256

extern void edit_double(int n, double d, int *sp, char *s, int *ep);
extern void cl_setup_printer(cl_object strm);
extern void cl_write_object(cl_object x);

/* global locks */

#ifdef ECL_THREADS
#if 0
#define HASH_TABLE_LOCK(h) if ((h)->hash.lockable) pthread_mutex_lock(&(h)->hash.lock)
#define HASH_TABLE_UNLOCK(h) if ((h)->hash.lockable) pthread_mutex_unlock(&(h)->hash.lock)
#define PACKAGE_LOCK(p) pthread_mutex_lock(&(p)->pack.lock)
#define PACKAGE_UNLOCK(p) pthread_mutex_unlock(&(p)->pack.lock)
#define PACKAGE_OP_LOCK() pthread_mutex_lock(&cl_core.global_lock)
#define PACKAGE_OP_UNLOCK() pthread_mutex_unlock(&cl_core.global_lock)
#define THREAD_OP_LOCK() pthread_mutex_lock(&cl_core.global_lock)
#define THREAD_OP_UNLOCK() pthread_mutex_unlock(&cl_core.global_lock)
#else
#define HASH_TABLE_LOCK(h) if ((h)->hash.lockable) if (pthread_mutex_lock(&(h)->hash.lock)) internal_error("")
#define PACKAGE_LOCK(p) if (pthread_mutex_lock(&(p)->pack.lock)) internal_error("")
#define PACKAGE_OP_LOCK() if (pthread_mutex_lock(&cl_core.global_lock)) internal_error("")
#define THREAD_OP_LOCK() if (pthread_mutex_lock(&cl_core.global_lock)) internal_error("")
#define HASH_TABLE_UNLOCK(h) if ((h)->hash.lockable) if (pthread_mutex_unlock(&(h)->hash.lock)) internal_error("")
#define PACKAGE_UNLOCK(p) if (pthread_mutex_unlock(&(p)->pack.lock)) internal_error("")
#define PACKAGE_OP_UNLOCK() if (pthread_mutex_unlock(&cl_core.global_lock)) internal_error("")
#define THREAD_OP_UNLOCK() if (pthread_mutex_unlock(&cl_core.global_lock)) internal_error("")
#endif
#else
#define HASH_TABLE_LOCK(h)
#define HASH_TABLE_UNLOCK(h)
#define PACKAGE_LOCK(p)
#define PACKAGE_UNLOCK(p)
#define PACKAGE_OP_LOCK()
#define PACKAGE_OP_UNLOCK()
#endif


/* read.d */
#define	RTABSIZE	CHAR_CODE_LIMIT	/*  read table size  */

/* time.d */

extern cl_fixnum ecl_runtime(void);

/* unixint.d */

extern bool ecl_interrupt_enable;

#ifdef __cplusplus
}
#endif
