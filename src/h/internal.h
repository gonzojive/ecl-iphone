/* -*- mode: c; c-basic-offset: 8 -*- */
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
extern void init_big_registers(void);
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
extern void init_stacks(struct cl_env_struct *, int *);
extern void init_unixint(int pass);
extern void init_unixtime(void);
#ifdef mingw32
extern void init_compiler(void);
#endif
extern void ecl_init_env(struct cl_env_struct *);
extern void init_lib_LSP(cl_object);

/* alloc.d/alloc_2.d */

extern cl_object ecl_alloc_bytecodes(cl_index data_size, cl_index code_size);

/* compiler.d */

struct cl_compiler_env {
	cl_object variables;		/* Variables, tags, functions, etc: the env. */
	cl_object macros;		/* Macros and function bindings */
	cl_fixnum lexical_level;	/* =0 if toplevel form */
	cl_object constants;		/* Constants for this form */
	cl_object lex_env;		/* Lexical env. for eval-when */
	cl_index env_depth;
	cl_index env_size;
	bool coalesce;
	bool stepping;
};

typedef struct cl_compiler_env *cl_compiler_env_ptr;

/* interpreter.d */

#define cl_stack_ref(n) cl_env.stack[n]
#define cl_stack_index() (cl_env.stack_top-cl_env.stack)

#define ECL_BUILD_STACK_FRAME(name,frame)	\
	struct ecl_stack_frame frame;\
	cl_object name = ecl_stack_frame_open((cl_object)&frame, 0);

/* ffi.d */

struct ecl_fficall {
	char *buffer_sp;
	size_t buffer_size;
	union ecl_ffi_values output;
	enum ecl_ffi_calling_convention cc;
	struct ecl_fficall_reg *registers;
	char buffer[ECL_FFICALL_LIMIT];
	cl_object cstring;
};

extern enum ecl_ffi_tag ecl_foreign_type_code(cl_object type);
extern enum ecl_ffi_calling_convention ecl_foreign_cc_code(cl_object cc_type);
extern void ecl_fficall_prepare(cl_object return_type, cl_object arg_types, cl_object cc_type);
extern void ecl_fficall_push_bytes(void *data, size_t bytes);
extern void ecl_fficall_push_int(int word);
extern void ecl_fficall_align(int data);

extern struct ecl_fficall_reg *ecl_fficall_prepare_extra(struct ecl_fficall_reg *registers);
extern void ecl_fficall_push_arg(union ecl_ffi_values *data, enum ecl_ffi_tag type);
extern void ecl_fficall_execute(void *f_ptr, struct ecl_fficall *fficall, enum ecl_ffi_tag return_type);
extern void ecl_dynamic_callback_call(cl_object callback_info, char* buffer);
extern void* ecl_dynamic_callback_make(cl_object data, enum ecl_ffi_calling_convention cc_type);

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

/* gfun.d, kernel.lsp */

#define GFUN_NAME(x) ((x)->instance.slots[0])
#define GFUN_SPEC(x) ((x)->instance.slots[1])
#define GFUN_COMB(x) ((x)->instance.slots[2])

/* package.d */

extern cl_object ecl_find_symbol_nolock(cl_object name, cl_object p, int *intern_flag);

/* print.d */

#define ECL_PPRINT_QUEUE_SIZE			128
#define ECL_PPRINT_INDENTATION_STACK_SIZE	256

#ifdef ECL_LONG_FLOAT
extern int edit_double(int n, long double d, int *sp, char *s, int *ep);
#else
extern int edit_double(int n, double d, int *sp, char *s, int *ep);
#endif
extern void cl_write_object(cl_object x, cl_object stream);

/* global locks */

#ifdef ECL_THREADS
# if defined(_MSC_VER) || defined(mingw32)
#  define pthread_mutex_lock(x) \
	 (WaitForSingleObject(*(HANDLE*)(x), INFINITE) != WAIT_OBJECT_0)
#  define pthread_mutex_unlock(x) (ReleaseMutex(*(HANDLE*)(x)) == 0)
# else
#  include <pthread.h>
#  if defined(__APPLE__) || defined(freebsd)
#   define PTHREAD_MUTEX_ERROR_CHECK_NP PTHREAD_MUTEX_ERROR_CHECK_NP
#  endif
# endif
# define HASH_TABLE_LOCK(h) if ((h)->hash.lockable) if (pthread_mutex_lock(&(h)->hash.lock)) ecl_internal_error("")
# define PACKAGE_LOCK(p) if (pthread_mutex_lock(&(p)->pack.lock)) ecl_internal_error("")
# define PACKAGE_OP_LOCK() if (pthread_mutex_lock(&cl_core.global_lock)) ecl_internal_error("")
# define THREAD_OP_LOCK() if (pthread_mutex_lock(&cl_core.global_lock)) ecl_internal_error("")
# define HASH_TABLE_UNLOCK(h) if ((h)->hash.lockable) if (pthread_mutex_unlock(&(h)->hash.lock)) ecl_internal_error("")
# define PACKAGE_UNLOCK(p) if (pthread_mutex_unlock(&(p)->pack.lock)) ecl_internal_error("")
# define PACKAGE_OP_UNLOCK() if (pthread_mutex_unlock(&cl_core.global_lock)) ecl_internal_error("")
# define THREAD_OP_UNLOCK() if (pthread_mutex_unlock(&cl_core.global_lock)) ecl_internal_error("")
#else
# define HASH_TABLE_LOCK(h)
# define HASH_TABLE_UNLOCK(h)
# define PACKAGE_LOCK(p)
# define PACKAGE_UNLOCK(p)
# define PACKAGE_OP_LOCK()
# define PACKAGE_OP_UNLOCK()
#endif /* ECL_THREADS */


/* read.d */
#ifdef ECL_UNICODE
#define	RTABSIZE	256		/*  read table size  */
#else
#define	RTABSIZE	CHAR_CODE_LIMIT	/*  read table size  */
#endif

/* time.d */

#define UTC_time_to_universal_time(x) ecl_plus(ecl_make_integer(x),cl_core.Jan1st1970UT)
extern cl_fixnum ecl_runtime(void);

/* unixint.d */

extern bool ecl_interrupt_enable;

#if defined(_MSC_VER) || defined(mingw32)
# include <float.h>
# if defined(_MSC_VER)
#   define FE_DIVBYZERO EM_ZERODIVIDE
#   define FE_OVERFLOW  EM_OVERFLOW
#   define FE_UNDERFLOW EM_UNDERFLOW
typedef int fenv_t;
# else
#   ifdef _MCW_EM
#    define MCW_EM _MCW_EM
#   else
#    define MCW_EM 0x0008001F
#   endif
#   define fenv_t int
# endif
# define feenableexcept(bits) { int cw = _controlfp(0,0); cw &= ~(bits); _controlfp(cw,MCW_EM); }
# define fedisableexcept(bits) { int cw = _controlfp(0,0); cw |= (bits); _controlfp(cw,MCW_EM); }
# define feholdexcept(bits) { *(bits) = _controlfp(0,0); _controlfp(0xffffffff, MCW_EM); }
# define fesetenv(bits) _controlfp(*(bits), MCW_EM)
#endif

#define ECL_PI_D 3.14159265358979323846264338327950288
#define ECL_PI_L 3.14159265358979323846264338327950288l
#define ECL_PI2_D 1.57079632679489661923132169163975144
#define ECL_PI2_L 1.57079632679489661923132169163975144l

/*
 * Fake several ISO C99 mathematical functions
 */

#ifndef HAVE_EXPF
# define expf(x) exp((float)x)
#endif
#ifndef HAVE_LOGF
# define logf(x) log((float)x)
#endif
#ifndef HAVE_SQRTF
# define sqrtf(x) sqrt((float)x)
#endif
#ifndef HAVE_SINF
# define sinf(x) sin((float)x)
#endif
#ifndef HAVE_COSF
# define cosf(x) cos((float)x)
#endif
#ifndef HAVE_TANF
# define tanf(x) tan((float)x)
#endif
#ifndef HAVE_SINHF
# define sinhf(x) sinh((float)x)
#endif
#ifndef HAVE_COSHF
# define coshf(x) cosh((float)x)
#endif
#ifndef HAVE_TANHF
# define tanhf(x) tanh((float)x)
#endif

#ifndef HAVE_CEILF
# define ceilf(x) ceil((float)x)
#endif
#ifndef HAVE_FLOORF
# define floorf(x) floor((float)x)
#endif
#ifndef HAVE_FABSF
# define fabsf(x) fabs((float)x)
#endif
#ifndef HAVE_FREXPF
# define frexpf(x,y) frexp((float)x,y)
#endif
#ifndef HAVE_LDEXPF
# define ldexpf(x,y) ldexp((float)x,y)
#endif

#ifdef __cplusplus
}
#endif
