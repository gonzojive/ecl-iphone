/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    stacks.h -- Bind/Jump/Frame stacks.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifdef __cplusplus
extern "C" {
#endif

/**************
 * BIND STACK
 **************/

typedef struct bds_bd {
	cl_object symbol;	/*  symbol  */
	cl_object value;	/*  previous value of the symbol  */
} *bds_ptr;

#define	bds_check  \
	((cl_env.bds_top >= cl_env.bds_limit)? bds_overflow() : (void)0)

#ifdef ECL_THREADS
extern ECL_API void bds_bind(cl_object symbol, cl_object value);
extern ECL_API void bds_push(cl_object symbol);
extern ECL_API void bds_unwind1();
extern ECL_API cl_object *ecl_symbol_slot(cl_object s);
#define SYM_VAL(s) (*ecl_symbol_slot(s))
#if 0
#define ECL_SET(s,v) ((s)->symbol.value=(v))
#define ECL_SETQ(s,v) (*ecl_symbol_slot(s)=(v))
#else
extern ECL_API cl_object ecl_set_symbol(cl_object s, cl_object v);
#define ECL_SET(s,v) (ecl_set_symbol(s,v))
#define ECL_SETQ(s,v) (ecl_set_symbol(s,v))
#endif
#else
#define SYM_VAL(s) ((s)->symbol.value)
#define ECL_SET(s,v) ((s)->symbol.value=(v))
#define ECL_SETQ(s,v) ((s)->symbol.value=(v))
#define	bds_bind(sym, val)  \
	(bds_check,(++cl_env.bds_top)->symbol = (sym),  \
	cl_env.bds_top->value = SYM_VAL(sym),  \
	SYM_VAL(sym) = (val))

#define bds_push(sym) \
	(bds_check,(++cl_env.bds_top)->symbol = (sym), cl_env.bds_top->value = SYM_VAL(sym))

#define	bds_unwind1()  \
	(SYM_VAL(cl_env.bds_top->symbol) = cl_env.bds_top->value, --cl_env.bds_top)
#endif /* ECL_THREADS */
extern ECL_API void bds_unwind_n(int n);

/****************************
 * INVOCATION HISTORY STACK
 ****************************/

typedef struct ihs_frame {
	struct ihs_frame *next;
	cl_object function;
	cl_object lex_env;
	cl_index index;
} *ihs_ptr;

#define ihs_push(r,f,e) do {	\
	(r)->next=cl_env.ihs_top; (r)->function=(f); (r)->lex_env=(e); \
	(r)->index=cl_env.ihs_top->index+1;\
	cl_env.ihs_top = (r); \
} while(0)

#define ihs_pop() do {\
	if (cl_env.ihs_top->next == NULL) ecl_internal_error("Underflow in IHS stack"); \
	cl_env.ihs_top = cl_env.ihs_top->next; \
} while(0)

extern ECL_API cl_object ihs_top_function_name(void);

/***************
 * FRAME STACK
 ***************/
/* Frames signal points in the code to which we can at any time jump.
 * Frames are established, for instance, by CATCH, BLOCK, TAGBODY,
 * LAMBDA, UNWIND-PROTECT, etc.
 *
 * Frames are established by frs_push(). For each call to frs_push()
 * there must be a corresponding frs_pop(). More precisely, since our
 * frame mechanism relies on the C stack and on the setjmp/longjmp
 * functions, any function that creates a frame must also destroy it
 * with frs_pop() before returning.
 *
 * Frames are identified by a value frs_val. This can be either a
 * unique identifier, created for each CATCH, BLOCK, etc, or a common
 * one ECL_PROTECT_TAG, used by UNWIND-PROTECT forms. The first type
 * of frames can be target of a search frs_sch() and thus one can jump
 * to them. The second type of frames are like barriers designed to
 * intercept the jumps to the outer frames and are called
 * automatically by the function unwind() whenever it jumps to a frame
 * which is beyond one of these barriers.
 */

typedef struct ecl_frame {
	jmp_buf		frs_jmpbuf;
	cl_object	frs_val;
	cl_index	frs_bds_top_index;
	ihs_ptr		frs_ihs;
	cl_index	frs_sp;
} *ecl_frame_ptr;

extern ECL_API ecl_frame_ptr _frs_push(register cl_object val);
#define frs_push(val)  ecl_setjmp(_frs_push(val)->frs_jmpbuf)
#define frs_pop() (cl_env.frs_top--)

/*******************
 * ARGUMENTS STACK
 *******************
 * Here we define how we handle the incoming arguments for a
 * function. Our calling conventions specify that at most
 * C_ARGUMENTS_LIMIT ar pushed onto the C stack. If the function
 * receives more than this number of arguments it will keep a copy of
 * _all_ those arguments _plus_ the remaining ones in the lisp
 * stack. The caller is responsible for storing and removing such
 * values.
 *
 * Given this structure, we need our own object for handling variable
 * argument list, cl_va_list. This object joins the C data type for
 * handling vararg lists and a pointer to the lisp stack, in case the
 * arguments were passed there.
 *
 * Note that keeping a direct reference to the lisp stack effectively
 * locks it in memory, preventing the block from being garbage
 * collected if the stack grows -- at least until all references are
 * eliminated --. This is something we have to live with and which
 * is somehow unavoidable, given that function arguments have to be
 * stored somewhere.
 */

#define cl_va_start(a,p,n,k) { \
	a[0].narg = (n)-(k); \
	va_start(a[0].args,p); \
	a[0].sp = ((n) <= C_ARGUMENTS_LIMIT)? 0 : _ecl_va_sp(a[0].narg); }
#define cl_va_arg(a) \
	(a[0].narg--,(a[0].sp? *(a[0].sp++) : va_arg(a[0].args,cl_object)))
#define cl_va_copy(dest,orig) { \
	dest[0].narg = orig[0].narg; \
	dest[0].sp = orig[0].sp; \
	va_copy(dest[0].args,orig[0].args); \
}
#define cl_va_end(a) \
	va_end(a[0].args)
#define	check_arg(n) \
	do { if (narg != (n)) FEwrong_num_arguments_anonym();} while(0)

/***********************
 * RETURN VALUES STACK
 ***********************/

#define NVALUES		cl_env.nvalues
#define VALUES(n)	cl_env.values[n]
#define return0()	return ((NVALUES = 0),Cnil)
#define return1(x)	return ((VALUES(0)=(x)),(NVALUES=1),VALUES(0))
#define returnn(x)	return x

/*****************************
 * LEXICAL ENVIRONMENT STACK
 *****************************/
/*
 * A lexical environment is a list of pairs, each one containing either
 * a variable definition, a tagbody or block tag, or a local function
 * definition.
 *
 *	lex_env ---> ( { record }* )
 *	record = variable | function | block_tag | tagbody_tag
 *
 *	variable = ( var_name[symbol] . value )
 *	function = ( function[bytecodes] . fun_name[symbol] )
 *	block_tag = ( tag[fixnum] . block_name[symbol] )
 *	tagbody_tag = ( tag[fixnum] . 0 )
 */

/*********************************
 * HIGH LEVEL CONTROL STRUCTURES *
 *********************************/

#define CL_NEWENV_BEGIN {\
	cl_index __i = cl_stack_push_values(); \

#define CL_NEWENV_END \
	cl_stack_pop_values(__i); }

#define CL_UNWIND_PROTECT_BEGIN {\
	bool __unwinding; ecl_frame_ptr __next_fr; \
	cl_index __nr; \
	if (frs_push(ECL_PROTECT_TAG)) { \
		__unwinding=1; __next_fr=cl_env.nlj_fr; \
	} else {

#define CL_UNWIND_PROTECT_EXIT \
	__unwinding=0; } \
	frs_pop(); \
	__nr = cl_stack_push_values();

#define CL_UNWIND_PROTECT_END \
	cl_stack_pop_values(__nr); \
	if (__unwinding) ecl_unwind(__next_fr); }

#define CL_BLOCK_BEGIN(id) { \
	cl_object id = new_frame_id(); \
	if (frs_push(id) == 0)

#define CL_BLOCK_END } \
	frs_pop()

#define CL_CATCH_BEGIN(tag) \
	if (frs_push(tag) == 0) {

#define CL_CATCH_END } \
	frs_pop();

#define CL_CATCH_ALL_BEGIN \
	if (frs_push(ECL_PROTECT_TAG) == 0) {

#define CL_CATCH_ALL_IF_CAUGHT } else {

#define CL_CATCH_ALL_END } \
	frs_pop()

#ifdef __cplusplus
}
#endif
