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
	if (cl_env.bds_top >= cl_env.bds_limit)  \
		bds_overflow()

#ifdef ECL_THREADS
extern void bds_bind(cl_object symbol, cl_object value);
extern void bds_push(cl_object symbol);
extern void bds_unwind1();
extern void bds_unwind_n(int n);
extern cl_object *ecl_symbol_slot(cl_object s);
#define SYM_VAL(s) (*ecl_symbol_slot(s))
#if 0
#define ECL_SET(s,v) ((s)->symbol.value=(v))
#define ECL_SETQ(s,v) (*ecl_symbol_slot(s)=(v))
#else
extern cl_object ecl_set_symbol(cl_object s, cl_object v);
#define ECL_SET(s,v) (ecl_set_symbol(s,v))
#define ECL_SETQ(s,v) (ecl_set_symbol(s,v))
#endif
#else
#define SYM_VAL(s) ((s)->symbol.value)
#define ECL_SET(s,v) ((s)->symbol.value=(v))
#define ECL_SETQ(s,v) ((s)->symbol.value=(v))
#define	bds_bind(sym, val)  \
	((++cl_env.bds_top)->symbol = (sym),  \
	cl_env.bds_top->value = SYM_VAL(sym),  \
	SYM_VAL(sym) = (val))

#define bds_push(sym) \
	((++cl_env.bds_top)->symbol = (sym), cl_env.bds_top->value = SYM_VAL(sym))

#define	bds_unwind1()  \
	(SYM_VAL(cl_env.bds_top->symbol) = cl_env.bds_top->value, --cl_env.bds_top)

#define bds_unwind_n(n) \
	bds_unwind(cl_env.bds_top - (n))
#endif /* ECL_THREADS */

/****************************
 * INVOCATION HISTORY STACK
 ****************************/

typedef struct ihs_frame {
	struct ihs_frame *next;
	cl_object function;
	cl_object lex_env;
	cl_index index;
} *ihs_ptr;

#define ihs_push(r,f) do {\
	(r)->next=cl_env.ihs_top; (r)->function=(f); (r)->lex_env= cl_env.lex_env; \
	(r)->index=cl_env.ihs_top->index+1;\
	cl_env.ihs_top = (r); \
} while(0)

#define ihs_pop() do {\
	cl_env.lex_env = cl_env.ihs_top->lex_env; \
	if (cl_env.ihs_top->next == NULL) internal_error("Underflow in IHS stack"); \
	cl_env.ihs_top = cl_env.ihs_top->next; \
} while(0)

extern cl_object ihs_top_function_name(void);

/***************
 * FRAME STACK
 ***************/
/*
frs_class |            frs_value                 |  frs_prev
----------+--------------------------------------+--------------
CATCH     | frame-id, i.e.                       |
	  |    throw-tag,                        |
	  |    block-id (uninterned symbol), or  | value of ihs_top
	  |    tagbody-id (uninterned symbol)    | when the frame
----------+--------------------------------------| was pushed
CATCHALL  |               NIL                    |
----------+--------------------------------------|
PROTECT   |               NIL                    |
----------------------------------------------------------------
*/

enum fr_class {
	FRS_CATCH,		/* for catch,block,tabbody */
	FRS_CATCHALL,		/* for catchall */
	FRS_PROTECT		/* for protect-all */
};

typedef struct frame {
	jmp_buf		frs_jmpbuf;
	cl_object	frs_lex;
	bds_ptr		frs_bds_top;
	enum fr_class	frs_class;
	cl_object	frs_val;
	ihs_ptr		frs_ihs;
	cl_index	frs_sp;
} *frame_ptr;

extern frame_ptr _frs_push(register enum fr_class clas, register cl_object val);

#define frs_push(class, val)  ecl_setjmp(_frs_push(class, val)->frs_jmpbuf)

#define frs_pop() (cl_env.frs_top--)

/*******************
 * C CONTROL STACK
 *******************/

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

cl_env.lex_env ------> ( tag0 value0 tag1 value1 ... )

	tag:		variable-name (symbol)
	value:		variable-value (any lisp object)

	tag:		:function
	value:		(function-name . function-object)

	tag:		:block
	value:		(block-name . frame-id)

*/

/*********************************
 * HIGH LEVEL CONTROL STRUCTURES *
 *********************************/

#define CL_NEWENV_BEGIN {\
	cl_index __i = cl_stack_push_values(); \
	cl_object __env = cl_env.lex_env;

#define CL_NEWENV_END \
	cl_stack_pop_values(__i); \
	cl_env.lex_env = __env; }

#define CL_UNWIND_PROTECT_BEGIN {\
	bool __unwinding; frame_ptr __next_fr; \
	cl_index __nr; \
	if (frs_push(FRS_PROTECT,Cnil)) { \
		__unwinding=1; __next_fr=cl_env.nlj_fr; \
	} else {

#define CL_UNWIND_PROTECT_EXIT \
	__unwinding=0; } \
	frs_pop(); \
	__nr = cl_stack_push_values();

#define CL_UNWIND_PROTECT_END \
	cl_stack_pop_values(__nr); \
	if (__unwinding) unwind(__next_fr); }

#define CL_BLOCK_BEGIN(id) { \
	cl_object id = new_frame_id(); \
	if (frs_push(FRS_CATCH,id) == 0)

#define CL_BLOCK_END } \
	frs_pop()

#define CL_CATCH_BEGIN(tag) \
	if (frs_push(FRS_CATCH,tag) == 0) {

#define CL_CATCH_END } \
	frs_pop();

#define CL_CATCH_ALL_BEGIN \
	if (frs_push(FRS_CATCH,Cnil) == 0) {

#define CL_CATCH_ALL_IF_CAUGHT } else {

#define CL_CATCH_ALL_END } \
	frs_pop()

#ifdef __cplusplus
}
#endif
