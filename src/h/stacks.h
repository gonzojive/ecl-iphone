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

/********************
 * INTERPRETER STACK
 ********************/

extern cl_index cl_stack_size;
extern cl_object *cl_stack;
extern cl_object *cl_stack_top;
extern cl_object *cl_stack_limit;

/**************
 * BIND STACK
 **************/

typedef struct bds_bd {
	cl_object bds_sym;	/*  symbol  */
	cl_object bds_val;	/*  previous value of the symbol  */
} *bds_ptr;

#ifdef THREADS
#define bds_limit       clwp->lwp_bds_limit
#define bds_top         clwp->lwp_bds_top
#define bds_org		clwp->lwp_bds_org
#define bds_size	clwp->lwp_bds_size
#else
extern size_t bds_size;
extern bds_ptr bds_org;
extern bds_ptr bds_limit;
extern bds_ptr bds_top;	/*  bind stack top  */
#endif
#define bind_stack      bds_org

#define	bds_check  \
	if (bds_top >= bds_limit)  \
		bds_overflow()

#define	bds_bind(sym, val)  \
	((++bds_top)->bds_sym = (sym),  \
	bds_top->bds_val = SYM_VAL(sym),  \
	SYM_VAL(sym) = (val))

#define bds_push(sym) \
	((++bds_top)->bds_sym = (sym), bds_top->bds_val = SYM_VAL(sym))

#define	bds_unwind1  \
	(SYM_VAL(bds_top->bds_sym) = bds_top->bds_val, --bds_top)

#define bds_unwind_n(n) \
	bds_unwind(bds_top - (n))

/****************************
 * INVOCATION HISTORY STACK
 ****************************/

typedef struct ihs_frame {
	struct ihs_frame *next;
	cl_object function;
	cl_object lex_env;
	cl_index index;
} *ihs_ptr;

extern ihs_ptr ihs_top;

#define ihs_push(r,f) do {\
	(r)->next=ihs_top; (r)->function=(f); (r)->lex_env= lex_env; \
	(r)->index=ihs_top->index+1;\
	ihs_top = (r); \
} while(0)

#define ihs_pop() do {\
	lex_env=ihs_top->lex_env; \
	if (ihs_top->next == NULL) internal_error("Underflow in IHS stack"); \
	ihs_top=ihs_top->next; \
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

#ifdef THREADS
#define frs_size	clwp->lwp_frs_size
#define frs_org		clwp->lwp_frs_org
#define frs_limit       clwp->lwp_frs_limit
#define frs_top         clwp->lwp_frs_top
#else
extern size_t frs_size;
extern frame_ptr frs_org;
extern frame_ptr frs_limit;
extern frame_ptr frs_top;
#endif
#define frame_stack	frs_org

extern frame_ptr _frs_push(register enum fr_class clas, register cl_object val);

#define frs_push(class, val)  ecl_setjmp(_frs_push(class, val)->frs_jmpbuf)

#define frs_pop() (frs_top--)

/*  global variables used during non-local jump  */

#ifdef THREADS
#define nlj_fr           clwp->lwp_nlj_fr
#define nlj_tag          clwp->lwp_nlj_tag
#else
extern frame_ptr nlj_fr;	/* frame to return  */
extern cl_object nlj_tag;	/* throw-tag, block-id, or */
				/* (tagbody-id . label).   */
#endif

/*******************
 * C CONTROL STACK
 *******************/

#ifdef THREADS
#define cs_limit       clwp->lwp_cs_limit
#define cs_org         clwp->lwp_cs_org
#define cssize         clwp->lwp_cssize
#else
extern int *cs_org;
extern int *cs_limit;
extern size_t cssize;
#endif

#ifdef DOWN_STACK
#define	cs_check(something) \
	if ((int *)(&something) < cs_limit) \
		cs_overflow()
#else
#define	cs_check(something) \
	if ((int *)(&something) > cs_limit) \
		cs_overflow()
#endif

#define	check_arg(n) \
	do { if (narg != (n)) FEwrong_num_arguments_anonym();} while(0)

#define cs_reserve(x)	if(&narg-(x) < cs_limit)  \
				cs_overflow();

/***********************
 * RETURN VALUES STACK
 ***********************/

#define VALUES(n)	Values[n]
#define return0()	return ((NValues = 0),Cnil)
#define return1(x)	return ((NValues = 1),(x))
#define return2(x,y)	return ((NValues = 2),(Values[1] = (y)),(x))
#define returnn(x)	return x

#ifdef THREADS
#error " Thread-safe NValues not yet implemented"
extern cl_object *Values;
#else
extern int NValues;
extern cl_object Values[];
#endif

/*****************************
 * LEXICAL ENVIRONMENT STACK
 *****************************/
/*

lex_env ------> ( tag0 value0 tag1 value1 ... )

	tag:		variable-name (symbol)
	value:		variable-value (any lisp object)

	tag:		:function
	value:		(function-name . function-object)

	tag:		:block
	value:		(block-name . frame-id)

*/

#ifdef THREADS
#define lex_env       clwp->lwp_lex_env
#else
extern cl_object lex_env;
#endif

#define lex_copy()		(void)0
#define lex_new()		lex_env = Cnil

/*********************************
 * HIGH LEVEL CONTROL STRUCTURES *
 *********************************/

#define CL_UNWIND_PROTECT_BEGIN {\
	bool __unwinding; frame_ptr __next_fr; \
	cl_index __nr; \
	if (frs_push(FRS_PROTECT,Cnil)) { \
		__unwinding=1; __next_fr=nlj_fr; \
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
