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

extern void cl_stack_push(cl_object o);
extern cl_object cl_stack_pop(void);
extern cl_index cl_stack_index(void);
extern void cl_stack_set_index(cl_index sp);
extern void cl_stack_pop_n(cl_index n);
extern void cl_stack_insert(cl_index where, cl_index n);
extern void cl_stack_push_varargs(cl_index n, va_list args);
extern void cl_stack_push_n(cl_index n, cl_object *args);

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

/****************************
 * INVOCATION HISTORY STACK
 ****************************/

extern cl_index ihs_top;

extern void ihs_push(cl_object fun);
extern cl_object ihs_top_function_name(void);
extern void ihs_pop(void);

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
	cl_index	frs_ihs;
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

#define	check_arg(n)  \
			if (narg != (n))  \
				check_arg_failed(narg, n)

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
extern cl_object Values[VSSIZE];
#endif

#define MV_SAVE(nr) \
   { int nr = NValues; cl_object mv_values[nr]; /* __GNUC__ */ \
       memcpy(mv_values, &VALUES(0), nr * sizeof(cl_object))
#define MV_RESTORE(nr) \
       memcpy(&VALUES(0), mv_values, (NValues = nr) * sizeof(cl_object));}
#define MV_SHIFT(nr, d) \
   { int i; for (i = (nr)-1; i >= 0; i--) VALUES(i+d) = VALUES(i);}
#define MV_VALUES(i) mv_values[i]


/*****************************
 * LEXICAL ENVIRONMENT STACK
 *****************************/
/*
		|---------------|
lex_env ------> |    lex-var	|	: lex_env[0]
		|---------------|
		|    lex-fd	|       : lex_env[1]
		|---------------|
		|    lex-tag	|       : lex_env[2]
		|---------------|
	lex-var:        (symbol value)      	; for local binding
	or		(symbol)                ; for special binding

	lex-fd:         (fun-name 'FUNCTION'   function)
	or		(macro-name 'MACRO' expansion-function)

	lex-tag:  	(tag    'TAG'  	frame-id)
	or		(block-name 'BLOCK' frame-id)

where 'FUN' is the LISP symbol with pname FUN, etc.
*/

#ifdef THREADS
#define lex_env       clwp->lwp_lex_env
#else
extern cl_object lex_env;
#endif

#define lex_copy()		(void)0
#define lex_new()		lex_env = Cnil

#ifdef __cplusplus
}
#endif
