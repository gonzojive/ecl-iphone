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

/********************
 * INTERPRETER STACK
 ********************/

extern cl_object stack;

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

typedef struct invocation_history {
	cl_object ihs_function;
	cl_object ihs_base;
} *ihs_ptr;

#ifdef THREADS
#define ihs_size	clwp->lwp_ihs_size
#define ihs_org		clwp->lwp_ihs_org
#define ihs_limit       clwp->lwp_ihs_limit
#define ihs_top         clwp->lwp_ihs_top
#else
extern size_t ihs_size;
extern ihs_ptr ihs_org;
extern ihs_ptr ihs_limit;
extern ihs_ptr ihs_top;
#endif
#define ihs_stack       ihs_org

#define	ihs_check  \
	if (ihs_top >= ihs_limit)  \
		ihs_overflow()

#define ihs_push(function, args) { \
	(++ihs_top)->ihs_function = (function);  \
	ihs_top->ihs_base = args; \
}

#define ihs_push_funcall(function) { \
	ihs_check; \
	(++ihs_top)->ihs_function = (function);  \
	ihs_top->ihs_base = Cnil; \
}

#define ihs_pop() 	(ihs_top--)

#define make_nil_block(r) { \
	cl_object x;  \
	lex_copy();  \
	x = new_frame_id();  \
	lex_block_bind(Cnil, x);  \
	r = frs_push(FRS_CATCH, x);  \
}

#define BLOCK(name,output) { \
	cl_object *lex_old = lex_env; lex_dcl; \
	cl_object _x; \
	lex_copy(); \
	_x = new_frame_id(); \
	lex_block_bind(name,_x); \
	if (frs_push(FRS_CATCH,_x) != 0) output = Values[0]; else
#define END_BLOCK \
	frs_pop(); \
	lex_env = lex_old; }

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

extern frame_ptr _frs_push(register enum fr_class class, register cl_object val);

#define frs_push(class, val)  ecls_setjmp(_frs_push(class, val)->frs_jmpbuf)

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
#endif THREADS

#ifdef DOWN_STACK
#define	cs_check(something) \
	if ((int *)(&something) < cs_limit) \
		cs_overflow()
#else
#define	cs_check(something) \
	if ((int *)(&something) > cs_limit) \
		cs_overflow()
#endif DOWN_STACK

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

#define lex_copy()		lex_env = CONS(car(lex_env),cdr(lex_env))
#define lex_new()		lex_env = CONS(Cnil,Cnil)
#define lex_var_sch(name)	assq((name),CAR(lex_env))
#define lex_fun_sch(name)	lex_sch(CDR(lex_env),(name),clSfunction)
#define lex_tag_sch(name)	lex_sch(CDR(lex_env),(name),clStag)
#define lex_block_sch(name)	lex_sch(CDR(lex_env),(name),clSblock)
