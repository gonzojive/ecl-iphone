/*
    lwp.h  -- Light weight processes.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/
#include <sys/ioctl.h>

typedef struct lpd {

  /* stacks.h - The bind stack */
  size_t lwp_bds_size;
  bds_ptr lwp_bds_org;
  bds_ptr lwp_bds_limit;
  bds_ptr lwp_bds_top;

  /* eval.h - The C stack */
  size_t lwp_cssize;	/* see reset_stack_limits */
  int *lwp_cs_org;
  int *lwp_cs_limit;

  /* stacks.h - The Invocation History stack */
  size_t lwp_ihs_size;
  ihs_ptr lwp_ihs_org;
  ihs_ptr lwp_ihs_limit;
  ihs_ptr lwp_ihs_top;

  /* stacks.h - The Frame stack */
  size_t lwp_frs_size;
  frame_ptr lwp_frs_org;
  frame_ptr lwp_frs_limit;
  frame_ptr lwp_frs_top;

    /* Non-local jumps */
  frame_ptr lwp_nlj_fr;
  cl_object lwp_nlj_tag;

  /* lex.h - Lexical environment */
  cl_object lwp_lex[3];
  cl_object *lwp_lex_env;

  /* vs.h - Multiple values */
  cl_object lwp_Values[VSSIZE];
  int	 lwp_nValues;		/* No. of values produced by Lresume */

  /* used in alloc.c */
  cl_object lwp_alloc_temporary;

  /* backq.c */
  int lwp_backq_level;		/* It should be initialized with 0 (zero) */

  /* format.c */
  int (*lwp_fmt_ch_fun)();
  cl_object lwp_fmt_stream;
  int lwp_ctl_origin;
  int lwp_ctl_index;
  int lwp_ctl_end;
  cl_object *lwp_fmt_base;
  int lwp_fmt_index;
  int lwp_fmt_end;
  int *lwp_fmt_jmp_buf;
  int lwp_fmt_indents;
  cl_object lwp_fmt_string;
  cl_object lwp_fmt_temporary_stream;
  cl_object lwp_fmt_temporary_string;
  int lwp_fmt_nparam;
  struct {
    int fmt_param_type;
    int fmt_param_value;
  } lwp_fmt_param[100];
  int lwp_fmt_spare_spaces;
  int lwp_fmt_line_length;

  /* lwp.c */
  cl_object lwp_thread;

  /* list.d */
  cl_object lwp_test_function;
  cl_object lwp_item_compared;
  bool (*lwp_tf)();
  cl_object lwp_key_function;
  cl_object (*lwp_kf)();

  /* print.d */
  cl_fixnum lwp_CIRCLEcounter;
  cl_object lwp_CIRCLEstack;
  cl_object lwp_PRINTstream;
  bool lwp_PRINTescape;
  bool lwp_PRINTpretty;
  bool lwp_PRINTcircle;
  int lwp_PRINTbase;
  bool lwp_PRINTradix;
  cl_object lwp_PRINTcase;
  bool lwp_PRINTgensym;
  int lwp_PRINTlevel;
  int lwp_PRINTlength;
  bool lwp_PRINTarray;
/* LWP 2
 */
  bool lwp_PRINTpackage;
  bool lwp_PRINTstructure;

  int (*lwp_output_ch_fun)();
  short lwp_queue[Q_SIZE];
  short lwp_indent_stack[IS_SIZE];
  int lwp_qh;
  int lwp_qt;
  int lwp_qc;
  int lwp_isp;
  int lwp_iisp;

  /* read.d */
  cl_object lwp_READtable;
  int lwp_READdefault_float_format;
  int lwp_READbase;
  bool lwp_READsuppress;
  bool lwp_preserving_whitespace_flag;
  bool lwp_escape_flag;
  cl_object lwp_delimiting_char;
  bool lwp_detect_eos_flag;
  bool lwp_in_list_flag;
  bool lwp_dot_flag;
  cl_object lwp_default_dispatch_macro;
  cl_object sharp_eq_context;
  cl_object (*lwp_read_ch_fun)();

  /* big.c */
  mp_limb lwp_bignum_register_limbs[3][BIGNUM_REGISTER_SIZE];
  cl_object lwp_bignum_registers[3];

  /* symbol.d */
  cl_object lwp_string_register;
  cl_object lwp_token;		/* They have to be initialized with
			         * alloc_simple_string and */
} lpd;


#define RUNNING		0
#define SUSPENDED	1
#define STOPPED		2
#define DEAD		3
#define WAITING		4
#define DELAYED		5

typedef struct pd {
  cl_object pd_thread;             /* point back to its thread	 */
  int	 pd_status;		/* RUNNING or STOPPED or DEAD */
  int	 *pd_base;		/* Stack Base */
#ifdef VAX
  int	 pd_env[16];		/* Stack Environment */ 
#else
  sigjmp_buf  pd_env;		/* Stack Environment */ 
#endif
  int	 pd_slice;              /* time out			 */
  FILE	 *pd_fp;		/* File pointer waiting input on */
  lpd	 *pd_lpd;               /* lisp process descriptor */
  struct pd *pd_next;

} pd;



#define PUSH(lpd)     { if ( running_head == NULL)               \
			  { running_head = lpd;                  \
			   running_tail = lpd;                   \
			   lpd->pd_next = NULL;   }              \
		         else { lpd->pd_next = running_head;     \
			    running_head = lpd; } }

#define ENQUEUE(lpd)  { if  ( running_head == NULL )              \
			   {  running_head = lpd;                 \
			      running_tail = lpd ;                \
			      running_tail->pd_next = NULL; }      \
			 else {  running_tail->pd_next = lpd;     \
			         lpd->pd_next = NULL;             \
			         running_tail = lpd;  } }


#define ROTQUEUE()	 { running_tail->pd_next = running_head; \
			    running_head = running_head->pd_next; \
			    running_tail = running_tail->pd_next; \
			    running_tail->pd_next = NULL; }


/*
#define PUSH(lpd)     ( running_head == NULL \
			 ? (running_head = running_tail = lpd, \
			    lpd->pd_next = NULL) \
		         : (lpd->pd_next = running_head, \
			    running_head = lpd ) )

#define ENQUEUE(lpd) ( running_head == NULL \
			 ? (running_head = running_tail = lpd, \
			    running_tail->pd_next = NULL) \
			 : (running_tail->pd_next = lpd, \
			    lpd->pd_next = NULL, \
			    running_tail = lpd) )


#define DEQUEUE(lpd)    ( lpd = running_head, \
			   running_head != NULL \
			   ? running_head = lpd->pd_next \
			   : NULL )

#define ROTQUEUE()	( running_head != NULL \
			 ? (running_tail->pd_next = running_head, \
			    running_head = running_head->pd_next, \
			    running_tail = running_tail->pd_next, \
			    running_tail->pd_next = NULL ) \
			 : NULL )

*/

#define FOREVER 1

#define SPAWNSTART 0
#define SPAWNEND 1
#define SCHEDULE 2
#define REMOVELWP 3

#define QUANTUM 20000		/* Microseconds */
#define REALQUANTUM 100000	/*      ""      */

#define STACK_SIZE (CSSIZE+CSGETA)	/* Words */
