/* -*- mode: c; c-basic-offset: 8 -*- */
/**********************************************************************
 ***
 ***  IMPORTANT: ANY CHANGE IN THIS FILE MUST BE MATCHED BY
 ***		 APPROPRIATE CHANGES IN THE INTERPRETER AND COMPILER
 ***		 IN PARTICULAR, IT MAY HURT THE THREADED INTERPRETER
 ***		 CODE.
 **********************************************************************/
/*
  OP_BLOCK	block-name{obj}
  ...
  OP_EXIT_FRAME
	Exits the innermost frame (created by OP_BLOCK, OP_DO, etc).

  OP_EXIT
	Executes the enclosed forms in a named block

  OP_PUSH
	Pushes the object in VALUES(0)

  OP_PUSHV	n{arg}, var{symbol}
	Pushes the value of the n-th local onto the stack. VAR is given
	for readability purposes only.

  OP_PUSHVS	var{symbol}
	Pushes the value of the symbol VAR onto the stack.

  OP_VAR	n{arg}
	Returns the value of the n-th local.

  OP_VARS	var{symbol}
	Returns the value of the symbol VAR.

  OP_PUSHQ	value{obj}
	Pushes "value" onto the stack.

  OP_PUSHV	n{arg}, var{symbol}
	Pushes the value of the n-th local. The name of the variable is
	kept for readability purposes.

  OP_PUSHVALUES
	Pushes the values output by the last form.

  OP_MCALL
  ...
  OP_EXIT
	Saves the stack pointer, executes several forms and
	funcalls VALUES(0) using the content of the stack.

  OP_CALLG	narg{arg}, function{symbol}
	Calls global "function" using the last "narg" values in the stack.
  OP_PCALLG	narg{arg}, function{symbol}
	Calls global "function" using the last "narg" values in the stack.
	The first result of the call is also pushed.

  OP_CALL	narg{arg}
	Calls VALUES(0) using the last "narg" values in the stack.
  OP_PCALL	narg{arg}
	Calls VALUES(0) using the last "narg" values in the stack.
	The first result of the call is also pushed.

  OP_FCALL	narg{arg}
	Pops NARG arguments from the stack, plus a function object and
	builds up a function call.

  OP_CATCH	dest{label}
  ...
  OP_EXIT
	Sets a catch point with the tag in VALUES(0). The end of
	the block is marked by "dest".

  OP_FLET	nfun{arg}
  ...
  OP_EXIT

  OP_LABELS	nfun{arg}
  ...
  OP_EXIT

  OP_FUNCTION	symbol{obj}

  OP_CLOSE	interpreted-function{obj}

  OP_GO		tag-name{obj}

  OP_THROW	tag-name{obj}

  OP_RETURN	tag-name{obj}

  OP_JMP	dest{label}
  OP_JNIL	dest{label}
  OP_JT		dest{label}

  OP_CASE	n{arg}
  	object1{obj}	dest1{label}
	object2{obj}	dest2{label}
	...
	objectn{obj}	destn{label}
	destx{label}
  dest1:
	...
	OP_EXIT
  dest2:
	...
	OP_EXIT
	...
  destn:
	...
	OP_EXIT
  destx:

  OP_DO		exit{label}
  	...
  OP_EXIT

  OP_PUSHENV
	...
  OP_EXIT

  OP_DOLIST
	OP_BIND	var{obj}
	OP_EXIT

 */

enum {
  OP_NOP,
  OP_QUOTE,
  OP_ENDP,
  OP_CONS,
  OP_CAR,
  OP_CDR,
  OP_LIST,
  OP_LISTA,
  OP_VAR,
  OP_VARS,
  OP_PUSH,
  OP_PUSHV,
  OP_PUSHVS,
  OP_PUSHQ,
  OP_CALLG,
  OP_CALL,
  OP_FCALL,
  OP_PCALLG,
  OP_PCALL,
  OP_PFCALL,
  OP_MCALL,
  OP_EXIT,
  OP_FLET,
  OP_LABELS,
  OP_LFUNCTION,
  OP_FUNCTION,
  OP_CLOSE,
  OP_GO,
  OP_RETURN,
  OP_THROW,
  OP_JMP,
  OP_JNIL,
  OP_JT,
  OP_JEQL,
  OP_JNEQL,
  OP_UNBIND,
  OP_UNBINDS,
  OP_BIND,
  OP_PBIND,
  OP_VBIND,
  OP_BINDS,
  OP_PBINDS,
  OP_VBINDS,
  OP_SETQ,
  OP_SETQS,
  OP_PSETQ,
  OP_PSETQS,
  OP_BLOCK,
  OP_DO,
  OP_CATCH,
  OP_TAGBODY,
  OP_EXIT_TAGBODY,
  OP_EXIT_FRAME,
  OP_PROTECT,
  OP_PROTECT_NORMAL,
  OP_PROTECT_EXIT,
  OP_MSETQ,
  OP_PROGV,
  OP_EXIT_PROGV,
  OP_PUSHVALUES,
  OP_POP,
  OP_POPVALUES,
  OP_PUSHMOREVALUES,
  OP_VALUES,
  OP_VALUEREG0,
  OP_NTHVAL,
  OP_NIL,
  OP_NOT,
  OP_PUSHNIL,
  OP_STEPIN,
  OP_STEPCALL,
  OP_STEPOUT,
  OP_MAXOPCODES = 128,
  OP_OPCODE_SHIFT = 7
};

#define MAX_OPARG 0x7FFF
#ifdef ECL_SMALL_BYTECODES
#define OPCODE_SIZE 1
#define OPARG_SIZE sizeof(cl_oparg)
typedef char cl_opcode;
#else
#define OPCODE_SIZE 1
#define OPARG_SIZE 1
typedef int16_t cl_opcode;
#endif
typedef int16_t cl_oparg;
#define READ_OPCODE(v)	(*(cl_opcode *)(v))
#define READ_OPARG(v)	(*(cl_oparg *)(v))
#define GET_OPCODE(v)	(*((cl_opcode *)(v)++))
#define GET_OPARG(v)	(*((cl_oparg *)(v)++))
#define GET_DATA(v,b)	(b->bytecodes.data[GET_OPARG(v)])
#define GET_LABEL(pc,v)	{pc = (v) + READ_OPARG(v); v += OPARG_SIZE;}

/**********************************************************************
 * THREADED INTERPRETER CODE
 *
 * By using labels as values, we can build a variant of the
 * interpreter code that leads to better performance because (i) it
 * saves a range check on the opcode size and (ii) each opcode has a
 * dispatch instruction at the end, so that the processor may better
 * predict jumps.
 */
#if (defined(__GNUC__) && !defined(__STRICT_ANSI__))
#define ECL_THREADED_INTERPRETER
#endif

#ifdef ECL_THREADED_INTERPRETER
#define BEGIN_SWITCH \
	THREAD_NEXT;
#define CASE(name) \
	LBL_##name:
#define THREAD_NEXT \
	goto *(&&LBL_OP_NOP + offsets[GET_OPCODE(vector)])
#else
#define BEGIN_SWITCH \
	switch (GET_OPCODE(vector))
#define THREAD_NEXT \
	goto BEGIN
#define CASE(name) \
	case name:
#endif

#if !defined(ECL_THREADED_INTERPRETER)
#define ECL_OFFSET_TABLE
#else
#define ECL_OFFSET_TABLE \
	static const int offsets[] = {\
		&&LBL_OP_NOP - &&LBL_OP_NOP,\
		&&LBL_OP_QUOTE - &&LBL_OP_NOP,\
		&&LBL_OP_ENDP - &&LBL_OP_NOP,\
		&&LBL_OP_CONS - &&LBL_OP_NOP,\
		&&LBL_OP_CAR - &&LBL_OP_NOP,\
		&&LBL_OP_CDR - &&LBL_OP_NOP,\
		&&LBL_OP_LIST - &&LBL_OP_NOP,\
		&&LBL_OP_LISTA - &&LBL_OP_NOP,\
		&&LBL_OP_VAR - &&LBL_OP_NOP,\
		&&LBL_OP_VARS - &&LBL_OP_NOP,\
		&&LBL_OP_PUSH - &&LBL_OP_NOP,\
		&&LBL_OP_PUSHV - &&LBL_OP_NOP,\
		&&LBL_OP_PUSHVS - &&LBL_OP_NOP,\
		&&LBL_OP_PUSHQ - &&LBL_OP_NOP,\
		&&LBL_OP_CALLG - &&LBL_OP_NOP,\
		&&LBL_OP_CALL - &&LBL_OP_NOP,\
		&&LBL_OP_FCALL - &&LBL_OP_NOP,\
		&&LBL_OP_PCALLG - &&LBL_OP_NOP,\
		&&LBL_OP_PCALL - &&LBL_OP_NOP,\
		&&LBL_OP_PFCALL - &&LBL_OP_NOP,\
		&&LBL_OP_MCALL - &&LBL_OP_NOP,\
		&&LBL_OP_EXIT - &&LBL_OP_NOP,\
		&&LBL_OP_FLET - &&LBL_OP_NOP,\
		&&LBL_OP_LABELS - &&LBL_OP_NOP,\
		&&LBL_OP_LFUNCTION - &&LBL_OP_NOP,\
		&&LBL_OP_FUNCTION - &&LBL_OP_NOP,\
		&&LBL_OP_CLOSE - &&LBL_OP_NOP,\
		&&LBL_OP_GO - &&LBL_OP_NOP,\
		&&LBL_OP_RETURN - &&LBL_OP_NOP,\
		&&LBL_OP_THROW - &&LBL_OP_NOP,\
		&&LBL_OP_JMP - &&LBL_OP_NOP,\
		&&LBL_OP_JNIL - &&LBL_OP_NOP,\
		&&LBL_OP_JT - &&LBL_OP_NOP,\
		&&LBL_OP_JEQL - &&LBL_OP_NOP,\
		&&LBL_OP_JNEQL - &&LBL_OP_NOP,\
		&&LBL_OP_UNBIND - &&LBL_OP_NOP,\
		&&LBL_OP_UNBINDS - &&LBL_OP_NOP,\
		&&LBL_OP_BIND - &&LBL_OP_NOP,\
		&&LBL_OP_PBIND - &&LBL_OP_NOP,\
		&&LBL_OP_VBIND - &&LBL_OP_NOP,\
		&&LBL_OP_BINDS - &&LBL_OP_NOP,\
		&&LBL_OP_PBINDS - &&LBL_OP_NOP,\
		&&LBL_OP_VBINDS - &&LBL_OP_NOP,\
		&&LBL_OP_SETQ - &&LBL_OP_NOP,\
		&&LBL_OP_SETQS - &&LBL_OP_NOP,\
		&&LBL_OP_PSETQ - &&LBL_OP_NOP,\
		&&LBL_OP_PSETQS - &&LBL_OP_NOP,\
		&&LBL_OP_BLOCK - &&LBL_OP_NOP,\
		&&LBL_OP_DO - &&LBL_OP_NOP,\
		&&LBL_OP_CATCH - &&LBL_OP_NOP,\
		&&LBL_OP_TAGBODY - &&LBL_OP_NOP,\
		&&LBL_OP_EXIT_TAGBODY - &&LBL_OP_NOP,\
		&&LBL_OP_EXIT_FRAME - &&LBL_OP_NOP,\
		&&LBL_OP_PROTECT - &&LBL_OP_NOP,\
		&&LBL_OP_PROTECT_NORMAL - &&LBL_OP_NOP,\
		&&LBL_OP_PROTECT_EXIT - &&LBL_OP_NOP,\
		&&LBL_OP_MSETQ - &&LBL_OP_NOP,\
		&&LBL_OP_PROGV - &&LBL_OP_NOP,\
		&&LBL_OP_EXIT_PROGV - &&LBL_OP_NOP,\
		&&LBL_OP_PUSHVALUES - &&LBL_OP_NOP,\
		&&LBL_OP_POP - &&LBL_OP_NOP,\
		&&LBL_OP_POPVALUES - &&LBL_OP_NOP,\
		&&LBL_OP_PUSHMOREVALUES - &&LBL_OP_NOP,\
		&&LBL_OP_VALUES - &&LBL_OP_NOP,\
		&&LBL_OP_VALUEREG0 - &&LBL_OP_NOP,\
		&&LBL_OP_NTHVAL - &&LBL_OP_NOP,\
		&&LBL_OP_NIL - &&LBL_OP_NOP,\
		&&LBL_OP_NOT - &&LBL_OP_NOP,\
		&&LBL_OP_PUSHNIL - &&LBL_OP_NOP,\
		&&LBL_OP_STEPIN - &&LBL_OP_NOP,\
		&&LBL_OP_STEPCALL - &&LBL_OP_NOP,\
		&&LBL_OP_STEPOUT - &&LBL_OP_NOP\
	}
#endif
