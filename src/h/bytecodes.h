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
  OP_DOLIST,
  OP_DOTIMES,
  OP_PROTECT,
  OP_PROTECT_NORMAL,
  OP_PROTECT_EXIT,
  OP_MSETQ,
  OP_PROGV,
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
  OP_MAXOPCODES = 128,
  OP_OPCODE_SHIFT = 7
};

/*
   If we we working with character pointers,
	typedef char cl_opcode;
	...
	#define OPCODE_SIZE sizeof(cl_opcode)
	#define OPARG_SIZE sizeof(cl_oparg)
   but since we are not...
 */
#define MAX_OPARG 0x7FFF
typedef char cl_opcode;
typedef int16_t cl_oparg;
#define OPCODE_SIZE 1
#define OPARG_SIZE sizeof(cl_oparg)
#define READ_OPCODE(v)	(*(cl_opcode *)(v))
#define READ_OPARG(v)	(*(cl_oparg *)(v))
#define GET_OPCODE(v)	(*((cl_opcode *)(v))++)
#define GET_OPARG(v)	(*((cl_oparg *)(v))++)
#define GET_DATA(v,b)	(b->bytecodes.data[*((cl_oparg *)(v))++])
#define GET_LABEL(pc,v)	{pc = (v) + *(cl_oparg *)v; v += OPARG_SIZE;}

