/*
  OP_NOP
	Sets VALUES(0) = NIL and NValues = 1

  OP_BLOCK	block-name{obj}
  ...
  OP_EXIT
	Executes the enclosed forms in a named block

  OP_PUSH
	Pushes the object in VALUES(0)

  OP_PUSHV	n{arg}, var{symbol}
	Pushes the value of the n-th local onto the stack. VAR is given
	for readability purposes only.

  OP_PUSHVS	var{symbol}
	Pushes the value of the symbol VAR onto the stack.

  OP_VAR	n{arg}, var{symbol}
	Returns the value of the n-th local. VAR is given for readability
	of diassembled code only.

  OP_VARS	var{symbol}
	Returns the value of the symbol VAR.

  OP_PUSHQ	value{obj}
	Pushes "value"

  OP_PUSHV	var{symbol}
	Pushes the value of the variable "var"

  OP_PUSHVALUES
	Pushes the values output by the last form

  OP_MCALL
  ...
  OP_EXIT
	Saves the stack pointer, executes several forms and
	funcalls VALUES(0) using the content of the stack.

  OP_CALL	narg{arg}, function{symbol}
	Calls "function" using the last "narg" values in the stack.
  OP_PCALL	narg{arg}, function{symbol}
	Calls "function" using the last "narg" values in the stack.
	The first result of the call is also pushed.

  OP_FCALL	narg{arg}
	Calls VALUES(0) using the last "narg" values in the stack.
  OP_PCALL	narg{arg}, function{symbol}
	Calls VALUES(0) using the last "narg" values in the stack.
	The first result of the call is also pushed.

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
  OP_BLOCK,
  OP_PUSH,
  OP_PUSHQ,
  OP_PUSHV,
  OP_PUSHVS,
  OP_PUSHVALUES,
  OP_VAR,
  OP_VARS,
  OP_MCALL,
  OP_CALL,
  OP_CALLG,
  OP_FCALL,
  OP_PCALL,
  OP_PCALLG,
  OP_PFCALL,
  OP_CATCH,
  OP_EXIT,
  OP_FLET,
  OP_LABELS,
  OP_FUNCTION,
  OP_CLOSE,
  OP_GO,
  OP_THROW,
  OP_JMP,
  OP_JNIL,
  OP_JT,
  OP_JEQ,
  OP_JNEQ,
  OP_BIND,
  OP_BINDS,
  OP_SETQ,
  OP_SETQS,
  OP_PBIND,
  OP_PBINDS,
  OP_PSETQ,
  OP_PSETQS,
  OP_UNBIND,
  OP_UNBINDS,
  OP_MBIND,
  OP_MSETQ,
  OP_PROGV,
  OP_VALUES,
  OP_NTHVAL,
  OP_MPROG1,
  OP_RETURN,
  OP_TAGBODY,
  OP_UNWIND,
  OP_QUOTE,
  OP_DOLIST,
  OP_DOTIMES,
  OP_DO,
  OP_HALT,
  OP_MAXOPCODES = 128,
  OP_OPCODE_SHIFT = 7
};

#define OPARG_SHIFT 16
#define MAX_OPARG (1 << (31 - OPARG_SHIFT) - 1)
#define SET_OPARG(o,n) ((cl_object)((cl_fixnum)(o) | ((n) << OPARG_SHIFT)))
#define GET_OPARG(o) ((cl_fixnum)(o) >> OPARG_SHIFT)
#define GET_OP(o) (((cl_fixnum)(o) & 0xFF) >> 2)
