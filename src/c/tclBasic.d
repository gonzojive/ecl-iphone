/*
 *
 * tclBasic. c  		- A library replacement for simulating 
 *				  a Tcl interpreter in ECoLisp
 *
 * Copyright (C) 1993,1994,1995 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
 * 
 *
 * Permission to use, copy, and/or distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that both the above copyright notice and this permission notice appear in
 * all copies and derived works.  Fees for distribution or use of this
 * software or derived works may only be charged with express written
 * permission of the copyright holder.  
 * This software is provided ``as is'' without express or implied warranty.
 *
 * This software is a derivative work of other copyrighted softwares; the
 * copyright notices of these softwares are placed in the file COPYRIGHTS
 *
 *
 *            Author: Erick Gallesio [eg@unice.fr]
 *    Creation date: 19-Feb-1993 22:15
 * Last file update: 11-Feb-1995 15:07
 *
 * Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
 *
 */

#include "ecls.h"
#include "tk.h"
#include "tclInt.h"

cl_object TkWidgetType;
Tcl_Interp *ECL_interp;

extern cl_object tk_package;

static Tcl_HashTable VarTable;	/* Global hash table retaining traced variables */

#define STRING_INPUT_STREAM(s, strm) \
  struct stream strm; \
  struct string string; \
  string.t = (short)t_string; \
  string.m = FALSE; \
  string.st_hasfillp = FALSE; \
  string.st_adjustable = FALSE; \
  string.st_displaced = Cnil; \
  string.st_dim = (string.st_fillp = strlen(s)) + 1; \
  string.st_self = s; \
  strm.t = (short)t_stream; \
  strm.m = FALSE; \
  strm.sm_mode = (short)smm_string_input; \
  strm.sm_fp = NULL; \
  strm.sm_object0 = (cl_object)&string; \
  strm.sm_object1 = OBJNULL; \
  strm.sm_int0 = 0; \
  strm.sm_int1 = string.st_fillp

static cl_object
eval_from_string(char *s)
{
  cl_object x;
  STRING_INPUT_STREAM(s, strm);
  x = @read(3, (cl_object)&strm, Cnil, OBJNULL);
  return (x != OBJNULL) ? eval(x, NULL) : Cnil;
}

static cl_object string_stream;
static char char_string[2] = { 0, 0};
static char *empty = "()";

static char *
object2string(cl_object x)
{
  extern VOID *malloc();
  switch (type_of(x)) {
  case t_string:
  case t_symbol:
    if (x == Cnil)
      return(empty);
    else
      return(x->string.self);
  case t_fixnum: {
    char *num = malloc(12);
    sprintf(num, "%d", fix(x));
    return(num);
  }
  case t_character: {
    char_string[0] = char_code(x);
    return char_string;
  }
  case t_cons: {
    extern cl_object @'si::*print-package*';
    string_stream->stream.object0->string.fillp = 0;
    string_stream->stream.int0 = string_stream->stream.int1 = 0;
    bds_bind(@'si::*print-package*', Ct);
    prin1(x, string_stream);
    bds_unwind1;
    return(string_stream->stream.object0->string.self);
  }
  case t_pathname:
    return namestring(x)->string.self;
  case t_shortfloat: {
    char *num = malloc(12);
    sprintf(num, "%f", sf(x));
    return(num);
  }
  case t_longfloat: {
    char *num = malloc(12);
    sprintf(num, "%f", lf(x));
    return(num);
  }
  case t_ratio: {
    char *num = malloc(12);
    if (FIXNUMP(x->ratio.num) && FIXNUMP(x->ratio.den)) {
      sprintf(num, "%d", fix(x->ratio.num) / fix(x->ratio.den));
      return(num);
    }
    break;
  }
  }
  FEerror("~S cannot be coerced to a C string.", 1, x);
}

/*****************************************************************************
 *
 *				Eval functions
 *
 *****************************************************************************/

int
Tcl_GlobalEval(Tcl_Interp *interp, char *s)
{
  cl_object result;

  if (*s == '\0') return TCL_OK;

  /* In some situations Tk appends some data (numbers) to the callback. This
   * arise for scrollbars and scales. These parameters are normally used to
   * reflect slider position. When such a situation arises, we have to 
   * specify the callback as a string and add a pair of parenthesis around
   * this string to form a valid sexpr. To recognize such cases, we look
   * at first character: if it is not an open parenthesis, we add a pair of ()
   * around the callback string 
   * 
   */

  if (*s != '(') {
    /* Build the command to evaluate by adding a pair of parenthesis */
    char buffer[strlen(s)+3];	/* __GNUC__ */
    sprintf(buffer, "(%s)", s);
    result = eval_from_string(buffer);
  }
  else result = eval_from_string(s);
  /* we might use TCL_DYNAMIC if object_to_string used malloc */
  Tcl_SetResult(interp, object2string(result), TCL_STATIC);
  return TCL_OK;
}

/* very simplistic. But do we need something more clever? */
int
Tcl_Eval(Tcl_Interp *interp, char *s)
{
  return Tcl_GlobalEval(interp, s);
}

int
Tcl_VarEval(Tcl_Interp *interp, /* Interpreter in which to execute command */
	    ...)		/* One or more strings to concatenate,
				   terminated with a NULL string. */
{
    va_list argList;
#define FIXED_SIZE 200
    char fixedSpace[FIXED_SIZE+1];
    int spaceAvl, spaceUsed, length;
    char *string, *cmd;
    int result;

    /*
     * Copy the strings one after the other into a single larger
     * string.  Use stack-allocated space for small commands, but if
     * the commands gets too large than call ckalloc to create the
     * space.
     */

    va_start(argList, interp);
    spaceAvl = FIXED_SIZE;
    spaceUsed = 0;
    cmd = fixedSpace;
    while (TRUE) {
	string = va_arg(argList, char *);
	if (string == NULL) {
	    break;
	}
	length = strlen(string);
	if ((spaceUsed + length) > spaceAvl) {
	    char *new;

	    spaceAvl = spaceUsed + length;
	    spaceAvl += spaceAvl/2;
	    new = ckalloc((unsigned) spaceAvl);
	    memcpy((VOID *) new, (VOID *) cmd, spaceUsed);
	    if (cmd != fixedSpace) {
		ckfree(cmd);
	    }
	    cmd = new;
	}
	strcpy(cmd + spaceUsed, string);
	spaceUsed += length;
    }
    va_end(argList);
    cmd[spaceUsed] = '\0';

    result = Tcl_GlobalEval(interp, cmd);
    if (cmd != fixedSpace) {
	ckfree(cmd);
    }
    return result;
}


static void
upcase(char *s, char *d)
{ 
  for ( ; *s != '\0'; s++)
    *d++ = toupper(*s);
  *d = '\0';
}

/*****************************************************************************
 *
 *	      Variable accesses (GetVar, GetVar2, SetVar, SetVar2)
 *
 *****************************************************************************/

char *
Tcl_GetVar(Tcl_Interp *interp, char *var, int flags)
{
  cl_object V;
  char VAR[strlen(var)+1];	/* __GNUC__ */
  upcase(var, VAR);
  V = SYM_VAL(_intern(VAR, tk_package));
  return (V == OBJNULL) ? NULL : object2string(V);
}

char *
Tcl_GetVar2(Tcl_Interp *interp, char *name1, char *name2, int flags)
{
  if (name2 && *name2) {
    char *res;
    char s[strlen(name1) + strlen(name2) + 8]; /* __GNUC__ */

    sprintf(s, "(AREF %s %s)", name1, name2);
    Tcl_GlobalEval(interp, s);
    return interp->result;
  }
  return Tcl_GetVar(interp, name1, flags);
}

char *
Tcl_SetVar(Tcl_Interp *interp, char *var, char *val, int flags)
{
  char VAR[strlen(var)+1];	/* __GNUC__ */
  upcase(var, VAR);
  /* Eval the following expression: (setq var val) */
  SYM_VAL(_intern(VAR, tk_package)) = make_simple_string(val);
/*  Tcl_ChangeValue(var);  in tcl-trace.c */
  return val;
}

char *
Tcl_SetVar2(Tcl_Interp *interp, char *name1, char *name2, char *val,
		  int flags)
{ 
  if (name2 && *name2) {
    char *res;
    char s[strlen(name1) + strlen(name2) + 16]; /* __GNUC__ */

    sprintf(s, "(SETF (AREF %s %s) %s)", name1, name2);
    Tcl_GlobalEval(interp, s);
    return interp->result;
  }
  return Tcl_SetVar(interp, name1, val, flags);
}

/*****************************************************************************
 *
 *			    Tcl command management
 *
 *****************************************************************************/

int
Tcl_DeleteCommand(Tcl_Interp *interp, char *cmdName)
{
  cl_object V   = _intern(cmdName, tk_package);

  if (SYM_FUN(V) == OBJNULL) return -1;
  SYM_FUN(V) = OBJNULL;		/* Undefine "cmdName" */
  SYM_VAL(V) = OBJNULL;		/* Undefine "cmdName" */
  return 0;
}

/* ECL should use lowercase symbols as default!!!
   In such case we could read with:

    STRING_INPUT_STREAM(s, strm);
    @read(3, (cl_object)&strm, Cnil, OBJNULL);
    result = VALUES(0);
    @read(3, (cl_object)&strm, Cnil, OBJNULL);
    if (VALUES(0) != OBJNULL) {
      result = CONS(result, Cnil);
      for (p = &CDR(result) ; ; p = &(CDR(*p))) {
	*p = CONS(VALUES(0), Cnil);
	@read(3, (cl_object)&strm, Cnil, OBJNULL);
	if (VALUES(0) == OBJNULL) break;
      }
    }
 */
static cl_object
parse_from_string(struct string *s, char **ep)
{
  if (isdigit(s->st_self[0])) {
    int n;
    cl_object num = parse_number(s->st_self, s->st_fillp, &n, 10);
    *ep = s->st_self + n;
    return num;
  }
  else {
    *ep = s->st_self + s->st_fillp;
    (cl_object)s = copy_simple_string(s);
    s->st_self[s->st_fillp] = '\0';
    return (cl_object)s;
  }
}

/* We must return strings since commands like 'text index 1.0+1c' return
   indexes (e.g. 1.1) which should not be converted to numbers*/
static cl_object
TkResult2Lisp(Tcl_Interp *interp)
{
  register char *s = interp->result;
  register cl_object result = Cnil, *p;
  extern cl_object Tk_root_window;
 
  if (strcmp(s, ".") == 0) return Tk_root_window;
  if (*s) {
    int i;
    char *e;
    struct stream strm;
    struct string string;
    string.t = (short)t_string;
    string.m = FALSE;
    string.st_hasfillp = FALSE;
    string.st_adjustable = FALSE;
    string.st_displaced = Cnil;
    string.st_self = s;
 
    e = strchr(s, ' ');
    if (e == NULL) {
      string.st_dim = (string.st_fillp = strlen(s))+1;
      result = copy_simple_string(&string);
    } else {
      /*  Result was a list of values, build a proper list */
      string.st_dim = (string.st_fillp = e-s)+1;
      *e = '\0';
      result = CONS(copy_simple_string(&string), Cnil);
      for (p = &CDR(result) ; ; p = &(CDR(*p))) {
        s = e+1;
        string.st_self = s;
        e = strchr(s, ' ');
        if (e == NULL) {
          string.st_dim = (string.st_fillp = strlen(s))+1;
          *p = CONS(copy_simple_string(&string), Cnil);
          break;
        }
        string.st_dim = (string.st_fillp = e-s)+1;
        *e = '\0';
        *p = CONS(copy_simple_string(&string), Cnil);
      }
    }
  }
  Tcl_ResetResult(interp); 
  return result;
}

#ifdef NEW
static cl_object
TkResult2Lisp(Tcl_Interp *interp)
{
  register char *s = interp->result;
  register cl_object result = Cnil, *p;
  extern cl_object Tk_root_window;

  if (strcmp(s, ".") == 0) return Tk_root_window;
  if (*s) {
    int i;
    char *e;
    struct stream strm;
    struct string string;
    string.t = (short)t_string;
    string.m = FALSE;
    string.st_hasfillp = FALSE;
    string.st_adjustable = FALSE;
    string.st_displaced = Cnil;
    string.st_self = s;

    e = strchr(s, ' ');
    if (e == NULL) {
      string.st_dim = (string.st_fillp = strlen(s))+1;
      result = parse_from_string(&string, &e);
    } else {
      /*  Result was a list of values, build a proper list */
      string.st_dim = (string.st_fillp = e-s)+1;
      result = CONS(parse_from_string(&string, &e), Cnil);
      for (p = &CDR(result) ; ; p = &(CDR(*p))) {
	s = e+1;
	string.st_self = s;
	e = strchr(s, ' ');
	if (e == NULL) {
	  string.st_dim = (string.st_fillp = strlen(s))+1;
	  *p = CONS(parse_from_string(&string, &e), Cnil);
	  break;
	}
	string.st_dim = (string.st_fillp = e-s)+1;
	*p = CONS(parse_from_string(&string, &e), Cnil);
      }
    }
  }
  Tcl_ResetResult(interp); 
  return result;
}
#endif

tclMethodDispatch(int narg, cl_object env, ...)
{
  va_list args;
  cl_object W = CAR(env);
  char *argv[narg];
  int i;
  Tcl_CmdProc *proc = (Tcl_CmdProc *)fix(SLOT(W, 0));
  ClientData clientData = (ClientData)fix(SLOT(W, 1));
  argv[0] = SLOT(W, 2)->symbol.name->string.self; /* command name */
  va_start(args, env);
  for (i = 1; i < narg; i++)
    argv[i] = object2string(va_arg(args, cl_object));
  /* if previous result was a symbol, proc could not write to interp->result
   * so we must clear it
   */
  Tcl_ResetResult(ECL_interp);
  if ((*proc)(clientData, ECL_interp, narg, argv) == TCL_ERROR)
    VALUES(0) = (cl_object)FEerror(ECL_interp->result, 0);
  else
    VALUES(0) = TkResult2Lisp(ECL_interp);
  return(1);
}

void
Tcl_CreateCommand(Tcl_Interp *interp, char *cmdName, Tcl_CmdProc *proc,
		  ClientData clientData,
		  Tcl_CmdDeleteProc *deleteProc)
{
  cl_object SYM, sym, W;
  char CMDNAME[strlen(cmdName)+1]; /* __GNUC__ */

  sym = _intern(cmdName, tk_package);
  /* Define a variable whose name is the command name */
  upcase(cmdName, CMDNAME);
  SYM = _intern(CMDNAME, tk_package);
  SYM_VAL(SYM) = sym;		/* evaluating to lower case symbol */
  @si::make-structure(4, TkWidgetType, MAKE_FIXNUM(proc),
		    MAKE_FIXNUM(clientData), sym);
  W = VALUES(0);

  /* Define a function whose name is the command name */
  SYM_FUN(sym) = (cl_object)make_cclosure(tclMethodDispatch, CONS(W, Cnil), NULL);
  SYM_FUN(SYM) = SYM_FUN(sym);
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_GetCommandInfo --
 *
 *	Returns various information about a Tcl command.
 *
 * Results:
 *	If cmdName exists in interp, then *infoPtr is modified to
 *	hold information about cmdName and 1 is returned.  If the
 *	command doesn't exist then 0 is returned and *infoPtr isn't
 *	modified.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_GetCommandInfo(Tcl_Interp *interp, /* Interpreter in which to look
					* for command. */
		   char *cmdName, /* Name of desired command. */
		   Tcl_CmdInfo *infoPtr) /* Where to store information about
					  * command. */
{
    cl_object v = _intern(cmdName, tk_package);
    
    if (!structure_subtypep(TYPE_OF(SYM_VAL(v)), TkWidgetType)) return 0;

    infoPtr->proc       = (Tcl_CmdProc *)fix(SLOT(SYM_VAL(v), 0));
    infoPtr->clientData = (ClientData)fix(SLOT(SYM_VAL(v), 1));
    infoPtr->deleteProc = NULL;
    infoPtr->deleteData = NULL;
    return 1;
}


/*****************************************************************************
 *
 *			  Tcl interpreter management
 *
 *****************************************************************************/

Tcl_Interp *
Tcl_CreateInterp()
{
  register Interp *iPtr = (Interp *) ckalloc(sizeof(Interp));
  
  iPtr->result		 = iPtr->resultSpace;
  iPtr->freeProc	 = 0;
  iPtr->errorLine	 = 0;
  iPtr->resultSpace[0]   = 0;
  
  iPtr->appendResult	 = NULL;
  iPtr->appendAvl	 = 0;
  iPtr->appendUsed	 = 0;

  strcpy(iPtr->pdFormat, "%g");

  return (Tcl_Interp *) iPtr;
}

void
Tcl_DeleteInterp(Tcl_Interp *interp)
{
  Interp *iPtr = (Interp *) interp;
  
  if (iPtr->appendResult != NULL) {
    ckfree(iPtr->appendResult);
  }
  ckfree((char *) iPtr);
}

init_tk()
{
#ifdef CLOS
  TkWidgetType = define a class with name: _intern("WIDGET", tk_package);
#else
  TkWidgetType = _intern("WIDGET", tk_package);
#endif
  string_stream = make_string_output_stream(64);
  register_root(&string_stream);
  Tcl_InitHashTable(&VarTable, TCL_STRING_KEYS);
}

/*
 * Dummies
 */
int
Tcl_Init(Tcl_Interp *interp)
{}

void
Tcl_CallWhenDeleted(
    Tcl_Interp *interp,		/* Interpreter to watch. */
    Tcl_InterpDeleteProc *proc,	/* Procedure to call when interpreter
				 * is about to be deleted. */
    ClientData clientData)	/* One-word value to pass to proc. */
{}

void
Tcl_DontCallWhenDeleted(interp, proc, clientData)
    Tcl_Interp *interp;		/* Interpreter to watch. */
    Tcl_InterpDeleteProc *proc;	/* Procedure to call when interpreter
				 * is about to be deleted. */
    ClientData clientData;	/* One-word value to pass to proc. */
{}

int
Tcl_SetCommandInfo(interp, cmdName, infoPtr)
    Tcl_Interp *interp;			/* Interpreter in which to look
					 * for command. */
    char *cmdName;			/* Name of desired command. */
    Tcl_CmdInfo *infoPtr;		/* Where to store information about
					 * command. */
{}

Tcl_Trace
Tcl_CreateTrace(interp, level, proc, clientData)
    Tcl_Interp *interp;		/* Interpreter in which to create the trace. */
    int level;			/* Only call proc for commands at nesting level
				 * <= level (1 => top level). */
    Tcl_CmdTraceProc *proc;	/* Procedure to call before executing each
				 * command. */
    ClientData clientData;	/* Arbitrary one-word value to pass to proc. */
{}

void
Tcl_DeleteTrace(interp, trace)
    Tcl_Interp *interp;		/* Interpreter that contains trace. */
    Tcl_Trace trace;		/* Token for trace (returned previously by
				 * Tcl_CreateTrace). */
{}

void
Tcl_AddErrorInfo(interp, message)
    Tcl_Interp *interp;		/* Interpreter to which error information
				 * pertains. */
    char *message;		/* Message to record. */
{}

int
Tcl_SetRecursionLimit(interp, depth)
    Tcl_Interp *interp;			/* Interpreter whose nesting limit
					 * is to be set. */
    int depth;				/* New value for maximimum depth. */
{}

/*----------------------------------------------------------------------
 * from tclVar.c
 *----------------------------------------------------------------------
 */

int
Tcl_TraceVar(interp, varName, flags, proc, clientData)
    Tcl_Interp *interp;		/* Interpreter in which variable is
				 * to be traced. */
    char *varName;		/* Name of variable;  may end with "(index)"
				 * to signify an array reference. */
    int flags;			/* OR-ed collection of bits, including any
				 * of TCL_TRACE_READS, TCL_TRACE_WRITES,
				 * TCL_TRACE_UNSETS, and TCL_GLOBAL_ONLY. */
    Tcl_VarTraceProc *proc;	/* Procedure to call when specified ops are
				 * invoked upon varName. */
    ClientData clientData;	/* Arbitrary argument to pass to proc. */
{
  Tcl_HashEntry *entry;
  int new;
  struct VarTrace *data;
  
  entry = Tcl_CreateHashEntry(&VarTable, varName, &new);
  
  /* Create the value associated to the "var" key */
  data= (struct VarTrace *) ckalloc((unsigned) sizeof (struct VarTrace));
  data->flags	   = flags & ~TCL_TRACE_UNSETS; /* Unset has no meaning in ECL */
  data->traceProc  = proc;
  data->clientData = clientData;
  data->nextPtr	   = (VarTrace *) (new ? NULL : Tcl_GetHashValue(entry));

  /* Put it in table */
  Tcl_SetHashValue(entry, (ClientData) data);
  
  return TCL_OK;
}


int
Tcl_TraceVar2(interp, part1, part2, flags, proc, clientData)
    Tcl_Interp *interp;		/* Interpreter in which variable is
				 * to be traced. */
    char *part1;		/* Name of scalar variable or array. */
    char *part2;		/* Name of element within array;  NULL means
				 * trace applies to scalar variable or array
				 * as-a-whole. */
    int flags;			/* OR-ed collection of bits, including any
				 * of TCL_TRACE_READS, TCL_TRACE_WRITES,
				 * TCL_TRACE_UNSETS, and TCL_GLOBAL_ONLY. */
    Tcl_VarTraceProc *proc;	/* Procedure to call when specified ops are
				 * invoked upon varName. */
    ClientData clientData;	/* Arbitrary argument to pass to proc. */
{
  if (*part2) {
    
  }
  return Tcl_TraceVar(interp, part1, flags, proc, clientData);
}

void
Tcl_UntraceVar(interp, varName, flags, proc, clientData)
    Tcl_Interp *interp;		/* Interpreter containing traced variable. */
    char *varName;		/* Name of variable;  may end with "(index)"
				 * to signify an array reference. */
    int flags;			/* OR-ed collection of bits describing
				 * current trace, including any of
				 * TCL_TRACE_READS, TCL_TRACE_WRITES,
				 * TCL_TRACE_UNSETS, and TCL_GLOBAL_ONLY. */
    Tcl_VarTraceProc *proc;	/* Procedure assocated with trace. */
    ClientData clientData;	/* Arbitrary argument to pass to proc. */
{
  Tcl_HashEntry *entry;
  register VarTrace *p, *prev;
  
  if (entry = Tcl_FindHashEntry(&VarTable, varName)) {
    /* Variable is traced. Try to find correponding trace function */
    flags &= ~TCL_TRACE_UNSETS; /* Unset has no meaning for us */

    p = (struct VarTrace *) Tcl_GetHashValue(entry);    
    for (prev=NULL; p ; prev=p, p=p->nextPtr) {
      if (p->traceProc == proc && p->flags == flags && p->clientData == clientData)
	break;
    }
    if (p) {
      if (prev == NULL) {
	if (p->nextPtr)
	  Tcl_SetHashValue(entry, (ClientData *) p->nextPtr);
	else 
	  Tcl_DeleteHashEntry(entry);
      }
      else
	prev->nextPtr = p->nextPtr;
      ckfree(p);
    }
  }
}

void
Tcl_UntraceVar2(interp, part1, part2, flags, proc, clientData)
    Tcl_Interp *interp;		/* Interpreter containing traced variable. */
    char *part1;		/* Name of variable or array. */
    char *part2;		/* Name of element within array;  NULL means
				 * trace applies to scalar variable or array
				 * as-a-whole. */
    int flags;			/* OR-ed collection of bits describing
				 * current trace, including any of
				 * TCL_TRACE_READS, TCL_TRACE_WRITES,
				 * TCL_TRACE_UNSETS, and TCL_GLOBAL_ONLY. */
    Tcl_VarTraceProc *proc;	/* Procedure assocated with trace. */
    ClientData clientData;	/* Arbitrary argument to pass to proc. */
{
  if (part2 && *part2) {
    char *s = malloc(strlen(part1) + strlen(part2) + 3);

    sprintf(s, "%s{%s}", part1, part2);
    Tcl_UntraceVar(interp, s, flags, proc, clientData);
    free(s);
  }
  else
    Tcl_UntraceVar(interp, part1, flags, proc, clientData);
}

/****
 * 
 * Tcl_ChangeValue
 *
 * This function is called by Lisp when a there's a global variable change
 * (using a tk-setq). "var" is a C string indicating the name of this
 * variable. If this variable is traced, call the C functions associated to it.
 *
 ****/

#define TRACING (1<<20)

void Tcl_ChangeValue(char *var)
{ 
  Tcl_HashEntry *entry;
  register VarTrace *data, *p;
  extern int Tk_initialized;

  if (!Tk_initialized) return;

  if (entry = Tcl_FindHashEntry(&VarTable, var)) {
    /* Variable is traced. Call all the associated traces */
    data = (struct VarTrace *) Tcl_GetHashValue(entry);
    
    for (p = data; p ; p = p->nextPtr) {
      /* Invoke trace procedure if not already active */
      if (p->flags & TRACING) 
	continue;

      p->flags |= TRACING;
      (*p->traceProc)(p->clientData, ECL_interp, var, "", p->flags);
    
      /* Unset our flag */
      p->flags &= ~TRACING;
    }
  }
}
