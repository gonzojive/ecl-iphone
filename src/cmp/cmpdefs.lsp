;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPDEF	Definitions

(si::package-lock "CL" nil)

(defpackage "C"
  (:nicknames "COMPILER")
  (:use "FFI" "CL" #+threads "MP")
  (:export "*COMPILER-BREAK-ENABLE*"
	   "*COMPILE-PRINT*"
	   "*COMPILE-TO-LINKING-CALL*"
	   "*COMPILE-VERBOSE*"
	   "*CC*"
	   "*CC-OPTIMIZE*"
	   "BUILD-ECL"
	   "BUILD-PROGRAM"
	   "BUILD-STATIC-LIBRARY"
	   "BUILD-SHARED-LIBRARY"
	   "*SUPPRESS-COMPILER-WARNINGS*"
	   "*SUPPRESS-COMPILER-NOTES*")
  (:import-from "SI" "GET-SYSPROP" "PUT-SYSPROP" "REM-SYSPROP" "MACRO"
		"*COMPILER-CONSTANTS*"))

(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILER STRUCTURES
;;;

;;;
;;; REF OBJECT
;;;
;;; Base object for functions, variables and statements. We use it to
;;; keep track of references to objects, how many times the object is
;;; referenced, by whom, and whether the references cross some closure
;;; boundaries.
;;;

(defstruct (ref (:print-object print-ref))
  name			;;; Identifier of reference.
  (ref 0 :type fixnum)	;;; Number of references.
  ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the index into the closure env
  ref-clb		;;; Cross local function reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the lex-address for the
			;;; block id, or NIL.
  read-nodes		;;; Nodes (c1forms) in which the reference occurs
)

(deftype OBJECT () `(not (or fixnum character short-float long-float)))

(defstruct (var (:include ref) (:constructor %make-var))
;  name		;;; Variable name.
;  (ref 0 :type fixnum)
		;;; Number of references to the variable (-1 means IGNORE).
;  ref-ccb	;;; Cross closure reference: T or NIL.
;  ref-clb	;;; Cross local function reference: T or NIL.
;  read-nodes	;;; Nodes (c1forms) in which the reference occurs
  set-nodes	;;; Nodes in which the variable is modified
  kind		;;; One of LEXICAL, CLOSURE, SPECIAL, GLOBAL, :OBJECT, :FIXNUM,
  		;;; :CHAR, :DOUBLE, :FLOAT, or REPLACED (used for
		;;; LET variables).
  (function *current-function*)
		;;; For local variables, in which function it was created.
		;;; For global variables, it doesn't have a meaning.
  (functions-setting nil)
  (functions-reading nil)
		;;; Functions in which the variable has been modified or read.
  (loc 'OBJECT)	;;; During Pass 1: indicates whether the variable can
		;;; be allocated on the c-stack: OBJECT means
		;;; the variable is declared as OBJECT, and CLB means
		;;; the variable is referenced across Level Boundary and thus
		;;; cannot be allocated on the C stack.  Note that OBJECT is
		;;; set during variable binding and CLB is set when the
		;;; variable is used later, and therefore CLB may supersede
		;;; OBJECT.
		;;; During Pass 2:
  		;;; For REPLACED: the actual location of the variable.
  		;;; For :FIXNUM, :CHAR, :FLOAT, :DOUBLE, :OBJECT:
  		;;;   the cvar for the C variable that holds the value.
  		;;; For LEXICAL or CLOSURE: the frame-relative address for
		;;; the variable in the form of a cons '(lex-levl . lex-ndx)
		;;;	lex-levl is the level of lexical environment
		;;;	lex-ndx is the index within the array for this env.
		;;; For SPECIAL and GLOBAL: the vv-index for variable name.
  (type t)	;;; Type of the variable.
  (index -1)    ;;; position in *vars*. Used by similar.
  )

;;; A function may be compiled into a CFUN, CCLOSURE or CCLOSURE+LISP_CLOSURE
;;; Here are examples of function FOO for the 3 cases:
;;; 1.  (flet ((foo () (bar))) (foo))		CFUN
;;; 2.  (flet ((foo () (bar))) #'foo)		CFUN+LISP_CFUN
;;; 3.  (flet ((foo () x)) #'(lambda () (foo))) CCLOSURE
;;; 4.  (flet ((foo () x)) #'foo)		CCLOSURE+LISP_CLOSURE

;;; A function can be referred across a ccb without being a closure, e.g:
;;;   (flet ((foo () (bar))) #'(lambda () (foo)))
;;;   [the lambda also need not be a closure]
;;; and it can be a closure without being referred across ccb, e.g.:
;;;   (flet ((foo () x)) #'foo)  [ is this a mistake in local-function-ref?]
;;; Here instead the lambda must be a closure, but no closure is needed for foo
;;;   (flet ((foo () x)) #'(lambda () (foo)))
;;; So we use two separate fields: ref-ccb and closure.
;;; A CCLOSURE must be created for a function when:
;;; 1. it appears within a FUNCTION construct and
;;; 2. it uses some ccb references (directly or indirectly).
;;; ref-ccb corresponds to the first condition, i.e. function is referred
;;;   across CCB. It is computed during Pass 1. A value of 'RETURNED means
;;;   that it is immediately within FUNCTION.
;;; closure corresponds to second condition and is computed in Pass 2 by
;;;   looking at the info-referred-vars and info-local-referred of its body.

;;; A LISP_CFUN or LISP_CLOSURE must be created when the function is returned.
;;; The LISP funob may then be referred locally or across LB or CB:
;;;     (flet ((foo (z) (bar z))) (list #'foo)))
;;;     (flet ((foo (z) z)) (flet ((bar () #'foo)) (bar)))
;;;     (flet ((foo (z) (bar z))) #'(lambda () #'foo)))
;;; therefore we need field funob.

(defstruct (fun (:include ref))
;  name			;;; Function name.
;  (ref 0 :type fixnum)	;;; Number of references.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the vs-address for the
			;;; function closure, or NIL.
;  ref-ccb		;;; Cross closure reference.
 			;;; During Pass1, T or NIL, depending on whether a
			;;; function object will be built.
			;;; During Pass2, the vs-address for the function
			;;; closure, or NIL.
;  ref-clb		;;; Unused.
;  read-nodes		;;; Nodes (c1forms) in which the reference occurs
  cfun			;;; The cfun for the function.
  (level 0)		;;; Level of lexical nesting for a function.
  (env 0)     		;;; Size of env of closure.
  (global nil)		;;; Global lisp function.
  (exported nil)	;;; Its C name can be seen outside the module.
  closure		;;; During Pass2, T if env is used inside the function
  var			;;; the variable holding the funob
  description		;;; Text for the object, in case NAME == NIL.
  lambda		;;; Lambda c1-form for this function.
  (minarg 0)		;;; Min. number arguments that the function receives.
  (maxarg call-arguments-limit)
			;;; Max. number arguments that the function receives.
  (parent *current-function*)
			;;; Parent function, NIL if global.
  (local-vars nil)	;;; List of local variables created here.
  (referred-vars nil)	;;; List of external variables referenced here.
  (referred-funs nil)	;;; List of external functions called in this one.
			;;; We only register direct calls, not calls via object.
  (child-funs nil)	;;; List of local functions defined here.
  )

(defstruct (blk (:include ref))
;  name			;;; Block name.
;  (ref 0 :type fixnum)	;;; Number of references.
;  ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the ccb-lex for the
			;;; block id, or NIL.
;  ref-clb		;;; Cross local function reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the lex-address for the
			;;; block id, or NIL.
;  read-nodes		;;; Nodes (c1forms) in which the reference occurs
  exit			;;; Where to return.  A label.
  destination		;;; Where the value of the block to go.
  var			;;; Variable containing the block ID.
  (type 'NIL)		;;; Estimated type.
  )

(defstruct (tag (:include ref))
;  name			;;; Tag name.
;  (ref 0 :type fixnum)	;;; Number of references.
;  ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
;  ref-clb		;;; Cross local function reference.
			;;; During Pass1, T or NIL.
;  read-nodes		;;; Nodes (c1forms) in which the reference occurs
  label			;;; Where to jump: a label.
  unwind-exit		;;; Where to unwind-no-exit.
  var			;;; Variable containing frame ID.
  index			;;; An integer denoting the label.
  )

(defstruct (info)
  (local-vars nil)	;;; List of var-objects created directly in the form.
  (type t)		;;; Type of the form.
  (sp-change nil)	;;; Whether execution of the form may change
			;;; the value of a special variable.
  (volatile nil)	;;; whether there is a possible setjmp. Beppe
  )

;;;
;;; VARIABLES
;;;

;;; --cmpinline.lsp--
;;;
;;; Empty info struct
;;;
(defvar *info* (make-info))

(defvar *inline-functions* nil)
(defvar *inline-blocks* 0)
;;; *inline-functions* holds:
;;;	(...( function-name . inline-info )...)
;;;
;;; *inline-blocks* holds the number of C blocks opened for declaring
;;; temporaries for intermediate results of the evaluation of inlined
;;; function calls.

;;; --cmputil.lsp--
;;;
;;; Variables and constants for error handling
;;;
(defvar *current-form* '|compiler preprocess|)
(defvar *first-error* t)
(defvar *error-count* 0)
(defvar *error-p* nil)
(defconstant *cmperr-tag* (cons nil nil))

(defvar *compile-print* t
  "This variable controls whether the compiler displays messages about
each form it processes. The default value is NIL.")

(defvar *compile-verbose* t
  "This variable controls whether the compiler should display messages about its
progress. The default value is T.")

(defvar *suppress-compiler-warnings* nil
  "This variable controls whether the compiler should issue warnings.
The default value is NIL.")

(defvar *suppress-compiler-notes* nil
  "This variable controls whether the compiler displays compilation notices.
The default value is NIL.")

(defvar *compiler-break-enable* nil)

(defvar *compiler-in-use* nil)
(defvar *compiler-input*)
(defvar *compiler-output1*)
(defvar *compiler-output2*)

;;; --cmpblock.lsp--
;;;
;;; List of defined blocks, including marks for boundaries of closures
;;;
(defvar *blocks* nil)

;;; --cmpcall.lsp--
;;;
;;; Whether to use linking calls.
;;;
(defvar *compile-to-linking-call* t)
(defvar *compiler-declared-globals*)

;;; --cmpenv.lsp--
;;;
;;; These default settings are equivalent to (optimize (speed 3) (space 0) (safety 2))
;;;
(defvar *safety* 2)
(defvar *speed* 3)
(defvar *space* 0)

;;;
;;; Compiled code uses the following kinds of variables:
;;; 1. Vi, declared explicitely, either unboxed or not (*lcl*, next-lcl)
;;; 2. Ti, declared collectively, of type object, may be reused (*temp*, next-temp)
;;; 4. lexi[j], for lexical variables in local functions
;;; 5. CLVi, for lexical variables in closures

(defvar *lcl* 0)		; number of local variables

(defvar *temp* 0)		; number of temporary variables
(defvar *max-temp* 0)		; maximum *temp* reached

(defvar *level* 0)		; nesting level for local functions

(defvar *lex* 0)		; number of lexical variables in local functions
(defvar *max-lex* 0)		; maximum *lex* reached

(defvar *env* 0)		; number of variables in current form
(defvar *max-env* 0)		; maximum *env* in whole function
(defvar *env-lvl* 0)		; number of levels of environments

(defvar *next-cmacro* 0)	; holds the last cmacro number used.
(defvar *next-vv* -1)		; holds the last VV index used.
(defvar *next-cfun* 0)		; holds the last cfun used.

;;;
;;; *tail-recursion-info* holds NIL, if tail recursion is impossible.
;;; If possible, *tail-recursion-info* holds
;;	( c1-lambda-form  required-arg .... required-arg ),
;;; where each required-arg is a var-object.
;;;
(defvar *tail-recursion-info* nil)

;;;
;;; *function-declarations* holds :
;;	(... ( { function-name | fun-object } arg-types return-type ) ...)
;;; Function declarations for global functions are ASSOCed by function names,
;;; whereas those for local functions are ASSOCed by function objects.
;;;
;;; The valid argment type declaration is:
;;	( {type}* [ &optional {type}* ] [ &rest type ] [ &key {type}* ] )
;;; though &optional, &rest, and &key return types are simply ignored.
;;;
(defvar *function-declarations* nil)
(defvar *allow-c-local-declaration* t)
(defvar *alien-declarations* nil)
(defvar *notinline* nil)

;;; --cmpexit.lsp--
;;;
;;; *last-label* holds the label# of the last used label.
;;; *exit* holds an 'exit', which is
;;	( label# . ref-flag ) or one of RETURNs (i.e. RETURN, RETURN-FIXNUM,
;;	RETURN-CHARACTER, RETURN-LONG-FLOAT, RETURN-SHORT-FLOAT, or
;;	RETURN-OBJECT).
;;; *unwind-exit* holds a list consisting of:
;;	( label# . ref-flag ), one of RETURNs, TAIL-RECURSION-MARK, FRAME,
;;	JUMP, BDS-BIND (each pushed for a single special binding), or a
;;	LCL (which holds the bind stack pointer used to unbind).
;;;
(defvar *last-label* 0)
(defvar *exit*)
(defvar *unwind-exit*)

;;; --cmpflet.lsp--
;;;
;;; During Pass 1, *funs* holds a list of fun objects, local macro definitions
;;; and the symbol 'CB' (Closure Boundary) or 'LB' (Level Boundary).
;;; 'CB' will be pushed on *funs* when the compiler begins to process a closure.
;;; 'LB' will be pushed on *funs* when the compiler begins to process a local
;;; function.
;;; A local macro definition is a list ( macro-name expansion-function).

(defvar *funs* nil)
(defvar *current-function* nil)

;;; --cmplog.lsp--
;;;
;;; Destination of output of different forms. See cmploc.lsp for types
;;; of destinations.
;;;
(defvar *destination*)

;;; --cmpmain.lsp--
;;;
;;; Do we debug the compiler? Then we need files not to be deleted.

(defvar *debug-compiler* nil)
(defvar *files-to-be-deleted* '())

;;; --cmptag.lsp--
;;;
;;; List of tags with marks for closure boundaries.
;;;
(defvar *tags* nil)

;;; --cmptop.lsp--
;;;
(defvar *volatile*)
(defvar *setjmps* 0)

(defvar *compile-time-too* nil)
(defvar *not-compile-time* nil)

(defvar *objects* nil)			; holds { ( object text vv-index ) }*
(defvar *keywords* nil)			; holds { ( key-list text vv-index ) }*
(defvar *load-time-values* nil)		; holds { ( vv-index form ) }*,
;;;  where each vv-index should be given an object before
;;;  defining the current function during loading process.

(defvar *compiler-constants* nil)	; a vector with all constants
					; only used in COMPILE

(defvar *proclaim-fixed-args* nil)	; proclaim automatically functions
					; with fixed number of arguments.
					; watch out for multiple values.

(defvar *global-var-objects* nil)	; var objects for global/special vars
(defvar *global-vars* nil)		; variables declared special
(defvar *global-funs* nil)		; holds	{ fun }*
(defvar *linking-calls* nil)		; holds { ( global-fun-name fun symbol c-fun-name var-name ) }*
(defvar *local-funs* nil)		; holds { fun }*
(defvar *top-level-forms* nil)		; holds { top-level-form }*
;;;
;;;     top-level-form:
;;;	  ( 'DEFUN'     fun-name cfun lambda-expr doc-vv sp )
;;;	| ( 'DEFMACRO'  macro-name cfun lambda-expr doc-vv sp )
;;;	| ( 'ORDINARY'  expr )
;;;	| ( 'DECLARE'   var-name-vv )
;;;	| ( 'DEFVAR'	var-name-vv expr doc-vv )
;;;	| ( 'CLINES'	string* )
;;;	| ( 'DEFCFUN'	header vs-size body )
;;;	| ( 'LOAD-TIME-VALUE' vv )
;;;	| ( 'DEFCBODY'	fun-name cfun arg-types type body ) ;;; Beppe
;;; Eliminated:
;;;	| ( 'DEFENTRY'	fun-name cfun cvspecs type cfun-name )
;;;	| ( 'DEFUNC'	fun-name cfun lambda-list string* ) ;;; Beppe

(defvar *reservations* nil)
(defvar *reservation-cmacro* nil)

;;; *reservations* holds (... ( cmacro . value ) ...).
;;; *reservation-cmacro* holds the cmacro current used as vs reservation.

(defvar *global-entries* nil)

;;; *self-destructing-fasl* = T means that, when a FASL module is
;;; being unloaded (for instance during garbage collection), the
;;; associated file will be deleted. We need this because on windows
;;; DLLs cannot be deleted if they have been opened with LoadLibrary.
;;; Thus, (COMPILE ...) cannot create a DLL, load it and delete it
;;; while it is being used.
(defvar *self-destructing-fasl* nil)

;;; *global-entries* holds (... ( fname cfun return-types arg-type ) ...).

;;; --cmpvar.lsp--
;;;
(defvar *vars* nil)
(defvar *undefined-vars* nil)
(defvar *special-binding* nil)
