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

(defpackage "C"
  (:nicknames "COMPILER")
  (:use "FFI" "CL")
  (:export *compiler-break-enable*
	   *compile-print*
	   *compile-to-linking-call*
	   *compile-verbose*
	   *cc*
	   *cc-optimize*
	   build-ecl
	   build-static-library
	   build-shared-library
	   shared-library-pathname
	   static-library-pathname
	   *suppress-compiler-warnings*
	   *suppress-compiler-notes*))

(in-package "COMPILER")

;;; Use structures of type vector to avoid creating
;;; normal structures before booting CLOS.

(defstruct (ref)
  name			;;; Identifier of reference.
  (ref 0 :type fixnum)	;;; Number of references.
  ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the index into the closure env
)

(defstruct (var (:include ref))
;  name		;;; Variable name.
;  (ref 0 :type fixnum)
		;;; Number of references to the variable (-1 means IGNORE).
		;;; During Pass 2: set below *register-min* for non register.
;  ref-ccb	;;; Cross closure reference: T or NIL.
  kind		;;; One of LEXICAL, SPECIAL, GLOBAL, OBJECT, FIXNUM,
  		;;; CHARACTER, LONG-FLOAT, SHORT-FLOAT, or REPLACED (used for
		;;; LET variables).
		;;; A value DUMMY is used for missing supplied-p keyword
		;;; variables
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
  		;;; For FIXNUM, CHARACTER, LONG-FLOAT, SHORT-FLOAT, OBJECT:
  		;;;   the cvar for the C variable that holds the value.
  		;;; For LEXICAL: the frame-relative address for the variable.
		;;; For SPECIAL and GLOBAL: the vv-index for variable name.
  (type t)	;;; Type of the variable.
  (index -1)    ;;; position in *vars*. Used by similar.
  )
(deftype var () '(satisfies var-p))

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
;;;   (flet ((foo () x)) #'foo)  [ is this a mistake in c1local-closure?]
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
;  ref-ccb		;;; Cross closure reference.
 			;;; During Pass1, T or NIL.
  cfun			;;; The cfun for the function.
  (level 0)		;;; Level of lexical nesting for a function.
  (env 0)     		;;; Size of env of closure.
  closure		;;; During Pass2, T if env is used inside the function
  var			;;; the variable holding the funob
  )
(deftype fun () '(satisifes fun-p))

(defstruct (blk (:include ref))
;  name			;;; Block name.
;  (ref 0 :type fixnum)	;;; Number of references.
;  ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the ccb-lex for the
			;;; block id, or NIL.
  ref-clb		;;; Cross local function reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the lex-address for the
			;;; block id, or NIL.
  exit			;;; Where to return.  A label.
  destination		;;; Where the value of the block to go.
  var			;;; Variable containing the block ID.
  )
(deftype blk () '(satisfies blk-p))

(defstruct (tag (:include ref))
;  name			;;; Tag name.
;  (ref 0 :type fixnum)	;;; Number of references.
;  ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
  ref-clb		;;; Cross local function reference.
			;;; During Pass1, T or NIL.
  label			;;; Where to jump.  A label.
  unwind-exit		;;; Where to unwind-no-exit.
  var			;;; Variable containing frame ID.
  )
(deftype tag () '(satisfies tag-p))

(defstruct (info)
  (changed-vars nil)	;;; List of var-objects changed by the form.
  (referred-vars nil)	;;; List of var-objects referred in the form.
  (type t)		;;; Type of the form.
  (sp-change nil)	;;; Whether execution of the form may change
			;;; the value of a special variable.
  (volatile nil)	;;; whether there is a possible setjmp. Beppe
;  (referred-tags nil)   ;;; Tags or block names referenced in the body.
  (local-referred nil)  ;;; directly referenced in the body:
;;;	each reference operator (c1call-symbol, c1go, c1return-from, c1vref
;;;	and c1setq1) adds the reference to the info-local-referred of the form
;;;	they appear in.
;;;	This information is not propagated to an enclosing function (see
;;;	add-info) so that we can determine exactly which frame is used
;;;	in the body of a function.
  )
(deftype info () '(satisfies info-p))

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
(defvar *compiler-output-data*)

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
(defvar *safe-compile* nil)
(defvar *compiler-check-args* nil)
(defvar *compiler-push-events* nil)
(defvar *speed* 3)
(defvar *space* 0)

;;;
;;; Compiled code uses the following kinds of variables:
;;; 1. Vi, declared explicitely, either unboxed or register (*lcl*, next-lcl)
;;; 2. Ti, declared collectively, of type object, may be reused (*temp*, next-temp)
;;; 3. Ui, declared collectively, of type unboxed (*unboxed*, next-unboxed)
;;; 4. lexi[j], for lexical variables in local functions
;;; 5. CLVi, for lexical variables in closures

(defvar *lcl* 0)		; number of local variables

(defvar *temp* 0)		; number of temporary variables
(defvar *max-temp* 0)		; maximum *temp* reached

(defvar *unboxed*)		; list of unboxed variables
(defvar *next-unboxed* 0)	; number of *unboxed* used.

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
;;	( fname  required-arg .... required-arg ),
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

;;; --cmplog.lsp--
;;;
;;; Destination of output of different forms. See cmploc.lsp for types
;;; of destinations.
;;;
(defvar *destination*)

;;; --cmptag.lsp--
;;;
;;; List of tags with marks for closure boundaries.
;;;
(defvar *tags* nil)

;;; --cmptop.lsp--
;;;
(defvar *funarg-vars*)
;;; Number of address registers available not counting the
;;; frame pointer and the stack pointer
;;; To do: If the regs hold data then there are really more available;
(defvar *free-address-registers* 5)
(defvar *free-data-registers* 6)
(defvar *volatile*)
(defvar *setjmps* 0)

(defvar *compile-time-too* nil)
(defvar *not-compile-time* nil)

(defvar *non-package-operation* nil)

(defvar *objects* nil)			; holds { ( object vv-index ) }*
(defvar *constants* nil)		; holds { ( symbol vv-index ) }*
(defvar *load-time-values* nil)		; holds { ( vv-index form ) }*,
;;;  where each vv-index should be given an object before
;;;  defining the current function during loading process.

(defvar *proclaim-fixed-args* nil)	; proclaim automatically functions
					; with fixed number of arguments.
					; watch out for multiple values.

(defvar *global-vars* nil)
(defvar *global-funs* nil)		; holds	{ ( global-fun-name cfun ... ) }*
(defvar *linking-calls* nil)		; holds { ( global-fun-name vv ) }*
(defvar *local-funs* nil)		; holds { ( closurep fun funob ) }*
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
;;;	| ( 'FUNCTION-CONSTANT'	vv-index fun )              ;;; Beppe
;;; Eliminated:
;;;	| ( 'DEFENTRY'	fun-name cfun cvspecs type cfun-name )
;;;	| ( 'DEFUNC'	fun-name cfun lambda-list string* ) ;;; Beppe

(defvar *reservations* nil)
(defvar *reservation-cmacro* nil)

;;; *reservations* holds (... ( cmacro . value ) ...).
;;; *reservation-cmacro* holds the cmacro current used as vs reservation.

(defvar *global-entries* nil)

;;; *global-entries* holds (... ( fname cfun return-types arg-type ) ...).

;;; --cmpvar.lsp--
;;;
(defvar *vars* nil)
(defvar *undefined-vars* nil)
(defvar *special-binding* nil)

(defvar *register-min* 3)		; criteria for putting in register.
(proclaim '(fixnum *register-min*))
