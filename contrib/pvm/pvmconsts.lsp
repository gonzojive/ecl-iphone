;;;-*- Mode: LISP; Syntax: Common LISP; Base: 10 -*-
;;;
;;; File = pvmconsts.lisp
;;;
;;; PVM constant definitions.
;;;

;;
;; Change log.
;; 25 March 1994. LISP_X_TYPE constants have contiguous values.
;;  This is to support the new representation for the read structure.
;;

;;;
;;; Constant definitions for type tags used to define
;;; message boundaries.
;;; The tags are all ad hoc and tailored to the needs of LISP.
;;; Each is represented by an integer.
;;;
;;;

(defconstant MESSAGE_START        1)
    ;; This says that there is going to be      
    ;; a new structure type that follows.
(defconstant LISP_NIL_TYPE        2) ; encode nil
(defconstant LISP_T_TYPE          3) ; encode t
(defconstant LISP_CHAR_TYPE       4)
(defconstant LISP_SHORTINT_TYPE   5)
(defconstant LISP_INT_TYPE        6)
(defconstant LISP_LONGINT_TYPE    7)
;(defconstant LISP_FLOAT_TYPE      8) not used in ECo or KCL
(defconstant LISP_DOUBLE_TYPE     9)
(defconstant LISP_SYMBOL_TYPE    10)
(defconstant LISP_STRING_TYPE    11)
(defconstant LISP_VECTOR_TYPE    12)
(defconstant LISP_LIST_TYPE      13)
 ;; If complex and rational are required, we can fit them in.
(defconstant LISP_OPAQUE_TYPE    14)
(defconstant LISP_MIN_USER_TYPE  15)

 
;;; 
;;; PVM constant definitions for error messages, together
;;; with the error function for PVM routines.
;;;

(defconstant %PvmOk           0)
(defconstant %PvmBadParam    -2)
(defconstant %PvmMismatch    -3)
(defconstant %PvmNoData      -5)
(defconstant %PvmNoHost      -6)
(defconstant %PvmNoFile      -7)
(defconstant %PvmNoMem      -10)
(defconstant %PvmBadMsg     -12)
(defconstant %PvmSysErr     -14)
(defconstant %PvmNoBuf      -15)
(defconstant %PvmNoSuchBuf  -16)
(defconstant %PvmNullGroup  -17)
(defconstant %PvmDupGroup   -18)
(defconstant %PvmNoGroup    -19)
(defconstant %PvmNotInGroup -20)
(defconstant %PvmNoInst     -21)
(defconstant %PvmHostFail   -22)
(defconstant %PvmNoParent   -23)
(defconstant %PvmNotImpl    -24)
(defconstant %PvmDSysErr    -25)
(defconstant %PvmBadVersion -26)
(defconstant %PvmOutOfRes   -27)
(defconstant %PvmDupHost    -28)
(defconstant %PvmCantStart  -29)
(defconstant %PvmAlready    -30)
(defconstant %PvmNoTask     -31)
(defconstant %PvmNoEntry    -32)
(defconstant %PvmDupEntry   -33)


(defun pvm-error (errno where)
  ;; quick hack for testing
  (unless (= errno %PvmOk)
	  (error "PVM error in ~s no. ~d~%" where errno)))


;;;
;;; Constants for pvm_advise
;;;

(defconstant %PvmDontRoute   1)
(defconstant %PvmAllowDirect 2)
(defconstant %PvmRouteDirect 3)

;;;
;;; Constants for pvm_initsend's encoding
;;;

(defconstant %PvmDataDefault 0) ; use XDR if heterogeneous
(defconstant %PvmDataRaw     1) ; no encoding
(defconstant %PvmDataInPlace 2) ; leave data in place.

;;;
;;; Constants for pvm_spawn.
;;; See the PVM manual p. 13 for details.
;;;

(defconstant %PvmTaskDefault 0)
(defconstant %PvmTaskHost    1)
(defconstant %PvmTaskArch    2)
(defconstant %PvmTaskDebug   4)
(defconstant %PvmTaskTrace   8)

