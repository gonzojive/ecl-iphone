;;;;
;;;; Initialization file for ECL/Tk
;;;;
;;;; Copyright (C) 1993,1994,1995 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;;; 
;;;; Permission to use, copy, and/or distribute this software and its
;;;; documentation for any purpose and without fee is hereby granted, provided
;;;; that both the above copyright notice and this permission notice appear in
;;;; all copies and derived works.  Fees for distribution or use of this
;;;; software or derived works may only be charged with express written
;;;; permission of the copyright holder.  
;;;; This software is provided ``as is'' without express or implied warranty.
;;;;
;;;; This software is a derivative work of other copyrighted softwares; the
;;;; copyright notices of these softwares are placed in the file COPYRIGHTS
;;;;
;;;;           Author: Erick Gallesio [eg@unice.fr]
;;;;    Creation date: 17-May-1993 12:35
;;;; Last file update:  9-Dec-1994 23:48
;;;;
;;;; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;;;;

(in-package "TK")

;; This is my personnal flavour. You'll probably wold like it
;(option "add" "ECL*foreground"  		"Grey20")
;(option "add" "ECL*background"  		"DarkSeaGreen3")
;(option "add" "ECL*Scrollbar*foreground" "DarkSeaGreen2")

;(option "add" "ECL*background"  		"#00feff")
;(option "add" "ECL*Scrollbar*foreground" "#00cdff")

(defstruct widget methods self name)

(defun string->widget (x) (symbol-function (intern x)))

;; Define some variables and utilities 

(defmacro tk-get (w option)
  `(nth 4 (funcall ,w "configure" ,option)))

(defmacro tk-setq (w option value)
  `(funcall ,w "configure" ,option ,value))

(defun do-bindings (class bindings)
  (dolist (l bindings) (bind class (car l) (cdr l))))

(defun & (&rest l)
  (do ((l l (cdr l))
       (res ""
	    (let ((e (car l)))
	      (concatenate 'string res
			     (cond ((stringp e) e)
				   ((symbolp e) (symbol-name e))
				   ((widget-p e) (symbol-name (widget-name e)))
				   ((numberp e) (number->string e)))))))
      ((null l) res)))

(defun get-focus ()
  (let ((f (focus)))
    (if (equal f "none") "none" (eval f))))


;; Turn off strict Motif look and feel as a default.
(defvar tk-strictMotif 		NIL)  


;; Following vars are used everywhere. So define them here 
(defvar window	 '())
(defvar relief	 '())
(defvar buttons	 0)
(defvar dragging	 '())
(defvar curr-focus	 '())
(defvar curr-grab	 "")
(defvar inMenuButton '())
(defvar posted	 NIL)
(defvar selectMode	 '())
(defvar window	 '())
(defvar activeBg	 '())
(defvar activeFg	 '())
(defvar x		 0)
(defvar y		 0)
(defvar buttonWindow '())
(defvar cursor	 "")
(defvar kill-buffer "") ;; One kill buffer shared between all texts. 

;;; Scheme compatibility
(defmacro catch-errors (&rest body)
  `(catch sys::*quit-tag*
    (let ((sys::*break-enable* nil)) ,@body)))


(defvar *lib-bindings*)
(defun def-bindings (class bindings)
  (push `(do-bindings ,class ',bindings) *lib-bindings*))

(let ((*default-pathname-defaults*
       (concatenate 'string sys:*system-directory* "tk/"))
      (*load-verbose* nil)
      (*lib-bindings* nil))
  (load "error")
  (load "button")
  (load "entry")
  (load "listbox")
  (load "menu")
  (load "scale")
  (load "scrollbar")
  (load "text")

  (setf (symbol-function 'tk-init)
	`(lambda ()
	  (unless (equal tk_version "3.6")
	    (error
	     "wrong version of Tk loaded: need 3.6 (actual version is ~A)" 
	     tk_version))
	  ,@*lib-bindings*
	  (fmakunbound 'tk-init)
;;;	   (ecl-menu)
	  ))
  )

#|
(autoload "dialog" 			make-dialog)
(autoload "help"   			make-help)
(autoload "inspect-main"		inspect view detail)
(autoload "editor"			make-editor ed)
|#
