;;;;
;;;; Listboxes bindings and procs
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
;;;;
;;;;           Author: Erick Gallesio [eg@unice.fr]
;;;;    Creation date: 17-May-1993 12:35
;;;; Last file update: 22-Nov-1993 16:04
;;;;
;;;; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;;;;

(in-package "TK")

;; ----------------------------------------------------------------------
;; Class bindings for listbox widgets.
;; ----------------------------------------------------------------------

(def-bindings "Listbox" '(
   ("<1>"		. (%W "select" "from" (%W "nearest" %y)))
   ("<B1-Motion>"	. (%W "select" "to" (%W "nearest" %y)))
   ("<2>"		. (%W "scan" "mark" %x %y))
   ("<Shift-B2-Motion>"	. (%W "scan" "dragto" %x %y))
   ("<3>"		. (%W "select" "adjust" (%W 'nearest %y)))
))

;; The procedure below may be invoked to change the behavior of
;; listboxes so that only a single item may be selected at once.
;; The arguments give one or more windows whose behavior should
;; be changed;  if one of the arguments is "Listbox" then the default
;; behavior is changed for all listboxes.

(defun tk-listbox-single-select (&rest args)
  (let ((new-binding '(%W "select" "from" (%W "nearest" %y))))
    (for-each (lambda (w)
		(bind w "<B1-Motion>" new-binding)
		(bind w "<3>" 	      new-binding))
	      args)))


