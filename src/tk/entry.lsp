;;;;
;;;; Entries bindings and procs
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
;;;; Last file update:  2-Jun-1994 12:42
;;;;
;;;;
;;;; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;;;;

(in-package "TK")

;; ----------------------------------------------------------------------
;; Class bindings for entry widgets.
;; ----------------------------------------------------------------------

;; Button 1 bindings

(def-bindings "Entry"  '(
  ;; Button 1 bindings
  ("<1>"		. (progn
			    (%W "icursor" '@%x)
			    (%W "select" "from" '@%x)
			    (when(equal (tk-get %W "-state") "normal") 
				 (focus %W))))
  ("<B1-Motion>"	. (%W "select" "to" '@%x))

  ;; Button 2 bindings
  ("<2>"		. (catch-errors 
			   (%W "insert" "insert" (selection "get"))
			   (tk-entrySeeCaret %W)))
  ("<Shift-2>"		. (%W "scan" "mark" %x))
  ("<Shift-B2-Motion>"	. (%W "scan" "dragto" %x))

  ;; Button 3 bindings
  ("<3>"		. (%W "select" "adjust" '@%x))
  
  ;; Special keys bindings
  ("<Control-a>"	. (progn 
			    (%W "icursor" 0) 
			    (tk-entrySeeCaret %W)))
  ("<Control-b>"	. (tk-backwardChar %W))
  ("<Control-c>"	. (%W "delete" 0 "end"))
  ("<Control-d>"	. (progn 
			    (%W "delete" "insert")
			    (tk-entrySeeCaret %W)))
  ("<Control-e>"	. (progn 
			    (%W "icursor" "end")
			    (tk-entrySeeCaret %W)))
  ("<Control-f>"	. (tk-forwardChar %W))
  ("<Control-g>"	. (%W "delete" 0 "end"))
  ("<Control-h>"	. (tk-entryBackspace %W))
  ("<Control-k>"	. (progn 
			    (%W "delete" "insert" "end")
			    (tk-entrySeeCaret %W)))
  ("<Control-w>"	. (catch-errors
			    (setq tk::kill-buffer
				  (%W "delete" 'sel.first 'sel.last))
			    (%W "delete" 'sel.first 'sel.last)
			    (tk-entrySeeCaret %W)))
  ("<Control-y>"	. (progn
			    (%W "insert" "insert" tk::kill-buffer)
			    (tk-entrySeeCaret %W)))
  ("<Delete>" 		. (tk-entryBackspace %W))
  ("<BackSpace>"	. (tk-entryBackspace %W))
  ("<Any-backslash>"	. (progn
			    (%W "insert" "insert" "\\")
			    (tk-entrySeeCaret %W)))
  ("<Any-quotedbl>"	. (progn
			    (%W "insert" "insert" "\"")
			    (tk-entrySeeCaret %W)))
  ("<Any-KeyPress>"	. (unless (equal "\\%A" "\\0")
			    (%W "insert" "insert" "%A")
			    (tk-entrySeeCaret %W)))
))
 *lib-bindings*)


;; Entries utility functions

(defun tk-entryIndex (w pos)
  (parse-integer (funcall w "index" pos)))

(defun tk-forwardChar (w)
  (funcall w "icursor" (+ (tk-entryIndex w "insert") 1))
  (tk-entrySeeCaret w))

(defun tk-backwardChar (w)
  (funcall w "icursor" (- (tk-entryIndex w "insert") 1))
  (tk-entrySeeCaret w))

(defun tk-entryBackspace (w)
  (let ((x  (- (tk-entryIndex w "insert") 1)))
    (if (>= x 0) (progn (funcall w "delete" x) (tk-entrySeeCaret w)))))

;; The procedure below is invoked after insertions.  If the caret is not
;; visible in the window then the procedure adjusts the entry's view to
;; bring the caret back into the window again.

(defun tk-entrySeeCaret (w)
  (let ((c     (tk-entryIndex w "insert"))
	(left  (tk-entryIndex w "@0"))
	(width (format NIL "@~A" (- (parse-integer (winfo "width" w)) 5))))

    (when (>= left c) 
      (funcall w "view" (- c (if (> c 0) 1 0 ))))
      
    (do ((right (tk-entryIndex w width) (tk-entryIndex w width)))
	((or (>= right c) (>= left c)))
      (setq left (+ left 1))
      (funcall w "view" left))))

;;;;;;;
;;tk-bindForTraversal Entry
;;;;;;;
