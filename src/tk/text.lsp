;;;;
;;;; Texts bindings and procs (bindings a` la emacs)
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
;;;; Last file update:  2-Jun-1994 12:41
;;;;
;;;; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;;;;

(in-package "TK")

;; Class bindings for text widgets. tk::selectMode holds one of
;; 'char, 'word, or 'line to indicate which selection mode is active.

(defvar tk::selectMode 'CHAR)

(def-bindings "Text" '(
  ("<1>"		. (progn
			    (setq tk::selectMode 'CHAR)
			    (%W "mark" "set" "insert" "@%x,%y")
			    (%W "mark" "set" "anchor" "@%x,%y")
			    (if (equal (tk-get %W "-state") "normal")
 				(focus %W))))
  ("<Double-1>"		. (progn
			    (setq tk::selectMode 'WORD)
			    (%W "mark" "set" "insert" "@%x,%y wordstart")
			    (tk-textSelectTo %W "insert")))
  ("<Triple-1>"		. (progn
			    (setq tk::selectMode "line")
			    (%W "mark" "set" "insert" "@%x,%y linestart")
			    (tk-textSelectTo %W "insert")))
  ("<B1-Motion>"	. (tk-textSelectTo %W "@%x,%y"))
  ("<Shift-1>" 		. (progn
;			    (tk-textResetAnchor %W "@%x,%y")
			    (tk-textSelectTo    %W "@%x,%y")))
  ("<Shift-B1-Motion>"	. (tk-textSelectTo %W "@%x,%y"))
  ("<2>"		. (catch-errors 
			    (%W "insert" "insert" (selection "get"))
			    (%W "yview" "-pickplace" "insert")))
  ("<Shift-2>"		. (%W "scan" "mark" %y))
  ("<Shift-B2-Motion>"	. (%W "scan" "dragto" %y))
  ("<Any-backslash>"	. (progn
			    (%W "insert" "insert" "\\")
			    (%W "yview" "-pickplace" "insert")))
  ("<Any-quotedbl>"	. (progn
			     (%W "insert" "insert" "\"")
			     (%W "yview" "-pickplace" "insert")))
  ("<Any-KeyPress>"	. (unless (equal "\\%A" "\\0")
			    (%W "insert" "insert" "%A")
			    (%W "yview" "-pickplace" "insert")))
  ("<Return>"		. (progn
			    (%W "insert" "insert" #\newline)
			    (%W "yview" "-pickplace" "insert")))
  ("<BackSpace>"	. (progn
			    (tk-textBackspace %W)
			    (%W "yview" "-pickplace" "insert")))
  ("<Delete>"		. (progn
			    (tk-textBackspace %W)
			    (%W "yview" "-pickplace" "insert")))
  ("<Control-a>"	. (%W "mark" "set" "insert" 
				(%W "index" "insert linestart")))
  ("<Control-b>"	. (tk-backward-char %W))
  ("<Control-d>"	. (%W "delete" "insert" "insert +1c"))
  ("<Control-e>"	. (%W "mark" "set" "insert" (%W "index" "insert lineend")))
  ("<Control-f>"	. (tk-forward-char %W))
  ("<Control-k>"	. (%W "delete" "insert" "insert lineend"))
  ("<Control-n>"	. (tk-next-line %W))
  ("<Control-o>"	. (progn 
			    (%W "insert" "insert" #\newline)
			    (tk-backward-char %W)))
  ("<Control-p>"	. (tk-previous-line %W))
  ("<Control-w>"	. (catch-errors 
			     (setq tk::kill-buffer 
				   (%W "get" "sel.first" "sel.last"))
			     (%W "delete" "sel.first" "sel.last")))
  ("<Control-y>"	. (%W "insert" "insert" tk::kill-buffer))
  ("<Up>"		. (tk-previous-line %W))
  ("<Down>"		. (tk-next-line %W))
  ("<Left>"		. (tk-backward-char %W))
  ("<Right>"		. (tk-forward-char %W))
))

;;tk-bindForTraversal Text
;;

(defun tk-previous-line (w)
  (funcall w "mark" "set" "insert" (funcall w "index" "insert -1line"))
  (funcall w "yview" "-pickplace" "insert"))

(defun tk-next-line (w)
  (funcall w "mark" "set" "insert" (funcall w "index" "insert +1line"))
  (funcall w "yview" "-pickplace" "insert"))

(defun tk-forward-char (w)
  (funcall w "mark" "set" "insert" (funcall w "index" "insert +1c"))
  (funcall w "yview" "-pickplace" "insert"))

(defun tk-backward-char (w)
  (funcall w "mark" "set" "insert" (funcall w "index" "insert -1c"))
  (funcall w "yview" "-pickplace" "insert"))
  
;; The procedure below is invoked when dragging one end of the selection.
;; The arguments are the text window name and the index of the character
;; that is to be the new end of the selection.

(defun tk-textSelectTo (w index)
  (let ((interval 
	 (case tk::selectMode
	   (CHAR (if (equal (funcall w "compare" index "<" "anchor") "1")
		     (cons index "anchor")
		     (cons "anchor" (funcall w "index"
					     (format NIL "~A+1c" index)))))
	   (WORD (cons (funcall w "index" (format NIL "~A wordstart" index))
		       (funcall w "index" "anchor wordend")))
	   (LINE (cons (funcall w "index" (format NIL "~A linestart" index))
		       (funcall w "index" "anchor lineend"))))))

    (funcall w "tag" "remove" "sel" "0.0" (car interval))
    (funcall w "tag" "add"    "sel" (car interval) (cdr interval))
    (funcall w "tag" "remove" "sel" (cdr interval) "end")))


;; The procedure below is invoked to backspace over one character in
;; a text widget.  The name of the widget is passed as argument.

(defun tk-textBackspace (w)
  (funcall w "delete" "insert-1c" "insert"))
