;;;; e d i t o r . l s p		-- A small editor to create enhanced
;;;;					   text (used for Help page construction)
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
;;;;           Author: Erick Gallesio [eg@kaolin.unice.fr]
;;;;    Creation date:  6-Dec-1993 17:25
;;;; Last file update: 17-Oct-1994 18:41
;;;;
;;;; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;;;;

(in-package "TK")

(provide "editor")

;;;;
;;;; Font definition
;;;;

(defvar STF-signature "STF-0.1")

(defvar normal-font "*-Courier-Medium-R-Normal-*-120-*")

(defvar all-fonts `(
   (normal		,normal-font)
   (fixed		"fixed")
   (big			"-*-times-*-r-*-*-*-240-*-*-*-*-*-*")
   (roman-12		"-*-times-*-r-*-*-*-120-*-*-*-*-*-*")
   (roman-14		"-*-times-*-r-*-*-*-140-*-*-*-*-*-*")
   (roman-16		"-*-times-*-r-*-*-*-160-*-*-*-*-*-*")
   (roman-18		"-*-times-*-r-*-*-*-180-*-*-*-*-*-*")
   (italic-12		"-*-times-*-i-*-*-*-120-*-*-*-*-*-*")
   (italic-14		"-*-times-*-i-*-*-*-140-*-*-*-*-*-*")
   (italic-16		"-*-times-*-i-*-*-*-160-*-*-*-*-*-*")
   (italic-18		"-*-times-*-i-*-*-*-180-*-*-*-*-*-*")
   (bold-12		"-*-helvetica-bold-r-*-*-*-120-*-*-*-*-*-*")
   (bold-14		"-*-helvetica-bold-r-*-*-*-140-*-*-*-*-*-*")
   (bold-16		"-*-helvetica-bold-r-*-*-*-160-*-*-*-*-*-*")
   (bold-18		"-*-helvetica-bold-r-*-*-*-180-*-*-*-*-*-*")
   (bold-italic-12	"-*-helvetica-bold-o-*-*-*-120-*-*-*-*-*-*")
   (bold-italic-14	"-*-helvetica-bold-o-*-*-*-140-*-*-*-*-*-*")
   (bold-italic-16	"-*-helvetica-bold-o-*-*-*-160-*-*-*-*-*-*")
   (bold-italic-18	"-*-helvetica-bold-o-*-*-*-180-*-*-*-*-*-*")
   (tty-12		"-adobe-courier-medium-*-*-*-*-120-*-*-*-*-*-*")
   (tty-14		"-adobe-courier-medium-*-*-*-*-140-*-*-*-*-*-*")
   (tty-16		"-adobe-courier-medium-*-*-*-*-160-*-*-*-*-*-*")
   (tty-18		"-adobe-courier-medium-*-*-*-*-180-*-*-*-*-*-*")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Fonts utilities
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unset-tags (editor-window start end)
  (dolist (tag all-fonts) 
    (funcall editor-window "tag" "remove" (car tag) start end)))
  
(defun set-font (editor-window font start end)
  ;; Be sure this tag exists
  (funcall editor-window "tag" "conf" font "-font" (cadr (assoc font all-fonts)))
  ;; Delete all the tags associated to this range
  (unset-tags editor-window start end)
  ;; Set a new tag for this character range
  (funcall editor-window "tag" "add" font start end))

(defun set-underline (editor-window start end)
  (funcall editor-window "tag" "conf" "underline" "-underline" T)
  (funcall editor-window "tag" "add" "underline" start end))

(defun fontify-selection (editor-window font)
  (setq editor-window (string->widget editor-window))
  (catch-errors
     (set-font editor-window 
		   font 
		   (funcall editor-window "index" "sel.first")
		   (funcall editor-window "index" "sel.last"))))

(defun underline-selection (editor-window value)
  (setq editor-window (string->widget editor-window))
  (catch-errors
     (let ((start (funcall editor-window "index" "sel.first"))
	   (end   (funcall editor-window "index" "sel.last")))
       ;; Remove all underlining information in this area
       (funcall editor-window "tag" "remove" "underline" start end)
       ;; Set underline if value is T
       (when value (set-underline editor-window start end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Scheme Text Format (STF) management
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-STF (editor-window)
  (list STF-signature
	(funcall editor-window "get" "1.0" "end")
	(let ((l '()))
	  (dolist (t (cons `(underline NIL) all-fonts))
	    (let ((tags (funcall editor-window "tag" "range" (car t))))
	      (unless (null tags)
		(setq l (cons (list (car t) tags) l)))))
	  l)))

(defun set-STF (editor-window STF)
  (let ((text (cadr STF)) (fmts (caddr STF)))
    ;; First insert new text
    (funcall editor-window "delete" "1.0" "end")
    (funcall editor-window "insert" "1.0" text)
    (funcall editor-window "mark" "set" "insert" "1.0")
    ;; And now enhence it
    (dolist (t fmts) 
      (do ((l (cadr t) (cddr l)))
	  ((null l))
	(if (equal (car t) "underline")
	    (set-underline editor-window (car l) (cadr l))
	    (set-font editor-window (car t) (car l) (cadr l))))))
  (update))
  
(defun write-file (editor-window file)
  (setq editor-window (string->widget editor-window))
  (with-open-file (s file :direction :output)
    (format s ";;;; ~S\n" STF-signature)
    (format s "~S\n" (get-STF editor-window))))


(defun write-file-ascii (editor-window file)
  (setq editor-window (string->widget editor-window))
  (with-open-file (s file :direction :output)
    (format s "~A" (funcall editor-window "get" "1.0" "end"))))

(defun read-file (editor-window file)
  (setq editor-window (string->widget editor-window))
  (with-open-file (s file)
    (let ((first-line (read-line s)))
      (if (string= first-line (format NIL ";;;; ~S" STF-signature))
	  ;; File is a STF file
	  (set-STF editor-window (read s))
	  ;; File must be read as a "normal" file
	  (progn
	    (funcall editor-window "delete" "1.0" "end")
	    (do ((l first-line (read-line s nil nil)))
		((null l))
	      (funcall editor-window "insert" "end" l)
	      (funcall editor-window "insert" "end" #\newline))
	    (funcall editor-window "mark" "set" "insert" "1.0")))))))

(defun get-filename (toplevel) ; return the content of the file name  entry
  (let ((entry (string->widget (& toplevel ".bt.e"))))
    (funcall entry "get")))

(defun set-filename (toplevel filename)
  (let ((entry (string->widget (& toplevel ".bt.e"))))
    (funcall entry "delete" 0 "end")
    (funcall entry "insert" 0 filename)))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Interface
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-editor (name &rest exit_code)
  (let* ((top 	     (toplevel name))
	 (menu-bar   (frame     (& name ".mb") "-bd" 2 "-relief" "groove"))
	 (bottom     (frame     (& name ".bt")))
	 (text-area  (frame     (& name ".ta")))
	 (exit_code  (if (null exit_code) `(destroy ,top) (car exit_code)))
	 (the-editor ()))

    ;;
    ;; Window manager management
    ;; 
    (wm "maxsize" name 1000 800)
    (wm "protocol" name "WM_DELETE_WINDOW" exit_code)
	
    ;;
    ;; Text area frame
    ;;
    (pack (scrollbar (& text-area ".sc") "-orient" "vert" 
		     			 "-bd" 2
					 "-relief" "groove"
		     			 "-command" (format NIL "~A \"yview\"" 
							  (& text-area ".ed")))
	  "-side" "left" "-fill" "y")
    (pack (text (& text-area ".ed") "-padx" 4 
				    "-pady" 4
				    "-bd" 2
				    "-wrap" "word"
				    "-relief" "groove"
				    "-yscroll" (format NIL "~A \"set\""
						     (& text-area ".sc")))
	  "-side" "right" "-expand" T "-fill" "both")

    (setq the-editor (& text-area ".ed"))

    ;;
    ;; Menu Creation
    ;;

    (let* ((File (menubutton (& menu-bar ".file") 
			     "-text" "File"
			     "-padx" 10
			     "-menu" (& menu-bar ".file.m")))
	   (m	 (string->widget (menu (& menu-bar ".file.m")))))

      (funcall m "add" "command" 
	      "-label" "  Read  "    
	      "-command" `(read-file ,the-editor (get-filename ,top)))
      (funcall m "add" "command" 
	      "-label" "  Save  "
	      "-command" `(write-file ,the-editor (get-filename ,top)))
      (funcall m "add" "command" 
	      "-label" "  Save Ascii  "
	      "-command" `(write-file-ascii ,the-editor (get-filename ,top)))
      (funcall m "add" "separator")
      (funcall m "add" "command" "-label" "  Quit  " "-command" exit_code)
      
      (pack File "-side" "left"))

    (let* ((Font (menubutton (& menu-bar ".font")		       
			     "-text" "Font" 
			     "-padx" 10
			     "-menu" (& menu-bar ".font.m")))
	   (m    (string->widget (menu (& menu-bar ".font.m")))))

      (dolist (font all-fonts)
	(funcall m "add" "command" 
		 "-label"    (car font)
		 "-font"     (cadr font)
		 "-command" `(fontify-selection ,the-editor
			      ',(car font))))
      (funcall m "add" "separator")
      (funcall m "add" "command"
	      "-label" "Underline"    
	      "-command" `(underline-selection ,the-editor T))
      (funcall m "add" "command" 
	      "-label" "No underline"
	      "-command" `(underline-selection ,the-editor NIL))

      (pack Font "-side" "left"))

    ;;
    ;; Bottom frame
    ;;
    (pack (label (& bottom ".l") "-text" "File name" "-padx" 10) "-side" "left")
    (pack (entry (& bottom ".e") "-relief" "ridge") "-side" "left" "-expand" T "-fill" "x")

    ;;
    ;; Pack everybody
    ;;
    (pack menu-bar  "-fill" "x")
    (pack text-area "-expand" T "-fill" "both")
    (pack bottom    "-fill" "x" "-ipady" 4 "-ipadx" 10)))


;; A simple editor accessible from prompt
(defun ed (&rest file)
  (require "editor")
  (let ((editor-name (gensym ".editor")))
    (make-editor editor-name)
    (unless (null file)
       (read-file (& editor-name ".ta.ed") (car file))
       (set-filename editor-name (car file)))))
