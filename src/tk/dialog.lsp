;;;;
;;;; Dialog box creation utility
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
;;;;    Creation date:  4-Aug-1993 11:05
;;;; Last file update:  5-Feb-1995 16:38
;;;;
;;;; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;;;;

(in-package "TK")

(provide "dialog")

(defvar dialog-old-focus "none")
(defvar dialog-old-grab  ())
(defvar button-pressed   NIL)

;;
;; make-dialog
;;
;; This procedure displays a dialog box following the spcifications given in
;; arguments. Arguments are given as keywords.
;;
;; window (.dialog)	Window name to use for dialog top-level.
;; title ("Dialog")	Title to display in dialog's decorative frame.
;; text ("")		Message to display in dialog.
;; bitmap ("")		Bitmap to display in dialog (empty string means none).
;; default (-1) 	Index of button that is to display the default ring
;;			(-1 means none).
;; grabbing (NIL)	Indicates if make-dialog must wait that a button be
;;			pressed before returning
;; buttons ('())	A list of couples indicating the button text and its
;;			associated action (a lambda)
;;
;; If grabbing is set, this procedure returns the button pressed index.
;;


(defun make-dialog (&key ((window   w)   ".dialog")
			 (title    "Dialog")
			 (text     "")
			 (bitmap   "")
			 (default  -1)
			 ((grab grabbing)     NIL)
			 (buttons  '()))

  (catch-errors (destroy w))

  ;; 1. Create the top-level window and divide it into top and bottom parts.
  (let ((w.top (& w ".top"))
	(w.bot (& w ".bot"))
	(w.msg (& w ".msg"))
	(w.bmp (& w ".bmp")))

    (toplevel w "-class" "Dialog")
    (wm "title" w title)
    (wm "iconname" w "Dialog")
	
    (pack (frame w.top "-relief" "raised" "-bd" 1)
	  (frame w.bot "-relief" "raised" "-bd" 1)
	  "-fill" "both")

    ;; 2. Fill the top part with bitmap and message.
    (pack (message w.msg "-aspect" 1000 "-text" text 
	  "-font" "-Adobe-Times-Medium-R-Normal-*-180-*")
	  "-in" w.top 
	  "-side" "right"
	  "-expand" T 
	  "-fill" "both"
	  "-padx" "5m" 
	  "-pady" "5m")

    (unless (equal bitmap "")
      (pack (label w.bmp "-bitmap" bitmap "-fg" "red")
	    "-in" w.top 
	    "-side" "left"
	    "-padx" "5m"
	    "-pady" "5m"))

    ;; 3. Create a row of buttons at the bottom of the dialog.
    (do ((i 0 (+ i 1)) (but buttons (cdr but)))
	((null but) '())
      
      (let ((name (& w ".button" i)))
	(button name "-text" (caar but) 
		"-command" `(progn
			   (focus dialog-old-focus)
			   (setq button-pressed ,i)
			   (destroy (string->widget ,(& w)))
			   (apply ,(cadar but) '())))
	(if (equal i default)
	    (let ((default (& w ".default")))
	    (frame default "-relief" "ridge" "-bd" 3)
	    (raise name default)
	    (pack default "-in" w.bot 
		  "-side" "left" 
		  "-expand" T
		  "-padx" 20 
		  "-pady" 8)
	    (pack name    "-in" default 
		  "-padx" 5
		  "-pady" 5
		  "-ipadx" 2
		  "-ipady" 2)
	    (bind w "<Return>" (tk-get (string->widget name) "-command")))
	    (pack name "-in" w.bot 
	    "-side" "left"
	    "-expand" 1
	    "-padx" 20
	    "-pady" 8
	    "-ipadx" 2
	    "-ipady" 2)))))

  ;; 4. Center window
  (center-window w)

  ;; 5. Set focus to the new window
  (setq dialog-old-focus (get-focus))
  (focus w)
  (when grabbing 
    (setq dialog-old-grab  (funcall grab "current" *root*))
    (funcall grab "set" w)
    (tkwait "variable" "button-pressed")
    (if (not (null dialog-old-grab))
	(funcall grab dialog-old-grab)))
  button-pressed)

(defun center-window (w)
  ;; Withdraw the window, then update all the geometry information
  ;; so we know how big it wants to be, then center the window in the
  ;; display and de-iconify it.

  (wm "withdraw" w)
  (update "idletasks")
  (let ((x (- (/ (parse-integer (winfo "screenwidth" w)) 2) 
	      (/ (parse-integer (winfo "reqwidth" w)) 2)
	      (winfo "vrootx" (eval (winfo "parent" w)))))
	(y (- (/ (parse-integer (winfo "screenheight" w)) 2)
	      (/ (parse-integer (winfo "reqheight" w)) 2)
	      (parse-integer (winfo "vrooty" (eval (winfo "parent" w)))))))
    (wm "geom" w (& "+" (floor x) "+" (floor y)))
    (wm "deiconify" w)))
