;;;;
;;;; e r r o r . l s p 		-- All the stuff going with error messages 
;;;;				   display
;;;;
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
;;;;
;;;;           Author: Erick Gallesio [eg@unice.fr]
;;;;    Creation date: 15-Sep-1993 14:11
;;;; Last file update:  5-Feb-1995 16:16
;;;;
;;;; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;;;;

(in-package "TK")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Data section
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar help-for-stackvue '("STF-0.1" "
ECL/tk stack window help~%~%~%
The ECL/tk stack window permits you to see the evaluation stack.Each line contains the parameters passed to an invocation of the eval procedure (last call to eval is on the first line).~%~%
To inspect an object, you can select it with the mouse (either by dragging the mouse or with a double click to select a whole word) and, once this is done, you can call the inspector by clicking the Inspect  button. This will bring an inspector window if it does not exist yet.Otherwise the selected object will be added to the list of inspected objects in your inspector.

" ((bold-italic-12 ("6.4" "6.14")) (italic-12 ("9.3" "9.10")) (roman-18 ("2.0" "3.0")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; report-error
;;;;
;;;;	   Redefine here report-error. This version of report-error needs Tk
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar %stack '())

(defun %truncate-string (s len)
  (if (> (string-length s) len)
      (format NIL "~A ..." (substring s 0 (- len 1)))
      s))

(defun report-error (head message obj)
  (setq %stack (%get-eval-stack)) ;; Take a photo of the stack as soon as possible 

  (let* ((who (if (null obj) "" (format NIL "~S" obj)))
	 (msg (%truncate-string (format NIL "~A~%~A~%~A" head message who) 200)))

    ;; Print message on standard error stream
    (format (current-error-port) "~%~A~A~A~%" 
	    			 head message (if (equal who "") 
						  "" 
						  (format NIL ": ~A" who)))

    (make-dialog "-window" '.report-error
		      "-title"  "ECL/tk error"
		      "-text"   msg
		      "-bitmap"  "error"
		      "-grabbing" T
		      "-default" 0
		      "-buttons" `(("    Dismiss    " (lambda () '()))
				 (" See the stack " (lambda () 
						      (display-stack       
						           (cddr %stack))))))
    (update)))

(defun display-stack (stack)
  (catch-errors (destroy ".stackvue"))
    
  ;; Build a toplevel
  (toplevel ".stackvue")
  (wm "title" .stackvue "ECL/tk stack")
  
  ;; Dispose items
  (label ".stackvue.l" "-text" "Stack content" "-fg" "RoyalBlue")
  (frame ".stackvue.f" "-borderwidth" 3 "-relief" "ridge")
  (frame ".stackvue.b")

  (pack 
     (button ".stackvue.b.i" "-text" "Inspect" 
	                     "-command" "(run-inspect)")
     (button ".stackvue.b.h" "-text" "Help"    
	                     "-command" "(make-help help-for-stackvue)")
     (button ".stackvue.b.q" "-text" "Quit"    "-command" "(destroy .stackvue)")
     "-side" "left" "-fill" "x" "-expand" T)

  (pack .stackvue.l "-side" "top")
  (pack .stackvue.f "-side" "top" "-expand" T "-fill" "both")
  (pack .stackvue.b "-side" "bottom" "-fill" "x")
  
  (scrollbar ".stackvue.f.sy"  "-command" ".stackvue.f.list \"yview\"" "-orient" "vert")
  (text ".stackvue.f.list" "-width" 60
			   "-height" 15
			   "-yscroll" ".stackvue.f.sy \"set\""
			   "-font" "fixed"
			   "-bd" 1
			   "-relief" "raised"
			   "-padx" 3
			   "-wrap" "none")

  (pack .stackvue.f.sy   "-side" "left"   "-fill" "y")
  (pack .stackvue.f.list "-expand" "yes"  "-fill" "both")

  ;; Center the window
  (center-window .stackvue)
  
  ;; Insert all the elements of the stack in the listbox
  (do ((stack stack (cdr stack)))
      ((null stack))
    (.stackvue.f.list "insert" "end" 
		      (%truncate-string (format NIL "~%~S" (uncode(car stack)))150)))
  ;; Insert a marker to delimit bottom of the stack
  (.stackvue.f.list "insert" "end" "~%<<< STACK BOTTOM >>>")
  (.stackvue.f.list "tag" "add" "bottom" "end linestart" "end")
  (.stackvue.f.list "tag" "conf" "bottom" "-font" "8x13bold" "-foreground" "Red")

    
  (setq %stack '()) ;; so it can be GC'ed

  (wm "maxsize" .stackvue 1000 1000)
  (tk-setq .stackvue.f.list "-state" "disabled"))

(defun run-inspect ()
  ;; Load inspect if necessary
  (require "inspect-main")

  (let ((obj '()))
    (if (catch-errors (setq obj (selection "get")))
	;; No selection available. Open a popup to say that
	(make-dialog "-title" "Information" 
			  "-text" "Nothing is selected for inspection"
			  "-bitmap"  "info"
			  "-default" 0
			  "-buttons" '(("    Dismiss    " (lambda () T))))
	;; Inspect the content of selection
	(inspect (read-from-string obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Misc 
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar errorInfo "")


;; For now tkerror does nothing since Tcl_AddErrorInfo simulation is not
;; correct. 


(defun tkerror (&rest message)
  ;; Important note: When a background error occurs, tk try to see if 
  ;; tkerror is bound to something. This is achieved by calling tkerror
  ;; with an empty message. In this case, nothing is printed
  (unless (null message)
     (format *standard-error* "**** Tk error (~S) ~S~%" (car message) errorInfo)))

