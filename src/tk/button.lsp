;;;;
;;;; Buttons, Check button and radio buttons bindings and procs
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
;;;; Last file update: 22-Nov-1993 16:00
;;;;
;;;;
;;;; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;;;;


;; Class bindings for various flavors of button widgets. tk::window
;; keeps track of the button containing the mouse, and tk::relief
;; saves the original relief of the button so it can be restored when
;; the mouse button is released.

(in-package "TK")

(let ((Button-bindings '(("<Any-Enter>"		. (tk-butEnter %W))
			 ("<Any-Leave>"		. (tk-butLeave %W))
			 ("<1>"			. (tk-butDown %W))
			 ("<ButtonRelease-1>" 	. (tk-butUp %W)))))

   (def-bindings "Button"      Button-bindings)
   (def-bindings "Checkbutton" Button-bindings)
   (def-bindings "Radiobutton" Button-bindings))

;; The procedure below is invoked when the mouse pointer enters a
;; button widget.  It records the button we're in and changes the
;; state of the button to active unless the button is disabled.

(defun tk-butEnter (w)
  (unless (equal (tk-get w "-state") "disabled")
     (unless tk-strictMotif (tk-setq w "-state" "active"))
     (setq tk::window w)))


;; The procedure below is invoked when the mouse pointer leaves a
;; button widget. It changes the state of the button back to
;; inactive.

(defun tk-butLeave (w)
  (unless (equal (tk-get w "-state") "disabled")
     (unless tk-strictMotif (tk-setq w "-state" "normal"))
     (setq tk::window "")))

;; The procedure below is invoked when the mouse button is pressed in
;; a button/radiobutton/checkbutton widget.  It records information
;; (a) to indicate that the mouse is in the button, and
;; (b) to save the button's relief so it can be restored later.

(defun tk-butDown (w)
  (setq tk::buttonWindow w)
  (setq tk::relief (tk-get w "-relief"))
  (unless (equal (tk-get w "-state") "disabled")
     (tk-setq w "-relief" "sunken")))

;; The procedure below is invoked when the mouse button is released
;; for a button/radiobutton/checkbutton widget.  It restores the
;; button's relief and invokes the command as long as the mouse
;; hasn't left the button.

(defun tk-butUp (w)
  (when (equal w tk::buttonWindow)
     (tk-setq w "-relief" tk::relief)
     (when (and (equal w tk::window)
		(not (equal (tk-get w "-state") "disabled")))
       (funcall w "invoke"))
     (setq tk::buttonWindow '())))
