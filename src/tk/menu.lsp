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
;;;; Last file update: 25-Nov-1993 15:54
;;;;
;;;; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;;;;

(in-package "TK")

;; ----------------------------------------------------------------------
;; Class bindings for menubutton widgets.  Variables used:
;; tk::posted -		keeps track of the menubutton whose menu is
;;				currently posted (or empty string, if none).
;; tk::inMenuButton-	if non-null, identifies menu button
;;				containing mouse pointer.
;; tk::relief -		keeps track of original relief of posted
;;				menu button, so it can be restored later.
;; tk::dragging -		if non-null, identifies menu button whose
;;				menu is currently being dragged in a tear-off
;;				operation.
;; curr-focus -		records old focus window so focus can be
;;				returned there after keyboard traversal
;;				to menu.
;; ----------------------------------------------------------------------


(def-bindings "Menubutton" '(
  ("<Any-Enter>"	 . (progn
			     (setq tk::inMenuButton %W)
			     (unless (equal (tk-get %W "-state") "disabled")
				(unless tk-strictMotif
				   (tk-setq %W "-state" "active")))))
  ("<Any-Leave>"	 . (progn
			     (setq tk::inMenuButton '())
			     (when (equal (tk-get %W "-state") "active")
				(tk-setq %W "-state" "normal"))))

  ("<1>"		 . (tk-mbButtonDown %W))
  ("<Any-ButtonRelease-1>" . (if (and (equal %W tk::posted)
				      (equal %W tk::inMenuButton))
				 (funcall (string->widget (tk-get %W "-menu")) "activate" 0)
				 (tk-mbUnpost)))
  ;; The binding below is trickier than it looks.  It's important to check
  ;; to see that another menu is posted in the "if" statement below.
  ;; The check is needed because some window managers (e.g. mwm in
  ;; click-to-focus mode) cause a button-press event to be preceded by
  ;; a B1-Enter event;  we don't want to process that B1-Enter event (if
  ;; we do, the grab may get mis-set so that the menu is non-responsive).
  ("<B1-Enter>"		 . (progn 
			     (setq tk::inMenuButton %W)
			     (when (and (not (equal (tk-get %W "-state") "disabled"))
					tk::posted)
				(unless tk-strictMotif
				   (tk-setq %W "-state" "active"))
				(tk-mbPost %W))))
  
  ("<2>" 		. (unless (or tk::posted 
				      (equal (tk-get %W "-state") "disabled"))
			     (setq tk::dragging %W)
			     (tk-execute-menu %W |%X| |%Y|)))

  ("<B2-Motion>"	. (unless (equal tk::dragging "")
			     (tk-execute-menu %W |%X| |%Y|)))

  ("<ButtonRelease-2>"	. (setq tk::dragging ""))
))


;; ----------------------------------------------------------------------
;; Class bindings for menu widgets.  tk-priv(x) and tk-priv(y) are used
;; to keep track of the position of the mouse cursor in the menu window
;; during dragging of tear-off menus.  tk-priv(window) keeps track of
;; the menu containing the mouse, if any.
;; ----------------------------------------------------------------------

(def-bindings "Menu" '(
  ("<Any-Enter>"	. (progn
			    (setq tk::window %W)
			    (%W "activate" "@%y")))
  ("<Any-Leave>"	. (progn
			    (setq tk::window '())
			    (%W "activate" "none")))
  ("<Any-Motion>"	. (when (equal tk::window %W)
				(%W "activate" "@%y")))

  ("<1>"		. (unless (equal curr-grab "") (grab curr-grab)))
  ("<ButtonRelease-1>"	. (tk-invokeMenu %W))

  ("<2>"		. (progn
			    (setq tk::x %x)
			    (setq tk::y %y)))
  ("<B2-Motion>"	. (unless tk::posted
			     (%W "post" (- %X  tk::x) (- %Y tk::y))))
  ("<B2-Leave>"		. ())
  ("<B2-Enter>"		. ())
  ("<Escape>" 		. (tk-mbUnpost))
  ("<Any-KeyPress>"	. (tk-traverseWithinMenu %W "%A"))
  ("<Left>" 		. (tk-nextMenu -1))
  ("<Right>"		. (tk-nextMenu 1))
  ("<Up>"		. (tk-nextMenuEntry -1))
  ("<Down>"		. (tk-nextMenuEntry 1))
  ("<Return>" 		. (tk-invokeMenu %W))
))

;; The procedure below is publically available.  It is used to identify
;; a frame that serves as a menu bar and the menu buttons that lie inside
;; the menu bar.  This procedure establishes proper "menu bar" behavior
;; for all of the menu buttons, including keyboard menu traversal.  Only
;; one menu bar may exist for a given top-level window at a time.
;; Arguments:
;;	
;; bar -			The path name of the containing frame.  Must
;;				be an ancestor of all of the menu buttons,
;;				since it will be be used in grabs.
;; additional arguments -	One or more menu buttons that are descendants
;;				of bar.  The order of these arguments
;;				determines the order of keyboard traversal.
;;				If no extra arguments are named then all of
;;				the menu bar information for bar is cancelled.

(defvar tk::menusFor   '())
(defvar tk::menuBarFor '())

(defun tk-menuBar (w &rest args)
  (format T "~S~%" args)
  (if (null args)
      (cdr (assoc w tk::menusFor))
      (let* ((win  (winfo "toplevel" w))
	     (tmp1 (assoc w tk::menusFor))
	     (tmp2 (assoc win tk::menuBarFor)))
	(format T "tmp1 ~S tmp2 ~S win ~S~%" tmp1 tmp2 win)

	(if tmp1 
	    (setf (cdr tmp1) args)
	    (push (cons w args) tk::menusFor))
	(if tmp2 
	    (setf (cdr tmp2) w)
	    (push (cons win w) tk::menuBarFor))
	(bind w "<Any-ButtonRelease-1>" '(tk-mbUnpost)))))

;; The procedure below is publically available.  It takes any number of
;; arguments that are names of widgets or classes.  It sets up bindings
;; for the widgets or classes so that keyboard menu traversal is possible
;; when the input focus is in those widgets or classes.

(defun tk-bindForTraversal (&rest args)
  (dolist (w args)
    (bind w "<Alt-KeyPress>" '(tk-traverseToMenu %W "%A"))
    (bind w "<F10>" (tk-firstMenu %W))))

;; The function below does all of the work of posting a menu (including
;; unposting any other menu that might currently be posted).  The "w"
;; argument is the name of the menubutton for the menu to be posted.
;; Note:  if w is disabled then the procedure does nothing.

(defun tk-mbPost (w)
  (when (equal (tk-get w "-state") "disabled")
    (return-from tk-mbPost NIL))

  (when (equal tk::posted w)
;;;      (grab "-global" curr-grab)
    (grab curr-grab)
    (return-from tk-mbPost NIL))

  (let* ((MenuName (tk-get w "-menu"))
	 (Menu (string->widget MenuName)))
    (when (equal MenuName "")
      (return-from tk-mbPost NIL))

    ;; if a menu is already posted, unpost it
    (when tk::posted (tk-mbUnpost))

    ;; Retain several graphical infos
    (setq tk::relief   (tk-get w "-relief"))
    (setq tk::activeBg (tk-get Menu "-activebackground"))
    (setq tk::activeFg (tk-get Menu "-activeforeground"))	

    (tk-setq w "-relief" "raised")
    (setq tk::posted w)

    (when (null curr-focus)
      (setq curr-focus (get-focus)))
    (focus MenuName)

    (when tk-strictMotif
      (tk-setq Menu "-activebackground" (tk-get Menu "-background"))
      (tk-setq Menu "-activeforeground" (tk-get Menu "-foreground")))
    (funcall Menu "activate" "none")
    (funcall Menu "post" (winfo "rootx" w)
	     (+ (parse-integer (winfo "rooty" w))
		(parse-integer (winfo "height" w))))

    (let* ((grb '())
	   (win (winfo "toplevel" w))
	   (tmp (assoc win tk::menuBarFor)))
      (if tmp
	  ;; menu associated 
	  (progn
	   (setq grb (cdr tmp))
	   (unless (member w (cdr (assoc grb tk::menusFor)))
	     (setq grb w)))
	  (setq grb w))
	 
      ;; Retain actual cursor and set it now to an arrow
      (setq tk::cursor (tk-get grb "-cursor"))
      (tk-setq grb "-cursor" "arrow")

      (setq curr-grab grb)
;;;	 (grab "-global" grb)
      (grab grb))))


;; The procedure below does all the work of unposting the menubutton that's
;; currently posted.  It takes no arguments.  Special notes:
;; 1. It's important to unpost the menu before releasing the grab, so
;;    that any Enter-Leave events (e.g. from menu back to main
;;    application) have mode NotifyGrab.
;; 2. Be sure to enclose various groups of commands in "catch" so that
;;    the procedure will complete even if the menubutton or the menu
;;    or the grab window has been deleted.

(defun tk-mbUnpost ()
  (when tk::posted
    (let ((w tk::posted))
      (catch-errors
	  (let* ((Menu (string->widget (tk-get w "-menu"))))
	    (funcall Menu "unpost")
	    (tk-setq Menu "-activebackground" tk::activeBg)
	    (tk-setq Menu "-activeforeground" tk::activeFg)
	    (tk-setq w "-relief" tk::relief)))
      (catch-errors
	  (tk-setq curr-grab "-cursor" tk::cursor))
      (focus curr-focus)
      (grab "release" curr-grab)
      (setq curr-focus '())
      (setq tk::posted NIL))))

;; Following function executes the menu-button associated menu (if it exists)
(defun tk-execute-menu (w x y)
  (let ((widget (string->widget (tk-get w "-menu"))))
    (when widget
      (funcall widget "post" x y))))


;; The procedure below invokes the active entry in the posted menu,
;; if there is one.  Otherwise it does nothing.

(defun tk-invokeMenu (menu)
  (let ((i (funcall menu "index" "active")))
    (unless (equal i "none")
      (tk-mbUnpost)
      (update "idletasks")
      (funcall menu "invoke" i))))

;; The procedure below is invoked when a button-1-down event is
;; received by a menu button.  If the mouse is in the menu button
;; then it posts the button's menu.  If the mouse isn't in the
;; button's menu, then it deactivates any active entry in the menu.
;; Remember, event-sharing can cause this procedure to be invoked
;; for two different menu buttons on the same event.

(defun tk-mbButtonDown (w)
  (unless (equal (tk-get w "-state") "disabled")
    (when (equal tk::inMenuButton w)
       (tk-mbPost w))))
