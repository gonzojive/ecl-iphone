;******************************************************************************
;
; Project       : STk-inspect, a graphical debugger for STk
;
; File name     : inspect-main.stk
; Creation date : Aug-10-1993
; Last update   : Sep-17-1993
;
; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;
;******************************************************************************
;
; This file implements the "General inspector".
;
;******************************************************************************

(in-package "TK")

(provide "inspect-main")
(require "inspect-misc")
(require "inspect-view")
(require "inspect-detail")
(require "inspect-help")

(defvar INSPECTOR_WIDGET_NAME 	 ".inspector")
(defvar inspected-objects-list   ())

(defun inspected? (obj) (member obj inspected-objects-list))

(defun inspect-frame-wid (obj)
  (widget INSPECTOR_WIDGET_NAME ".f1." (object-symbol obj)))
(defun inspect-frame-str (obj)
  (& INSPECTOR_WIDGET_NAME ".f1." (object-symbol obj)))

(defun inspect-l-wid (obj) (widget (inspect-frame-str obj) ".l"))
(defun inspect-l-str (obj) (& (inspect-frame-str obj) ".l"))
(defun inspect-e-wid (obj) (widget (inspect-frame-str obj) ".e"))
(defun inspect-e-str (obj) (& (inspect-frame-str obj) ".e"))
(defun inspect-mb-wid (obj) (widget (inspect-frame-str obj) ".mb"))
(defun inspect-mb-str (obj) (& (inspect-frame-str obj) ".mb"))
(defun inspect-m-str (obj) (& (inspect-frame-str obj) ".mb.m"))
(defun inspect-m-wid (obj) (widget (inspect-frame-str obj) ".mb.m"))


;---- Inspector menu

(defun create-inspect-menu (obj)
  (let ((w (eval (menu (inspect-m-str obj)))))
    (funcall w "add" "command" "-label" "Uninspect" 
	     "-command" `(inspect-menu-Uninspect ',(object-symbol obj)))
    (funcall w "add" "command" "-label" "Detail"
	     "-command" `(inspect-menu-Detail ',(object-symbol obj)))
    (if (detailed? obj) ((inspect-m-wid obj) "disable" "Detail"))
    (funcall w "add" "command" "-label" "View" 
	     "-command" `(inspect-menu-View ',(object-symbol obj)))
    (if (viewed? obj) ((inspect-m-wid obj) "disable" "View")))
  )

(defun inspect-menu-Eval (obj)
  (set obj (eval (funcall (inspect-e-wid obj) "get"))))

(defun inspect-menu-Quote (obj)
  (set obj (funcall (inspect-e-wid obj) "get")))

(defun inspect-menu-Uninspect (key)
  (uninspect (find-object-infos  key)))

(defun inspect-menu-Detail (key)
  (let ((obj (find-object-infos  key)))
    (detail obj)
    (funcall (inspect-m-wid obj) "disable" "Detail")
    (if (viewed? obj) (funcall (view-m-wid obj) "disable" "Detail"))))

(defun inspect-menu-View (key)
  (let ((obj (find-object-infos  key)))
    (view obj)
    (funcall (inspect-m-wid obj) "disable" "View")
    (if (detailed? obj) (funcall (detail-m-wid obj) "disable" "View"))))

(defun create-inspector ()
  (let ((w (toplevel INSPECTOR_WIDGET_NAME)))
    (wm "title" w "General inspector")
    (wm "maxsize" w SCREEN_WIDTH SCREEN_HEIGHT)
    (define menu-w (create-menu-widget (& INSPECTOR_WIDGET_NAME ".menu")))
    (pack menu-w "-side" "top" "-fill" "x" "-padx" 4 "-pady" 2)
    (funcall (widget menu-w ".help.m") "add" "command" "-label" "General inspector"
     "-command" "(make-help General-Inspector-help)")
    (pack (menubutton (& INSPECTOR_WIDGET_NAME ".menu.command") "-text" "Command")
	  "-side" "left")
    (define cmd-w (eval (menu (& INSPECTOR_WIDGET_NAME ".menu.command.m"))))
    (cmd-w "add" "command" "-label" "Uninspect all"
	   "-command" "(destroy-inspector)")
    (cmd-w "add" "command" "-label" "Undebug" "-command" "(undebug)")
    (tk-setq (widget INSPECTOR_WIDGET_NAME ".menu.command") "-menu" cmd-w)
    (pack (frame (& INSPECTOR_WIDGET_NAME ".caption"))
	  "-side" "top" "-fill" "x" "-padx" 4)
    (pack (label (& INSPECTOR_WIDGET_NAME ".caption.l1")
		 "-text" "Objects" "-width" 20)
	  "-side" "left")
    (pack (label (& INSPECTOR_WIDGET_NAME ".caption.l2")
		 "-text" "Values" "-width" 40)
	  "-side" "left" "-padx" 4)
    (pack (frame (& INSPECTOR_WIDGET_NAME ".f1"))
	  "-fill" "both" "-expand" "yes" "-padx" 4 "-pady" 2))
  )

(defun destroy-inspector () 
  (mapcar #'uninspect-object inspected-objects-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; inspect
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inspect (obj)
  (when (= (winfo "exist" INSPECTOR_WIDGET_NAME) 0) (create-inspector))
  ;; Kludge to avoid problems . Should be modified (eg)
  (let ((obj-val (inspect::eval obj)))
    (when (equal (inspect::typeof obj-val) "widget")
	  (setq obj obj-val)))

  (unless (inspected? obj)
     (inspect-object obj)
     (let ((obj-val (format NIL "~S" (inspect::eval obj))))
       (pack (frame (inspect-frame-str obj)) "-side" "top" "-fill" "x")
       (pack (menubutton (inspect-mb-str obj)
			 "-relief" "raised" "-bd" 2 "-bitmap" BITMAP_MENU)
	     "-side" "right")
       (pack (label (inspect-l-str obj) "-relief" "groove" "-bd" 2
		    "-anchor" "w" "-text" (format NIL "~S" obj)
		    "-width" 20 "-font" MEDIUM_FONT)
	     "-side" "left")
       (pack (entry (inspect-e-str obj) "-relief" "sunken" "-bd" 2 "-width" 40) 
	     "-fill" "x" "-expand" "yes" "-padx" 4)
       (create-inspect-menu obj)
       (tk-setq (inspect-mb-wid obj) "-menu" (inspect-m-wid obj))
       
       (let ((E (inspect-e-wid obj)))
	 (E "insert" 0 obj-val)

	 ;; If obj is a symbol, lets the entry modifiable. Otherwise let it as is
	 (if (modifiable-object? obj)
	     (begin
	       (bind E "<Return>" 	`(inspect-menu-Eval ',obj))
	       (bind E "<Shift-Return>" `(inspect-menu-Quote ',obj)))
	     (inspect::shadow-entry E)))))

  ;; Destroy Event -> set the list of inspected object to '()
  (bind  INSPECTOR_WIDGET_NAME "<Destroy>" '(setq inspected-objects-list '()))

  ;; Allow resizing only in width
  (update "idletasks")
  (let ((req-h (winfo "reqheight" INSPECTOR_WIDGET_NAME)))
    (wm "minsize" INSPECTOR_WIDGET_NAME 0 req-h)
    (wm "maxsize" INSPECTOR_WIDGET_NAME SCREEN_WIDTH req-h)
    (wm "geometry" INSPECTOR_WIDGET_NAME 
	(& (winfo "width" INSPECTOR_WIDGET_NAME) "x" req-h))))

(defun inspect-object (obj)
  (setq inspected-objects-list (cons obj inspected-objects-list))
  (unless (object-infos obj)
	  (add-object-infos obj)
	  (if (symbolp obj) (trace-var obj `(update-object ',obj)))))

(defun inspect-display (obj)
  (let ((entry-w (inspect-e-wid obj)))
    (entry-w "delete" 0 "end")
    (entry-w "insert" 0 (->object (eval obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; uninspect
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uninspect (obj)
  (when (inspected? obj) (uninspect-object obj))     
  (update "idletasks")
  (when (= (winfo "exist" INSPECTOR_WIDGET_NAME) 1)
	(let ((req-h (winfo "reqheight" INSPECTOR_WIDGET_NAME)))
	  (wm "minsize" INSPECTOR_WIDGET_NAME 0 req-h)
	  (wm "maxsize" INSPECTOR_WIDGET_NAME SCREEN_WIDTH req-h)
	  (wm "geometry" INSPECTOR_WIDGET_NAME 
	      (& (winfo "width" INSPECTOR_WIDGET_NAME) "x" req-h)))))


(defun uninspect-object (obj)
  (setq inspected-objects-list (list-remove obj inspected-objects-list))
  (destroy (inspect-frame-wid obj))
  (when (null inspected-objects-list) (destroy INSPECTOR_WIDGET_NAME))
  (if (detailed? obj) (funcall (detail-m-wid obj) "enable" "Inspect"))
  (if (viewed? obj)   (funcall (view-m-wid obj)   "enable" "Inspect"))
  (unless (or (detailed? obj) (viewed? obj))
	  (remove-object-infos obj)
	  (if (symbolp obj) (untrace-var obj))))
