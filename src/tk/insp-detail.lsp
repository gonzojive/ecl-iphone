;******************************************************************************
;
; Project       : STk-inspect, a graphical debugger for STk.
;
; File name     : inspect-detail.stk
; Creation date : Aug-30-1993
; Last update   : Sep-17-1993
;
;******************************************************************************
;
; This file implements the different kinds of "Detailers".
;
;******************************************************************************

(in-package "TK")

(provide "inspect-detail")

;---- detailer widget

(define DETAILER_WIDGET_NAME ".detailer")
(define detailed-objects-list ())

(defun detail-tl-wid (obj) (widget DETAILER_WIDGET_NAME (object-symbol obj)))
(defun detail-tl-str (obj) (& DETAILER_WIDGET_NAME (object-symbol obj)))
(defun detail-l-wid (obj) (widget (detail-tl-str obj) ".f1.l"))
(defun detail-l-str (obj) (& (detail-tl-str obj) ".f1.l"))
(defun detail-e-wid (obj) (widget (detail-tl-str obj) ".f1.e"))
(defun detail-e-str (obj) (& (detail-tl-str obj) ".f1.e"))
(defun detail-m-wid (obj) (widget (detail-tl-str obj) ".menu.command.m"))
(defun detail-m-str (obj) (& (detail-tl-str obj) ".menu.command.m"))

(defun detailed? (obj) (member obj detailed-objects-list))

(defun detail (obj)
  (if (member (inspect::typeof (inspect::eval obj))
	      '(list pair vector closure widget))
      (unless (detailed? obj) (detail-object obj))
      (error "The object ~s can not be detailed" obj)))

(defun detail-object (obj)
  (setq detailed-objects-list (cons obj detailed-objects-list))
  (unless (object-infos obj)
	  (add-object-infos obj)
	  (if (symbolp obj) (trace-var obj `(update-object ',obj))))
  (let ((obj-val (inspect::eval obj)))
    (case (inspect::typeof obj-val)
      ((list pair vector) (detail-VPL obj))
      ((closure)          (detail-procedure obj))
      ((widget) 	  (when (= (winfo "exists" (detail-tl-wid obj-val)) 0)
				(detail-widget obj))))))

(defun undetail (obj)
  (if (detailed? obj) (undetail-object obj)))

(defun undetail-object (obj)
  (let ((top (detail-tl-wid obj)))
    (setq detailed-objects-list (list-remove obj detailed-objects-list))
    (if (inspected? obj) ((inspect-m-wid obj) "enable" "Detail"))
    (if (viewed? obj) ((view-m-wid obj) "enable" "Detail"))
    (unless (or (inspected? obj) (viewed? obj))
	    (remove-object-infos obj)
	    (if (symbolp obj) (untrace-var obj)))
    ;; If toplevel exists (i.e. it is not a <Destroy> event) destroy it
    (if (= (winfo "exists" top) 1)
	(destroy top))))

(defun detail-display (obj)
  (case (inspect::typeof (inspect::eval obj))
    ((vector pair list) (detail-VPL-display obj))
    ((closure) (detail-procedure-display obj))
    ((widget) (detail-widget-display obj))))


;---- Detailer menu -----------------------------------------------------------

(defun detail-menu-Eval (entry obj)
  (eval-string (format NIL "(setq ~a ~a)" obj (entry "get"))))

(defun detail-menu-Quote (entry obj)
  (eval-string (format NIL "(setq ~a '~a)" obj (entry "get"))))

(defun detail-menu-Inspect (key)
  (let ((obj (find-object-infos key)))
    (inspect obj)
    ((widget (detail-tl-str obj) ".menu.command.m") "disable" "Inspect")
    (if (viewed? obj) ((view-w-wid obj) "disable" "Inspect"))))

(defun detail-menu-Undetail (key) (undetail (find-object-infos key)))

(defun detail-menu-View (key)
  (let ((obj (find-object-infos key)))
    (view obj)
    ((widget (detail-tl-str obj) ".menu.command.m") "disable" "View")
    (if (inspected? obj) ((inspect-m-wid obj) "disable" "View"))))


;---- VPL menu ----------------------------------------------------------------

(defun get-VPL-index (obj)
  (let ((s (tk-get (VPL-l-wid obj) "-text")))
    (string->number (substring s 6 (string-length s)))))

(defun get-VPL-value (obj) ((VPL-e-wid obj) "get"))
(defun set-VPL-index&value (obj index)
  (tk-setq (VPL-l-wid obj) "-text" (& "Value " index))
  (let ((value-w (VPL-e-wid obj)))
    (value-w "delete" 0 "end")
    (value-w "insert" 0 (->object ((VPL-vlb-wid obj) "get" index)))))

(defun VPL-menu-Eval (obj)
  (define index (get-VPL-index obj))
  ((VPL-vlb-wid obj) "delete" index)
  ((VPL-vlb-wid obj) "insert" index
		     (->object (eval-string (get-VPL-value obj))))
  (modify-VPL obj))

(defun VPL-menu-Quote (obj)
  (define index (get-VPL-index obj))
  ((VPL-vlb-wid obj) "delete" index)
  ((VPL-vlb-wid obj) "insert" index (get-VPL-value obj))
  (modify-VPL obj))


;---- VPL detailer ------------------------------------------------------------

(defun VPL-l-wid (obj) (widget (detail-tl-str obj) ".value.l"))
(defun VPL-l-str (obj) (& (detail-tl-str obj) ".value.l"))
(defun VPL-e-wid (obj) (widget (detail-tl-str obj) ".value.e"))
(defun VPL-e-str (obj) (& (detail-tl-str obj) ".value.e"))
(defun VPL-ilb-wid (obj) (widget (detail-tl-str obj) ".list.lb1"))
(defun VPL-ilb-str (obj) (& (detail-tl-str obj) ".list.lb1"))
(defun VPL-vlb-wid (obj) (widget (detail-tl-str obj) ".list.lb2"))
(defun VPL-vlb-str (obj) (& (detail-tl-str obj) ".list.lb2"))

(defun create-detail-toplevel-widget (obj)
  (define w (create-toplevel-widget (detail-tl-str obj)))
  (define id-w (widget w ".id"))
  (define menu-w (widget w ".menu"))
  (set-id-label1 id-w "Object" 6)
  (set-id-label2 id-w "Value" 6)
  ((widget menu-w ".help.m") "add" "command" "-label" "Detailer"
			     "-command" "(make-help Detailer-help)")
  (pack (menubutton (& menu-w ".command") "-text" "Command") "-side" "left")
  (define cmd-w (eval (menu (& menu-w ".command.m"))))
  (tk-setq (widget menu-w ".command") "-menu" cmd-w)
  (cmd-w "add" "command" "-label" "Inspect" 
	 	       "-command" `(detail-menu-Inspect ',(object-symbol obj)))
  (if (inspected? obj) (cmd-w "disable" "Inspect"))
  (cmd-w "add" "command" "-label" "Undetail"
	 	       "-command" `(detail-menu-Undetail ',(object-symbol obj)))
  (cmd-w "add" "command" "-label" "View" 
		       "-command" `(detail-menu-View ',(object-symbol obj)))
  (if (viewed? obj) (cmd-w "disable" "View"))

  (if (modifiable-object? obj)
      (begin
	(bind (widget w ".id.f2.e") "<Return>" 
	      `(detail-menu-Eval %W ',obj))
	(bind (widget w ".id.f2.e") "<Shift-Return>" 
	      `(detail-menu-Quote %W ',obj)))
      (begin
	(set-id-value id-w (format NIL "~S" (inspect::eval obj)))
	(inspect::shadow-entry (widget w ".id.f2.e"))))
  
  (bind w "<Destroy>" `(detail-menu-Undetail ',obj))
  w)

(defun detail-VPL (obj)
  (define w (create-detail-toplevel-widget obj))
  ((widget w ".menu.help.m") "add" "command")
  (tk-setq (widget w ".id.f1.l2") "-width" 20)
  (wm "maxsize" w SCREEN_WIDTH SCREEN_HEIGHT)
  (pack (frame (& w ".value")) "-side" "top" "-fill" "x" "-padx" 4 "-pady" 2)
  (pack (label (& w ".value.l") "-text" "Value 0") "-side" "left")
  (pack (entry (& w ".value.e") "-relief" "sunken" "-bd" 2) "-fill" "x")
  (pack (frame (& w ".list") "-relief" "sunken" "-bd" 2)
	"-fill" "both" "-expand" "yes" "-padx" 4 "-pady" 2)
  (pack (scrollbar (& w ".list.vsb") "-orient" "vertical")
	(listbox (& w ".list.lb1") "-relief" "raised" "-bd" 2 "-geometry" "4x8")
	"-side" "left" "-fill" "y")
  (pack (listbox (& w ".list.lb2") "-relief" "raised" "-bd" 2)
	"-fill" "both" "-expand" "yes")
  (tk-listbox-single-select (& w ".list.lb1") (& w ".list.lb2"))
  (if (modifiable-object? obj)
      (begin
	(bind (widget w ".value.e") "<Return>" `(VPL-menu-Eval ',obj))
	(bind (widget w ".value.e") "<Shift-Return>" `(VPL-menu-Quote ',obj)))
      (inspect::shadow-entry (widget w ".value.e")))

  (bind (widget w ".list.lb1") "<Button-1>" `(VPL-select ',obj %y))
  (bind (widget w ".list.lb2") "<Button-1>" `(VPL-select ',obj %y))
  (tk-setq (widget w ".list.vsb") "-command" (& "scroll-VPL " w))
  (tk-setq (widget w ".list.lb2") "-yscroll" (& w ".list.vsb \"set\""))
  (detail-VPL-display obj))

(defun VPL-select (obj y)
  (let ((index-w (VPL-ilb-wid obj))
	(value-w (VPL-vlb-wid obj))
	(entry-w (VPL-e-wid obj))
	(index ()))
    (value-w "select" "from" (value-w "nearest" y))
    (setq index (value-w "curselection"))
    (tk-setq (VPL-l-wid obj) "-text" (& "Value " index))
    (let ((state (tk-get entry-w "-state")))
      (tk-setq entry-w "-state" "normal")
      (entry-w "delete" 0 "end")
      (entry-w "insert" 0 (->object (value-w "get" index)))
      (tk-setq entry-w "-state" state))
    (focus entry-w)))
  
(defun scroll-VPL (w x &rest param)
  ((widget w ".list.lb1") "yview" x)
  ((widget w ".list.lb2") "yview" x))

(defun select-VPL-value (w index)
  (let ((index-l (widget w ".value.l"))
	(value-e (widget w ".value.e")))
    (tk-setq index-l "-text" index)
    (value-e "delete" 0 "end")
    (value-e "insert" 0 (->object ((widget w ".list.lb2") "get" index)))
    (focus value-e)))

;---- VPL display

(defun detail-VPL-display (obj)
  (define id-w (& (detail-tl-str obj) ".id"))
  (set-id-object id-w (->object obj))
  (set-id-value id-w (->object (inspect::eval obj)))
  (case (inspect::typeof (inspect::eval obj))
    ((list) (detail-VPL-display-list obj))
    ((pair) (detail-VPL-display-pair obj))
    ((vector) (detail-VPL-display-vector obj)))
  (let ((index (get-VPL-index obj)))
    (if (< index ((VPL-ilb-wid obj) "size"))
	(set-VPL-index&value obj index)
	(set-VPL-index&value obj 0))))

(defun detail-VPL-display-list (obj)
  (define w (detail-tl-wid obj))
  (wm "title" w "List detailer")
  ((widget w ".menu.help.m") "entryconfig" 2 "-label" "List detailer"
			     "-command" "(make-help List-detailer-help)")
  (let ((obj-val (inspect::eval obj))
	(index-w (VPL-ilb-wid obj))
	(value-w (VPL-vlb-wid obj))
	(index 0))
    (index-w "delete" 0 "end")
    (value-w "delete" 0 "end")
    (until (null obj-val)
	   (index-w "insert" "end" index)
	   (value-w "insert" "end" (->object (car obj-val)))
	   (setq obj-val (cdr obj-val))
	   (setq index (+ index 1)))))

(defun detail-VPL-display-pair (obj)
  (define w (detail-tl-wid obj))
  (wm "title" w "Pair detailer")
  ((widget w ".menu.help.m") "entryconfig" 2 "-label" "Pair detailer"
			     "-command" "(make-help Pair-detailer-help)")
  (let ((obj-val (inspect::eval obj))
	(index-w (VPL-ilb-wid obj))
	(value-w (VPL-vlb-wid obj))
	(index 0))
    (index-w "delete" 0 "end")
    (value-w "delete" 0 "end")
    (while (pair? obj-val)
	   (index-w "insert" "end" index)
	   (value-w "insert" "end" (->object (car obj-val)))
	   (setq obj-val (cdr obj-val))
	   (setq index (+ index 1)))
    (index-w "insert" "end" (& "." index))
    (value-w "insert" "end" (->object obj-val))))

(defun detail-VPL-display-vector (obj)
  (define w (detail-tl-wid obj))
  (wm "title" w "Vector detailer")
  ((widget w ".menu.help.m") "entryconfig" 2 "-label" "Vector detailer"
			     "-command" "(make-help Vector-detailer-help)")
  (let* ((obj-val (inspect::eval obj))
	 (length (vector-length obj-val))
	 (index-w (VPL-ilb-wid obj))
	 (value-w (VPL-vlb-wid obj)))
    (index-w "delete" 0 "end")
    (value-w "delete" 0 "end")
    (for ((index 0 (+ index 1)))
	 (< index length)
	 (index-w "insert" "end" index)
	 (value-w "insert" "end" (->object (vector-ref obj-val index))))))

;---- VPL modify

(defun modify-VPL (obj)
  (case (inspect::typeof (inspect::eval obj))
    ((list) (modify-VPL-list obj))
    ((pair) (modify-VPL-pair obj))
    ((vector) (modify-VPL-vector obj))))

(defun modify-VPL-list (obj)
  (let* ((value-w (VPL-vlb-wid obj))
	 (cmd (format NIL "(setq ~S '(" obj))
	 (size (value-w "size")))
    (for ((i 0 (+ i 1)))
	 (< i size)
	 (setq cmd (string-append cmd (->object (value-w "get" i)) " ")))
    (setq cmd (string-append cmd "))"))
    (eval-string cmd)))

(defun modify-VPL-pair (obj)
  (let* ((value-w (VPL-vlb-wid obj))
	 (cmd (format NIL "(setq ~S '(" obj))
	 (size (value-w "size"))
	 (size-1 (- size 1)))
    (for ((i 0 (+ i 1)))
	 (< i size-1)
	 (setq cmd (string-append cmd (->object (value-w "get" i)) " ")))
    (setq cmd (string-append cmd ". " (->object (value-w "get" size-1)) "))"))
    (eval-string cmd)))

(defun modify-VPL-vector (obj)
  (let* ((value-w (VPL-vlb-wid obj))
	 (cmd (format NIL "(setq ~S '#(" obj))
	 (size (value-w "size")))
    (for ((i 0 (+ i 1)))
	 (< i size)
	 (setq cmd (string-append cmd (->object (value-w "get" i)) " ")))
    (setq cmd (string-append cmd "))"))
    (eval-string cmd)))




;---- Procedure detailer ------------------------------------------------------

(defun inspect::pretty-print (body) (pp (uncode body) NIL))

(defun detail-procedure-set (obj)
  (define text-w (widget (detail-tl-str obj) ".body.t"))
  (eval-string (format NIL "(setq ~a ~a)" obj (text-w "get" "1.0" "end"))))

(defun detail-procedure (obj)
  (define w (create-detail-toplevel-widget obj))
  (wm "title" w "Procedure detailer")
  (wm "maxsize" w SCREEN_WIDTH SCREEN_HEIGHT)
  ((widget w ".menu.help.m") "add" "command" "-label" "Procedure detailer"
			     "-command" "(make-help Procedure-detailer-help)")
  (pack (label (& w ".menu.set") "-text" "Set") "-side" "left")
  (bind (widget w ".menu.set") "<ButtonPress-1>" `(detail-procedure-set ',obj))
  (pack (frame (& w ".body") "-relief" "sunken" "-bd" 2)
	"-fill" "both" "-expand" "yes" "-padx" 4 "-pady" 2)
  (pack (scrollbar (& w ".body.vsb")
		   "-orient" "vertical"
		   "-command" (format NIL "~a \"yview\"" (& w ".body.t")))
	"-side" "left" "-fill" "y")
  (pack (text (& w ".body.t")
	      "-relief" "raised" "-bd" 2 "-width" 60 "-height" 16
	      "-yscroll" (format NIL "~a \"set\"" (& w ".body.vsb")))
	"-fill" "both" "-expand" "yes")
  (detail-procedure-display obj))

(defun detail-procedure-display (obj)
  (define obj-val (inspect::eval obj))
  (define id-w (& (detail-tl-str obj) ".id"))
  (set-id-object id-w (->object obj))
  (set-id-value id-w (->object obj-val))
  (define body (procedure-body obj-val))
  (define text-w (widget (detail-tl-str obj) ".body.t"))
  (tk-setq text-w "-state" "normal")
  (text-w "delete" "1.0" "end")
  (text-w "insert" "1.0" (inspect::pretty-print body))
  (unless (symbolp obj)
     (inspect::shadow-entry text-w)))


;---- Widget detailer ---------------------------------------------------------

(defun detail-widget (obj)
  (define w (create-detail-toplevel-widget obj))
  (wm "title" w "Widget detailer")
  (tk-setq (widget w ".id.f1.l2") "-width" 40)
  ((widget w ".menu.help.m") "add" "command" "-label" "Widget detailer"
			     "-command" "(make-help Widget-detailer-help)")
  (pack (menubutton (& w ".menu.bindings") "-text" "Bindings") "-side" "left")
  (tk-setq (widget w ".menu.bindings") "-menu" (menu (& w ".menu.bindings.m")))
  (detail-widget-create-options obj)
  (detail-widget-display obj))

(defun detail-widget-create-options (obj)
  (define w-str (detail-tl-str obj))
  (catch-errors (destroy (& w-str ".options")))
  (pack (frame (& w-str ".options") "-relief" "raised" "-bd" 2)
	"-fill" "both" "-expand" "yes" "-padx" 4 "-pady" 2)
  (pack (frame (& w-str ".options.class")) 
	"-side" "top" "-fill" "x" "-padx" 4 "-pady" 4)
  (pack (label (& w-str ".options.class.l1")
	       "-text" "Class" "-width" 16 "-anchor" "e")
	"-side" "left")
  (pack (label (& w-str ".options.class.l2")
	       "-relief" "groove" "-bd" 2 "-anchor" "w" "-font" ITALIC-MEDIUM_FONT)
	"-fill" "x")
  (let ((options-infos ((eval obj) "config"))
	(i 1))
    (for-each
     (lambda (infos)
       (if (= 5 (length infos))
	   (let ((option-w (& w-str ".options.f" i))
		 (s        (symbol->string (car infos))))
	     (pack (frame option-w) "-side" "top" "-fill" "x" "-padx" 4)
	     (pack (label (& option-w ".l")
			  "-text" (substring s 1 (string-length s))
			  "-width" 16 "-anchor" "e")
		   "-side" "left")
	     (pack (entry (& option-w ".e") "-relief" "sunken" "-bd" 2) "-fill" "x")
	     (bind (& option-w ".e") "<Return>"      `(WID-eval-option ',obj %W))
	     (bind (& option-w ".e") "<Shift-Return>"`(WID-quote-option ',obj %W))
	     (setq i (+ i 1)))))
     options-infos))
  (pack (frame (& w-str ".options.children"))
	"-side" "top" "-fill" "x" "-padx" 4 "-pady" 4)
  (pack (label (& w-str ".options.children.1")
	       "-text" "Children" "-width" 16 "-anchor" "e")
	"-side" "left")
  (pack (entry (& w-str ".options.children.e")
	       "-relief" "groove" "-bd" 2 "-state" "disabled" "-font" MEDIUM_FONT)
	"-fill" "x")
  (update "idletasks")
  (define req-h (winfo "reqheight" w-str))
  (wm "minsize" w-str 0 req-h)
  (wm "maxsize" w-str SCREEN_WIDTH req-h))

(defun WID-bindings-menu-str (obj) (& (detail-tl-str obj) ".menu.bindings.m"))
(defun WID-bindings-menu-wid (obj)
  (widget (detail-tl-str obj) ".menu.bindings.m"))

(defun binding->string (binding)
  (let ((binding (if (string? binding) binding (symbol->string binding))))
    (substring binding 1 (- (string-length binding) 1))))

(defun WID-bindings-menu-add (obj binding)
  (if (catch-errors ((WID-bindings-menu-wid obj) "index" binding))
      ((WID-bindings-menu-wid obj) "add" "command" 
	   "-label"    (symbol->string binding)
	   "-command" `(show-binding ',(object-symbol obj) 
				   ,(symbol->string  binding)))))

(defun show-binding (key binding)
  (let* ((obj     (find-object-infos key))
	 (obj-val (inspect::eval obj))
	 (name    (string-lower (binding->string binding)))
	 (body    (bind obj-val binding)))
    
    (if (null body) (setq body (bind (winfo "class" obj-val) binding)))
    ((WID-bindings-menu-wid obj) "disable" binding)
    (define w (& (detail-tl-str obj) "._" name))
    (create-toplevel-widget w)
    (wm "title" w "Widget binding")
    (wm "maxsize" w SCREEN_WIDTH SCREEN_HEIGHT)
    (set-id-label1 (& w ".id") "Widget" 6)
    (set-id-object (& w ".id") (->object obj))
    (set-id-label2 (& w ".id") "Binding" 6)
    (set-id-value (& w ".id") binding)
    (inspect::shadow-entry (string->widget (& w ".id.f2.e")))
    (pack (button (& w ".menu.dismiss") 
		  "-text" "Dismiss" 
		  "-relief" "flat"
		  "-command" `(progn 
			      ((WID-bindings-menu-wid ,obj-val) 
			       		"enable" ',binding)
			      (destroy ,w)))
	  "-side" "left")

    (pack (button (& w ".menu.set") 
		  "-text" "Set binding"
		  "-relief" "flat"
		  "-command" `(bind ,obj-val ,binding ((widget ,w ".body.t")
						     "get" "1.0" "end")))
	  "-side" "left")
    (pack (frame (& w ".body") "-relief" "sunken" "-bd" 2)
	  "-fill" "both" "-expand" "yes" "-padx" 4 "-pady" 2)
    (pack (scrollbar (& w ".body.vsb") "-orient" "vertical")
	  "-side" "left" "-fill" "y")
    (pack (text (& w ".body.t") "-relief" "raised" "-bd" 2 "-width" 60 "-height" 8)
	  "-fill" "both" "-expand" "yes")
    ((widget w ".body.t") "insert" "1.0" (inspect::pretty-print body))))


(defun detail-widget-display (obj)
  (define obj-val (inspect::eval obj))
  (define w-str (detail-tl-str obj))
  (define id-w (widget w-str ".id"))
  (set-id-object id-w (->object obj))
  (set-id-value id-w (->object obj-val))
  (tk-setq (widget w-str ".options.class.l2") "-text" (winfo "class" obj-val))
  (define children-w (widget w-str ".options.children.e"))
  (tk-setq children-w "-state" "normal")
  (children-w "delete" 0 "end")
  (children-w "insert" 0 (winfo "children" obj-val))
  (tk-setq children-w "-state" "disabled")
  (let ((options-infos (obj-val "config"))
	(i 1))
    (for-each
     (lambda (infos)
       (if (= 5 (length infos))
	   (let ((option-w (widget w-str ".options.f" i ".e")))
	     (option-w "delete" 0 "end")
	     (option-w "insert" 0 (nth 4 infos))
	     (setq i (+ i 1)))))
     options-infos))
  (define menu-w (WID-bindings-menu-wid obj))
  (menu-w "delete" 0 "last")
  (for-each (lambda (binding) (WID-bindings-menu-add obj binding))
	    (bind obj-val))
  (menu-w "add" "separator")
  (for-each (lambda (binding) (WID-bindings-menu-add obj binding))
	    (bind (winfo "class" obj-val))))

(defun WID-eval-option (obj window)
  (let ((parent (winfo "parent" window)))
    (eval-string 
     (format NIL "(tk-setq ~a "-~a" ~s)"
	        obj
		(tk-get (widget parent ".l") "-text")
		(eval-string (window "get"))))))

(defun WID-quote-option (obj window)
 (let ((parent (winfo "parent" window)))
    (eval-string 
     (format NIL "(tk-setq ~a "-~a" ~s)"
	        obj
		(tk-get (widget parent ".l") "-text")
		(window "get")))))
