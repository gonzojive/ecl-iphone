;******************************************************************************
;
; Project       : STk-inspect, a graphical debugger for STk.
;
; File name     : inspect-view.stk
; Creation date : Aug-30-1993
; Last update   : Sep-17-1993
;
; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;
;******************************************************************************
;
; This file implements the different sort of "Viewers".
;
;******************************************************************************

(in-package "TK")

(provide "inspect-view")

(defun view-tl-wid (obj) (widget VIEW_WIDGET_NAME (object-symbol obj)))
(defun view-tl-str (obj) (& VIEW_WIDGET_NAME (object-symbol obj)))
(defun view-l-wid (obj) (widget (view-tl-str obj) ".id.f1.l2"))
(defun view-l-str (obj) (& (view-tl-str obj) ".id.f1.l2"))
(defun view-e-wid (obj) (widget (view-tl-str obj) ".id.f2.e"))
(defun view-e-str (obj) (& (view-tl-str obj) ".id.f2.e"))
(defun view-m-wid (obj) (widget (view-tl-str obj) ".menu.command.m"))
(defun view-m-str (obj) (& (view-tl-str obj) ".menu.command.m"))
(defun view-c-wid (obj) (widget (view-tl-str obj) ".f3.c"))
(defun view-c-str (obj) (& (view-tl-str obj) ".f3.c"))


;---- Viewer menu -------------------------------------------------------------

(defun view-menu-Eval (obj)
  (set obj (eval (funcall (view-e-wid obj) "get")))))

(defun view-menu-Quote (obj)
  (set obj (funcall (view-e-wid obj) "get")))

(defun view-menu-Inspect (key)
  (let ((obj (find-object-infos key)))
    (inspect obj)
    (funcall (widget (view-tl-str obj) ".menu.command.m") "disable" "Inspect")
    (if (detailed? obj) (funcall (detail-m-wid obj) "disable" "Inspect"))))

(defun view-menu-Detail (key)
  (let ((obj (find-object-infos key)))
    (detail obj)
    (funcall (widget (view-tl-str obj) ".menu.command.m") "disable" "Detail")
    (if (inspected? obj) (funcall (inspect-m-wid obj) "disable" "Detail"))))

(defun view-menu-Unview (key) 
  (unview (find-object-infos key)))


;---- Viewer ------------------------------------------------------------------

(define VIEW_WIDGET_NAME ".viewer")
(define viewed-objects-list ())

(defun viewed? (obj) (member obj viewed-objects-list))

(defun view (obj)
  (unless (viewed? obj) (view-object obj)))

(defun view-object (obj)
  (setq viewed-objects-list (cons obj viewed-objects-list))
  (unless (object-infos obj)
	  (add-object-infos obj)
	  (if (symbolp obj) (trace-var obj `(update-object ',obj))))
  (view-create obj))

(defun unview (obj)
  (when (viewed? obj) (unview-object obj)))

(defun unview-object (obj)
  (let ((top (view-tl-wid obj)))
    (setq viewed-objects-list (list-remove obj viewed-objects-list))
    (if (inspected? obj) (funcall (inspect-m-wid obj) "enable" "View"))
    (if (detailed? obj) (funcall (detail-m-wid obj) "enable" "View"))
    (unless (or (inspected? obj) (detailed? obj))
	    (remove-object-infos obj)
	    (if (symbolp obj) (untrace-var obj)))
    ;; If toplevel exists (i.e. it is not a <Destroy> event) destroy it
    (if (= (winfo "exists" top) 1)
	(destroy top))))

(defun view-create (obj)
  (let ((obj-val (inspect::eval obj)))
    (case (inspect::typeof obj-val)
      ((widget)  (when (= (winfo "exists" (view-tl-wid obj-val)) 0)
			 (view-widget-create obj-val)))
      ((closure) (view-procedure-create obj))
      (else      (view-object-create obj)))))

(defun view-display (obj)
  (case (object-type obj)
    ((widget) (view-widget-display (inspect::eval obj)))
    ((closure) (view-procedure-display obj))
    (else (view-object-display obj))))


;---- Object/Procedure viewer -------------------------------------------------

(define CAR_COLOR "gray90")
(define CDR_COLOR "gray70")
(define ARROW_COLOR "black")
(define TEXT_COLOR "black")

(defun highlightItem (canvas color1 color2)
(let ((item (car (funcall canvas "find" "withtag" "current"))))
    (if (equal (tki-get canvas item "-fill") color1)
	(tki-set canvas item "-fill" color2)
	(tki-set canvas item "-fill" color1))))

(defun find-car/cdr (fct count l)
  (defun _find-car/cdr (fct count l path)
    (if (not (consp l))
	(if (null path)
	    NIL
	    (_find-car/cdr fct count (caar path) (cdr path)))
	(if (equal 0 count)
	    (fct l)
	    (_find-car/cdr fct (- count 1) (cdr l) (cons l path)))))
  (_find-car/cdr fct count l ()))

(defun double1OnCar (obj)
  (let* ((canvas (view-c-wid obj))
	 (item (car (funcall canvas "find" "withtag" "current")))
	 (cars (funcall canvas "find" "withtag" 'CAR)))
    (view (find-car/cdr car (list-first item cars) (inspect::eval obj)))))

(defun double1OnCdr (canvas obj)
  (let ((item (car (funcall canvas "find" "withtag" "current")))
	(cdrs (funcall canvas "find" "withtag" 'CDR)))
    (view (find-car/cdr cdr (list-first item cdrs) (inspect::eval obj)))))

(defun text-width (text font)
  (canvas ".text-width")
  (define bbox
    (.text-width "bbox" (.text-width "create" "text" 0 0 "-text" text "-font" font)))
  (destroy .text-width)
  (- (caddr bbox) (car bbox)))

(defun view-create-toplevel (obj)
  (define w (create-toplevel-widget (view-tl-str obj)))
  (define id-w (widget w ".id"))
  (set-id-label1 id-w "Object" 6)
  (set-id-label2 id-w "Value" 6)

  (define menu-w (widget w ".menu"))
  (funcall (widget w ".menu.help.m") "add" "command" "-label" "Viewer"
			     "-command" '(make-help Viewer-help))
  (pack (menubutton (& menu-w ".command") "-text" "Command") "-side" "left")
  (define cmd-w (eval (menu (& menu-w ".command.m"))))
  (tk-setq (widget menu-w ".command") "-menu" cmd-w)
  (cmd-w "add" "command" "-label" "Inspect" 
	 	       "-command" `(view-menu-Inspect ',(object-symbol obj)))
  (if (inspected? obj) (cmd-w "disable" "Inspect"))
  (cmd-w "add" "command" "-label" "Detail" 
	 		"-command" `(view-menu-Detail ',(object-symbol obj)))
  (if (detailed? obj) (cmd-w "disable" "Detail"))
  (cmd-w "add" "command" "-label" "Unview" 
	 	       "-command" `(view-menu-Unview ',(object-symbol obj)))

  (if (modifiable-object? obj)
      (begin
	(bind (widget w ".id.f2.e") "<Return>" `(view-menu-Eval ',obj))
	(bind (widget w ".id.f2.e") "<Shift-Return>" `(view-menu-Quote ',obj)))
      (begin
	(funcall (view-e-wid obj) "insert" 0 (format NIL "~S" (inspect::eval obj)))
	(inspect::shadow-entry (widget w ".id.f2.e"))))


  (pack (frame (& w ".f3") "-relief" "sunken" "-bd" 2)
	"-fill" "both" "-expand" "yes" "-padx" 4 "-pady" 2)
  (pack (scrollbar (& w ".f3.vsb") "-orient" "vertical")
	"-side" "left" "-fill" "y")
  (pack (scrollbar (& w ".f3.hsb") "-orient" "horizontal")
	"-side" "bottom" "-fill" "x")
  (pack (canvas (view-c-str obj) "-relief" "raised" "-bd" 2)
	"-fill" "both" "-expand" "yes")
  (tk-setq (widget w ".f3.vsb") "-command" (& (view-c-str obj) " \"yview\""))
  (tk-setq (widget w ".f3.hsb") "-command" (& (view-c-str obj) " \"xview\""))
  (tk-setq (view-c-wid obj) "-yscroll" (& w ".f3.vsb \"set\""))
  (tk-setq (view-c-wid obj) "-xscroll" (& w ".f3.hsb \"set\""))
  (bind w "<Destroy>" `(view-menu-Unview ',(object-symbol obj)))
  w)

(defun view-object/procedure-create (obj)
  (let ((w (view-create-toplevel obj))
	(c (view-c-wid obj))
	(c-name (widget-name c)))
    (declare (special w))
    (wm "title" w "Object viewer")
    (wm "maxsize" w SCREEN_WIDTH SCREEN_HEIGHT)
    (funcall c "bind" 'CAR "<Enter>" `(highlightItem ,c-name CAR_COLOR "red"))
    (funcall c "bind" 'CAR "<Leave>" `(highlightItem ,c-name CAR_COLOR "red"))
    (funcall c "bind" 'CAR "<Double-1>" `(double1OnCar ',obj))
    (funcall c "bind" 'CDR "<Enter>" `(highlightItem ,c-name CDR_COLOR "blue"))
    (funcall c "bind" 'CDR "<Leave>" `(highlightItem ,c-name CDR_COLOR "blue"))
    (funcall c "bind" 'CDR "<Double-1>" `(double1OnCdr ,c-name ',obj))
    w))

(defun view-object-create (obj)
  (let ((w (view-object/procedure-create obj)))
    (declare (special w))
    (view-object-display obj)))

(defun view-object-display (obj)
  (wm "title"  (view-tl-wid obj) "Object viewer")
  (define obj-val (inspect::eval obj))
  (tk-setq (view-l-wid obj) "-text" (->object obj))
  (funcall (view-e-wid obj) "delete" 0 "end")
  (funcall (view-e-wid obj) "insert" 0 (->object obj-val))
  (view-object/procedure-display (view-c-wid obj) obj-val)) 

(defun view-procedure-create (obj)
  (let ((w (view-object/procedure-create obj)))
    (declare (special w))
    (view-procedure-display obj)))

(defun view-procedure-display (obj)
  (wm "title"  (view-tl-wid obj) "Procedure viewer")
  (define obj-val (inspect::eval obj))
  (tk-setq (view-l-wid obj) "-text" (->object obj))
  (funcall (view-e-wid obj) "delete" 0 "end")
  (funcall (view-e-wid obj) "insert" 0 (->object obj-val))
  (view-object/procedure-display (view-c-wid obj) (procedure-body obj-val)))

(defun view-object/procedure-display (c obj-val)
  (define grid-h 60) ; horizontal spacing between grid lines
  (define grid-v 40) ; vertical spacing between grid lines
  (define cons-h 40) ; horizontal size of cons cell
  (define cons-v 20) ; vertical size of cons cell
  (define cons-h/2 (quotient cons-h 2))
  (define cons-v/2 (quotient cons-v 2))
  (define arrow-space 2) ; space between arrow and box
  (defun x-h (x) (* x grid-h))
  (defun y-v (y) (* y grid-v))
  (define font "-adobe-helvetica-bold-r-*-*-*-120-*-*-*-*-*-*")

  (defun draw-cons-cell (x y)
    (let ((h (x-h x)) (v (y-v y)))
      (c "create" 'rectangle h v (+ h cons-h/2 1) (+ v cons-v)
	 "-fill" CAR_COLOR "-tag" 'CAR)
      (c "create" 'rectangle (+ h cons-h/2) v (+ h cons-h) (+ v cons-v)
	 "-fill" CDR_COLOR "-tag" 'CDR)))

  (defun car-arrow-pos (x y d)
    (let ((h (x-h x)) (v (y-v y)))
      (list (+ h (quotient cons-h 4)) (+ v cons-v/2) (+ h (quotient cons-h 4))
	    (+ v cons-v/2 (- (* d grid-v) (+ cons-v/2 arrow-space))))))

  (defun draw-car-arrow (x y d) ; draw arrow downwards 'd' grid squares
    (let ((pos (car-arrow-pos x y d)))
      (if (and (= x 0) (= y 0))
	  (eval `(funcall ,c "create" "line" ,@pos "-arrow" "last" "-arrowshape" "8 8 3"))
	  (eval `(funcall ,c "create" "line" ,@pos "-arrow" "last" "-arrowshape" "8 8 3"
		     "-tag" 'CAR_ARROW)))))

  (defun draw-car-text (x y d text)
    (let ((pos (car-arrow-pos x y d)))
      (if (<= (text-width text font) grid-h)
	  (c "create" "text" (caddr pos) (cadddr pos)
	     "-anchor" "n" "-font" font "-text" text "-tag" 'CAR_TEXT)
	  (let* ((text-l (label (& c "." (gensym "__g"))
				:relief "groove" "-bd" 2
				:text text "-anchor" "w" "-font" font))
		 (item (c "create" 'window (caddr pos) (+ 2 (cadddr pos))
			  "-window" text-l "-anchor" "n" "-width" (- grid-h 2)
			  "-tags" 'LONG_CAR_TEXT)))
	    (bind text-l "<Enter>" 
		  `(funcall ,(widget-name c) 'itemconfig ,item
				     "-width" ,(+ 3 (text-width text font))))
	    (bind text-l "<Leave>" 
		  `(funcall ,(widget-name c) 'itemconfig ,item
				     "-width" ,(- grid-h 2)))))))

  (defun cdr-arrow-pos (x y d)
    (let ((h (x-h x)) (v (y-v y)))
      (list (+ h (quotient (* cons-h 3) 4)) (+ v cons-v/2)
	    (+ h (quotient (* cons-h 3) 4)
	       (- (* d grid-h) (+ (quotient (* cons-h 3) 4) arrow-space)))
	    (+ v cons-v/2))))
  
  (defun draw-cdr-arrow (x y d) ; draw arrow to the right 'd' grid squares
    (let ((pos (cdr-arrow-pos x y d)))
      (eval `(funcall ,c "create" "line" ,@pos "-arrow" "last" "-arrowshape" "8 8 3"
		 "-tag" 'CDR_ARROW))))

  (defun draw-cdr-text (x y d text)
    (let ((pos (cdr-arrow-pos x y d)))
      (c "create" "text" (caddr pos) (cadddr pos)
	 "-anchor" "w" "-font" font "-text" text "-tag" 'CDR_TEXT)))

  (defun draw-nil (x y) ; draw nil in cdr of cons cell
    (let ((h (x-h x)) (v (y-v y)))
      (c "create" "line" (+ h cons-h/2) v (+ h cons-h) (+ v cons-v))
      (c "create" "line" (+ h cons-h/2) (+ v cons-v -1) (+ h cons-h) (- v 1))))

  (defun object-length (obj-val)
    (cond ((null obj-val) 0)
	  ((consp obj-val) (+ 1 (object-length (cdr obj-val))))
	  (else (+ 1 (quotient (text-width (->object obj-val) font)
				 grid-h)))))
    
  (defun initial-profile () 0)
  (defun car-profile (p) (if (consp p) (car p) p))
  (defun cdr-profile (p) (if (consp p) (cdr p) p))

  (defun make-profile (len p)
    (defun fit1 (len p)
      (if (> len 1)
	  (let ((p* (fit1 (- len 1) (cdr-profile p))))
	    (cons (car-profile p*) p*))
	  (fit2 (+ (car-profile p) 1) p)))
    (defun fit2 (y p)
      (if (consp p)
	  (cons (max y (car-profile p)) (fit2 y (cdr-profile p)))
	  (max y p)))
    (fit1 len p))

  (defun draw-list (lst x y p)
    (draw-cons-cell x y)
    (let* ((tail (cdr lst))
	   (tail-p (cdr-profile p))
	   (new-p (cond ((null tail)
			 (draw-nil x y)
			 tail-p)
			((consp tail)
			 (draw-cdr-arrow x y 1)
			 (draw-list tail (+ x 1) y tail-p))
			(else
			 (draw-cdr-arrow x y 1)
			 (draw-cdr-text x y 1 (->object tail))
			 tail-p))))
      (draw-object (car lst) x y (cons (car-profile p) new-p))))

  (defun draw-object (obj-val x y p)
    (if (consp obj-val)
        (let* ((len (object-length obj-val))
	       (new-p (make-profile len p))
	       (yy (car-profile new-p)))
	  (draw-car-arrow x y (- yy y))
	  (draw-list obj-val x yy new-p))
	(let ((text (->object obj-val)))
          (draw-car-arrow x y 1)
	  (draw-car-text x y 1 text)
          (make-profile 1 p))))

  (c "delete" "all")
  (draw-object obj-val 0 0 (initial-profile))
  (adjust-scrollregion c 20))


;---- Widget viewer -----------------------------------------------------------

(define show-widget
  (let ((bg-color ())
	(box-color ()))
    (lambda (obj item press)
      (let* ((canv-w (view-c-wid (inspect::eval obj)))
	     (tags (funcall canv-w 'gettags item))
	     (wid (inspect::eval (nth 1 tags))))
	(if press
	    (progn
	      (setq box-color (tki-get canv-w item "-fill"))
	      (setq bg-color (tk-get wid "-bg"))
	      (tki-set canv-w item "-fill" "magenta")
	      (tk-setq wid "-bg" "magenta"))
	    (progn
	      (tki-set canv-w item "-fill" box-color)
	      (tk-setq wid "-bg" bg-color)))))))

(defun inspect-sub-widget (obj who)
  (catch-errors 
   (inspect (inspect::eval (nth 1 (funcall (view-c-wid obj) 'gettags who))))))

(defun view-widget-create (obj)
  (define w (view-create-toplevel obj))
  (define obj-val (inspect::eval obj))
  (wm "maxsize" w SCREEN_WIDTH SCREEN_HEIGHT)
  (pack (frame (& w ".menu.level")) "-side" "left")
  (pack (label (& w ".menu.level.l") "-text" "Level") "-side" "left")
  (pack (entry (& w ".menu.level.e") "-relief" "sunken" "-bd" 2 "-width" 4)
	"-side" "left")
  (funcall (widget w ".menu.level.e") "insert" 0 9999)
  (bind (widget (view-tl-str obj) ".menu.level.e") "<Return>"
	`(view-widget-modify-level ',(object-symbol obj)))

  (define c (view-c-wid obj))
  (c "bind" '|CLASS| "<Double-1>" 
     `(inspect-sub-widget ,(widget-name obj-val) 
			  "current"))
  (c "bind" '|CLASS_NAME| "<Double-1>" 
     `(inspect-sub-widget ,(widget-name obj-val) 
			  (car (funcall ,(widget-name c) "find" "below" "current"))))

  (c "bind" '|CLASS| "<ButtonPress-1>" 
     		    `(show-widget ,(widget-name obj-val) "current" T))
  (c "bind" '|CLASS| "<ButtonRelease-1>" 
		    `(show-widget ,(widget-name obj-val) "current" NIL))
  (c "bind" '|CLASS_NAME| "<ButtonPress-1>"
     `(show-widget ,(widget-name obj-val)
		   (car (funcall ,(widget-name c) "find" "below" "current")) T))
  (c "bind" '|CLASS_NAME| "<ButtonRelease-1>"
     `(show-widget ,(widget-name obj-val) 
		   (car (funcall ,(widget-name c) "find" "below" "current")) NIL))
  (view-widget-display obj))

(defun view-widget-set-level (obj level)
  (funcall (widget (view-tl-str obj) ".menu.level.e") "delete" 0 "end")
  (funcall (widget (view-tl-str obj) ".menu.level.e") "insert" 0 level))

(defun view-widget-get-level (obj)
  (let ((level (funcall (widget (view-tl-str obj) ".menu.level.e") 'get)))
    (if (equal "" level) 9999 (string->number level))))

(defun view-widget-modify-level (key)
  (let ((obj (find-object-infos key)))
    (unless (view-widget-get-level obj) (view-widget-set-level obj 9999))
    (view-widget-clear obj)
    (view-widget-display obj)))

(defun get-children (wid)
  (let ((children (winfo 'children wid)))
    (if (listp children) children (list children))))

(defun view-widget-clear (obj) (funcall (view-c-wid obj) "delete" "all"))

(defun view-widget-display (obj)
  (wm "title" (view-tl-wid obj) "Widget viewer")
  (define obj-wid obj)
  (define canv (view-c-wid obj))
  (define h-grid 60)
  (define v-grid 40)
  (define h-box 80) (define h-box/2 (/ h-box 2))
  (define v-box 20) (define v-box/2 (/ v-box 2))
  (define y-global 40)
  (define level (view-widget-get-level obj))
  (define level-min level)
  (defun _display (wid x level)
    (let* ((name (winfo 'name wid))
	   (class (winfo 'class wid))
;	   (children (winfo 'children wid))
	   (children (get-children wid))
	   (y y-global))
      (funcall canv "create" 'rectangle (- x h-box/2) (- y v-box) (+ x h-box/2) y 
	    "-fill" "gray90" "-tags" (format NIL "CLASS ~a" (->string wid)))
      (funcall canv "create" "text" x (- y v-box/2)
	    "-anchor" "center" "-text" class "-font" HELVETICA_MO12 
	    "-tags" "CLASS_NAME")
      (funcall canv "create" "text" (+ x h-box/2 10) (- y v-box/2)
	    "-anchor" "w" "-text" name "-font" HELVETICA_BR12)
      (if (null children)
	  (setq level-min (min level level-min))
	  (if (> level 0)
	      (let ((y-child y))
		(for-each
		 (lambda (child)
		   (setq y-global (+ y-global v-grid))
		   (setq y-child y-global)
		   (_display child (+ x h-grid) (- level 1)))
		 children)
		(funcall canv "create" "line" x y x (- y-child v-box/2)))
	      (progn
		(setq level-min 0)
		(funcall canv "create" "line" x y x (+ y v-box/2) "-stipple" "gray50"))))
      (unless (equal obj-wid wid)
	      (funcall canv "create" "line" 
		    (- x h-box/2) (- y v-box/2) (- x h-grid) (- y v-box/2)))))

  (set-id-object (& (view-tl-str obj) ".id") (format NIL "~S" obj))
  (set-id-value (& (view-tl-str obj) ".id") (format NIL "~S" (inspect::eval obj)))
  (funcall (view-c-wid obj) "delete" "all")
  (_display obj-wid 0 level)
  (view-widget-set-level obj (- level level-min))
  (adjust-scrollregion canv 20))

(defun adjust-scrollregion (canv offset)
  (multiple-value-bind (x1 y1 x2 y2)
      (funcall canv "bbox" "all")
    (tk-setq canv "-scrollregion"
	     (&& (- x1 offset) (- y1 offset)
		 (+ x2 offset) (+ y2 offset))))
  (funcall canv "xyview" 0)
  (funcall canv "yview" 0))

(defun view-widget (obj)
  (view-widget-create obj)
  (view-widget-display obj))
