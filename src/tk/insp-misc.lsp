;******************************************************************************
;
; Project       : STk-inspect, a graphical debugger for STk
;
; File name     : inspect-misc.stk
; Creation date : Aug-30-1993
; Last update   : Sep-17-1993
;
;******************************************************************************
;
; This file contains definitions often used.
;
;******************************************************************************

(in-package "TK")

(provide "inspect-misc")

(defvar BITMAP_MENU 		(& "@" tk_library "/bitmaps/menu.bm"))
(defvar FIXED_FONT 		"-adobe-courier-bold-r-*-*-*-140-*-*-*-*-*-*")
(defvar MEDIUM_FONT 		"-adobe-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*")
(defvar BOLD_FONT 		"-adobe-helvetica-bold-r-*-*-*-120-*-*-*-*-*-*")
(defvar ITALIC-MEDIUM_FONT 	"-adobe-helvetica-medium-o-*-*-*-120-*-*-*-*-*-*")

(defvar COURIER_BR14 		"-adobe-courier-bold-r-*-*-*-140-*-*-*-*-*-*")
(defvar HELVETICA_BR12 		"-adobe-helvetica-bold-r-*-*-*-120-*-*-*-*-*-*")
(defvar HELVETICA_BO12 		"-adobe-helvetica-bold-o-*-*-*-120-*-*-*-*-*-*")
(defvar HELVETICA_MR12 		"-adobe-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*")
(defvar HELVETICA_MO12 		"-adobe-helvetica-medium-o-*-*-*-120-*-*-*-*-*-*")
(defvar HELVETICA_MO10 		"-adobe-helvetica-medium-o-*-*-*-100-*-*-*-*-*-*")
(defvar SCREEN_WIDTH		(winfo "vrootwidth"  *root*))
(defvar SCREEN_HEIGHT		(winfo "vrootheight" *root*))


;******************************************************************************
;
; General definitions and macros extending STk.
;
;******************************************************************************

;---- A special eval
(defun inspect::eval (x)
  (if (and (symbolp x) (symbol-bound? x)) (eval x) x))

;---- Predicates

(defmacro not-equal (x y) `(not (equal ,x ,y)))
(defmacro different? (x y) `(not (equal ,x ,y)))

;---- Operators

(defmacro <> (x y)
  `(not (= ,x ,y)))

;---- Display

(defun write\n (&rest l)
  (until (null l)
         (write (car l))
         (setq l (cdr l)))
  (newline))

(defun display\n (&rest l)
  (until (null l)
         (display (car l))
         (setq l (cdr l)))
  (newline))


;---- Control structures

(defmacro for (var test . body)
  `(do ,var
       ((not ,test))
       ,@body))

;---- Strings

(defun ->string (obj)
  (if (widget? obj)
      (widget->string obj)
      (format NIL "~A" obj)))

(defun ->object (obj)
  (if (widget? obj)
      (widget->string obj)
      (format NIL "~S" obj)))

(defun list->str (l)
  (if (null l)
      ""
      (let loop ((l l) (s ""))
        (let ((car-l (car l)) (cdr-l (cdr l)) (elem ()))
          (if (list? car-l)
              (setq elem (string-append "(" (list->str car-l) ")"))
              (setq elem (->string car-l)))
          (if (null cdr-l)
              (string-append s elem)
              (loop cdr-l (string-append s elem " ")))))))

;---- Vectors

(defun vector-index (v value)
  (let ((length (vector-length v))
        (index NIL))
    (for ((i (- length 1) (- i 1)))
         (>= i 0)
         (if (equal (vector-ref v i) value) (setq index i)))
    index))


;---- Lists

(defun list-first (obj lst)
  (defun _list-first (obj lst index)
    (cond ((null lst) NIL)
          ((equal obj (car lst)) index)
          (else (_list-first obj (cdr lst) (+ index 1)))))
  (_list-first obj lst 0))


(defmacro list-set! (lst index value)
  `(progn
     (setq ,lst (list->vector ,lst))
     (vector-set! ,lst ,index ,value)
     (setq ,lst (vector->list ,lst))))


(defun list-remove (obj lst)
  (defun _list-remove (obj lst prev-lst)
    (cond ((null lst) prev-lst)
          ((equal obj (car lst)) (append prev-lst (cdr lst)))
          (else (_list-remove obj (cdr lst) (append prev-lst
                                                    (list (car lst)))))))
  (_list-remove obj lst ()))


;---- Tk goodies

(defmacro widget (&rest etc)
  `(string->widget (& ,@etc)))

(defun && (&rest l)
  (if (null l)
      ""
      (let loop ((l l) (s ""))
        (if (null (cdr l))
            (string-append s (->string (car l)))
            (loop (cdr l) (string-append s (->string (car l)) " "))))))

(defmacro tki-get (canvas item option)
  `(nth 2 (funcall ,canvas 'itemconfigure ,item ,option)))

(defmacro tki-set (canvas item option value)
  `(funcall ,canvas 'itemconfigure ,item ,option ,value))

(defmacro @ (x y)
  `(& "@" ,x "," ,y))

;******************************************************************************
;
; 
;
;******************************************************************************

(define objects-infos-list ())

(defun object-infos (obj)  (assoc obj objects-infos-list))
(defun object-type (obj)   (nth 1 (object-infos obj)))
(defun object-symbol (obj) (nth 2 (object-infos obj)))

(defun add-object-infos (obj)
  (setq objects-infos-list
	(cons (list obj (inspect::typeof obj) (gensym "__g"))
	      objects-infos-list)))

(defun remove-object-infos (obj)
  (setq objects-infos-list
	(list-remove (object-infos obj) objects-infos-list)))

(defun find-object-infos (key)
  (let ((found NIL))
    (do ((l objects-infos-list (cdr l)))
	((or found (null l)) found)
      (when (equal (nth 2 (car l)) key) 
	    (setq found (nth 0 (car l)))))))

(defun detailer-type (obj-type)
  (case obj-type
    ((vector pair list) 'VPL)
    ((procedure) 'PROCEDURE)
    ((widget) 'WIDGET)
    (else 'UNKNOWN)))

(defun viewer-type (obj-type)
  (case obj-type
    ((procedure) 'PROCEDURE)
    ((widget) 'WIDGET)
    (else 'GENERAL)))

(defun update-object (obj)
  (let* ((obj-val (inspect::eval obj))
	 (old-type (object-type obj))
	 (obj-type (inspect::typeof obj-val)))
    (unless (equal old-type obj-type)
	    (let ((obj-sym (object-symbol obj)))
	      (remove-object-infos obj)
	      (setq objects-infos-list
		    (cons (list obj obj-type obj-sym) objects-infos-list))))
    (if (inspected? obj) (inspect-display obj))
    (if (detailed? obj)
	(if (equal (detailer-type old-type) (detailer-type obj-type))
	    (detail-display obj)
	    (progn
	      (undetail obj)
	      (if (different? 'UNKNOWN (detailer-type obj-type)) 
		  (detail obj)))))
    (if (viewed? obj)
	(if (equal (viewer-type old-type) (viewer-type obj-type))
	    (view-display obj)
	    (progn
	      (unview obj)
	      (view obj))))))

;---- Undebug

(defun undebug ()
  (for-each (lambda (obj-infos)
	      (let ((obj (car obj-infos)))
		(if (symbolp obj) (untrace-var obj))))
	    objects-infos-list)
  (destroy INSPECTOR_WIDGET_NAME)
  (setq inspected-objects-list ())
  (for-each (lambda (obj) (destroy (detail-tl-wid obj))) detailed-objects-list)
  (setq detailed-objects-list ())
  (for-each (lambda (obj) (destroy (view-tl-wid obj))) viewed-objects-list)
  (setq viewed-objects-list ())
  (setq objects-infos-list ()))

;---- id widget

(defun create-id-widget (str)
  (define wid (frame str))
  (pack (frame (& str ".f1")) "-side" "top" "-fill" "x")
  (pack (label (& str ".f1.l1") "-anchor" "w") "-side" "left")
  (pack (label (& str ".f1.l2") 
	       "-relief" "groove" "-bd" 2 "-anchor" "w" "-font" MEDIUM_FONT)
	"-fill" "x" "-expand" "yes")
  (pack (frame (& str ".f2")) "-side" "top" "-fill" "x")
  (pack (label (& str ".f2.l") "-anchor" "w") "-side" "left")
  (pack (entry (& str ".f2.e") "-relief" "sunken" "-bd" 2)
	"-fill" "x" "-expand" "yes")
  wid)

(defun set-id-label1 (wid text width) 
  ((widget wid ".f1.l1") 'config "-text" text "-width" width))
(defun set-id-label2 (wid text width)
  ((widget wid ".f2.l") 'config "-text" text "-width" width))

(defun set-id-object (wid text) (tk-setq (widget wid ".f1.l2") "-text" text))
(defun get-id-object (wid) (tk-get (widget wid ".f1.l2") "-text"))
(defun set-id-value (wid text)
  ((widget wid ".f2.e") "delete" 0 "end")
  ((widget wid ".f2.e") "insert" 0 text))
(defun get-id-value (wid) ((widget wid ".f2.e") 'get))


;---- menu widget

(defun create-menu-widget (str)
  (define wid (frame str "-relief" "raised" "-bd" 2))
  (pack (menubutton (& str ".help") "-text" "Help") "-side" "right")
  (tk-setq (widget str ".help") "-menu" (menu (& str ".help.m")))
  ((widget str ".help.m") "add" "command" "-label" "STk-inspect"
			  "-command" '(make-help STk-inspect-help))
  wid)


;---- toplevel widget

(defun create-toplevel-widget (str)
  (define wid (toplevel str))
  (pack (create-id-widget (& str ".id")) "-side" "top" "-fill" "x" "-padx" 4 "-pady" 2)
  (pack (create-menu-widget (& str ".menu"))
	"-side" "top" "-fill" "x" "-padx" 4 "-pady" 2)
  wid)

(defun inspect::shadow-entry (e)
  (tk-setq e "-state" "disabled")
  (tk-setq e "-bd" 1)
  (tk-setq e "-bg" "grey50")
  (tk-setq e "-fg" "grey95"))
  

(defun modifiable-object? (obj)
  (and (symbolp obj) (symbol-bound? obj) (not (widget? (inspect::eval obj)))))
