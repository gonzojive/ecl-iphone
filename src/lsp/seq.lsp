;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                           sequence routines

(in-package "SYSTEM")

(defun error-sequence-type (type)
  (declare (si::c-local))
  (error 'simple-type-error
	 :datum type
	 :expected-type 'sequence
	 :format-control "~S does not specify a sequence type"
	 :format-arguments (list type)))

(defun error-sequence-length (type size)
  (declare (si::c-local))
  (error 'simple-type-error
	 :format-control
	 "Cannot create a sequence of size ~S which matches type ~S."
	 :format-arguments (list size type)
	 :expected-type type
	 :datum NIL))

(defun closest-vector-type (type)
  (let (elt-type length name args)
    (cond ((consp type)
	   (setq name (first type) args (cdr type)))
	  ((si::instancep type)
	   (setf name (class-name type) args nil))
	  (t
	   (setq name type args nil)))
    (case name
      ((VECTOR)
       (setq elt-type (if (endp args) '* (first args))
	     length (if (endp (rest args)) '* (second args))))
      ((SIMPLE-VECTOR)
       (setq elt-type 'T
	     length (if (endp args) '* (first args))))
      ((STRING SIMPLE-STRING)
       (setq elt-type 'BASE-CHAR
	     length (if (endp args) '* (first args))))
      ((BIT-VECTOR SIMPLE-BIT-VECTOR)
       (setq elt-type 'BIT
	     length (if (endp args) '* (first args))))
      ((ARRAY SIMPLE-ARRAY)
       (when (or (endp (rest args))
		 (atom (setq length (second args)))
		 (endp length)
		 (not (endp (rest length))))
	 (error-sequence-type type))
       (setq elt-type (upgraded-array-element-type (first args))
	     length (first (second args))))
      (t
       ;; We arrive here when the sequence type is not easy to parse.
       ;; We give up trying to guess the length of the sequence.
       ;; Furthermore, we also give up trying to find if the element
       ;; type is *. Instead we just compare with some specialized
       ;; types and otherwise fail.
       (dolist (i '((SIMPLE-STRING . BASE-CHAR)
		    (STRING . BASE-CHAR)
		    (BIT-VECTOR . BIT)
		    ((VECTOR EXT::BYTE8) . EXT::BYTE8)
		    ((VECTOR EXT::INTEGER8) . EXT::INTEGER8)
		    ((VECTOR FIXNUM) . FIXNUM)
		    ((VECTOR SHORT-FLOAT) . SHORT-FLOAT)
		    ((VECTOR LONG-FLOAT) . LONG-FLOAT)
		    ((VECTOR T) . T))
		(if (subtypep type 'vector)
		    ;; Does this have to be a type-error?
		    ;; 17.3 for MAKE-SEQUENCE says it should be an error,
		    ;; but does not specialize what kind.
		    (error 'simple-type-error
			   :datum type
			   :expected-type 'sequence
			   :format-control "Cannot find the element type in sequence type ~S"
			   :format-arguments (list type))
		    (error-sequence-type type)))
	  (when (subtypep type (car i))
	    (setq elt-type (cdr i) length '*)
	    (return)))))
    (values elt-type length)))

(defun make-sequence (type size	&key (initial-element nil iesp) &aux sequence)
  "Args: (type length &key initial-element)
Creates and returns a sequence of the given TYPE and LENGTH.  If INITIAL-
ELEMENT is given, then it becomes the elements of the created sequence.  The
default value of INITIAL-ELEMENT depends on TYPE."
  (if (subtypep type 'LIST)
      (progn
	(cond ((subtypep 'LIST type)
	       (make-list size :initial-element initial-element))
	      ((subtypep type 'NIL)
	       (error-sequence-type type))
	      ((subtypep type 'NULL)
	       (unless (zerop size)
		 (error-sequence-length type size)))
	      ((subtypep type 'CONS)
	       (when (zerop size)
		 (error-sequence-length type size))))
	(make-list size :initial-element initial-element))
      (multiple-value-bind (element-type length)
	  (closest-vector-type type)
	(setq sequence (sys:make-vector (if (eq element-type '*) T element-type)
					size nil nil nil nil))
	(unless (or (eql length '*) (eql length size))
	  (error-sequence-length type size))
	(when iesp
	  (do ((i 0 (1+ i))
	       (size size))
	      ((>= i size))
	    (declare (fixnum i size))
	    (setf (elt sequence i) initial-element)))
	sequence)))

(defun make-seq-iterator (sequence &optional (start 0))
  (cond ((null start)
	 (setf start 0))
	((not (integerp start))
	 (error "Value ~A is not a valid index into sequence ~A" start sequence)))
  (cond ((>= start (length sequence))
	 nil)
	((consp sequence)
	 (nthcdr start sequence))
	(t
	 start)))

(defun seq-iterator-ref (sequence iterator)
  (if (si::fixnump iterator)
      (elt sequence iterator)
      (first iterator)))

(defun seq-iterator-set (sequence iterator value)
  (if (si::fixnump iterator)
      (setf (elt sequence iterator) value)
      (setf (first iterator) value)))

(defun seq-iterator-next (sequence iterator)
  (if (fixnump iterator)
      (and (< (incf iterator) (length sequence))
	   iterator)
      (rest iterator)))

(defun concatenate (result-type &rest sequences)
  "Args: (type &rest sequences)
Returns a new sequence of the specified type, consisting of all elements of
SEQUENCEs."
  (do ((x (make-sequence result-type
			 (apply #'+ (mapcar #'length sequences))))
       (s sequences (cdr s))
       (i 0))
      ((null s) x)
    (declare (fixnum i))
    (do ((j 0 (1+ j))
         (n (length (car s))))
        ((>= j n))
      (declare (fixnum j n))
      (setf (elt x i) (elt (car s) j))
      (incf i))))


(defun map (result-type function sequence &rest more-sequences)
  "Args: (type function sequence &rest more-sequences)
Creates and returns a sequence of TYPE with K elements, with the N-th element
being the value of applying FUNCTION to the N-th elements of the given
SEQUENCEs, where K is the minimum length of the given SEQUENCEs."
  (setq more-sequences (cons sequence more-sequences))
  (let ((l (apply #'min (mapcar #'length more-sequences))))
    (if (null result-type)
        (do ((i 0 (1+ i))
             (l l))
            ((>= i l) nil)
          (declare (fixnum i l))
          (apply function (mapcar #'(lambda (z) (elt z i))
                                  more-sequences)))
        (let ((x (make-sequence result-type l)))
          (do ((i 0 (1+ i))
               (l l))
              ((>= i l) x)
            (declare (fixnum i l))
            (setf (elt x i)
                  (apply function (mapcar #'(lambda (z) (elt z i))
                                          more-sequences))))))))

(eval-when (eval compile)
(defmacro def-seq-bool-parser (name doc test end-value)
 `(defun ,name (predicate sequence &rest more-sequences)
    ,doc
    (setq more-sequences (cons sequence more-sequences))
    (do ((it (mapcar #'make-seq-iterator more-sequences))
         (val (make-sequence 'list (length more-sequences))))
        (nil)
      (declare (optimize (safety 0)))
      (do ((i it (cdr i))
           (v val (cdr v))
	   (s more-sequences (cdr s)))
          ((null i))
        (unless (car i) (return-from ,name ,end-value))
        (rplaca v (seq-iterator-ref (car s) (car i)))
        (rplaca i (seq-iterator-next (car s) (car i))))
      (let ((that-value
             (apply predicate val)))
        ,test)))))

(def-seq-bool-parser some
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs satisfies PREDICATE;
NIL otherwise."
  (when that-value (return that-value))
  nil)

(def-seq-bool-parser every
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE; NIL otherwise."
  (unless that-value (return nil))
  t)

#|
(def-seq-bool-parser notany
  "Args: (predicate sequence &rest more-sequences)
Returns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL
otherwise."
  (when that-value (return nil))
  t)

(def-seq-bool-parser notevery
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs does not satisfy
PREDICATE; NIL otherwise."
  (unless that-value (return t))
  nil)
|#

(defun every* (predicate &rest sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE and all sequences
have the same length; NIL otherwise."
  (and (apply #'= (mapcar #'length sequences))
       (apply #'every predicate sequences)))


(defun notany (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL
otherwise."
  (not (apply #'some predicate sequence more-sequences)))


(defun notevery (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs does not satisfy
PREDICATE; NIL otherwise."
  (not (apply #'every predicate sequence more-sequences)))

(defun map-into (result-sequence function &rest sequences)
  (let ((nel (apply #'min (if (vectorp result-sequence)
			      (array-dimension result-sequence 0)
			      (length result-sequence))
		    (mapcar #'length sequences))))
    (declare (fixnum nel))
    ;; Set the fill pointer to the number of iterations
    (when (and (vectorp result-sequence)
	       (array-has-fill-pointer-p result-sequence))
      (setf (fill-pointer result-sequence) nel))
    ;; Perform mapping
    (dotimes (k nel result-sequence)
      (declare (fixnum nel))
      (setf (elt result-sequence k)
	    (apply function (mapcar #'(lambda (v) (elt v k)) sequences))))))
