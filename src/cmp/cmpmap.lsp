;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPMAP  Map functions.

(in-package "COMPILER")

#+nil
(progn

(defun map-apply-function (fname args)
  (mapcar #'(lambda (x) `(,fname ,x)) args))

(defun expand-mapcar (function args car-p)
  (let* ((handy (gensym))
	 (output (gensym))
	 (handies (mapcar #'(lambda (x) (gensym)) args))
	 (test-end `(OR ,@(map-apply-function 'ENDP handies)))
	 (values (if car-p (map-apply-function 'CAR handies) handies))
	 (cdrs (map-apply-function 'CDR handies)))
    (my-pprint
    `(do* ((,output (cons nil nil))
	   (,handy ,output)
	   ,@(mapcar #'list handies args cdrs))
      (,test-end (cdr ,output))
      (setf ,handy (setf (cdr ,handy) (cons nil nil)))
      (setf (car ,handy) (funcall ,function ,@values))
      )))
)

(define-compiler-macro mapcar (fname first-arg &rest args)
  (expand-mapcar fname (list* first-arg args) t))

(define-compiler-macro maplist (fname first-arg &rest args)
  (expand-mapcar fname (list* first-arg args) nil))

(defun expand-mapc (function args car-p)
  (let* ((output (gensym))
	 (handies (mapcar #'(lambda (x) (gensym)) args))
	 (test-end `(OR ,@(map-apply-function 'ENDP handies)))
	 (values (if car-p (map-apply-function 'CAR handies) handies))
	 (cdrs (map-apply-function 'CDR handies)))
    (my-pprint
    `(do* (,@(mapcar #'list handies args cdrs)
	   (output ,(first handies)))
        (,test-end output)
      (funcall ,function ,@values))))
)

(define-compiler-macro mapc (fname first-arg &rest args)
  (expand-mapc fname (list* first-arg args) t))

(define-compiler-macro mapl (fname first-arg &rest args)
  (expand-mapc fname (list* first-arg args) nil))

(defun expand-mapcan (function args car-p)
  (let* ((handy (gensym))
	 (value (gensym))
	 (output (gensym))
	 (handies (mapcar #'(lambda (x) (gensym)) args))
	 (test-end `(OR ,@(map-apply-function 'ENDP handies)))
	 (values (if car-p (map-apply-function 'CAR handies) handies)))
    (my-pprint
    `(do* (,value
	   (,output (cons nil nil))
	   (,handy ,output)
	   ,@(mapcar #'list handies args))
      (,test-end (cdr ,output))
      (when (setf value (funcall ,function ,@values))
	(setf (cdr ,handy) ,value)
	(setf ,handy (last ,value))))))
)

(define-compiler-macro mapcan (fname first-arg &rest args)
  (expand-mapcan fname (list* first-arg args) t))

(define-compiler-macro mapcon (fname first-arg &rest args)
  (expand-mapcan fname (list* first-arg args) nil))

)