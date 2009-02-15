;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPPROP Type propagation.

(in-package "COMPILER")

(defun type-from-array-elt (array)
  "Input is a lisp type representing a valid subtype of ARRAY. Output is
either the array element type or NIL, denoting that we are not able to
compute it. This version only handles the simplest cases."
  (cond ((eq array 'string)
         'character)
        ((eq array 'base-string)
         'base-char)
        ((member array '(array vector simple-vector simple-array))
         t)
        ((atom array)
         nil)
        ((not (member (first array) '(array vector simple-vector simple-array)))
         nil)
        ((null (rest array))
         t)
        (t
         (second array))))

(def-type-propagator si::aset (fname obj array &rest indices)
  (let* ((array-type (c1form-primary-type array))
         (elt-type (or (type-from-array-elt array) t)))
    (values (list* elt-type array-type (make-list (length indices) :initial-element 'si::index))
            elt-type)))

(def-type-propagator aref (fname array &rest indices)
  (let* ((array-type (c1form-primary-type array))
         (elt-type (or (type-from-array-elt array) t)))
    (values (list* array-type (make-list (length indices) :initial-element 'si::index))
            elt-type)))
