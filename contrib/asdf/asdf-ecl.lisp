;;; Copyright (c) 2005, Michael Goffioul (michael dot goffioul at swing dot be)
;;;
;;;   This program is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Library General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2 of the License, or (at your option) any later version.
;;;
;;;   See file '../../Copyright' for full details.
;;;
;;; ECL SPECIFIC OPERATIONS FOR ASDF
;;;

(in-package :asdf)

;;;
;;; COMPILE-OP / LOAD-OP
;;;
;;; We change these operations to produce both system and FASL files.
;;;

(defmethod initialize-instance :after ((instance compile-op) &key &allow-other-keys)
  (setf (slot-value instance 'system-p) t))

(defmethod output-files ((o compile-op) (c cl-source-file))
  (list (compile-file-pathname (component-pathname c) :type :object)))

(defmethod perform ((o load-op) (c cl-source-file))
  (loop for i in (input-files o c)
	collect (let ((output (compile-file-pathname i)))
		  (c:build-fasl output :lisp-files (list i))
		  (load output))))

(defmethod output-files ((o load-op) (c cl-source-file))
  (loop for i in (input-files o c)
	collect (compile-file-pathname i :type :fasl)))

;;;
;;; BUNDLE-OP
;;;

(defclass bundle-op (operation)
  ((type :initarg :type :initform :fasl :accessor bundle-op-type)
   (monolithic :initarg :monolithic :initform nil :accessor bundle-op-monolithic)
   (build-args :initarg :args :initform nil :accessor bundle-op-build-args)))

(defmethod initialize-instance :after ((instance bundle-op) &rest initargs &key &allow-other-keys)
  (setf (bundle-op-build-args instance)
	(remove-keys '(type monolithic) (slot-value instance 'original-initargs))))

(defun bundle-components (bundle system)
  ;; Using LOAD-OP we gather all the components that are to be compiled
  ;; and loaded, as well as the right ordering. Out of these components
  ;; we can select the ones we want.
  (let* ((op-list (traverse (make-instance 'load-op :force t) system)))
    (loop for (op . component) in op-list
	  when (and (typep op 'load-op)
		    (or (bundle-op-monolithic bundle)
			(eq (component-system component) system)))
	  collect component)))

(defmethod input-files ((o bundle-op) (c system))
  (loop for i in (bundle-components o c)
	with aux-op = (make-instance 'compile-op)
	nconc (output-files aux-op i)))

(defmethod output-files ((o bundle-op) (c system))
  (let ((name (component-name c)))
    (when (bundle-op-monolithic o)
      (setf name (concatenate 'base-string name "-mono")))
    (list
     (merge-pathnames (compile-file-pathname name :type (bundle-op-type o))
		      (component-relative-pathname c)))))

(defmethod perform ((o bundle-op) (c t))
  nil)

(defmethod perform ((o bundle-op) (c system))
  (let* ((object-files (input-files o c))
	 (output (first (output-files o c))))
    (apply #'c::builder (bundle-op-type o) output :lisp-files object-files
	   (bundle-op-build-args o))))

(defmethod traverse ((o bundle-op) (c system))
  (let* ((load-tree (traverse (make-instance 'compile-op :parent o) c))
	 (tree (call-next-method)))
    (append load-tree tree)))

(defun make-build (&rest args)
  (apply #'operate 'bundle-op args))

(export 'make-build)
(export 'build-op)
