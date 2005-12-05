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

(defclass load-record-op (operation) ())

(defmethod operation-done-p ((o load-record-op) (c component))
  nil)

(defmethod component-depends-on ((o load-record-op) (c component))
  (let ((deps (component-original-depends-on c))
	(c-deps (call-next-method)))
    (when (and deps (not (typep c 'system)))
      (push `(load-record-op ,@deps) c-deps))
    c-deps))

(defun load-file-list (component)
  (let* ((op (make-instance 'load-record-op))
	 (steps (traverse op component)))
    (loop for (o . c) in steps
	  when (typep c 'cl-source-file)
	  collect c)))

(defclass build-op (compile-op)
  ((type :initarg :type :initform :fasl :accessor build-op-type)
   (monolithic :initarg :monolithic :initform t :accessor build-op-monolithic)
   (args :initarg :args :initform nil :accessor build-op-args)))

(defmethod initialize-instance :after ((instance build-op) &rest initargs &key &allow-other-keys)
  (setf (slot-value instance 'system-p) t)
  (let ((args (remove-keys '(type) (slot-value instance 'original-initargs)))
	(sub-type (build-op-type instance)))
    (case sub-type
      ((:fasl :dll :shared-library) (when (build-op-monolithic instance) (setf sub-type :lib)))
      (:program (setf sub-type :lib))
      (t))
    (setf (slot-value instance 'original-initargs)
	  (append `(:type ,sub-type)
		  args))))

(defmethod component-depends-on ((o build-op) (c component))
  (let ((deps (component-original-depends-on c))
	(c-deps (call-next-method)))
    (when deps
      (push `(build-op ,@deps) c-deps))
    c-deps))

(defmethod output-files ((o build-op) (c cl-source-file))
  (list (compile-file-pathname (component-pathname c) :type :object)))

(defun get-object-files (component)
  (loop for c in (load-file-list component)
	collect (car (output-files (make-instance 'build-op) c))))

(defmethod output-files ((o build-op) (c system))
  (list (merge-pathnames (component-pathname c)
			 (compile-file-pathname (component-name c) :type (build-op-type o)))))

(defmethod input-files ((o build-op) (c system))
  (append (get-object-files c)
	  (and (component-original-depends-on c)
	       (build-op-monolithic o)
	       (loop for d in (component-original-depends-on c)
		     collect (car (output-files (make-instance 'build-op :type :lib) (find-system d)))))))

(defmethod perform ((o build-op) (c system))
  (let ((obj-files (get-object-files c))
	(out-file (car (output-files o c)))
	(deps (component-original-depends-on c)))
    (when (and deps (build-op-monolithic o))
      (setq obj-files
	    (append (loop for d in deps
			  collect (make-symbol d))
		    obj-files)))
    (apply #'c::builder (build-op-type o) out-file :lisp-files obj-files (build-op-args o))))

(defmethod traverse ((o build-op) (c system))
  (let* ((load-tree (traverse (make-instance 'load-source-op :parent o) c))
	 (tree (call-next-method)))
    (append load-tree tree)))

(defun make-build (&rest args)
  (apply #'operate 'build-op args))

(dolist (sym '(build-op make-build))
  (export sym))
