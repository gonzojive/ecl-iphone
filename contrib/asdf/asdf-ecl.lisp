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
   (monolithic :initarg :monolithic :initform nil)
   (name-suffix :initarg :name-suffix :initform nil)
   (build-args :initarg :args :initform nil :accessor bundle-op-build-args)))

(defclass lib-op (bundle-op)
  ((type :initarg :type :initform :lib :accessor bundle-op-type)))

(defclass dll-op (bundle-op)
  ((type :initarg :type :initform :dll :accessor bundle-op-type)))

(defmethod bundle-op-monolithic ((bundle bundle-op))
  (or (eq (bundle-op-type bundle) :program)
      (slot-value bundle 'monolithic)))

(defmethod initialize-instance :after ((instance bundle-op) &rest initargs
				       &key (name-suffix nil name-suffix-p) &allow-other-keys)
  (unless name-suffix-p
    (setf (slot-value instance 'name-suffix)
	  (if (and (slot-value instance 'monolithic)
		   (not (eq (slot-value instance 'type) :program)))
	      "-mono"
	      "")))
  (setf (bundle-op-build-args instance)
	(remove-keys '(type monolithic name-suffix)
		     (slot-value instance 'original-initargs))))

(defun gather-components (op-type system &key gather-all include-self)
  ;; This function creates a list of components, matched together with an
  ;; operation. This list may be restricted to sub-components of SYSTEM if
  ;; GATHER-ALL = NIL (default), and it may include the system itself.
  (let ((operation (make-instance op-type)))
    (append
     (loop for (op . component) in (traverse (make-instance 'load-op :force t) system)
	when (and (typep op 'load-op)
		  (or gather-all (eq (component-system component) system))
		  (or (eq op-type 'compile-op) (typep component 'system)))
	collect (progn
		  (when (eq component system) (setf include-self nil))
		  (cons operation component)))
     (and include-self (list (cons operation system))))))

(defun bundle-sub-operations (o c)
  ;; Builds a list of pairs (operation . component) which contains all the
  ;; dependencies of this bundle. This list is used by TRAVERSE and also
  ;; by INPUT-FILES. The dependencies depend on the strategy, as explained
  ;; below.
  (if (bundle-op-monolithic o)
      ;; First we handle monolithic bundles. These are standalone systems
      ;; which contain everything, including other ASDF systems required
      ;; by the current one. A PROGRAM is always monolithic.
      (ecase (bundle-op-type o)
	((:dll :shared-library :program :fasl)
	 ;; Gather the static libraries of all components.
	 ;; They will be linked together into the resulting target.
	 ;; Incude own system.
	 (gather-components 'lib-op c :gather-all t :include-self t))
	((:lib :static-library)
	 ;; Gather the object files of all systems and subsystems.
	 (gather-components 'compile-op c :gather-all t)))
      ;; Here we analyze non-monolithic versions. They are not standalone
      ;; but we do not care about the dependencies, except in the case of
      ;; shared libraries, that must be linked against the shared libraries
      ;; they depend on.
      (ecase (bundle-op-type o)
	((:dll :shared-library)
	 ;; Gather the dynamically linked libraries of all components.
	 ;; They will be linked into this new shared library, together
	 ;; with the object files of this module.
	 (append (gather-components 'dll-op c :gather-all t)
		 (gather-components 'compile-op c :gather-all nil)))
	((:fasl :lib :static-library)
	 ;; We do not care about other modules, but just provide our
	 ;; own compiled files.
	 (gather-components 'compile-op c :gather-all nil)))))

(defmethod input-files ((o bundle-op) (c system))
  (loop for (sub-op . sub-c) in (bundle-sub-operations o c)
     nconc (output-files sub-op sub-c)))

(defmethod output-files ((o bundle-op) (c system))
  (let ((name (concatenate 'base-string (component-name c)
			   (slot-value o 'name-suffix))))
    (list (merge-pathnames (compile-file-pathname name :type (bundle-op-type o))
			   (component-relative-pathname c)))))

(defmethod perform ((o bundle-op) (c t))
  nil)

(defmethod perform ((o bundle-op) (c system))
  (let* ((object-files (input-files o c))
	 (output (first (output-files o c))))
    (apply #'c::builder (bundle-op-type o) output :lisp-files object-files
	   (bundle-op-build-args o))))

(defmethod traverse ((o bundle-op) (c system))
  (let ((tree (call-next-method)))
    ;; We run over our sub operations in reverse order (from most generic
    ;; to most specific), appending the resulting trees. For some unknown
    ;; reason, we still get PRUNE-OPs which we have to remove from the tree.
    (append (nreverse (delete 'pruned-op
			      (loop for (sub-op . sub-c) in (reverse (bundle-sub-operations o c))
				 nconc (reverse (traverse sub-op sub-c)))
			      :key #'car))
	    tree)))

(defun make-build (&rest args)
  (apply #'operate 'bundle-op args))

(export 'make-build)
(export 'build-op)
