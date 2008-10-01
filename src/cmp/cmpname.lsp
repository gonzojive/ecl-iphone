;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2007, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPNAME Unambiguous init names for object files
;;;;
;;;; Every object file in a lisp library or combined FASL (such as the
;;;; compiler), needs a function that creates its data and installs the
;;;; functions. This initialization function has a C name which needs
;;;; to be unique. This file has functions to create such names.

(in-package "COMPILER")

(defvar *counter* 0)

(defun encode-number-in-name (number)
  ;; Encode a number in an alphanumeric identifier which is a valid C name.
  (declare (si::c-local))
  (cond ((zerop number) "0")
	((minusp number) (encode-number-in-name (- number)))
	(t
	 (do* ((code "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
	       (base (length code))
	       (output '())
	       (digit 0))
	      ((zerop number) (coerce (nreverse output) 'base-string))
	   (multiple-value-setq (number digit) (floor number base))
	   (push (char code digit) output)))))

(defun unique-init-name (file)
  "Create a unique name for this initialization function. The current algorithm
relies only on the name of the source file and the time at which it is built. This
should be enough to prevent name collisions for object files built in the same
machine."
  (let* ((path (pathname file))
	 (path-hash (logxor (ash (sxhash path) 8)
			    (ash (sxhash (cddr (pathname-directory path))) 16)
			    (sxhash (pathname-name path))))
	 (seconds (get-universal-time))
	 (ms (+ (* seconds 1000)
		(mod (floor (* 1000 (get-internal-real-time))
			    internal-time-units-per-second)
		     1000)))
	 (tag (concatenate 'base-string
			   "_ecl"
			   (encode-number-in-name path-hash)
			   "_"
			   (encode-number-in-name ms))))
    (cmpnote "Creating tag: ~S for ~S" tag file)
    tag))

(defun init-name-tag (init-name)
  (concatenate 'base-string "@EcLtAg" ":" init-name "@"))

(defun search-tag (stream tag)
  (declare (si::c-local))
  (do* ((eof nil)
	(key (concatenate 'list tag ":"))
	(string key))
       (nil)
    (let ((c (read-char stream nil nil)))
      (cond ((null c) (return nil))
	    ((not (equal c (pop string)))
	     (setf string key))
	    ((null string)
	     (return t))))))

(defun read-name (stream)
  (declare (si::c-local))
  (concatenate 'string
	       (loop with c = t
		  until (or (null (setf c (read-char stream nil nil)))
			    (equal c #\@))
		  collect c)))

(defun find-init-name (file &key (tag "@EcLtAg"))
  "Search for the initialization function in an object file. Since the
initialization function in object files have more or less unpredictable
names, we store them in a string in the object file. This string is recognized
by the TAG it has at the beginning This function searches that tag and retrieves
the function name it precedes."
  (with-open-file (stream file :direction :input)
    (cmpnote "Scanning ~S" file)
    (when (search-tag stream tag)
      (let ((name (read-name stream)))
	(cmpnote "Found tag: ~S" name)
	name))))

(defun remove-prefix (prefix name)
  (if (equal 0 (search prefix name))
      (subseq name (length prefix) nil)
      name))

(defun guess-init-name (pathname &key (kind (guess-kind pathname)))
  (if (eq kind :object)
    (or (and (probe-file pathname)
	     (find-init-name pathname))
	(error "Cannot find out entry point for binary file" pathname))
    (compute-init-name pathname :kind kind)))

(defun compute-init-name (pathname &key (kind (guess-kind pathname)))
  (let ((filename (pathname-name pathname)))
    (case kind
      ((:object :c)
       (unique-init-name pathname))
      ((:fasl :fas)
       (init-function-name "CODE" :kind :fas))
      ((:static-library :lib)
       (init-function-name (remove-prefix +static-library-prefix+ filename)
			   :kind :lib))
      ((:shared-library :dll)
       (init-function-name (remove-prefix +shared-library-prefix+ filename)
			   :kind :dll))
      ((:program)
       "init_ECL_PROGRAM")
      (otherwise
       (error "C::BUILDER cannot accept files of kind ~s" kind)))))
