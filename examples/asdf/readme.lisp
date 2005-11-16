;;; Copyright (c) 2005, Juan Jose Garcia-Ripoll
;;;
;;;   This program is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Library General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2 of the License, or (at your option) any later version.
;;;
;;;   See file '../../Copyright' for full details.
;;;
;;; This an extremely simple example of how to build standalone programs and
;;; unified fasl files from a system definition file.  You should peruse this
;;; file and also test it by loading it on your copy of ECL.
;;;

;;;
;;; First of all, we need to include the ASDF module and the compiler
;;;

(require 'asdf)
(require 'cmp)
(use-package :asdf)

(setf *load-verbose* nil)
(setf c::*compile-verbose* nil)
(setf c::*suppress-compiler-warnings* t)
(setf c::*suppress-compiler-notes* t)

;;;
;;; This will show you what is running behind the walls of ASDF. Everything
;;; is built on top of the powerful C::BUILDER routine, which allows one
;;; to build anything from executables to shared libraries.
;;;
;;(trace c::builder)

;;;
;;; Next we create a definition containing the files in our project.
;;; Notice that file2.lisp depends on file1.lisp, hence the ":serial t"
;;;
(princ "

Loading definition file.

")

(defsystem #:example
    :serial t
    :components ((:file "file1")
		 (:file "file2")))

;;;
;;; Now we attempt building a single FASL file containing all those files.
;;; Notice that we remove any previous fasl file.
;;;

(princ "

Building FASL file 'example.fas'

")
(asdf:make-build :example :type :fasl)

;;;
;;; Now we load the previous file!
;;;

(princ "

Loading FASL file example.fas

")
(load "example.fas")

;;;
;;; Now that it worked, we attempt building a single program file with everything.
;;;

(princ "

Building standalone executable 'example' ('example.exe' in Windows)

")
(asdf:make-build :example :type :program)

;;;
;;; Test the program
;;;

(princ "

Executing standalone file 'example'

")
(ext:system "./example")

;;;
;;; Clean up everything
;;;

(mapc #'delete-file (append (directory "*.o")
			    (directory "*.obj")
			    (directory "example.*")))
