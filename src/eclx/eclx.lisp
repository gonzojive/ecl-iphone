;;; -*- Mode: Lisp; Package: USER; Base: 10; Syntax: Common-Lisp -*-

(in-package "COMMON-LISP-USER")

(load "sys:cmp.so")

;;; Aid function:

(defvar *only-load* nil)

;;; Then compile and load the true system:

(proclaim '(optimize (safety 2) (speed 1)))

(let* ((files (list 
	       "split-sequence"
               "package"
               "depdefs"
               "clx"
               "dependent"
               "macros"				; these are just macros
               "bufmac"				; these are just macros
               "buffer"
               "display"
               "gcontext"
               "input"
               "requests"
               "fonts"
               "graphics"
               "text"
               "attributes"
               "translate"
               "keysyms"
               "manager"
               "image"
               "resource"))
       (objects (mapcar #'(lambda (x)
			    (load (setq x (merge-pathnames ".lisp" x)))
			    (unless *only-load*
			      (compile-file x :system-p t)))
			files)))
  (unless *only-load*
    #-dlopen
    (c::build-static-library "eclx" :lisp-files objects)
    #+dlopen
    (c::build-shared-library "eclx" :lisp-files objects)))

;(load "clx2/demo/hello.lisp")
;(xlib::hello-world "")
