;;; based on v1.6 -*- mode: lisp -*-
;;;; Test suite for the Common Lisp condition system
;;;; Written by David Gadbois <gadbois@cs.utexas.edu> 30.11.1993
(in-package :cl-user)

;;;
;;; Helpers
;;;

#+(or clisp allegro cmu sbcl)
(my-assert
 #+CLISP
 (defun my-cpl (class)
   (clos::class-precedence-list (clos:find-class class))
   )
 #+ALLEGRO
 (defun my-cpl (class)
   (clos:finalize-inheritance (find-class class))
   (clos:class-precedence-list (find-class class))
   )
 #+cmu
 (defun my-cpl (class)
   (pcl:class-precedence-list (find-class class))
   )
 #+sbcl
 (defun my-cpl (class)
   (sb-pcl:class-precedence-list (find-class class))
   )
 MY-CPL)

(my-assert
 (defun check-superclasses (class expected)
   (let ((expected (list* class 't
			  #+CLISP 'clos:standard-object
			  #+ALLEGRO 'standard-object
			  #+(or cmu sbcl) 'instance
			  'condition expected))
	 (super (mapcar #' #+CLISP clos:class-name
			   #+ALLEGRO class-name
			   #+cmu pcl:class-name
			   #+sbcl sb-pcl:class-name
			   (my-cpl class))))
     (and (null (set-difference super expected))
	  (null (set-difference expected super)))))
 CHECK-SUPERCLASSES)

;;;
;;; IGNORE-ERRORS
;;;
;;; If this does not work, none of the tests that check for getting an error
;;; will.

;;; IGNORE-ERRORS should work.
(my-assert
 (multiple-value-bind (value condition)
     (ignore-errors (error "Foo"))
   (list value (type-of condition)))
 (nil simple-error))

;;; IGNORE-ERRORS should not interfere with values in non-error situations.
(my-assert
 (multiple-value-list
  (ignore-errors (values 23 42)))
 (23 42))

;;;
;;; Predefined condition types.
;;;

(my-assert
 (check-superclasses 'warning '()) T)


(my-assert
 (check-superclasses 'style-warning '(warning))
 T)

(my-assert
 (check-superclasses 'serious-condition '())
 T)

(my-assert
 (check-superclasses 'error '(serious-condition))
 T)

(my-assert
 (check-superclasses 'cell-error '(error serious-condition))
 T)

(my-assert
 (check-superclasses 'parse-error '(error serious-condition))
 T)

(my-assert
 (check-superclasses 'storage-condition '(serious-condition))
 T)

(my-assert
 (check-superclasses 'simple-error '(simple-condition error serious-condition))
 T)

(my-assert
 (check-superclasses 'simple-condition '())
 T)

(my-assert
 (check-superclasses 'simple-warning '(simple-condition warning))
 T)

(my-assert
 (check-superclasses 'file-error '(error serious-condition))
 T)

(my-assert
 (check-superclasses 'control-error '(error serious-condition))
 T)

(my-assert
 (check-superclasses 'program-error '(error serious-condition))
 T)

(my-assert
 (check-superclasses 'undefined-function '(cell-error error serious-condition))
 T)

(my-assert
 (check-superclasses 'arithmetic-error '(error serious-condition))
 T)

(my-assert
 (check-superclasses 'division-by-zero '(arithmetic-error error serious-condition))
 T)

(my-assert
 (check-superclasses 'floating-point-invalid-operation '(arithmetic-error error serious-condition))
 T)

(my-assert
 (check-superclasses 'floating-point-inexact '(arithmetic-error error serious-condition))
 T)

(my-assert
 (check-superclasses 'floating-point-overflow '(arithmetic-error error serious-condition))
 T)

(my-assert
 (check-superclasses 'floating-point-underflow '(arithmetic-error error serious-condition))
 T)

(my-assert
 (check-superclasses 'unbound-slot '(cell-error error serious-condition))
 T)

(my-assert
 (check-superclasses 'package-error '(error serious-condition))
 T)

(my-assert
 (check-superclasses 'print-not-readable '(error serious-condition))
 T)

(my-assert
 (check-superclasses 'reader-error '(parse-error stream-error error serious-condition))
 T)

(my-assert
 (check-superclasses 'stream-error '(error serious-condition))
 T)

(my-assert
 (check-superclasses 'end-of-file '(stream-error error serious-condition))
 T)

(my-assert
 (check-superclasses 'unbound-variable '(cell-error error serious-condition))
 T)

(my-assert
 (check-superclasses 'type-error '(error serious-condition))
 T)

(my-assert
 (check-superclasses 'simple-type-error
		     '(simple-condition
		       type-error error serious-condition))
 T
 "Condition Type SIMPLE-TYPE-ERROR 

Class Precedence List:

simple-type-error, simple-condition, type-error, error, serious-condition, condition, t 
")

;;;
;;; Defining conditions.
;;;
(my-assert
 (progn (define-condition test () ()) t)
 T)

(my-assert
 (check-superclasses  'test '())
 T)

(my-assert
 (progn (define-condition test2 (test) ()) t)
 T)

(my-assert
 (check-superclasses 'test2 '(test))
 T)

(my-assert
 (progn (define-condition test3 (test2 simple-condition) ()) t)
 T)

(my-assert
 (check-superclasses 'test3 '(test2 test simple-condition))
 T)

;;;
;;; Making conditions
;;;
(my-assert
 (progn (make-condition 'test) t)
 T)

(my-assert
 (ignore-errors (progn (make-condition 'integer) t))
 NIL)

;;;
;;; :REPORT option to DEFINE-CONDITION
;;;
(my-assert
 (progn (define-condition test4 (test3)
	  ()
	  (:report (lambda (condition stream)
		     (format stream "Yow! -- ~S" (type-of condition)))))
	t)
 T)

(my-assert
 (with-output-to-string (s) (princ (make-condition 'test4) s))
 "Yow! -- TEST4")

(my-assert
 (progn (define-condition test5 (test4) ()) t)
 T)

(my-assert
 (with-output-to-string (s) (princ (make-condition 'test5) s))
 "Yow! -- TEST5")

(my-assert
 (with-output-to-string (s)
   (princ (make-condition 'test3
			  :format-control "And How! -- ~S"
			  :format-arguments '(23)) s))
 "And How! -- 23"
 "From simple-condition:

The type simple-condition represents conditions that are signaled by
signal whenever a format-control is supplied as the function's first
argument. The format control and format arguments are initialized with
the initialization arguments named :format-control and
:format-arguments to make-condition, and are accessed by the functions
simple-condition-format-control and
simple-condition-format-arguments. If format arguments are not
supplied to make-condition, nil is used as a default. "
 )

;;;
;;; Condition slots.
;;;
(my-assert
 (progn (define-condition test6 (test4)
	  ((foo :initarg :foo :initform 23 :accessor test6-foo))
	  (:report (lambda (condition stream)
		     (format stream "~S -- ~S"
			     (type-of condition)
			     (test6-foo condition)))))
	t)
 T)

(my-assert
 (test6-foo (make-condition 'test6))
 23)

(my-assert
 (test6-foo (make-condition 'test6 :foo 42))
 42)

(my-assert
 (setf (test6-foo (make-condition 'test6 :foo 42)) 17)
 17)

(my-assert
 (with-output-to-string (s) (princ (make-condition 'test6 :foo 42) s))
 "TEST6 -- 42")

;;;
;;; HANDLER-BIND
;;;

;;; You do not have to bind handlers.
(my-assert
 (ignore-errors
   (handler-bind
    ()
    (error "Foo")))
 nil)

;;; Handlers should not interfere with values in non-error situations.
(my-assert
 (multiple-value-list
  (block foo
    (handler-bind
     ((error #'(lambda (c)
		 (declare (ignore c))
		 (return-from foo 23))))
     (values 42 17))))
 (42 17))

;;; Handlers should work.
(my-assert
 (multiple-value-list
  (block foo
    (handler-bind
     ((error #'(lambda (c)
		 (declare (ignore c))
		 (return-from foo (values 23 17)))))
     (error "Foo"))))
 (23 17))

;;; Only the appropriate handlers should be called.
(my-assert
 (ignore-errors
   (block foo
     (handler-bind
      ((type-error #'(lambda (c)
		       (declare (ignore c))
		       (return-from foo 23))))
      (error "Foo"))))
 nil)

;;; Handlers can be specified type expressions.
(my-assert
 (block foo
   (handler-bind
    (((or type-error error)
      #'(lambda (c)
	  (declare (ignore c))
	  (return-from foo 23))))
    (error "Foo")))
 23
 "typespecifier can be non-trivial.")

;;; Handlers should be undone.
(my-assert
 (ignore-errors
   (block foo
     (let ((first-time t))
       (handler-bind
	((error
	  #'(lambda (c)
	      (declare (ignore c))
	      (if first-time
		  (progn
		    (setq first-time nil)
		    (error "Bar"))
		  (return-from foo 23)))))
	(error "Foo")))))
 nil)

;;; Handlers should be undone.
(my-assert
 (block foo
   (let ((first-time t))
     (handler-bind
      ((error
	#'(lambda (c)
	    (declare (ignore c))
	    (return-from foo 23))))
      (handler-bind
       ((error
	 #'(lambda (c)
	     (declare (ignore c))
	     (if first-time
		 (progn
		   (setq first-time nil)
		   (error "Bar"))
		 (return-from foo 42)))))
       (error "Foo")))))
 23)

;;; Handlers in the same cluster should be accessible.
(my-assert
 (ignore-errors
   (block foo
     (handler-bind
      ((error
	#'(lambda (c) (declare (ignore c)) nil))
       (error
	#'(lambda (c)
	    (declare (ignore c))
	    (return-from foo 23))))
      (error "Foo"))))
 23
 "If a handler declines (ie. just return) the next available is used, so
 the first one just returns nil, and the second, returning 23 is called")

;;; Multiple handlers should work.
(my-assert
 (block foo
   (handler-bind
    ((type-error
      #'(lambda (c)
	  (declare (ignore c))
	  (return-from foo 42)))
     (error
      #'(lambda (c)
	  (declare (ignore c))
	  (return-from foo 23))))
    (error "Foo")))
 23)

;;; Handlers should be undone.
(my-assert
 (block foo
   (handler-bind
    ((error #'(lambda (c)
		(declare (ignore c))
		(return-from foo 23))))
    (block bar
      (handler-bind
       ((error #'(lambda (c)
		   (declare (ignore c))
		   (return-from foo 42))))
       (return-from bar)))
    (error "Foo")))
 23)

;;;
;;; HANDLER-CASE
;;;

;;; HANDLER-CASE should handle errors.
(my-assert
 (multiple-value-list
  (handler-case
   (error "Foo")
   (error (c) (when (typep c 'error) (values 23 42)))))
 (23 42))

;;; Except those it doesn't handle.
(my-assert
 (ignore-errors
   (handler-case
    (error "Foo")
    (type-error () 23)))
 NIL)

;;; You don't have to specify handlers.
(my-assert
 (ignore-errors
   (handler-case
    (error "Foo")))
 NIL)

;;; HANDLER-CASE should not interfere with values in non-error situations.
(my-assert
 (multiple-value-list
  (handler-case
   (values 42 17)
   (error () 23)))
 (42 17))

;;; :NO-ERROR should return values.
(my-assert
 (multiple-value-list
  (handler-case
   (values 23 42)
   (:no-error (a b)
	      (values b a))))
 (42 23))

;;; Except when there is an error.
(my-assert
 (handler-case
  (error "Foo")
  (error () 23)
  (:no-error (&rest args) (declare (ignore args)) 42))
 23)

;;; Or if it is not the last clause.
(my-assert
 (handler-case
  23
  (:no-error (v) (1+ v))
  (error () 42))
 24
 "The spec is not 100% clear here...
Macro HANDLER-CASE 

Syntax:

handler-case expression [[{error-clause}* | no-error-clause]] => result*

clause::= error-clause | no-error-clause

So in the cause thing the no-error-clause can be everwhere,
in the real thing it looks like it can only be last.

Need to ask comp.lang.lisp...

")

;;; Multiple handlers should be OK.
(my-assert
 (handler-case
  (error "Foo")
  (type-error () 23)
  (error () 42))
 42)

;;; Handlers should get undone.
(my-assert
 (ignore-errors
   (progn
     (block foo
       (handler-case
	(return-from foo 23)
	(error () 42)))
     (error "Foo")))
 NIL)

;;; Ditto.
(my-assert
 (ignore-errors
   (block foo
     (let ((first-time t))
       (handler-case
	(error "Foo")
	(error ()
	       (if first-time
		   (progn
		     (setf first-time nil)
		     (error "Bar"))
		   (return-from foo 23)))))))
 NIL)



