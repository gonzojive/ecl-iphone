;;;; Test suite for the Common Lisp condition system
;;;; Written by David Gadbois <gadbois@cs.utexas.edu> 30.11.1993

;;;
;;; Helpers
;;;

#+CLISP
(defun my-cpl (class)
  (clos::class-precedence-list (clos:find-class class))
)
#+ALLEGRO
(defun my-cpl (class)
  (clos:finalize-inheritance (find-class class))
  (clos:class-precedence-list (find-class class))
)
MY-CPL

(defun check-superclasses (class expected)
  (let ((expected (list* class 't #+CLISP 'clos:standard-object #+ALLEGRO 'standard-object 'condition expected))
        (super (mapcar #' #+CLISP clos:class-name #+ALLEGRO class-name (my-cpl class))))
    (and (null (set-difference super expected))
         (null (set-difference expected super)))))
CHECK-SUPERCLASSES

;;;
;;; IGNORE-ERRORS
;;;
;;; If this does not work, none of the tests that check for getting an error
;;; will.

;;; IGNORE-ERRORS should work.
(multiple-value-bind (value condition)
    (ignore-errors (error "Foo"))
  (list value (type-of condition)))
(nil simple-error)

;;; IGNORE-ERRORS should not interfere with values in non-error situations.
(multiple-value-list
    (ignore-errors (values 23 42)))
(23 42)

;;;
;;; Predefined condition types.
;;;

(check-superclasses 'warning '()) T
(check-superclasses 'style-warning '(warning)) T
(check-superclasses 'serious-condition '()) T
(check-superclasses 'error '(serious-condition)) T
(check-superclasses 'cell-error '(error serious-condition)) T
(check-superclasses 'parse-error '(error serious-condition)) T
(check-superclasses 'storage-condition '(serious-condition)) T
(check-superclasses 'simple-error '(simple-condition error serious-condition)) T
(check-superclasses 'simple-condition '()) T
(check-superclasses 'simple-warning '(simple-condition warning)) T
(check-superclasses 'file-error '(error serious-condition)) T
(check-superclasses 'control-error '(error serious-condition)) T
(check-superclasses 'program-error '(error serious-condition)) T
(check-superclasses 'undefined-function '(cell-error error serious-condition)) T
(check-superclasses 'arithmetic-error '(error serious-condition)) T
(check-superclasses 'division-by-zero '(arithmetic-error error serious-condition)) T
(check-superclasses 'floating-point-invalid-operation '(arithmetic-error error serious-condition)) T
(check-superclasses 'floating-point-inexact '(arithmetic-error error serious-condition)) T
(check-superclasses 'floating-point-overflow '(arithmetic-error error serious-condition)) T
(check-superclasses 'floating-point-underflow '(arithmetic-error error serious-condition)) T
(check-superclasses 'unbound-slot '(cell-error error serious-condition)) T
(check-superclasses 'package-error '(error serious-condition)) T
(check-superclasses 'print-not-readable '(error serious-condition)) T
(check-superclasses 'reader-error '(parse-error stream-error error serious-condition)) T
(check-superclasses 'stream-error '(error serious-condition)) T
(check-superclasses 'end-of-file '(stream-error error serious-condition)) T
(check-superclasses 'unbound-variable '(cell-error error serious-condition)) T
(check-superclasses 'type-error '(error serious-condition)) T
(check-superclasses 'simple-type-error '(#-ANSI-CL simple-error simple-condition type-error error serious-condition)) T

;;;
;;; Defining conditions.
;;;
(progn (define-condition test () ()) t)
T

(check-superclasses  'test '())
T

(progn (define-condition test2 (test) ()) t)
T

(check-superclasses 'test2 '(test))
T

(progn (define-condition test3 (test2 simple-condition) ()) t)
T

(check-superclasses 'test3 '(test2 test simple-condition))
T

;;;
;;; Making conditions
;;;
(progn (make-condition 'test) t)
T

(ignore-errors (progn (make-condition 'integer) t))
NIL

;;;
;;; :REPORT option to DEFINE-CONDITION
;;;
(progn (define-condition test4 (test3)
         ()
         (:report (lambda (condition stream)
                    (format stream "Yow! -- ~S" (type-of condition)))))
       t)
T

(with-output-to-string (s) (princ (make-condition 'test4) s))
"Yow! -- TEST4"

(progn (define-condition test5 (test4) ()) t)
T

(with-output-to-string (s) (princ (make-condition 'test5) s))
"Yow! -- TEST5"

(with-output-to-string (s)
  (princ (make-condition 'test3
           #-ANSI-CL :format-string #+ANSI-CL :format-control "And How! -- ~S"
           :format-arguments '(23)) s))
"And How! -- 23"

;;;
;;; Condition slots.
;;;
(progn (define-condition test6 (test4)
         ((foo :initarg :foo :initform 23 :accessor test6-foo))
         (:report (lambda (condition stream)
                    (format stream "~S -- ~S"
                            (type-of condition)
                            (test6-foo condition)))))
       t)
T

(test6-foo (make-condition 'test6))
23

(test6-foo (make-condition 'test6 :foo 42))
42

(setf (test6-foo (make-condition 'test6 :foo 42)) 17)
17

(with-output-to-string (s) (princ (make-condition 'test6 :foo 42) s))
"TEST6 -- 42"

;;;
;;; HANDLER-BIND
;;;

;;; You do not have to bind handlers.
(ignore-errors
 (handler-bind
     ()
   (error "Foo")))
nil

;;; Handlers should not interfere with values in non-error situations.
(multiple-value-list
    (block foo
      (handler-bind
          ((error #'(lambda (c)
                      (declare (ignore c))
                      (return-from foo 23))))
        (values 42 17))))
(42 17)

;;; Handlers should work.
(multiple-value-list
    (block foo
      (handler-bind 
          ((error #'(lambda (c)
                      (declare (ignore c))
                      (return-from foo (values 23 17)))))
        (error "Foo"))))
(23 17)

;;; Only the appropriate handlers should be called.
(ignore-errors
 (block foo
   (handler-bind 
       ((type-error #'(lambda (c)
                        (declare (ignore c))
                        (return-from foo 23))))
     (error "Foo"))))
nil

;;; Handlers can be specified type expressions.
(block foo
  (handler-bind 
      (((or type-error error)
        #'(lambda (c)
            (declare (ignore c))
            (return-from foo 23))))
    (error "Foo")))
23

;;; Handlers should be undone.
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
nil

;;; Handlers should be undone.
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
23

;;; Handlers in the same cluster should be accessible.
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
#-ANSI-CL nil #+ANSI-CL 23

;;; Multiple handlers should work.
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
23

;;; Handlers should be undone.
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
23

;;;
;;; HANDLER-CASE
;;;

;;; HANDLER-CASE should handle errors.
(multiple-value-list
    (handler-case 
        (error "Foo")
      (error (c) (when (typep c 'error) (values 23 42)))))
(23 42)

;;; Except those it doesn't handle.
(ignore-errors
 (handler-case
     (error "Foo")
   (type-error () 23)))
NIL

;;; You don't have to specify handlers.
(ignore-errors
 (handler-case
     (error "Foo")))
NIL

;;; HANDLER-CASE should not interfere with values in non-error situations.
(multiple-value-list
    (handler-case
        (values 42 17)
      (error () 23)))
(42 17)

;;; :NO-ERROR should return values.
(multiple-value-list
    (handler-case
        (values 23 42)
      (:no-error (a b)
        (values b a))))
(42 23)

;;; Except when there is an error.
(handler-case
    (error "Foo")
  (error () 23)
  (:no-error (&rest args) (declare (ignore args)) 42))
23

;;; Or if it is not the last clause.
(handler-case
    23
  (:no-error (v) (1+ v))
  (error () 42))
#-ANSI-CL 23 #+ANSI-CL 24

;;; Multiple handlers should be OK.
(handler-case
    (error "Foo")
  (type-error () 23)
  (error () 42))
42

;;; Handlers should get undone.
(ignore-errors
 (progn 
   (block foo
     (handler-case
         (return-from foo 23)
       (error () 42)))
   (error "Foo")))
NIL

;;; Ditto.
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
NIL
      


