;;; section 9: conditions -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))


(my-assert
 (subtypep 'arithmetic-error 'condition)
 t)

(my-assert
 (subtypep 'floating-point-overflow 'condition)
 t)

(my-assert
 (subtypep 'simple-type-error 'condition)
 t)

(my-assert
 (subtypep 'cell-error 'condition)
 t)

(my-assert
 (subtypep 'floating-point-underflow 'condition)
 t)

(my-assert
 (subtypep 'simple-warning 'condition)
 t)

(my-assert
 (subtypep 'condition 'condition)
 t)

(my-assert
 (subtypep 'package-error 'condition)
 t)

(my-assert
 (subtypep 'storage-condition 'condition)
 t)

(my-assert
 (subtypep 'control-error 'condition)
 t)

(my-assert
 (subtypep 'parse-error 'condition)
 t)

(my-assert
 (subtypep 'stream-error 'condition)
 t)

(my-assert
 (subtypep 'division-by-zero 'condition)
 t)

(my-assert
 (subtypep 'print-not-readable 'condition)
 t)

(my-assert
 (subtypep 'style-warning 'condition)
 t)

(my-assert
 (subtypep 'end-of-file 'condition)
 t)

(my-assert
 (subtypep 'program-error 'condition)
 t)

(my-assert
 (subtypep 'type-error 'condition)
 t)

(my-assert
 (subtypep 'error 'condition)
 t)

(my-assert
 (subtypep 'reader-error 'condition)
 t)

(my-assert
 (subtypep 'unbound-slot 'condition)
 t)

(my-assert
 (subtypep 'file-error 'condition)
 t)

(my-assert
 (subtypep 'serious-condition 'condition)
 t)

(my-assert
 (subtypep 'unbound-variable 'condition)
 t)

(my-assert
 (subtypep 'floating-point-inexact 'condition)
 t)

(my-assert
 (subtypep 'simple-condition 'condition)
 t)

(my-assert
 (subtypep 'undefined-function 'condition)
 t)

(my-assert
 (subtypep 'floating-point-invalid-operation 'condition)
 t)

(my-assert
 (subtypep 'simple-error 'condition)
 t)

(my-assert
 (subtypep 'warning   'condition)
 t)


