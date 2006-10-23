#|
Build this module with (compile-file "uffi.lsp")
Load it with (load "uffi.fas")
|#

(uffi:load-foreign-library "/usr/lib/libm.dylib")

(uffi:def-function ("sin" c-sin) ((arg :double))
                   :returning :double)

(format t "~%Lisp sin:~t~d~%C sin:~t~d~%Difference:~t~d"
	(sin 1.0d0) (c-sin 1.0d0) (- (sin 1.0d0) (c-sin 1.0d0)))
