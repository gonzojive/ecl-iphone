;;;-*-Mode: LISP; Syntax: Common LISP; Base: 10-*-
;;;
;;; File = load.lsp
;;; Load file for ECL<->PVM interface modules.
;;;

(load "pvmconsts")
(load "pvmlisp")
(si:faslink "pvmecl" "-L/project/pvm/pvm3/lib/SUN4 -lgpvm3 -lpvm3 -lc")
;(load "pvmecl")
(load "eclreader")
