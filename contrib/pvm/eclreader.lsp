;;;-*-Mode: LISP; Syntax: Common LISP; Base: 10-*-
;;;
;;; File = eclreader.lsp
;;; Definition of reader for ECoLISP.
;;;
;;; (c) 1994, I.D. Alexander-Craig, all rights reserved.
;;;
;;;



;;;;****************************************************************;;;;
;;;;                                                                ;;;;
;;;; Definition of the basic reader that is needed by KCL.          ;;;;
;;;; The following function should be called when loading the       ;;;;
;;;; object reader for KCL. This is called the default reader       ;;;;
;;;; for KCL.                                                       ;;;;
;;;;                                                                ;;;;
;;;;                                                                ;;;;
;;;;****************************************************************;;;;

(defparameter *default-reader* ())

(defparameter *default-reader-specs*
  (list
   (list 'NULL
          LISP_NIL_TYPE
	  *
         #'(lambda (rdr)
              (declare (ignore rdr))
             ()))
   (list T
         LISP_T_TYPE
         '*
         #'(lambda (rdr)
             (declare (ignore rdr))
             t))
   (list 'STANDARD-CHAR ;; CHARACTER
         LISP_CHAR_TYPE
         #'(lambda (obj rdr)
             (declare (ignore rdr))
             (C-obuffer-char obj))
         #'(lambda (rdr)
             (declare (ignore rdr))
             (C-ibuffer-char)))
   (list 'FIXNUM
          LISP_INT_TYPE
         #'(lambda (obj rdr)
             (declare (ignore rdr))
             (C-obuffer-int obj))
         #'(lambda (rdr)
             (declare (ignore rdr))
             (C-ibuffer-int)))
   (list 'BIGNUM
         LISP_LONGINT_TYPE
         #'(lambda (obj rdr)
             (declare (ignore rdr))
             (C-obuffer-longint obj))
         #'(lambda (rdr)
             (declare (ignore rdr))
             (C-ibuffer-longint)))
   (list 'LONG-FLOAT ;;FLOAT
          LISP_DOUBLE_TYPE
         #'(lambda (obj rdr)
             (declare (ignore rdr))
             (C-obuffer-double obj))
         #'(lambda (rdr)
             (declare (ignore rdr))
             (C-ibuffer-double)))
   (list 'SYMBOL
         LISP_SYMBOL_TYPE
         #'(lambda (obj rdr)
             (declare (ignore rdr))
             (cond ((eq obj t)
		    (C-obuffer-t))
		   ((null obj)
		    (C-obuffer-nil))
		   (t
		    (let ((pname (symbol-name obj)))
		      (C-obuffer-symbol pname (length pname))))))
         #'(lambda (rdr)
             (declare (ignore rdr))
             (C-ibuffer-symbol)))
   (list 'STRING ;; SIMPLE-STRING
         LISP_STRING_TYPE
         #'(lambda (obj rdr)
             (declare (ignore rdr))
             (C-obuffer-string obj (length obj)))
         #'(lambda (rdr)
             (declare (ignore rdr))
             (C-ibuffer-string)))
   (list 'VECTOR
         LISP_VECTOR_TYPE
         #'(lambda (obj rdr)
             (encode-vector obj rdr))
         #'(lambda (rdr)
             (decode-vector rdr)))
   (list 'CONS
         LISP_LIST_TYPE
         #'(lambda (obj rdr)
             (encode-list obj rdr))
         #'(lambda (rdr)
             (decode-list rdr)))))

;; For testing only:

(defparameter *rdr* ())

(defun init-default-reader ()
  (setq *default-reader* (make-object-reader))
  (initialise-reader-object
   *default-reader*
   *default-reader-specs*)
  (values))

(format t "Creating reader:~%")
(init-default-reader)
(format t "Done.~%~%")

;;; For testing only:

(setq *rdr* *default-reader*)

(defun restart-reader ()
  (setq *default-reader* ()
        rdr ())
  (init-default-reader)
  (setq *rdr* *default-reader*)
  (values))
