;;; section 21: streams -*- mode: lisp -*-
(in-package :cl-user)


(my-assert
 (subtypep 'stream 't)
 t)

(my-assert
 (subtypep 'broadcast-stream 'stream)
 t)

(my-assert
 (subtypep 'concatenated-stream 'stream)
 t)

(my-assert
 (subtypep 'echo-stream 'stream)
 t)

(my-assert
 (subtypep 'file-stream 'stream)
 t)

(my-assert
 (subtypep 'string-stream 'stream)
 t)

(my-assert
 (subtypep 'synonym-stream 'stream)
 t)

(my-assert
 (subtypep 'two-way-stream 'stream)
 t)


;;; input-stream-p

(my-assert
 (input-stream-p *standard-input*)
 t)

(my-assert
 (input-stream-p *terminal-io*)
 t)

(my-assert
 (input-stream-p (make-string-output-stream))
 nil)

(my-assert
 (output-stream-p *standard-output*)
 t)

(my-assert
 (output-stream-p *terminal-io*)
 t)

(my-assert
 (output-stream-p (make-string-input-stream "jr"))
 nil)

;;; open-stream-p

(my-assert
 (open-stream-p *standard-input*)
 t)

;;; read-byte

(my-assert
 (with-open-file (s "/tmp/temp-bytes"
		    :direction :output
		    :element-type 'unsigned-byte)
		 (write-byte 101 s))
 101)

(my-assert
 (with-open-file (s "/tmp/temp-bytes" :element-type 'unsigned-byte)
		 (list (read-byte s) (read-byte s nil 'eof)))
 (101 EOF))

;;; peek-char

(my-assert
 (with-input-from-string (input-stream "    1 2 3 4 5")
			 (list (peek-char t input-stream)
			       (peek-char #\4 input-stream)
			       (peek-char nil input-stream)))
 (#\1 #\4 #\4))

;;; read-char

(my-assert
 (with-input-from-string (is "0123")
			 (let ((a nil))
			   (do ((c (read-char is) (read-char is nil 'the-end)))
			       ((not (characterp c)))
			     (setq a (cons c a)))
			   a))
 (#\3 #\2 #\1 #\0))

;;; make-concatenated-stream

(my-assert
 (read (make-concatenated-stream
	(make-string-input-stream "1")
	(make-string-input-stream "2")))
 12)






