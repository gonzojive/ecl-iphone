;;;;  Copyright (c) 2004, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;        The CLOS IO library.

(in-package "SI")

;;;
;;; The following methods constitute the whole of the interface that a CLOS
;;; class has to offer in order to be accepted by READ, WRITE, CLOSE, etc.
;;; Here we provide dummy methods that fail with a TYPE-ERROR, which is the
;;; condition type expected by the ANSI standard.
;;;

(defmethod ext::stream-input-p ((stream t))
  (not-a-clos-stream stream 'ext::stream-input-))

(defmethod ext::stream-output-p ((stream t))
  (not-a-clos-stream stream 'ext:stream-output-p))

(defmethod ext::stream-close ((stream t))
  (not-a-clos-stream stream 'ext:stream-close))

(defmethod ext::stream-read-char ((stream t))
  (not-a-clos-stream stream 'ext:stream-read-char))

(defmethod ext::stream-unread-char ((stream t) char)
  (not-a-clos-stream stream 'ext:stream-unread-char))

(defmethod ext::stream-write-char ((stream t) char)
  (not-a-clos-stream stream 'ext:stream-write-char))

(defmethod ext::stream-force-output ((stream t))
  (not-a-clos-stream stream 'ext:stream-force-output))

(defmethod ext::stream-clear-input ((stream t))
  (not-a-clos-stream stream 'ext:stream-clear-input))

(defmethod ext::stream-clear-output ((stream t))
  (not-a-clos-stream stream 'ext:stream-clear-output))

(defmethod ext::stream-listen ((stream t))
  (not-a-clos-stream stream 'ext:stream-listen))

(defmethod ext::stream-interactive-p ((stream t))
  (not-a-clos-stream stream 'ext:stream-interactive-p))

(defun not-a-clos-stream (object method-name)
  (declare (ext::c-local))
  (error 'simple-type-error
	 :datum object
	 :expected-type 'STREAM
	 :format-control
	 "The object ~S~%is neither a common-lisp STREAM nor a CLOS object with method ~A."
	 :format-arguments (list object method-name)))
