;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  MP.LSP  -- Multiprocessing capabilities.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

#-threads
(defpackage "MP"
  (:use "CL")
  (:export "WITH-LOCK" "WITHOUT-INTERRUPTS"))

(in-package "MP")

(defmacro with-lock ((lock) &body body)
  #-threads
  `(progn ,@body)
  #+threads
  `(let ((%the-lock ,lock))
    (unwind-protect
	 (progn
	   (mp::get-lock %the-lock)
	   ,@body)
      (mp::giveup-lock %the-lock))))

(defmacro without-interrupts (&body body)
  `(let ((si:*interrupt-enable* nil))
    (multiple-value-prog1
	(progn ,@body)
      (si::check-pending-interrupts))))
