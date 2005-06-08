;;; Copyright (c) 2005, Michael Goffioul (michael dot goffioul at swing dot be)
;;;
;;;   This program is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Library General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2 of the License, or (at your option) any later version.
;;;
;;;   See file '../../Copyright' for full details.
;;;
;;; COMPILE THE WIN32 EXAMPLES
;;;

(require :cmp)
(let ((c::*ld-format* (concatenate 'string c::*ld-format* " user32.lib kernel32.lib gdi32.lib comdlg32.lib")))
  (compile-file "win32.lisp" :c-file t))

(load "txtedit.lisp")

(format t "

** Run (WIN32::TXTEDIT [FILENAME]) to launch the application example.

")
