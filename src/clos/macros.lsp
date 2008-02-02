;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

(defmacro mapappend (fun &rest args)
  `(reduce #'append (mapcar ,fun ,@args)))

(defmacro ensure-up-to-date-instance (instance)
  `(let ((i ,instance))
    (unless (or (si::structurep i) (eq (si::instance-sig i) (class-slots (si::instance-class i))))
      (update-instance i))))



