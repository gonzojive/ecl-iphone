;;;;  Copyright (c) 2001, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; FFI	Symbols used in the foreign function interface

(defpackage "FFI"
  (:export clines
	   defcfun
	   defentry
	   defla
	   defcbody			; Beppe
	   definline			; Beppe
	   defunC			; Beppe
	   void
	   object
	   char*			; Beppe
	   ;;char
	   int
	   ;;float
	   double
	   ))

(in-package "FFI")

(defmacro clines (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))

(defmacro defcfun (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))

(defmacro defentry (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))

(defmacro defla (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))

(defmacro defcbody (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))

(defmacro definline (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))

(defmacro defunC (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))

