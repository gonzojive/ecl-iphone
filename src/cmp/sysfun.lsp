;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;; CMPSYSFUN   Database for system functions.
;;;
;;; Copyright (c) 2003, Juan Jose Garcia Ripoll
;;; Copyright (c) 1991, Giuseppe Attardi. All rights reserved.
;;;    Copying of this file is authorized to users who have executed the true
;;;    and proper "License Agreement for ECoLisp".
;;;
;;; DATABASE OF FUNCTION PROCLAMATIONS AND INLINE EXPANSIONS
;;;
;;; What follows is the complete list of function type proclamations for the
;;; most important functions in the ECL core library, together with some useful
;;; inline expansions.
;;;
;;; The function proclamations are created with PROCLAIM-FUNCTION, as in
;;;
;;;	(PROCLAIM-FUNCTION function-name ([arg-type]*) return-type
;;;		&key no-sp-change predicate no-side-effects)
;;;
;;; with the following interpretation: ARG-TYPE and RETURN-TYPE denote the most
;;; general types for the input and output values of this function. If the
;;; compiler detects that some of the values passed to this function does not
;;; match these types, it will generate an error. NO-SP-CHANGE should be
;;; supplied if the function is known to not change any special variable. A more
;;; strict declaration is NO-SIDE-EFFECTS which means that the function's output
;;; does only depend in the input values, that these input values are not
;;; changed, and that under normal conditions (i.e. no error signaled) the
;;; function has no side effect (i.e. does not change global variables, does not
;;; perform input/output, etc). Notice that allocating memory and creating new
;;; elementary objects (i.e. conses, floats, integers, etc) is not considered a
;;; side effect, while creating other objects (classes, streams, structures) is.
;;;
;;; Inline expansions, on the other hand, have the following syntax
;;;
;;;	(DEF-INLINE function-name kind ([arg-type]*) return-rep-type
;;;		expansion-string)
;;;
;;; Here, ARG-TYPE is the list of argument types belonging to the lisp family,
;;; while RETURN-REP-TYPE is a representation type, i.e. the C type of the
;;; output expression. EXPANSION-STRING is a C/C++ expression template, like the
;;; ones used by C-INLINE. Finally, KIND can be :ALWAYS, :SAFE or :UNSAFE,
;;; depending on whether the inline expression should be applied always, in safe
;;; or in unsafe compilation mode, respectively.
;;;

(in-package "COMPILER")

(defmacro proclaim-function (name arg-types return-type
			     &key no-sp-change predicate no-side-effects)
  (unless (or (null arg-types)
	      (equal arg-types '(*)))
    (put-sysprop name 'proclaimed-arg-types
		 (mapcar #'(lambda (x) (if (eql x '*) '* (type-filter x)))
			 arg-types)))
  (when (and return-type (not (eq 'T return-type)))
    (put-sysprop name 'proclaimed-return-type
		 (if (eql return-type '*) '* (type-filter return-type t))))
  (when no-sp-change
    (put-sysprop name 'no-sp-change t))
  (when predicate
    (put-sysprop name 'predicate t))
  (when no-side-effects
    (put-sysprop name 'no-side-effects t))
  (rem-sysprop name ':inline-always)
  (rem-sysprop name ':inline-safe)
  (rem-sysprop name ':inline-unsafe)
  nil)

(defmacro def-inline (name safety arg-types return-rep-type expansion
                      &key (one-liner t)
		      &aux arg-rep-types)
  (setf safety
	(case safety
	  (:unsafe :inline-unsafe)
	  (:safe :inline-safe)
	  (:always :inline-always)
	  (t (error "In DEF-INLINE, wrong value of SAFETY"))))
  (setf arg-rep-types
	(mapcar #'(lambda (x) (if (eq x '*) x (lisp-type->rep-type x)))
		arg-types))
  (let ((inline-info
	 (make-inline-info :arg-rep-types arg-rep-types
			   :return-rep-type return-rep-type
			   :return-type (rep-type->lisp-type return-rep-type)
			   :arg-types arg-types
;			   :side-effects (not (get-sysprop name 'no-side-effects))
                           :one-liner one-liner
			   :expansion expansion)))
    (put-sysprop name safety (cons inline-info (get-sysprop name safety))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUXILIARY TYPES
;;

(deftype string-designator () '(or string symbol character))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ALL FUNCTION DECLARATIONS AND INLINE FORMS
;;;

(proclaim-function si:make-pure-array (*) array)
(proclaim-function si:make-vector (*) vector)
(proclaim-function aref (array *) t :no-side-effects t)
(def-inline aref :unsafe (t t t) t
 "@0;ecl_aref(#0,fix(#1)*(#0)->array.dims[1]+fix(#2))")
(def-inline aref :unsafe ((array t) t t) t
 "@0;(#0)->array.self.t[fix(#1)*(#0)->array.dims[1]+fix(#2)]")
(def-inline aref :unsafe ((array bit) t t) :fixnum
 "@0;ecl_aref_bv(#0,fix(#1)*(#0)->array.dims[1]+fix(#2))")
(def-inline aref :unsafe ((array t) fixnum fixnum) t
 "@0;(#0)->array.self.t[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array bit) fixnum fixnum) :fixnum
 "@0;ecl_aref_bv(#0,(#1)*(#0)->array.dims[1]+#2)")
(def-inline aref :unsafe ((array base-char) fixnum fixnum) :char
 "@0;(#0)->base_string.self[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array double-float) fixnum fixnum) :double
 "@0;(#0)->array.self.df[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array single-float) fixnum fixnum) :float
 "@0;(#0)->array.self.sf[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array fixnum) fixnum fixnum) :fixnum
 "@0;(#0)->array.self.fix[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :always (t t) t "ecl_aref1(#0,fixint(#1))")
(def-inline aref :always (t fixnum) t "ecl_aref1(#0,#1)")
(def-inline aref :unsafe (t t) t "ecl_aref1(#0,fix(#1))")
(def-inline aref :unsafe (t fixnum) t "ecl_aref1(#0,#1)")
(def-inline aref :unsafe ((array bit) t) :fixnum "ecl_aref_bv(#0,fix(#1))")
(def-inline aref :unsafe ((array bit) fixnum) :fixnum "ecl_aref_bv(#0,#1)")
(def-inline aref :unsafe ((array base-char) fixnum) t
 "CODE_CHAR((#0)->base_string.self[#1])")
#+unicode
(def-inline aref :unsafe ((array character) fixnum fixnum) t
 "@0;(#0)->string.self[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array double-float) fixnum) t
 "ecl_make_doublefloat((#0)->array.self.df[#1])")
(def-inline aref :unsafe ((array single-float) fixnum) t
 "ecl_make_singlefloat((#0)->array.self.sf[#1])")
(def-inline aref :unsafe ((array fixnum) fixnum) t
 "MAKE_FIXNUM((#0)->array.self.fix[#1])")
(def-inline aref :unsafe ((array base-char) fixnum) :fixnum
 "(#0)->base_string.self[#1]")
#+unicode
(def-inline aref :unsafe ((array character) fixnum) :fixnum
 "CHAR_CODE((#0)->base_string.self[#1])")
(def-inline aref :unsafe ((array base-char) fixnum) :char
 "(#0)->base_string.self[#1]")
(def-inline aref :unsafe ((array double-float) fixnum) :double
 "(#0)->array.self.df[#1]")
(def-inline aref :unsafe ((array single-float) fixnum) :float
 "(#0)->array.self.sf[#1]")
(def-inline aref :unsafe ((array fixnum) fixnum) :fixnum
 "(#0)->array.self.fix[#1]")

(proclaim-function si:aset (t array *) nil)
(def-inline si:aset :unsafe (t t t t) t
 "@0;ecl_aset(#1,fix(#2)*(#1)->array.dims[1]+fix(#3),#0)")
(def-inline si:aset :unsafe (t t fixnum fixnum) t
 "@0;ecl_aset(#1,(#2)*(#1)->array.dims[1]+(#3),#0)")
(def-inline si:aset :unsafe (t (array t) fixnum fixnum) t
 "@1;(#1)->array.self.t[#2*(#1)->array.dims[1]+#3]= #0")
(def-inline si:aset :unsafe (t (array bit) fixnum fixnum) :fixnum
 "@0;ecl_aset_bv(#1,(#2)*(#1)->array.dims[1]+(#3),fix(#0))")
(def-inline si:aset :unsafe (base-char (array base-char) fixnum fixnum) :char
 "@1;(#1)->base_string.self[#2*(#1)->array.dims[1]+#3]= #0")
(def-inline si:aset :unsafe (double-float (array double-float) fixnum fixnum)
 :double "@1;(#1)->array.self.df[#2*(#1)->array.dims[1]+#3]= #0")
(def-inline si:aset :unsafe (single-float (array single-float) fixnum fixnum)
 :float "@1;(#1)->array.self.sf[#2*(#1)->array.dims[1]+#3]= #0")
(def-inline si:aset :unsafe (fixnum (array fixnum) fixnum fixnum) :fixnum
 "@1;(#1)->array.self.fix[#2*(#1)->array.dims[1]+#3]= #0")
(def-inline si:aset :unsafe (fixnum (array bit) fixnum fixnum) :fixnum
 "@0;ecl_aset_bv(#1,(#2)*(#1)->array.dims[1]+(#3),#0)")
(def-inline si:aset :always (t t t) t "ecl_aset1(#1,fixint(#2),#0)")
(def-inline si:aset :always (t t fixnum) t "ecl_aset1(#1,#2,#0)")
(def-inline si:aset :unsafe (t t t) t "ecl_aset1(#1,fix(#2),#0)")
(def-inline si:aset :unsafe (t (array t) fixnum) t
 "(#1)->vector.self.t[#2]= #0")
(def-inline si:aset :unsafe (t (array bit) fixnum) :fixnum
 "ecl_aset_bv(#1,#2,fix(#0))")
(def-inline si:aset :unsafe (base-char (array base-char) fixnum) :char
 "(#1)->base_string.self[#2]= #0")
#+unicode
(def-inline si:aset :unsafe (character (array character) fixnum) t
 "(#1)->string.self[#2]= #0")
(def-inline si:aset :unsafe (double-float (array double-float) fixnum) :double
 "(#1)->array.self.df[#2]= #0")
(def-inline si:aset :unsafe (single-float (array single-float) fixnum) :float
 "(#1)->array.self.sf[#2]= #0")
(def-inline si:aset :unsafe (fixnum (array fixnum) fixnum) :fixnum
 "(#1)->array.self.fix[#2]= #0")
(def-inline si:aset :unsafe (fixnum (array bit) fixnum) :fixnum
 "ecl_aset_bv(#1,#2,#0)")

(proclaim-function row-major-aref (array fixnum) t :no-side-effects t)
(def-inline row-major-aref :always (array fixnum) t "ecl_aref(#0,#1)")

(proclaim-function si:row-major-aset (array fixnum t) t)
(def-inline si:row-major-aset :always (array fixnum t) t "ecl_aset(#0,#1,#2)")

(proclaim-function array-element-type (array) t)
(proclaim-function array-rank (array) fixnum)
(proclaim-function array-dimension (array fixnum) fixnum)
(proclaim-function array-total-size (array) t :no-side-effects t)
(def-inline array-total-size :unsafe (t) :fixnum "((#0)->array.dim)")

(proclaim-function adjustable-array-p (array) t :predicate t)
(proclaim-function array-displacement (array) (values t fixnum) :predicate t)
(proclaim-function svref (simple-vector fixnum) t :no-side-effects t)
(def-inline svref :always (t t) t "ecl_aref1(#0,fixint(#1))")
(def-inline svref :always (t fixnum) t "ecl_aref1(#0,#1)")
(def-inline svref :unsafe (t t) t "(#0)->vector.self.t[fix(#1)]")
(def-inline svref :unsafe (t fixnum) t "(#0)->vector.self.t[#1]")

(proclaim-function si:svset (simple-vector fixnum t) t)
(def-inline si:svset :always (t t t) t "ecl_aset1(#0,fixint(#1),#2)")
(def-inline si:svset :always (t fixnum t) t "ecl_aset1(#0,#1,#2)")
(def-inline si:svset :unsafe (t t t) t "((#0)->vector.self.t[fix(#1)]=(#2))")
(def-inline si:svset :unsafe (t fixnum t) t "(#0)->vector.self.t[#1]= #2")

(proclaim-function array-has-fill-pointer-p (*) t :predicate t)
(proclaim-function fill-pointer (vector) fixnum :no-side-effects t)
(def-inline fill-pointer :unsafe (t) :fixnum "((#0)->vector.fillp)")

(proclaim-function si:fill-pointer-set (vector fixnum) fixnum)
(def-inline si:fill-pointer-set :unsafe (t fixnum) :fixnum
 "((#0)->vector.fillp)=(#1)")

(proclaim-function si:replace-array (*) t)

;; file assignment.d

(proclaim-function set (symbol t) t)
(proclaim-function si:fset (symbol t) t)
(proclaim-function makunbound (symbol) t)
(proclaim-function fmakunbound (symbol) t)
(proclaim-function si:clear-compiler-properties (*) t)

;; file cfun.d

(proclaim-function si:compiled-function-name (*) t)

;; file character.d

(proclaim-function standard-char-p (character) t :predicate t)
(def-inline standard-char-p :always (character) :bool "ecl_standard_char_p(#0)")

(proclaim-function graphic-char-p (character) t :predicate t)
(def-inline graphic-char-p :always (character) :bool "ecl_graphic_char_p(#0)")

(proclaim-function alpha-char-p (character) t :predicate t :no-side-effects t)
(def-inline alpha-char-p :always (character) :bool "ecl_alpha_char_p(#0)")

(proclaim-function upper-case-p (character) t :predicate t :no-side-effects t)
(def-inline upper-case-p :always (character) :bool "ecl_upper_case_p(#0)")

(proclaim-function lower-case-p (character) t :predicate t :no-side-effects t)
(def-inline lower-case-p :always (character) :bool "ecl_lower_case_p(#0)")

(proclaim-function both-case-p (character) t :predicate t :no-side-effects t)
(def-inline both-case-p :always (character) :bool "ecl_both_case_p(#0)")

(proclaim-function digit-char-p (character *) t :no-side-effects t)

(proclaim-function alphanumericp (character) t :predicate t :no-side-effects t)
(def-inline alphanumericp :always (character) :bool "ecl_alphanumericp(#0)")

(proclaim-function character (t) character)
(proclaim-function char= (character *) t :predicate t :no-side-effects t)
(def-inline char= :always (t t) :bool "ecl_char_code(#0)==ecl_char_code(#1)")
(def-inline char= :always (character character) :bool "(#0)==(#1)")

(proclaim-function char/= (character *) t :predicate t :no-side-effects t)
(def-inline char/= :always (t t) :bool "ecl_char_code(#0)!=ecl_char_code(#1)")
(def-inline char/= :always (character character) :bool "(#0)!=(#1)")

(proclaim-function char< (character *) t :predicate t :no-side-effects t)
(def-inline char< :always (character character) :bool "(#0)<(#1)")

(proclaim-function char> (character *) t :predicate t :no-side-effects t)
(def-inline char> :always (character character) :bool "(#0)>(#1)")

(proclaim-function char<= (character *) t :predicate t :no-side-effects t)
(def-inline char<= :always (character character) :bool "(#0)<=(#1)")

(proclaim-function char>= (character *) t :predicate t :no-side-effects t)
(def-inline char>= :always (character character) :bool "(#0)>=(#1)")

(proclaim-function char-equal (character *) t :predicate t)
(proclaim-function char-not-equal (character *) t :predicate t)
(proclaim-function char-lessp (character *) t :predicate t)
(proclaim-function char-greaterp (character *) t :predicate t)
(proclaim-function char-not-greaterp (character *) t :predicate t)
(proclaim-function char-not-lessp (character *) t :predicate t)
(proclaim-function character (*) character)
(proclaim-function char-code (character) fixnum :no-side-effects t)
(def-inline char-code :always (character) :fixnum "#0")

(proclaim-function code-char (fixnum) character :no-side-effects t)
(def-inline code-char :always (fixnum) :char "#0")

(proclaim-function char-upcase (character) character :no-side-effects t)
(def-inline char-upcase :always (base-char) :char "toupper(#0)")

(proclaim-function char-downcase (character) character :no-side-effects t)
(def-inline char-downcase :always (base-char) :char "tolower(#0)")

(proclaim-function digit-char (fixnum *) (or character null))
(proclaim-function char-int (character) fixnum :no-side-effects t)
(def-inline char-int :always (character) :fixnum "#0")

(proclaim-function char-name (character) (or string null))
(proclaim-function name-char (string) (or character null))

;; file error.d

(proclaim-function error (t *) t)
(proclaim-function cerror (t t *) t)

;; file stacks.d

(proclaim-function si:ihs-top (t) t)
(proclaim-function si:ihs-fun (*) t)
(proclaim-function si:ihs-env (*) t)
(proclaim-function si:frs-top (*) t)
(proclaim-function si::frs-vs (*) t)
(proclaim-function si:frs-bds (*) t)
(proclaim-function si:frs-tag (*) t)
(proclaim-function si:frs-ihs (*) t)
(proclaim-function si:bds-top (*) t)
(proclaim-function si:bds-var (*) t)
(proclaim-function si:bds-val (*) t)
(proclaim-function si::vs-top (*) t)
(proclaim-function si::vs (*) t)
(proclaim-function si:sch-frs-base (*) t)

;; file eval.d

(proclaim-function apply (t t *) t)
(proclaim-function funcall (t *) t)
(proclaim-function eval (t) t)
(proclaim-function evalhook (t t t *) t)
(proclaim-function applyhook (t t t t *) t)
(proclaim-function constantp (t) t :predicate t)
(proclaim-function si:unlink-symbol (*) t)
(proclaim-function si::link-enable (*) t)

;; file file.d

(proclaim-function make-synonym-stream (symbol) synonym-stream)
(proclaim-function make-broadcast-stream (*) broadcast-stream)
(proclaim-function make-concatenated-stream (*) concatenated-stream)
(proclaim-function make-two-way-stream (stream stream) two-way-stream)
(proclaim-function make-echo-stream (stream stream) echo-stream)
(proclaim-function make-string-input-stream (*) string-stream)
(proclaim-function make-string-output-stream (*) string-stream)
(def-inline make-string-output-stream :always () string-stream
 "ecl_make_string_output_stream(128)")

(proclaim-function get-output-stream-string (string-stream) string)
(proclaim-function streamp (t) t :predicate t)
(proclaim-function input-stream-p (stream) t :predicate t)
(def-inline input-stream-p :always (stream) :bool "ecl_input_stream_p(#0)")

(proclaim-function output-stream-p (t) t :predicate t)
(def-inline input-stream-p :always (stream) :bool "ecl_output_stream_p(#0)")

(proclaim-function stream-element-type (t) t)
(proclaim-function close (stream *) t)
(proclaim-function file-position (stream *) t)
(proclaim-function file-length (stream) t)
(proclaim-function si:make-string-output-stream-from-string (string) string-stream)

;; file gbc.d / alloc_2.d

(proclaim-function si::room-report (*) t)
(proclaim-function si::reset-gbc-count (*) t)
(proclaim-function gbc (*) t)

;; file unixfsys.d

(proclaim-function truename (t) t)
(proclaim-function rename-file (t t *) t)
(proclaim-function si:specialp (t) t :predicate t)
(proclaim-function delete-file (t) t)
(proclaim-function probe-file (t) t)
(proclaim-function file-write-date (t) t)
(proclaim-function file-author (t) t)
(proclaim-function pathname (t) t)
(proclaim-function user-homedir-pathname (*) t)
(proclaim-function directory (t) t)
(proclaim-function si:chdir (t *) pathname)
(proclaim-function si:getcwd (*) pathname)
(proclaim-function si:mkdir (t fixnum) string)

;; file unixint.d

(proclaim-function ext:catch-signal (t t) t)

;; file format.d

(proclaim-function format (t string *) t)

;; file hash.d

(proclaim-function make-hash-table (*) t)
(proclaim-function hash-table-p (t) t :predicate t)
(proclaim-function values (*) *)
(proclaim-function gethash (t t *) (values t t))
(proclaim-function remhash (t t) t)
(proclaim-function maphash (t t) t)
(proclaim-function clrhash (t) t)
(proclaim-function hash-table-count (t) si::index)
(proclaim-function sxhash (t) fixnum)
(proclaim-function si:hash-set (*) t)

;; file list.d

(proclaim-function car (list) t :no-side-effects t)
(def-inline car :always (cons) t "CAR(#0)")
(def-inline car :unsafe (t) t "CAR(#0)")

(proclaim-function cdr (list) t :no-side-effects t)
(def-inline cdr :always (cons) t "CDR(#0)")
(def-inline cdr :unsafe (t) t "CDR(#0)")

(proclaim-function caar (list) t :no-side-effects t)
(def-inline caar :always (cons) t "CAAR(#0)")
(def-inline caar :unsafe (t) t "CAAR(#0)")

(proclaim-function cadr (list) t :no-side-effects t)
(def-inline cadr :always (cons) t "CADR(#0)")
(def-inline cadr :unsafe (t) t "CADR(#0)")

(proclaim-function cdar (list) t :no-side-effects t)
(def-inline cdar :always (cons) t "CDAR(#0)")
(def-inline cdar :unsafe (t) t "CDAR(#0)")

(proclaim-function cddr (list) t :no-side-effects t)
(def-inline cddr :always (cons) t "CDDR(#0)")
(def-inline cddr :unsafe (t) t "CDDR(#0)")

(proclaim-function caaar (list) t :no-side-effects t)
(def-inline caaar :always (cons) t "CAAAR(#0)")
(def-inline caaar :unsafe (t) t "CAAAR(#0)")

(proclaim-function caadr (list) t :no-side-effects t)
(def-inline caadr :always (cons) t "CAADR(#0)")
(def-inline caadr :unsafe (t) t "CAADR(#0)")

(proclaim-function cadar (list) t :no-side-effects t)
(def-inline cadar :always (cons) t "CADAR(#0)")
(def-inline cadar :unsafe (t) t "CADAR(#0)")

(proclaim-function caddr (list) t :no-side-effects t)
(def-inline caddr :always (cons) t "CADDR(#0)")
(def-inline caddr :unsafe (t) t "CADDR(#0)")

(proclaim-function cdaar (list) t :no-side-effects t)
(def-inline cdaar :always (cons) t "CDAAR(#0)")
(def-inline cdaar :unsafe (t) t "CDAAR(#0)")

(proclaim-function cdadr (list) t :no-side-effects t)
(def-inline cdadr :always (cons) t "CDADR(#0)")
(def-inline cdadr :unsafe (t) t "CDADR(#0)")

(proclaim-function cddar (list) t :no-side-effects t)
(def-inline cddar :always (cons) t "CDDAR(#0)")
(def-inline cddar :unsafe (t) t "CDDAR(#0)")

(proclaim-function cdddr (list) t :no-side-effects t)
(def-inline cdddr :always (cons) t "CDDDR(#0)")
(def-inline cdddr :unsafe (t) t "CDDDR(#0)")

(proclaim-function caaaar (list) t :no-side-effects t)
(def-inline caaaar :always (cons) t "CAAAAR(#0)")
(def-inline caaaar :unsafe (t) t "CAAAAR(#0)")

(proclaim-function caaadr (list) t :no-side-effects t)
(def-inline caaadr :always (cons) t "CAAADR(#0)")
(def-inline caaadr :unsafe (t) t "CAAADR(#0)")

(proclaim-function caadar (list) t :no-side-effects t)
(def-inline caadar :always (cons) t "CAADAR(#0)")
(def-inline caadar :unsafe (t) t "CAADAR(#0)")

(proclaim-function caaddr (list) t :no-side-effects t)
(def-inline caaddr :always (cons) t "CAADDR(#0)")
(def-inline caaddr :unsafe (t) t "CAADDR(#0)")

(proclaim-function cadaar (list) t :no-side-effects t)
(def-inline cadaar :always (cons) t "CADAAR(#0)")
(def-inline cadaar :unsafe (t) t "CADAAR(#0)")

(proclaim-function cadadr (list) t :no-side-effects t)
(def-inline cadadr :always (cons) t "CADADR(#0)")
(def-inline cadadr :unsafe (t) t "CADADR(#0)")

(proclaim-function caddar (list) t :no-side-effects t)
(def-inline caddar :always (cons) t "CADDAR(#0)")
(def-inline caddar :unsafe (t) t "CADDAR(#0)")

(proclaim-function cadddr (list) t :no-side-effects t)
(def-inline cadddr :always (cons) t "CADDDR(#0)")
(def-inline cadddr :unsafe (t) t "CADDDR(#0)")

(proclaim-function cdaaar (list) t :no-side-effects t)
(def-inline cdaaar :always (cons) t "CDAAAR(#0)")
(def-inline cdaaar :unsafe (t) t "CDAAAR(#0)")

(proclaim-function cdaadr (list) t :no-side-effects t)
(def-inline cdaadr :always (cons) t "CDAADR(#0)")
(def-inline cdaadr :unsafe (t) t "CDAADR(#0)")

(proclaim-function cdadar (list) t :no-side-effects t)
(def-inline cdadar :always (cons) t "CDADAR(#0)")
(def-inline cdadar :unsafe (t) t "CDADAR(#0)")

(proclaim-function cdaddr (list) t :no-side-effects t)
(def-inline cdaddr :always (cons) t "CDADDR(#0)")
(def-inline cdaddr :unsafe (t) t "CDADDR(#0)")

(proclaim-function cddaar (list) t :no-side-effects t)
(def-inline cddaar :always (cons) t "CDDAAR(#0)")
(def-inline cddaar :unsafe (t) t "CDDAAR(#0)")

(proclaim-function cddadr (list) t :no-side-effects t)
(def-inline cddadr :always (cons) t "CDDADR(#0)")
(def-inline cddadr :unsafe (t) t "CDDADR(#0)")

(proclaim-function cdddar (list) t :no-side-effects t)
(def-inline cdddar :always (cons) t "CDDDAR(#0)")
(def-inline cdddar :unsafe (t) t "CDDDAR(#0)")

(proclaim-function cddddr (list) t :no-side-effects t)
(def-inline cddddr :always (cons) t "CDDDDR(#0)")
(def-inline cddddr :unsafe (t) t "CDDDDR(#0)")

(proclaim-function cons (t t) cons :no-side-effects t)
(def-inline cons :always (t t) t "CONS(#0,#1)")

(proclaim-function tree-equal (t t *) t :predicate t)
(proclaim-function endp (list) t :predicate t :no-side-effects t)
(def-inline endp :safe (t) :bool "ecl_endp(#0)")
(def-inline endp :unsafe (t) :bool "#0==Cnil")

(proclaim-function list-length (list) (or nil (integer 0 *)))
(proclaim-function nth (integer list) t :no-side-effects t)
(def-inline nth :always (t t) t "ecl_nth(fixint(#0),#1)")
(def-inline nth :always (fixnum t) t "ecl_nth(#0,#1)")
(def-inline nth :unsafe (t t) t "ecl_nth(fix(#0),#1)")
(def-inline nth :unsafe (fixnum t) t "ecl_nth(#0,#1)")

(proclaim-function first (list) t :no-side-effects t)
(def-inline first :always (cons) t "ECL_CONS_CAR(#0)")
(def-inline first :unsafe (t) t "CAR(#0)")

(proclaim-function second (list) t :no-side-effects t)
(def-inline second :always (cons) t "CADR(#0)")
(def-inline second :unsafe (t) t "CADR(#0)")

(proclaim-function third (list) t :no-side-effects t)
(def-inline third :always (cons) t "CADDR(#0)")
(def-inline third :unsafe (t) t "CADDR(#0)")

(proclaim-function fourth (list) t :no-side-effects t)
(def-inline fourth :always (cons) t "CADDDR(#0)")
(def-inline fourth :unsafe (t) t "CADDDR(#0)")

(proclaim-function fifth (list) t)
(proclaim-function sixth (list) t)
(proclaim-function seventh (list) t)
(proclaim-function eighth (list) t)
(proclaim-function ninth (list) t)
(proclaim-function tenth (list) t)
(proclaim-function rest (list) t :no-side-effects t)
(def-inline rest :always (cons) t "ECL_CONS_CDR(#0)")
(def-inline rest :unsafe (t) t "CDR(#0)")

(proclaim-function nthcdr (fixnum list) t :no-side-effects t)
(def-inline nthcdr :always (t t) t "ecl_nthcdr(fixint(#0),#1)")
(def-inline nthcdr :always (fixnum t) t "ecl_nthcdr(#0,#1)")
(def-inline nthcdr :unsafe (t t) t "ecl_nthcdr(fix(#0),#1)")
(def-inline nthcdr :unsafe (fixnum t) t "ecl_nthcdr(#0,#1)")

(proclaim-function last (list) t)
(def-inline last :always (t) t "ecl_last(#0,1)")
(proclaim-function list (*) list :no-side-effects t)
(def-inline list :always nil t "Cnil")
(def-inline list :always (t) t "ecl_list1(#0)")

(proclaim-function list* (t *) list :no-side-effects t)
(def-inline list* :always (t) t "#0")
(def-inline list* :always (t t) t "CONS(#0,#1)")

(proclaim-function make-list (fixnum *) list)
(proclaim-function append (*) list :no-side-effects t)
(def-inline append :always (t t) t "ecl_append(#0,#1)")

(proclaim-function copy-list (list) list)
(proclaim-function copy-alist (list) list)
(proclaim-function copy-tree (t) t)
(proclaim-function revappend (list t) t)
(proclaim-function nconc (*) t)
(def-inline nconc :always (t t) t "ecl_nconc(#0,#1)")

(proclaim-function nreconc (list t) t)
(proclaim-function butlast (list *) list)
(def-inline butlast :always (t) t "ecl_butlast(#0,1)")
(proclaim-function nbutlast (list *) list)
(def-inline nbutlast :always (t) t "ecl_nbutlast(#0,1)")
(proclaim-function ldiff (list t) list)
(proclaim-function rplaca (cons t) cons)
(proclaim-function rplacd (cons t) cons)
(proclaim-function subst (t t t *) t)
(proclaim-function subst-if (t t t *) t)
(proclaim-function subst-if-not (t t t *) t)
(proclaim-function nsubst (t t t *) t)
(proclaim-function nsubst-if (t t t *) t)
(proclaim-function nsubst-if-not (t t t *) t)
(proclaim-function sublis (list t *) t)
(proclaim-function nsublis (list t *) t)
(proclaim-function member (t list *) list)
(proclaim-function member-if (t list *) list)
(proclaim-function member-if-not (t list *) list)
(proclaim-function member1 (t t t t t) t)
(proclaim-function tailp (t list) t :predicate t)
(proclaim-function adjoin (t list *) list)
(proclaim-function acons (t t list) list)
(proclaim-function pairlis (list list *) list)
(proclaim-function assoc (t list *) list)
(proclaim-function assoc-if (t list *) list)
(proclaim-function assoc-if-not (t list *) list)
(proclaim-function rassoc (t list *) list)
(proclaim-function rassoc-if (t list *) list)
(proclaim-function rassoc-if-not (t list *) list)
(proclaim-function si:memq (t t t) t)

;; file macros.d

(proclaim-function si::define-macro (*) t)
(proclaim-function macroexpand (t *) (values t t))
(proclaim-function macroexpand-1 (t *) (values t t))

;; file main.d

(proclaim-function quit (*) t)
(proclaim-function identity (t) t)
(proclaim-function si:argc (*) t)
(proclaim-function si:argv (*) t)
(proclaim-function si:getenv (*) t)
(proclaim-function si:reset-stack-limits (*) t)
(proclaim-function si:pointer (*) t)

;; file mapfun.d

(proclaim-function mapcar (t t *) t)
(proclaim-function maplist (t t *) t)
(proclaim-function mapc (t t *) t)
(proclaim-function mapl (t t *) t)
(proclaim-function mapcan (t t *) t)
(proclaim-function mapcon (t t *) t)

;; file multival.d

(proclaim-function values (*) t)
(proclaim-function values-list (t) *)

;; file num_arith.d

(proclaim-function + (*) t :no-side-effects t)
(def-inline + :always (t t) t "ecl_plus(#0,#1)")
(def-inline + :always (fixnum-float fixnum-float) :double
 "(double)(#0)+(double)(#1)")
(def-inline + :always (fixnum-float fixnum-float) :float
 "(float)(#0)+(float)(#1)")
(def-inline + :always (fixnum fixnum) :fixnum "(#0)+(#1)")

(proclaim-function - (t *) t :no-side-effects t)
(def-inline - :always (t) t "ecl_negate(#0)")
(def-inline - :always (t t) t "ecl_minus(#0,#1)")
(def-inline - :always (fixnum-float fixnum-float) :double
 "(double)(#0)-(double)(#1)")
(def-inline - :always (fixnum-float fixnum-float) :float
 "(float)(#0)-(float)(#1)")
(def-inline - :always (fixnum fixnum) :fixnum "(#0)-(#1)")
(def-inline - :always (fixnum-float) :double "-(double)(#0)")
(def-inline - :always (fixnum-float) :float "-(float)(#0)")
(def-inline - :always (fixnum) :fixnum "-(#0)")

(proclaim-function * (*) t :no-side-effects t)
(def-inline * :always (t t) t "ecl_times(#0,#1)")
(def-inline * :always (fixnum-float fixnum-float) :double
 "(double)(#0)*(double)(#1)")
(def-inline * :always (fixnum-float fixnum-float) :float
 "(float)(#0)*(float)(#1)")
(def-inline * :always (fixnum fixnum) t "fixnum_times(#0,#1)")
(def-inline * :always (fixnum fixnum) :fixnum "(#0)*(#1)")

(proclaim-function / (t *) t :no-side-effects t)
(def-inline / :always (t t) t "ecl_divide(#0,#1)")
(def-inline / :always (fixnum-float fixnum-float) :double
 "(double)(#0)/(double)(#1)")
(def-inline / :always (fixnum-float fixnum-float) :float
 "(float)(#0)/(float)(#1)")
(def-inline / :always (fixnum fixnum) :fixnum "(#0)/(#1)")

(proclaim-function 1+ (t) t :no-side-effects t)
(def-inline 1+ :always (t) t "ecl_one_plus(#0)")
(def-inline 1+ :always (fixnum-float) :double "(double)(#0)+1")
(def-inline 1+ :always (fixnum-float) :float "(float)(#0)+1")
(def-inline 1+ :always (fixnum) :fixnum "(#0)+1")

(proclaim-function 1- (t) t :no-side-effects t)
(def-inline 1- :always (t) t "ecl_one_minus(#0)")
(def-inline 1- :always (fixnum-float) :double "(double)(#0)-1")
(def-inline 1- :always (fixnum-float) :float "(float)(#0)-1")
(def-inline 1- :always (fixnum) :fixnum "(#0)-1")

(proclaim-function conjugate (t) t)
(proclaim-function gcd (*) t)
(proclaim-function lcm (t *) t)

;; file num_co.d

(proclaim-function float (t *) t :no-side-effects t)
(def-inline float :always (t single-float) :float "ecl_to_double(#0)")
(def-inline float :always (t double-float) :double "ecl_to_double(#0)")
(def-inline float :always (fixnum-float) :double "((double)(#0))")
(def-inline float :always (fixnum-float) :float "((float)(#0))")

(proclaim-function numerator (t) t)
(proclaim-function denominator (t) t)
(proclaim-function floor (t *) (values t t) :no-side-effects t)
(def-inline floor :always (fixnum fixnum) :fixnum
 "@01;(#0>=0&&#1>0?(#0)/(#1):ecl_ifloor(#0,#1))")

(proclaim-function ceiling (t *) (values t t))
(proclaim-function truncate (t *) (values t t) :no-side-effects t)
(def-inline truncate :always (fixnum-float) :fixnum "(cl_fixnum)(#0)")

(proclaim-function round (t *) (values t t))
(proclaim-function mod (t t) t :no-side-effects t)
(def-inline mod :always (fixnum fixnum) :fixnum
 "@01;(#0>=0&&#1>0?(#0)%(#1):ecl_imod(#0,#1))")

(proclaim-function rem (t t) t :no-side-effects t)
(def-inline rem :always (fixnum fixnum) :fixnum "(#0)%(#1)")

(proclaim-function decode-float (t) (values t t t))
(proclaim-function scale-float (t t) t)
(proclaim-function float-radix (t) fixnum)
(proclaim-function float-sign (t *) t)
(proclaim-function float-digits (t) fixnum)
(proclaim-function float-precision (t) fixnum)
(proclaim-function integer-decode-float (t) (values t t t))
(proclaim-function complex (t *) t)
(proclaim-function realpart (t) t)
(proclaim-function imagpart (t) t)
(proclaim-function = (t *) t :predicate t :no-side-effects t)
(def-inline = :always (t t) :bool "ecl_number_equalp(#0,#1)")
(def-inline = :always (fixnum-float fixnum-float) :bool "(#0)==(#1)")

(proclaim-function /= (t *) t :predicate t :no-side-effects t)
(def-inline /= :always (t t) :bool "!ecl_number_equalp(#0,#1)")
(def-inline /= :always (fixnum-float fixnum-float) :bool "(#0)!=(#1)")

(proclaim-function < (t *) t :predicate t :no-side-effects t)
(def-inline < :always (t t) :bool "ecl_number_compare(#0,#1)<0")
(def-inline < :always (fixnum-float fixnum-float) :bool "(#0)<(#1)")

(proclaim-function > (t *) t :predicate t :no-side-effects t)
(def-inline > :always (t t) :bool "ecl_number_compare(#0,#1)>0")
(def-inline > :always (fixnum-float fixnum-float) :bool "(#0)>(#1)")

(proclaim-function <= (t *) t :predicate t :no-side-effects t)
(def-inline <= :always (t t) :bool "ecl_number_compare(#0,#1)<=0")
(def-inline <= :always (fixnum-float fixnum-float) :bool "(#0)<=(#1)")

(proclaim-function >= (t *) t :predicate t :no-side-effects t)
(def-inline >= :always (t t) :bool "ecl_number_compare(#0,#1)>=0")
(def-inline >= :always (fixnum-float fixnum-float) :bool "(#0)>=(#1)")

(proclaim-function max (t *) t :no-side-effects t)
(def-inline max :always (t t) t "@01;(ecl_number_compare(#0,#1)>=0?#0:#1)")
(def-inline max :always (fixnum fixnum) :fixnum "@01;(#0)>=(#1)?#0:#1")

(proclaim-function min (t *) t :no-side-effects t)
(def-inline min :always (t t) t "@01;(ecl_number_compare(#0,#1)<=0?#0:#1)")
(def-inline min :always (fixnum fixnum) :fixnum "@01;(#0)<=(#1)?#0:#1")

;; file num_log.d

(proclaim-function logand (*) integer :no-side-effects t)
(def-inline logand :always nil t "MAKE_FIXNUM(-1)")
(def-inline logand :always nil :fixnum "-1")
(def-inline logand :always (t t) t "ecl_boole(ECL_BOOLAND,(#0),(#1))")
(def-inline logand :always (fixnum fixnum) :fixnum "((#0) & (#1))")

(proclaim-function logandc1 (integer integer) integer :no-side-effects t)
(def-inline logandc1 :always (t t) t "ecl_boole(ECL_BOOLANDC1,(#0),(#1))")
(def-inline logandc1 :always (fixnum fixnum) :fixnum "(~(#0) & (#1))")

(proclaim-function logandc2 (integer integer) integer :no-side-effects t)
(def-inline logandc2 :always (t t) t "ecl_boole(ECL_BOOLANDC2,(#0),(#1))")
(def-inline logandc2 :always (fixnum fixnum) :fixnum "((#0) & ~(#1))")

(proclaim-function logeqv (*) integer :no-side-effects t)
(def-inline logeqv :always nil t "MAKE_FIXNUM(-1)")
(def-inline logeqv :always nil :fixnum "-1")
(def-inline logeqv :always (t t) t "ecl_boole(ECL_BOOLEQV,(#0),(#1))")
(def-inline logeqv :always (fixnum fixnum) :fixnum "(~( (#0) ^ (#1) ))")

(proclaim-function logior (*) integer :no-side-effects t)
(def-inline logior :always nil t "MAKE_FIXNUM(0)")
(def-inline logior :always nil :fixnum "0")
(def-inline logior :always (t t) t "ecl_boole(ECL_BOOLIOR,(#0),(#1))")
(def-inline logior :always (fixnum fixnum) :fixnum "((#0) | (#1))")

(proclaim-function lognand (integer integer) integer :no-side-effects t)
(def-inline lognand :always (t t) t "ecl_boole(ECL_BOOLNAND,(#0),(#1))")
(def-inline lognand :always (fixnum fixnum) :fixnum "(~( (#0) & (#1) ))")

(proclaim-function lognor (integer integer) integer :no-side-effects t)
(def-inline lognor :always (t t) t "ecl_boole(ECL_BOOLNOR,(#0),(#1))")
(def-inline lognor :always (fixnum fixnum) :fixnum "(~( (#0) | (#1) ))")

(proclaim-function lognot (integer) integer :no-side-effects t)
(def-inline lognot :always (t) t "ecl_boole(ECL_BOOLXOR,(#0),MAKE_FIXNUM(-1))")
(def-inline lognot :always (fixnum) :fixnum "(~(#0))")

(proclaim-function logorc1 (integer integer) integer :no-side-effects t)
(def-inline logorc1 :always (t t) t "ecl_boole(ECL_BOOLORC1,(#0),(#1))")
(def-inline logorc1 :always (fixnum fixnum) :fixnum "(~(#0) | (#1))")

(proclaim-function logorc2 (integer integer) integer :no-side-effects t)
(def-inline logorc2 :always (t t) t "ecl_boole(ECL_BOOLORC2,(#0),(#1))")
(def-inline logorc2 :always (fixnum fixnum) :fixnum "((#0) | ~(#1))")

(proclaim-function logxor (*) integer :no-side-effects t)
(def-inline logxor :always nil t "MAKE_FIXNUM(0)")
(def-inline logxor :always nil :fixnum "0")
(def-inline logxor :always (t t) t "ecl_boole(ECL_BOOLXOR,(#0),(#1))")
(def-inline logxor :always (fixnum fixnum) :fixnum "((#0) ^ (#1))")

(proclaim-function boole (t t t) t :no-side-effects t)
(def-inline boole :always (fixnum t t) t "ecl_boole((#0),(#1),(#2))")

(proclaim-function logbitp (t t) t :predicate t :no-side-effects t)
(def-inline logbitp :always ((integer -29 29) fixnum) :bool "(#1 >> #0) & 1")

(proclaim-function ash (integer integer) t)
(proclaim-function logcount (t) t)
(proclaim-function integer-length (t) fixnum)
(proclaim-function si:bit-array-op (*) t)
(proclaim-function zerop (t) t :predicate t :no-side-effects t)
(def-inline zerop :always (t) :bool "ecl_zerop(#0)")
(def-inline zerop :always (fixnum-float) :bool "(#0)==0")

(proclaim-function plusp (t) t :predicate t :no-side-effects t)
(def-inline plusp :always (t) :bool "ecl_plusp(#0)")
(def-inline plusp :always (fixnum-float) :bool "(#0)>0")

(proclaim-function minusp (t) t :predicate t :no-side-effects t)
(def-inline minusp :always (t) :bool "ecl_minusp(#0)")
(def-inline minusp :always (fixnum-float) :bool "(#0)<0")

(proclaim-function oddp (t) t :predicate t :no-side-effects t)
(def-inline oddp :always (t) :bool "ecl_oddp(#0)")
(def-inline oddp :always (fixnum fixnum) :bool "(#0) & 1")

(proclaim-function evenp (t) t :predicate t :no-side-effects t)
(def-inline evenp :always (t) :bool "ecl_evenp(#0)")
(def-inline evenp :always (fixnum fixnum) :bool "~(#0) & 1")

(proclaim-function random (t *) t)
(proclaim-function make-random-state (*) t)
(proclaim-function random-state-p (t) t :predicate t)
(proclaim-function expt (t t) t :no-side-effects t)
(def-inline expt :always (t t) t "cl_expt(#0,#1)")
(def-inline expt :always ((integer 2 2) (integer 0 29)) :fixnum "(1<<(#1))")
(def-inline expt :always ((integer 0 0) t) :fixnum "0")
(def-inline expt :always ((integer 1 1) t) :fixnum "1")

(proclaim-function log (t *) t :no-side-effects t)
(def-inline log :always (fixnum-float) :double "log((double)(#0))")
(def-inline log :always (fixnum-float) :float "(float)log((double)(#0))")

(proclaim-function sqrt (number) number :no-side-effects t)
(def-inline sqrt :always ((or (long-float 0.0 *) (double-float 0.0 *))) :double "sqrt((double)(#0))")
(def-inline sqrt :always ((or (single-float 0.0 *) (short-float 0.0 *))) :float "(float)sqrt((double)(#0))")

(proclaim-function sin (number) number :no-side-effects t)
(def-inline sin :always (fixnum-float) :double "sin((double)(#0))")
(def-inline sin :always (fixnum-float) :float "(float)sin((double)(#0))")

(proclaim-function cos (number) number :no-side-effects t)
(def-inline cos :always (fixnum-float) :double "cos((double)(#0))")
(def-inline cos :always (fixnum-float) :float "(float)cos((double)(#0))")

(proclaim-function tan (number) number :no-side-effects t)
(def-inline tan :always (fixnum-float) :double "tan((double)(#0))")
(def-inline tan :always (fixnum-float) :float "(float)tan((double)(#0))")

(proclaim-function atan (t *) t)

;; file package.d

(proclaim-function make-package (t *) t)
(proclaim-function si:select-package (t) t)
(proclaim-function find-package (t) t)
(proclaim-function package-name (t) t)
(proclaim-function package-nicknames (t) t)
(proclaim-function rename-package (t t *) t)
(proclaim-function package-use-list (t) t)
(proclaim-function package-used-by-list (t) t)
(proclaim-function package-shadowing-symbols (t) t)
(proclaim-function list-all-packages (*) t)
(proclaim-function intern (string *) (values t t))
(proclaim-function find-symbol (string *) (values t t))
(proclaim-function unintern (symbol t) t)
(proclaim-function export (t *) t)
(proclaim-function unexport (t *) t)
(proclaim-function import (t *) t)
(proclaim-function shadowing-import (t *) t)
(proclaim-function shadow (t *) t)
(proclaim-function use-package (t *) t)
(proclaim-function unuse-package (t *) t)
(proclaim-function si::package-internal (*) t)
(proclaim-function si::package-external (*) t)

;; file pathname.d

(proclaim-function pathname (t) t)
(proclaim-function parse-namestring (t *) t)
(proclaim-function merge-pathnames (t *) t)
(proclaim-function make-pathname (*) t)
(proclaim-function pathnamep (t) t :predicate t)
(proclaim-function pathname-host (t) t)
(proclaim-function pathname-device (t) t)
(proclaim-function pathname-directory (t) t)
(proclaim-function pathname-name (t) t)
(proclaim-function pathname-type (t) t)
(proclaim-function pathname-version (t) t)
(proclaim-function wild-pathname-p (t *) t)
(proclaim-function namestring (t) string)
(proclaim-function file-namestring (t) string)
(proclaim-function directory-namestring (t) string)
(proclaim-function host-namestring (t) string)
(proclaim-function enough-namestring (t *) string)

(proclaim-function null (t) t :predicate t :no-side-effects t)
(def-inline null :always (t) :bool "#0==Cnil")

(proclaim-function symbolp (t) t :predicate t :no-side-effects t)
(def-inline symbolp :always (t) :bool "SYMBOLP(#0)")

(proclaim-function atom (t) t :predicate t :no-side-effects t)
(def-inline atom :always (t) :bool "ATOM(#0)")

(proclaim-function consp (t) t :predicate t :no-side-effects t)
(def-inline consp :always (t) :bool "CONSP(#0)")

(proclaim-function listp (t) t :predicate t :no-side-effects t)
(def-inline listp :always (t) :bool "@0;LISTP(#0)")

(proclaim-function numberp (t) t :predicate t :no-side-effects t)
(def-inline numberp :always (t) :bool "ecl_numberp(#0)")

(proclaim-function integerp (t) t :predicate t :no-side-effects t)
(def-inline integerp :always (t) :bool
 "@0;type_of(#0)==t_fixnum||type_of(#0)==t_bignum")

(proclaim-function rationalp (t) t :predicate t)
(proclaim-function floatp (t) t :predicate t :no-side-effects t)
(def-inline floatp :always (t) :bool "floatp(#0)")

(proclaim-function complexp (t) t :predicate t)
(proclaim-function characterp (t) t :predicate t :no-side-effects t)
(def-inline characterp :always (t) :bool "CHARACTERP(#0)")

(proclaim-function base-char-p (t) t :predicate t :no-side-effects t)
(def-inline base-char-p :always (character) :bool "BASE_CHAR_P(#0)")

(proclaim-function stringp (t) t :predicate t :no-side-effects t)
(def-inline stringp :always (t) :bool "ecl_stringp(#0)")

(proclaim-function base-string-p (t) t :predicate t :no-side-effects t)
(def-inline base-string-p :always (t) :bool "type_of(#0)==t_base_string")

(proclaim-function bit-vector-p (t) t :predicate t :no-side-effects t)
(def-inline bit-vector-p :always (t) :bool "(type_of(#0)==t_bitvector)")

(proclaim-function vectorp (t) t :predicate t :no-side-effects t)
#-unicode
(def-inline vectorp :always (t) :bool "@0;type_of(#0)==t_vector||
type_of(#0)==t_base_string||
type_of(#0)==t_bitvector")
#+unicode
(def-inline vectorp :always (t) :bool "@0;type_of(#0)==t_vector||
type_of(#0)==t_base_string||
type_of(#0)==t_string||
type_of(#0)==t_bitvector")

(proclaim-function vector-push (t vector) (or fixnum null) :no-sp-change t)
(proclaim-function vector-push-extend (t vector *) fixnum :no-sp-change t)
(proclaim-function simple-string-p (t) t :predicate t)
(proclaim-function simple-bit-vector-p (t) t :predicate t)
(proclaim-function simple-vector-p (t) t :predicate t)
(proclaim-function arrayp (t) t :predicate t :no-side-effects t)
(def-inline arrayp :always (t) :bool "@0;ARRAYP(#0)")

(proclaim-function packagep (t) t :predicate t)
(proclaim-function functionp (t) t :predicate t)
(proclaim-function compiled-function-p (t) t :predicate t)
(proclaim-function eq (t t) t :predicate t :no-side-effects t)
(def-inline eq :always (t t) :bool "(#0)==(#1)")
(def-inline eq :always (fixnum fixnum) :bool "(#0)==(#1)")

(proclaim-function eql (t t) t :predicate t :no-side-effects t)
(def-inline eql :always (t t) :bool "ecl_eql(#0,#1)")
(def-inline eql :always (character t) :bool
 "(CHARACTERP(#1) && (#0)==CHAR_CODE(#1))")
(def-inline eql :always (t character) :bool
 "(CHARACTERP(#0) && CHAR_CODE(#0)==(#1))")
(def-inline eql :always (character character) :bool "(#0)==(#1)")
(def-inline eql :always ((not (or complex bignum ratio float)) t) :bool
 "(#0)==(#1)")
(def-inline eql :always (t (not (or complex bignum ratio float))) :bool
 "(#0)==(#1)")
(def-inline eql :always (fixnum fixnum) :bool "(#0)==(#1)")

(proclaim-function equal (t t) t :predicate t :no-side-effects t)
(def-inline equal :always (t t) :bool "ecl_equal(#0,#1)")
(def-inline equal :always (fixnum fixnum) :bool "(#0)==(#1)")

(proclaim-function equalp (t t) t :predicate t :no-side-effects t)
(def-inline equalp :always (t t) :bool "ecl_equalp(#0,#1)")
(def-inline equalp :always (fixnum fixnum) :bool "(#0)==(#1)")

(proclaim-function not (t) t :predicate t :no-side-effects t)
(def-inline not :always (t) :bool "(#0)==Cnil")

;; file print.d, read.d

(proclaim-function clear-output (*) NULL)
(def-inline clear-output :always (stream) NULL "(ecl_clear_output(#0),Cnil)")

(proclaim-function finish-output (*) NULL)
(def-inline finish-output :always (stream) NULL "(ecl_finish_output(#0),Cnil)")

(proclaim-function force-output (*) NULL)
(def-inline finish-output :always (stream) NULL "(ecl_force_output(#0),Cnil)")

(proclaim-function fresh-line (*) t)
(proclaim-function listen (*) t)
(proclaim-function peek-char (*) t)
(proclaim-function pprint (t *) t)
(proclaim-function prin1 (t *) t)
(def-inline prin1 :always (t t) t "ecl_prin1(#0,#1)")
(def-inline prin1 :always (t) t "ecl_prin1(#0,Cnil)")

(proclaim-function princ (t *) t)
(def-inline princ :always (t t) t "ecl_princ(#0,#1)")
(def-inline princ :always (t) t "ecl_princ(#0,Cnil)")

(proclaim-function print (t *) t)
(def-inline print :always (t t) t "ecl_print(#0,#1)")
(def-inline print :always (t) t "ecl_print(#0,Cnil)")

(proclaim-function probe-file (t) t :predicate t)
(proclaim-function unread-char (t *) t)
(proclaim-function read (*) t)
(proclaim-function read-char (*) t)
(proclaim-function read-delimited-list (t *) t)
(proclaim-function read-line (*) (values t t))
(proclaim-function read-preserving-whitespace (*) t)
(proclaim-function terpri (*) t :predicate t)
(def-inline terpri :always (t) t "ecl_terpri(#0)")
(def-inline terpri :always nil t "ecl_terpri(Cnil)")

(proclaim-function write (t *) t)
(proclaim-function write-byte (fixnum stream) t)
(proclaim-function write-char (t *) t)
(def-inline write-char :always (t) t "@0;(ecl_princ_char(ecl_char_code(#0),Cnil),(#0))")

(proclaim-function write-line (t *) t)
(proclaim-function write-string (t *) t)
(proclaim-function read-char-no-hang (*) t)
(proclaim-function clear-input (*) NULL)
(def-inline clear-input :always (stream) NULL "(ecl_clear_input(#0),Cnil)")

(proclaim-function parse-integer (t *) t)
(proclaim-function read-byte (t *) t)
(proclaim-function copy-readtable (*) t :no-side-effects t)
(def-inline copy-readtable :always (null null) t "standard_readtable")

(proclaim-function readtablep (t) t :predicate t)
(proclaim-function set-syntax-from-char (t t *) t)
(proclaim-function set-macro-character (t t *) t)
(proclaim-function get-macro-character (t *) t)
(proclaim-function make-dispatch-macro-character (*) t)
(proclaim-function set-dispatch-macro-character (*) t)
(proclaim-function get-dispatch-macro-character (*) t)
(proclaim-function si:string-to-object (t) t)
(proclaim-function si:standard-readtable (t) t)
(proclaim-function symbol-function (t) t)
(proclaim-function fboundp (symbol) t :predicate t)
(proclaim-function symbol-value (symbol) t)
(proclaim-function boundp (symbol) t :predicate t :no-side-effects t)
(def-inline boundp :always (symbol) :bool "SYM_VAL(#0)!=OBJNULL")

(proclaim-function macro-function (symbol) t)
(proclaim-function special-operator-p (symbol) t :predicate t)

;; file unixsys.d

(proclaim-function si:system (*) t)

;; file sequence.d

(proclaim-function elt (sequence fixnum) t :no-side-effects t)
(def-inline elt :always (t t) t "ecl_elt(#0,fixint(#1))")
(def-inline elt :always (t fixnum) t "ecl_elt(#0,#1)")
(def-inline elt :unsafe (t t) t "ecl_elt(#0,fix(#1))")
(def-inline elt :unsafe (t fixnum) t "ecl_elt(#0,#1)")

(proclaim-function si:elt-set (sequence fixnum t) t)
(def-inline si:elt-set :always (t t t) t "ecl_elt_set(#0,fixint(#1),#2)")
(def-inline si:elt-set :always (t fixnum t) t "ecl_elt_set(#0,#1,#2)")
(def-inline si:elt-set :unsafe (t t t) t "ecl_elt_set(#0,fix(#1),#2)")

(proclaim-function subseq (sequence fixnum *) sequence)
(proclaim-function copy-seq (sequence) sequence)
(proclaim-function length (sequence) fixnum :no-side-effects t)
(def-inline length :always (t) t "cl_length(#0)")
(def-inline length :always (t) :fixnum "ecl_length(#0)")
(def-inline length :unsafe ((array t)) :fixnum "(#0)->vector.fillp")
(def-inline length :unsafe (string) :fixnum "(#0)->base_string.fillp")

(proclaim-function reverse (sequence) sequence)
(proclaim-function nreverse (sequence) sequence)

;; file character.d

(proclaim-function char (string fixnum) character :no-side-effects t)
(def-inline char :always (t t) t "cl_char(#0,#1)")
(def-inline char :always (t fixnum) t "ecl_aref1(#0,#1)")
(def-inline char :unsafe (t t) t "CODE_CHAR((#0)->base_string.self[fix(#1)])")
(def-inline char :unsafe (t fixnum) :fixnum "(#0)->base_string.self[#1]")
(def-inline char :unsafe (t fixnum) :char "(#0)->base_string.self[#1]")

(proclaim-function si:char-set (string fixnum character) character)
(def-inline si:char-set :always (t t t) t "si_char_set(#0,#1,#2)")
(def-inline si:char-set :always (t fixnum t) t "ecl_aset1(#0,#1,#2)")
(def-inline si:char-set :unsafe (t t t) t
 "@2;((#0)->base_string.self[fix(#1)]=ecl_char_code(#2),(#2))")
(def-inline si:char-set :unsafe (t fixnum character) :char
 "(#0)->base_string.self[#1]= #2")

(proclaim-function schar (simple-string fixnum) character :no-side-effects t)
(def-inline schar :always (t t) t "ecl_elt(#0,fixint(#1))")
(def-inline schar :always (t fixnum) t "ecl_elt(#0,#1)")
(def-inline schar :unsafe ((array base-char (*)) t) t "CODE_CHAR((#0)->base_string.self[fix(#1)])")
#-unicode
(def-inline schar :unsafe (t t) :fixnum "(#0)->base_string.self[fix(#1)]")
#-unicode
(def-inline schar :unsafe (t fixnum) :fixnum "(#0)->base_string.self[#1]")
(def-inline schar :unsafe ((array base-char (*)) t) :fixnum "(#0)->base_string.self[fix(#1)]")
(def-inline schar :unsafe ((array base-char (*)) fixnum) :fixnum "(#0)->base_string.self[#1]")
(def-inline schar :unsafe ((array base-char (*)) fixnum) :char "(#0)->base_string.self[#1]")
#+unicode
(def-inline schar :unsafe ((array character (*)) t) t "(#0)->string.self[fix(#1)]")

(proclaim-function si:schar-set (string fixnum character) character)
(def-inline si:schar-set :always (t t t) t "ecl_elt_set(#0,fixint(#1),#2)")
(def-inline si:schar-set :always (t fixnum t) t "ecl_elt_set(#0,#1,#2)")
#-unicode
(def-inline si:schar-set :unsafe (t t t) t
 "@2;((#0)->base_string.self[fix(#1)]=ecl_char_code(#2),(#2))")
#-unicode
(def-inline si:schar-set :unsafe (t fixnum base-char) :char
 "(#0)->base_string.self[#1]= #2")
(def-inline si:schar-set :unsafe ((array base-char (*)) t t) t
 "@2;((#0)->base_string.self[fix(#1)]=ecl_char_code(#2),(#2))")
(def-inline si:schar-set :unsafe ((array base-char (*)) fixnum base-char) :char
 "(#0)->base_string.self[#1]= #2")
#+unicode
(def-inline si:schar-set :unsafe ((array character (*)) fixnum t) :char
 "(#0)->string.self[#1]= #2")

(proclaim-function string= (string-designator string-designator *) t :predicate t :no-side-effects t)
(def-inline string= :always (string string) :bool "ecl_string_eq(#0,#1)")

(proclaim-function string< (string-designator string-designator *) t :predicate t)
(proclaim-function string> (string-designator string-designator *) t :predicate t)
(proclaim-function string<= (string-designator string-designator *) t :predicate t)
(proclaim-function string>= (string-designator string-designator *) t :predicate t)
(proclaim-function string/= (string-designator string-designator *) t :predicate t)
(proclaim-function string-equal (string-designator string-designator *) t :predicate t
 :no-side-effects t)
(proclaim-function string-lessp (string-designator string-designator *) t :predicate t)
(proclaim-function string-greaterp (string-designator string-designator *) t :predicate t)
(proclaim-function string-not-lessp (string-designator string-designator *) t :predicate t)
(proclaim-function string-not-greaterp (string-designator string-designator *) t :predicate t)
(proclaim-function string-not-equal (string-designator string-designator *) t :predicate t)
(proclaim-function make-string (fixnum *) string)
(proclaim-function string-trim (t string-designator) string)
(proclaim-function string-left-trim (t string-designator) string)
(proclaim-function string-right-trim (t string-designator) string)
(proclaim-function string-upcase (string-designator *) string)
(proclaim-function string-downcase (string-designator *) string)
(proclaim-function string-capitalize (string-designator *) string)
(proclaim-function nstring-upcase (string *) string)
(proclaim-function nstring-downcase (string *) string)
(proclaim-function nstring-capitalize (string *) string)
(proclaim-function string (t) string :predicate t)
(proclaim-function string-concatenate (t) string)

;; file structure.d

(proclaim-function si:make-structure (t *) t)
(proclaim-function copy-structure (t) t)
(proclaim-function si:structure-name (t) symbol :no-side-effects t)
(def-inline si:structure-name :always (structure) symbol "SNAME(#0)")

(proclaim-function si:structure-ref (t t fixnum) t :no-side-effects t)
(def-inline si:structure-ref :always (t t fixnum) t "ecl_structure_ref(#0,#1,#2)")

(proclaim-function si:structure-set (t t fixnum t) t)
(def-inline si:structure-set :always (t t fixnum t) t
 "ecl_structure_set(#0,#1,#2,#3)")

(proclaim-function si:structurep (t) t :predicate t)
(proclaim-function si:structure-subtype-p (t t) t :predicate t)

(proclaim-function si:*make-special (*) t)
(proclaim-function si:*make-constant (*) t)

;; file symbol.d

(proclaim-function get (symbol t *) t :no-side-effects t)
(def-inline get :always (t t t) t "ecl_get(#0,#1,#2)")
(def-inline get :always (t t) t "ecl_get(#0,#1,Cnil)")

(proclaim-function remprop (symbol t) t)
(proclaim-function symbol-plist (symbol) t :predicate t :no-side-effects t)

(proclaim-function getf (t t *) t)
(proclaim-function get-properties (t t) *)
(proclaim-function symbol-name (symbol) string :no-side-effects t)
(def-inline symbol-name :always (t) string "ecl_symbol_name(#0)")

(proclaim-function make-symbol (string) symbol)
(proclaim-function copy-symbol (symbol *) symbol)
(proclaim-function gensym (*) symbol)
(proclaim-function gentemp (*) symbol)
(proclaim-function symbol-package (symbol) t)
(proclaim-function keywordp (t) t :predicate t)
(proclaim-function si:put-f (*) (t t))
(proclaim-function si:rem-f (*) (t t))
(proclaim-function si:set-symbol-plist (symbol t) t)
(proclaim-function si:putprop (t t t) t)
(proclaim-function si:put-sysprop (t t t) t)
(proclaim-function si:get-sysprop (t t t) t)
(proclaim-function si:rem-sysprop (t t) t)

;; file tcp.d

(proclaim-function si::open-tcp-stream (t t) t)

;; file unixtime.d

(proclaim-function get-universal-time () t)
(proclaim-function get-decoded-time () *)
(proclaim-function get-internal-run-time () t)
(proclaim-function get-internal-real-time () t)
(proclaim-function sleep (real) t)

;; file typeof.d

(proclaim-function type-of (t) t)

;; AKCL addition

(proclaim-function si:copy-stream (t t) t)

;; file seq.lsp

(proclaim-function make-seq-iterator (t *) t :no-sp-change t)
(proclaim-function seq-iterator-ref (sequence t) t :no-sp-change t)
(proclaim-function seq-iterator-set (sequence t t) t :no-sp-change t)
(proclaim-function seq-iterator-next (sequence t) t :no-sp-change t)

;; Additions used by the compiler.
;; The following functions do not exist. They are always expanded into the
;; given C code. References to these functions are generated in the C1 phase.

(proclaim-function shift>> (*) nil :no-side-effects t)
(def-inline shift>> :always (fixnum fixnum) :fixnum "((#0) >> (- (#1)))")

(proclaim-function shift<< (*) nil :no-side-effects t)
(def-inline shift<< :always (fixnum fixnum) :fixnum "((#0) << (#1))")

(proclaim-function short-float-p (*) nil :predicate t :no-side-effects t)
#-short-float
(def-inline short-float-p :always (t) :bool "type_of(#0)==t_singlefloat")
#+short-float
(def-inline short-float-p :always (t) :bool "type_of(#0)==t_shortfloat")

(proclaim-function single-float-p (*) nil :predicate t :no-side-effects t)
(def-inline single-float-p :always (t) :bool "type_of(#0)==t_singlefloat")

(proclaim-function double-float-p (*) nil :predicate t :no-side-effects t)
(def-inline double-float-p :always (t) :bool "type_of(#0)==t_doublefloat")

(proclaim-function long-float-p (*) nil :predicate t :no-side-effects t)
#-long-float
(def-inline long-float-p :always (t) :bool "type_of(#0)==t_doublefloat")
#+long-float
(def-inline long-float-p :always (t) :bool "type_of(#0)==t_longfloat")

(proclaim-function si:fixnump (*) nil :predicate t :no-side-effects t)
(def-inline si:fixnump :always (t) :bool "FIXNUMP(#0)")
(def-inline si:fixnump :always (fixnum) :bool "1")

(proclaim-function si:put-properties (*) nil :no-sp-change t)

(proclaim-function c::ldb1 (fixnum fixnum fixnum) fixnum :no-side-effects t)
(def-inline c::ldb1 :always (fixnum fixnum fixnum) :fixnum
 "((((~((cl_fixnum)-1 << (#0))) << (#1)) & (cl_fixnum)(#2)) >> (#1))")
(def-inline c::ldb1 :always (fixnum fixnum fixnum) t
 "MAKE_FIXNUM((((~((cl_fixnum)-1 << (#0))) << (#1)) & (cl_fixnum)(#2)) >> (#1))")

;; Functions only available with CLOS

#+clos(progn
(proclaim-function si:allocate-raw-instance (t t fixnum) t)
(proclaim-function si:instance-ref-safe (t fixnum) t)
(proclaim-function si:instance-ref (t fixnum) t :no-side-effects t)
(def-inline si:instance-ref :always (t fixnum) t "ecl_instance_ref((#0),(#1))")
(def-inline si:instance-ref :unsafe (standard-object fixnum) t
 "(#0)->instance.slots[#1]")

(proclaim-function si:instance-set (t fixnum t) t)
(def-inline si:instance-set :unsafe (t fixnum t) t
 "ecl_instance_set((#0),(#1),(#2))")
(def-inline si:instance-set :unsafe (standard-object fixnum t) t
 "(#0)->instance.slots[#1]=(#2)")

(proclaim-function si:instance-class (t) t :no-side-effects t)
(def-inline si:instance-class :always (standard-object) t "CLASS_OF(#0)")
(proclaim-function si:instance-class-set (t t) t)
(proclaim-function si:instancep (t) t :predicate t)
(def-inline si::instancep :always (t) :bool "@0;ECL_INSTANCEP(#0)")
(proclaim-function si:unbound (*) t :predicate t :no-side-effects t)
(def-inline si:unbound :always nil t "ECL_UNBOUND")

(proclaim-function si:sl-boundp (t) t :predicate t :no-side-effects t)
(def-inline si:sl-boundp :always (t) :bool "(#0)!=ECL_UNBOUND")

(proclaim-function si:sl-makunbound (t fixnum) t :predicate t)

(proclaim-function standard-instance-access (standard-object fixnum) t :no-side-effects t)
(def-inline standard-instance-access :always (standard-object fixnum) t "ecl_instance_ref((#0),(#1))")
(def-inline standard-instance-access :unsafe (standard-object fixnum) t
 "(#0)->instance.slots[#1]")

(proclaim-function funcallable-standard-instance-access (funcallable-standard-object fixnum) t :no-side-effects t)
(def-inline funcallable-standard-instance-access :always (funcallable-standard-object fixnum) t "ecl_instance_ref((#0),(#1))")
(def-inline funcallable-standard-instance-access :unsafe (funcallable-standard-object fixnum) t
 "(#0)->instance.slots[#1]")

(proclaim-function associate-methods-to-gfun (generic-function *) generic-function)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTIONS WHICH CAN BE CALLED FROM C
;;;
;;; The following two lists contain all functions in the core library which do
;;; not belong to the C part of the library, but which should have an exported C
;;; name that users (and compiled code) can refer to. This means, for instance, that
;;; MAKE-ARRAY will be compiled to a function called cl_make_array, etc.
;;;

(in-package "SI")

(defvar c::*in-all-symbols-functions*
  '(;; arraylib.lsp
    make-array vector array-dimensions array-in-bounds-p array-row-major-index
    bit sbit bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1
    bit-andc2 bit-orc1 bit-orc2 bit-not
    vector-push vector-push-extend vector-pop adjust-array
    ;; conditions.lsp
    si::safe-eval
    ;; iolib.lsp
    read-from-string write-to-string prin1-to-string princ-to-string
    y-or-n-p yes-or-no-p
    ;; listlib.lsp
    union nunion intersection nintersection set-difference nset-difference
    set-exclusive-or nset-exclusive-or subsetp rassoc-if rassoc-if-not
    assoc-if assoc-if-not member-if member-if-not subst-if subst-if-not
    nsubst-if nsubst-if-not
    ;; mislib.lsp
    logical-pathname-translations load-logical-pathname-translations decode-universal-time
    encode-universal-time get-decoded-time
    ensure-directories-exist si::simple-program-error si::signal-simple-error
    ;; module.lsp
    provide require
    ;; numlib.lsp
    isqrt phase signum cis
    asin acos asinh acosh atanh ffloor fceiling ftruncate fround
    logtest byte byte-size byte-position ldb ldb-test mask-field dpb
    deposit-field
    ;; packlib.lsp
    find-all-symbols apropos apropos-list
    find-relative-package package-parent package-children
    ;; predlib.lsp
    upgraded-array-element-type upgraded-complex-part-type typep subtypep coerce
    do-deftype
    ;; seq.lsp
    make-sequence concatenate map some every notany notevery map-into
    ;; seqlib.lsp
    reduce fill replace
    remove remove-if remove-if-not delete delete-if delete-if-not
    count count-if count-if-not substitute substitute-if substitute-if-not
    nsubstitute nsubstitute-if nsubstitute-if-not find find-if find-if-not
    position position-if position-if-not remove-duplicates
    delete-duplicates mismatch search sort stable-sort merge constantly
    ;; pprint.lsp
    pprint-fill copy-pprint-dispatch pprint-dispatch
    pprint-linear pprint-newline pprint-tab pprint-tabular
    set-pprint-dispatch pprint-indent .
    #-clos
    nil
    #+clos
    (;; combin.lsp
     method-combination-error
     invalid-method-error
     #-(or) standard-instance-access ; this function is a synonym for si:instance-ref
     #-(or) funcallable-standard-instance-access ; same for this one
     subclassp of-class-p
     )
))

(proclaim
  `(si::c-export-fname #+ecl-min ,@c::*in-all-symbols-functions*
    si::ecase-error si::etypecase-error si::do-check-type
    ccase-error typecase-error-string find-documentation find-declarations
    si::check-keyword si::check-arg-length si::dm-too-few-arguments si::dm-bad-key
    remove-documentation si::get-documentation
    si::set-documentation si::expand-set-documentation
    si::packages-iterator
    si::pprint-logical-block-helper si::pprint-pop-helper
    si::make-seq-iterator si::seq-iterator-ref si::seq-iterator-set si::seq-iterator-next
    si::structure-type-error si::define-structure
    si::coerce-to-list si::coerce-to-vector
    #+formatter
    ,@'(
    format-princ format-prin1 format-print-named-character
    format-print-integer
    format-print-cardinal format-print-ordinal format-print-old-roman
    format-print-roman format-fixed format-exponential
    format-general format-dollars
    format-relative-tab format-absolute-tab
    format-justification
	)
    #+clos
    ,@'(;; defclass.lsp
     clos::ensure-class
     ;; boot.lsp
     clos::slot-boundp
     clos::slot-makunbound
     clos::slot-value
     clos::slot-exists-p
     ;; combin.lsp
     clos::simple-code-walker
     ;; standard.lsp
     clos::safe-instance-ref
     clos::standard-instance-set
     ;; kernel.lsp
     clos::install-method
     clos::class-id
     clos::class-direct-superclasses
     clos::class-direct-subclasses
     clos::class-slots
     clos::class-precedence-list
     clos::class-direct-slots
     clos::default-initargs-of
     clos::generic-function-lambda-list
     clos::generic-function-argument-precedence-order
     clos::generic-function-method-combination
     clos::generic-function-method-class
     clos::generic-function-methods
     clos::method-generic-function
     clos::method-lambda-list
     clos::method-specializers
     clos::method-qualifiers
     clos::method-function
     clos::method-plist
     clos::associate-methods-to-gfun
     ;; method.lsp
     clos::pop-next-method
     )))

