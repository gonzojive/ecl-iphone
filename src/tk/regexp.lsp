;;;;
;;;; r e g e x p . l s p 		-- Regular expressions
;;;;
;;;;
;;;; Copyright (C) 1993,1994,1995 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;;; 
;;;; Permission to use, copy, and/or distribute this software and its
;;;; documentation for any purpose and without fee is hereby granted, provided
;;;; that both the above copyright notice and this permission notice appear in
;;;; all copies and derived works.  Fees for distribution or use of this
;;;; software or derived works may only be charged with express written
;;;; permission of the copyright holder.  
;;;; This software is provided ``as is'' without express or implied warranty.
;;;;
;;;;
;;;;           Author: Erick Gallesio [eg@unice.fr]
;;;;    Creation date:  9-Nov-1994 13:24
;;;; Last file update: 10-Jan-1995 23:47
;;;;

(if (symbol-bound? '%init-regexp)
    ;; Regexp module is in the core interpreter
    (%init-regexp)
    ;; Try to load regexp module dynamically
    (load "sregexp.so"))

(defun replace-string (string ind1 ind2 new)
  (string-append (substring string 0 ind1)
		 new
		 (substring string ind2 (string-length string))))

(define regexp-replace		NIL)
(define regexp-replace-all	NIL)

(let ()

  ;; Utility function
  ;; Given a string  and a set of substitutions, return the substitued string
  (defun replace-submodels (string subst match)
    (if (= (length match) 1)
	;; There is no sub-model
	subst
	;; There are at least one sub-model to replace
	(let Loop ((subst subst))
	  (let ((pos ((string->regexp "\\\\[0-9]") subst)))
	    (if pos
		;; At least one \x in the substitution string
		(let* ((index (+ (caar pos) 1))
		       (val   (string->number (substring subst index (+ index 1)))))
		  (if (>= val (length match))
		      (error "regexp-replace: cannot match \\~A in model" val)
		      ;; Build a new subst with the current \x remplaced by 
		      ;; its value. Iterate for further \x
		      (Loop (replace-string subst 
					    (caar pos)
					    (cadar pos)
					    (apply substring string
						   (nth val match))))))
		;; No \x in substitution string
		subst)))))

  ;; If there is a match, call replace-submodels; otherwise return string unmodified
  ;; This function takes an iterator function to allow multiple substitution
  ;; (iterator function = Identity for regexp-replace)
  (setq regexp-replace 
	(lambda (pat str subst)
	  (let* ((regexp (cond
			  ((regexp? pat) pat)
			  ((string? pat) (string->regexp pat))
			  (else  (error "regexp-replace: Bad pattern '~1'" pat))))
		 (match   (regexp str)))
	    (if match
		;; There was a match
		(replace-string str 
				(caar match) 
				(cadar match) 
				(replace-submodels str subst match))
		;; No match, return the original string
		str))))

  (setq regexp-replace-all 
	(lambda (pat str subst)
	  (let* ((regexp (cond
			  ((regexp? pat) pat)
			  ((string? pat) (string->regexp pat))
			  (else     (error "regexp-replace-all: Bad pattern '~1'" 
					   pat))))
		 (match   (regexp str)))
	    (if match
		;; There was a match
		(regexp-replace-all 
		 	pat 
			(replace-string str 
					(caar match) 
					(cadar match) 
					(replace-submodels str subst match))
			subst)
		;; No match, return the original string
		str)))))

(provide "regexp")
