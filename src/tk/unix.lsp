;;;;
;;;; u n i x  . l s p			-- Some unix stuff
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
;;;;           Author: Erick Gallesio [eg@kaolin.unice.fr]
;;;;    Creation date: 29-Mar-1994 17:36
;;;; Last file update:  2-Nov-1994 16:41
;;;;

;;;; This file implements
;;;;	(basename f)		
;;;;    (dirname f)
;;;;	(decompose-file-name f)		return f expoded in a list
;;;;	(file-is-directory?  f)
;;;;	(file-is-regular?    f)
;;;;	(file-is-readable?   f)
;;;;	(file-is-writable?   f)


(define basename 	    '())
(define dirname  	    '())
(define decompose-file-name '())


(let ()
  (defun delete-trailing-slashes (s)
    (let ((pos (- (string-length s) 1)))
      (while (and (>= pos 0) (char=? (string-ref s pos) #\/)) 
	     (setq pos (- pos 1)))
      (if (= pos -1)
	  "/"
	  (substring s 0 (+ pos 1)))))

  (defun decompose (name)
    (if (equal name "/")
	(cons "/" "")
	(progn
	  (let* ((f    (delete-trailing-slashes name))
	    	 (len  (string-length f))
		 (pos  (- len 1)))		 
	    
	    ;; find last slash
	    (while (and (>= pos 0) (not (char=? (string-ref f pos) #\/)))
		   (setq pos (- pos 1)))

	    (case pos
	      (-1   (cons "." (substring f 0 len)))
	      (0    (cons "/" (substring f 1 len)))
	      (else (cons (delete-trailing-slashes (substring f 0 pos))
			  (substring f (+ pos 1) len))))))))


  (setq basename 	    (lambda (file) (cdr (decompose file))))
  (setq dirname  	    (lambda (file) (car (decompose file))))
  (setq decompose-file-name (lambda (file) 
			      (letrec ((decomp (lambda (file res)
						 (if (equal file "/")
						     (cons file res)
						     (let ((r (decompose file)))
						       (decomp (car r)
							       (cons (cdr r)
								      res)))))))
				(decomp file '())))))

(provide "unix")
