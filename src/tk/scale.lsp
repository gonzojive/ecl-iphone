;;;;
;;;; Scale bindings and procs
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
;;;; This software is a derivative work of other copyrighted softwares; the
;;;; copyright notices of these softwares are placed in the file COPYRIGHTS
;;;;
;;;;
;;;;           Author: Erick Gallesio [eg@unice.fr]
;;;;    Creation date: 17-May-1993 12:35
;;;; Last file update: 22-Nov-1993 16:06
;;;;
;;;; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;;;;

(in-package "TK")

;; ----------------------------------------------------------------------
;; Class bindings for scale widgets.  When strict Motif is requested,
;; the bindings use tk::buttons and tk::activeFg to set the
;; "-activeforeground" color to -foreground when the mouse is in the window
;; and restore it when the mouse leaves.
;; ----------------------------------------------------------------------

(def-bindings "Scale"
  '(("<Any-Enter>" 	  . (when tk::tk-strictMotif 
			      (setq tk::activeFg 
				    (tk-get %W "-activeforeground"))
			      (tk-setq %W "-activeforeground"
				       (tk-get %W "-sliderforeground"))))
    ("<Any-Leave>" 	  . (when (and tk::tk-strictMotif (= tk::buttons 0))
			      (tk-setq %W "-activeforeground" tk::activeFg))
     )
    ("<Any-ButtonPress>"   . (setq tk::buttons (+ tk::buttons 1)))
    ("<Any-ButtonRelease>" . (setq tk::buttons (- tk::buttons 1)))
    ))
