;;;;
;;;; Help management
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
;;;;           Author: Erick Gallesio [eg@unice.fr]
;;;;    Creation date: 14-Sep-1993 13:30
;;;; Last file update: 27-Apr-1994 09:36
;;;;
;;;; Modified for ECL by Giuseppe Attardi [attardi@di.unipi.it]
;;;;

(provide "help")
(require "editor")

(defun make-help (STF)
  (catch-errors (destroy ".help"))
  (toplevel ".help")
  (wm "title" .help "ECL help")
  (wm "maxsize" .help 800 800)
  
  ; Create a dismiss button
  (button ".help.b" "-text" "Dismiss" "-command" '(destroy .help))

  ; Create a text widget with the content of list
  (frame ".help.f" "-relief" "sunken" "-bd" 2)
  (pack (scrollbar ".help.f.sb"
		   "-orient" "vertical" "-command" ".help.f.t \"yview\"")
	"-side" "left" "-fill" "y")
  (pack (text ".help.f.t" 
	      "-relief" "raised" "-bd" 2 "-padx" 12 "-pady" 12
	      "-height" 18 "-wrap" "word" "-yscroll" ".help.f.sb \"set\"")
	"-fill" "both" "-expand" T)


  (set-STF .help.f.t STF)
  ; Set the text read only
  (tk-setq .help.f.t "-state" "disabled")
  
  (pack .help.b "-side" "bottom" "-fill" "x" "-padx" 4 "-pady" 2)
  (pack .help.f "-fill" "both" "-expand" T "-padx" 4 "-pady" 2))
