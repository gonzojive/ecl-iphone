;;;;
;;;; i n i t . l s p			-- The file launched at startup
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
;;;;    Creation date: ??-Sep-1993 ??:??
;;;; Last file update: 12-Feb-1995 12:00
;;;;

(defvar @undefined (if NIL T))
(defvar *argc* (length *argv*)) 

(defvar !		system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Misc
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun widget? (obj)
  (and (tk-command? obj) (not (catch (obj "configure")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Autoloads
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload "unix" 	basename dirname decompose-file-name)
(autoload "process"	run-process process?)
(autoload "regexp"	string->regexp regexp? regexp-replace regexp-replace-all)

;; martine packages 
(autoload "pp"  pp)
(autoload "trace" tracef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; A set of binding to allow building of image files which contain the 
;;;; STklos-Tk classes.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar Tk:button	NIL)
(defvar Tk:checkbutton	NIL)
(defvar Tk:canvas	NIL)
(defvar Tk:entry	NIL)
(defvar Tk:frame	NIL)
(defvar Tk:label 	NIL)
(defvar Tk:listbox	NIL)
(defvar Tk:menu		NIL)
(defvar Tk:menubutton	NIL)
(defvar Tk:message	NIL)
(defvar Tk:scale	NIL)
(defvar Tk:scrollbar	NIL)
(defvar Tk:radiobutton	NIL)
(defvar Tk:toplevel	NIL)

(defvar Tk:after	NIL)
(defvar Tk:bind		NIL)
(defvar Tk:destroy	NIL)
(defvar Tk:focus	NIL)
(defvar Tk:grab		NIL)
(defvar Tk:lower	NIL)
(defvar Tk:option	NIL)
(defvar Tk:pack		NIL)
(defvar Tk:place	NIL)
(defvar Tk:raise	NIL)
(defvar Tk:selection	NIL)
(defvar Tk:tk		NIL)
(defvar Tk:tkwait	NIL)
(defvar Tk:update	NIL)
(defvar Tk:winfo	NIL)
(defvar Tk:wm		NIL)
