(require 'cl)

(defun replace-in-files (matches files)
  (save-excursion
    (mapc (lambda (file)
	    (switch-to-buffer (or (find-buffer-visiting file) (find-file file)))
	    (mapc (lambda (x)
		    (beginning-of-buffer)
		    (let ((case-fold-search nil))
		      (print x)
		      (while (search-forward-regexp (car x) nil t)
			(replace-match (cdr x) nil t)))
		    (save-buffer)
		    )
		  matches))
	  files)))

(defun ecl-load-symbols ()
  (interactive)
  (beginning-of-buffer)
  (while (re-search-forward ";;; Address = \\([0-9a-f]*\\)" nil t)
    (let ((address (buffer-substring (match-beginning 1)
				     (match-end 1))))
      (re-search-backward ";;; Loading \\(/.*\.o\\)$")
      (let ((file (buffer-substring (match-beginning 1)
				    (match-end 1))))
	(print file) (print address)
	(save-excursion
	  (gud-call (format "add-symbol-file %s 0x%s" file address))))
      (next-line 2))))

(defvar ecl-search-string)

(defun search-ecl (string)
  (interactive "sString: ")
  (setq ecl-search-string string)
  (let ((remaining (member (buffer-file-name (current-buffer)) ecl-files)))
    (dolist (i (or remaining ecl-files))
      (let ((b (find-buffer-visiting i)))
	(unless (equal b (current-buffer))
	  (print b)
	  (switch-to-buffer b)
	  (beginning-of-buffer)))
      (print '*)
      (setq case-fold-search t)
      (if (search-forward string nil t)
	  (return)))))

(defun rewrite-ecl ()
  (interactive)
  (save-excursion
    (let ((case-fold-search nil))
      (while (re-search-forward "\\bL\\([XA_a-z=<>/0-9*]+\\)" nil t)
	(let* ((b (buffer-substring (match-beginning 0) (match-end 0)))
	       (a (buffer-substring (match-beginning 1) (match-end 1))))
	  (dotimes (i (length a))
	    (case (aref a i)
	      (?_ (aset a i ?-))
	      (?X (aset a i ?*))
	      (?A (aset a i ?&))))
	  (setq a (concat "@" a))
	  (when (y-or-n-p (concat "Replace " b " with " a " "))
	    (replace-match a t t))))))
  (save-excursion
    (let ((case-fold-search nil))
      (while (re-search-forward "\\bsiL\\([XA_a-z=<>/0-9*]+\\)" nil t)
	(let* ((b (buffer-substring (match-beginning 0) (match-end 0)))
	       (a (buffer-substring (match-beginning 1) (match-end 1))))
	  (dotimes (i (length a))
	    (case (aref a i)
	      (?_ (aset a i ?-))
	      (?X (aset a i ?*))
	      (?A (aset a i ?&))))
	  (setq a (concat "@si::" a))
	  (when (y-or-n-p (concat "Replace " b " with " a " "))
	    (replace-match a t t))))))
  (save-excursion
    (let ((case-fold-search nil))
      (while (re-search-forward "\\bS\\([XA_a-z=<>/0-9*]+\\)" nil t)
	(let* ((b (buffer-substring (match-beginning 0) (match-end 0)))
	       (a (buffer-substring (match-beginning 1) (match-end 1))))
	  (dotimes (i (length a))
	    (case (aref a i)
	      (?_ (aset a i ?-))
	      (?X (aset a i ?*))
	      (?A (aset a i ?&))))
	  (setq a (concat "@'" a "'"))
	  (when (y-or-n-p (concat "Replace " b " with " a " "))
	    (replace-match a t t))))))
  (save-excursion
    (let ((case-fold-search nil))
      (while (re-search-forward "\\bclS\\([XA_a-z=<>/0-9*]+\\)" nil t)
	(let* ((b (buffer-substring (match-beginning 0) (match-end 0)))
	       (a (buffer-substring (match-beginning 1) (match-end 1))))
	  (dotimes (i (length a))
	    (case (aref a i)
	      (?_ (aset a i ?-))
	      (?X (aset a i ?*))
	      (?A (aset a i ?&))))
	  (setq a (concat "@'" a "'"))
	  (when (y-or-n-p (concat "Replace " b " with " a " "))
	    (replace-match a t t))))))
  (save-excursion
    (let ((case-fold-search nil))
      (while (re-search-forward "\\bV\\([XA_a-z=<>/0-9*]+\\)" nil t)
	(let* ((b (buffer-substring (match-beginning 0) (match-end 0)))
	       (a (buffer-substring (match-beginning 1) (match-end 1))))
	  (dotimes (i (length a))
	    (case (aref a i)
	      (?_ (aset a i ?-))
	      (?X (aset a i ?*))
	      (?A (aset a i ?&))))
	  (setq a (concat "@'*" a "*'"))
	  (when (y-or-n-p (concat "Replace " b " with " a " "))
	    (replace-match a t t))))))
  (save-excursion
    (let ((case-fold-search nil))
      (while (re-search-forward "\\bsiS\\([XA_a-z=<>/0-9*]+\\)" nil t)
	(let* ((b (buffer-substring (match-beginning 0) (match-end 0)))
	       (a (buffer-substring (match-beginning 1) (match-end 1))))
	  (dotimes (i (length a))
	    (case (aref a i)
	      (?_ (aset a i ?-))
	      (?X (aset a i ?*))
	      (?A (aset a i ?&))))
	  (setq a (concat "@'si::" a "'"))
	  (when (y-or-n-p (concat "Replace " b " with " a " "))
	    (replace-match a t t))))))
  (save-excursion
    (let ((case-fold-search nil))
      (while (re-search-forward "\\bsiV\\([XA_a-z=<>/0-9*]+\\)" nil t)
	(let* ((b (buffer-substring (match-beginning 0) (match-end 0)))
	       (a (buffer-substring (match-beginning 1) (match-end 1))))
	  (dotimes (i (length a))
	    (case (aref a i)
	      (?_ (aset a i ?-))
	      (?X (aset a i ?*))
	      (?A (aset a i ?&))))
	  (setq a (concat "@'si::*" a "*'"))
	  (when (y-or-n-p (concat "Replace " b " with " a " "))
	    (replace-match a t t))))))
  (save-excursion
    (let ((case-fold-search nil))
      (while (re-search-forward "\\bK\\([XA_a-z=<>/0-9*]+\\)" nil t)
	(let* ((b (buffer-substring (match-beginning 0) (match-end 0)))
	       (a (buffer-substring (match-beginning 1) (match-end 1))))
	  (dotimes (i (length a))
	    (case (aref a i)
	      (?_ (aset a i ?-))
	      (?X (aset a i ?*))
	      (?A (aset a i ?&))))
	  (setq a (concat "@':" a "'"))
	  (when (y-or-n-p (concat "Replace " b " with " a " "))
	    (replace-match a t t)))))))

(defun search-next-ecl ()
  (interactive)
  (search-ecl ecl-search-string))

(defun back-to-emacs ()
  (interactive)
  (switch-to-buffer "emacs.el"))

(defun next-ecl ()
  (interactive)
  (let ((remaining (member (buffer-file-name (current-buffer)) ecl-files)))
    (when (cdr remaining)
      (switch-to-buffer (find-buffer-visiting (cadr remaining))))))

(global-set-key [?\221 ?\C-i] 'back-to-emacs)
(global-set-key [?\221 ?\C-s] 'search-ecl)
(global-set-key [?\221 ?\C-n] 'search-next-ecl)
(global-set-key [?\221 ?\C-m] 'next-ecl)
(global-set-key [?\221 ?\C-p] 'ecl-load-symbols)

(global-set-key [?\M-p ?\C-i] 'back-to-emacs)
(global-set-key [?\M-p ?\C-s] 'search-ecl)
(global-set-key [?\M-p ?\C-n] 'search-next-ecl)
(global-set-key [?\M-p ?\C-m] 'next-ecl)
(global-set-key [?\M-p ?\C-p] 'ecl-load-symbols)

(setq auto-mode-alist (acons "\\.d\\'" 'c-mode auto-mode-alist))

(setq ecl-files
      (mapcar (lambda (x) (concat (subseq (buffer-file-name (current-buffer)) 0 -13) x))
	      '(
"h/object.h"
"h/eval.h"
"h/external.h"
"h/stacks.h"
"c/character.d"
"c/gfun.d"
"c/num_comp.d"
"c/string.d"
"c/hash.d"
"c/num_log.d"
"c/structure.d"
"c/clos.d"
"c/num_pred.d"
"c/symbol.d"
"c/cmpaux.d"
"c/instance.d"
"c/num_rand.d"
"c/tclBasic.d"
"c/all_symbols.d"
"c/symbols_list.h"
"c/num_sfun.d"
"c/tcp.d"
"c/alloc.d"
"c/number.d"
"c/time.d"
"c/alloc_2.d"
"c/dosdummy.d"
"c/package.d"
"c/tkMain.d"
"c/apply.d"
"c/dostimes.d"
"c/list.d"
"c/pathname.d"
"c/array.d"
"c/dpp.c"
"c/load.d"
"c/predicate.d"
"c/typespec.d"
"c/assignment.d"
"c/earith.d"
"c/lwp.d"
"c/print.d"
;"c/unify.d"
"c/backq.d"
"c/error.d"
"c/macros.d"
"c/profile.d"
"c/unixfsys.d"
"c/big.d"
"c/eval.d"
"c/main.d"
"c/unixint.d"
"c/file.d"
"c/mapfun.d"
"c/read.d"
"c/unixsys.d"
"c/format.d"
"c/reference.d"
"c/num_arith.d"
"c/sequence.d"
"c/cfun.d"
"c/gbc.d"
"c/num_co.d"
"c/stacks.d"
"c/interpreter.d"
"c/compiler.d"
"c/disassembler.d"
"c/multival.d"
"lsp/ansi.lsp"
"lsp/arraylib.lsp"
"lsp/assert.lsp"
"lsp/autoload.lsp"
"lsp/cmpinit.lsp"
"lsp/defmacro.lsp"
"lsp/defpackage.lsp"
"lsp/defstruct.lsp"
"lsp/describe.lsp"
"lsp/evalmacros.lsp"
"lsp/export.lsp"
"lsp/helpfile.lsp"
"lsp/iolib.lsp"
"lsp/listlib.lsp"
"lsp/loop.lsp"
"lsp/loop2.lsp"
"lsp/mislib.lsp"
"lsp/module.lsp"
"lsp/numlib.lsp"
"lsp/packlib.lsp"
"lsp/predlib.lsp"
"lsp/proclaim.lsp"
"lsp/seq.lsp"
"lsp/seqlib.lsp"
"lsp/setf.lsp"
"lsp/thread.lsp"
"lsp/top.lsp"
"lsp/trace.lsp"
"lsp/util.lsp"
"clos/boot.lsp"
"clos/builtin.lsp"
"clos/change.lsp"
"clos/cmpinit.lsp"
"clos/combin.lsp"
"clos/defclass.lsp"
"clos/fixup.lsp"
"clos/generic.lsp"
"clos/init.lsp"
"clos/inspect.lsp"
"clos/kernel.lsp"
"clos/macros.lsp"
"clos/method.lsp"
"clos/precomp.lsp"
"clos/print.lsp"
"clos/slot.lsp"
"clos/standard.lsp"
"clos/stdmethod.lsp"
"clos/walk.lsp"
"clos/conditions.lsp"
"cmp/cmpbind.lsp"
"cmp/cmpblock.lsp"
"cmp/cmpcall.lsp"
"cmp/cmpcatch.lsp"
"cmp/cmpdefs.lsp"
"cmp/cmpenv.lsp"
"cmp/cmpeval.lsp"
"cmp/cmpexit.lsp"
"cmp/cmpflet.lsp"
"cmp/cmpfun.lsp"
"cmp/cmpif.lsp"
"cmp/cmpinline.lsp"
"cmp/cmplam.lsp"
"cmp/cmplet.lsp"
"cmp/cmploc.lsp"
"cmp/cmpmac.lsp"
"cmp/cmpmain.lsp"
"cmp/cmpmap.lsp"
"cmp/cmpmulti.lsp"
"cmp/cmpspecial.lsp"
"cmp/cmptag.lsp"
"cmp/cmptest.lsp"
"cmp/cmptop.lsp"
"cmp/cmptype.lsp"
"cmp/cmputil.lsp"
"cmp/cmpvar.lsp"
"cmp/cmpwt.lsp"
"cmp/sysfun.lsp"
; "clx/attributes.lsp"
; "clx/buffer.lsp"
; "clx/bufmac.lsp"
; "clx/clx.lsp"
; "clx/clxmain.lsp"
; "clx/cmpinit.lsp"
; "clx/defsystem.lsp"
; "clx/depdefs.lsp"
; "clx/dependent.lsp"
; "clx/display.lsp"
; "clx/doc.lsp"
; "clx/ecldep.lsp"
; "clx/ecllock.lsp"
; "clx/fonts.lsp"
; "clx/gcontext.lsp"
; "clx/graphics.lsp"
; "clx/image.lsp"
; "clx/init.lsp"
; "clx/input.lsp"
; "clx/keysyms.lsp"
; "clx/macros.lsp"
; "clx/manager.lsp"
; "clx/package.lsp"
; "clx/provide.lsp"
; "clx/requests.lsp"
; "clx/resource.lsp"
; "clx/sockcl.lsp"
; "clx/text.lsp"
; "clx/translate.lsp"
)))

(mapcar 'find-file ecl-files)

(defun ecl-revert ()
  (interactive)
  (mapcar '(lambda (x) (let ((a (find-buffer-visiting x)))
			 (and a (switch-to-buffer a)
			      (revert-buffer t t))))
	  ecl-files))
