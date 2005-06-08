;;; Copyright (c) 2005, Michael Goffioul (michael dot goffioul at swing dot be)
;;;
;;;   This program is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Library General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2 of the License, or (at your option) any later version.
;;;
;;;   See file '../../Copyright' for full details.
;;;
;;; SAMPLE TEXT EDITOR APPLICATION USING THE WIN32 API
;;;

(require "WIN32" "win32")

(in-package "WIN32")

(defvar *txtedit-class-registered* nil)
(defvar *txtedit-width* 800)
(defvar *txtedit-height* 600)

(defvar *txtedit-edit* *NULL*)
(defvar *txtedit-dirty* nil)
(defvar *txtedit-default-title* "ECL Text Editor")

(defparameter +IDM_OPEN+ 100)
(defparameter +IDM_QUIT+ 101)
(defparameter +IDM_SAVE+ 102)
(defparameter +IDM_SAVEAS+ 103)
(defparameter +IDM_NEW+ 104)
(defparameter +IDM_CUT+ 105)
(defparameter +IDM_COPY+ 106)
(defparameter +IDM_PASTE+ 107)
(defparameter +IDM_UNDO+ 108)
(defparameter +IDM_SELECTALL+ 109)
(defparameter +IDM_ABOUT+ 110)
(defparameter +EDITCTL_ID+  1000)

(defparameter *txtedit-about-text*
"Text Editor for ECL.

This application serves as a demonstrator
for the WIN32 FFI interface of ECL.

Copyright (C), Michael Goffioul, 2005.")

(defun create-menus ()
  ;(return *NULL*)
  (let ((bar (createmenu))
	(file_pop (createpopupmenu))
	(edit_pop (createpopupmenu))
	(help_pop (createpopupmenu)))
    ;; File menu
    (appendmenu bar (logior *MF_STRING* *MF_POPUP*) (make-wparam file_pop) "&File")
    (appendmenu file_pop *MF_STRING* +IDM_NEW+ "&New	Ctrl+N")
    (appendmenu file_pop *MF_STRING* +IDM_OPEN+ "&Open...	Ctrl+O")
    (appendmenu file_pop *MF_SEPARATOR* 0 "")
    (appendmenu file_pop *MF_STRING* +IDM_SAVE+ "&Save	Ctrl+S")
    (appendmenu file_pop *MF_STRING* +IDM_SAVEAS+ "Save &As...")
    (appendmenu file_pop *MF_SEPARATOR* 0 "")
    (appendmenu file_pop *MF_STRING* +IDM_QUIT+ "&Exit	Ctrl+Q")
    ;; Edit menu
    (appendmenu bar (logior *MF_STRING* *MF_POPUP*) (make-wparam edit_pop) "&Edit")
    (appendmenu edit_pop *MF_STRING* +IDM_UNDO+ "&Undo	Ctrl+Z")
    (appendmenu edit_pop *MF_SEPARATOR* 0 "")
    (appendmenu edit_pop *MF_STRING* +IDM_CUT+ "&Cut	Ctrl+X")
    (appendmenu edit_pop *MF_STRING* +IDM_COPY+ "Cop&y	Ctrl+C")
    (appendmenu edit_pop *MF_STRING* +IDM_PASTE+ "&Paste	Ctrl+V")
    (appendmenu edit_pop *MF_SEPARATOR* 0 "")
    (appendmenu edit_pop *MF_STRING* +IDM_SELECTALL+ "&Select All	Ctrl+A")
    ;; Help menu
    (appendmenu bar (logior *MF_STRING* *MF_POPUP*) (make-wparam help_pop) "&Help")
    (appendmenu help_pop *MF_STRING* +IDM_ABOUT+ "&About...")
    bar))

(defun create-accels ()
  (macrolet ((add-accel (key ID accTable pos)
	       `(with-foreign-object (a 'ACCEL)
		  (setf (get-slot-value a 'ACCEL 'fVirt) (logior *FCONTROL* *FVIRTKEY*))
		  (setf (get-slot-value a 'ACCEL 'key) (char-code ,key))
		  (setf (get-slot-value a 'ACCEL 'cmd) ,ID)
		  (setf (deref-array ,accTable '(* ACCEL) ,pos) a))))
    (let ((accTable (allocate-foreign-object 'ACCEL 5)))
      (add-accel #\Q +IDM_QUIT+ accTable 0)
      (add-accel #\N +IDM_NEW+ accTable 1)
      (add-accel #\O +IDM_OPEN+ accTable 2)
      (add-accel #\S +IDM_SAVE+ accTable 3)
      (add-accel #\A +IDM_SELECTALL+ accTable 4)
      (prog1
	(createacceleratortable accTable 5)
	(free-foreign-object accTable)))))

(defun is-default-title (hwnd)
  (string= (string-right-trim "*" (getwindowtext hwnd)) *txtedit-default-title*))

(defun unix2dos (str)
  (let ((new-str (make-array (length str) :element-type 'character :adjustable t :fill-pointer 0))
	(return-p nil)
	c)
    (with-output-to-string (out new-str)
      (do ((it (si::make-seq-iterator str) (si::seq-iterator-next str it)))
	  ((null it))
        (case (setq c (si::seq-iterator-ref str it))
	  (#\Return (setq return-p t))
	  (#\Newline (unless return-p (write-char #\Return out)) (setq return-p nil))
	  (t (setq return-p nil)))
	(write-char c out)))
    new-str))

(defun read-file (pn hwnd)
  (format t "~&reading ~S~%" pn)
  (with-open-file (f pn)
    (let* ((len (file-length f))
	   (buf (make-string len)))
      (read-sequence buf f)
      (setwindowtext *txtedit-edit* (unix2dos buf))
      (setq *txtedit-dirty* nil)
      (setwindowtext hwnd (namestring pn)))))

(defun save-file (pn hwnd)
  (unless pn
    (setq pn (string-right-trim "*" (getwindowtext hwnd))))
  (with-open-file (f pn :direction :output :if-does-not-exist :create :if-exists :supersede)
    (let ((txt (getwindowtext *txtedit-edit*)))
      (write-sequence txt f)
      (setwindowtext hwnd (namestring pn))
      (setq *txtedit-dirty* nil))))

(defun txtedit-proc (hwnd umsg wparam lparam)
  (cond ((= umsg *WM_DESTROY*)
	 (postquitmessage 0)
	 0)
	((= umsg *WM_CLOSE*)
	 (if (and *txtedit-dirty*
		  (let ((m-result (messagebox *txtedit-edit* "Do you want to save changes?" "Confirmation"
					      (logior *MB_YESNOCANCEL* *MB_ICONQUESTION*))))
		    (cond ((= m-result *IDNO*) nil)
			  ((= m-result *IDCANCEL*) t)
			  ((= m-result *IDYES*) (warn "Not implemented") t))))
	   0
	   (destroywindow hwnd)))
	((= umsg *WM_CREATE*)
	 (setq *txtedit-edit* (createwindowex *WS_EX_CLIENTEDGE* "EDIT" ""
					      (logior *WS_CHILD* *WS_VISIBLE* *WS_HSCROLL* *WS_VSCROLL*
						      *ES_AUTOHSCROLL* *ES_AUTOVSCROLL* *ES_MULTILINE* *ES_LEFT*)
					      0 0 0 0 hwnd (make-ID +EDITCTL_ID+) *NULL* *NULL*))
	 (sendmessage *txtedit-edit* *WM_SETFONT* (make-wparam (getstockobject *SYSTEM_FIXED_FONT*)) 0)
	 (with-cast-int-pointer (lparam CREATESTRUCT)
	   (let ((params (get-slot-value lparam 'CREATESTRUCT 'lpCreateParams)))
	     (unless (null-pointer-p params)
	       (read-file (convert-from-foreign-string params) hwnd))))
	 0)
	((= umsg *WM_SIZE*)
	 (unless (null-pointer-p *txtedit-edit*)
	   (movewindow *txtedit-edit* 0 0 (loword lparam) (hiword lparam) *TRUE*))
	 0)
	((= umsg *WM_SETFOCUS*)
	 (unless (null-pointer-p *txtedit-edit*)
	   (setfocus *txtedit-edit*))
	 0)
	((= umsg *WM_COMMAND*)
	 (let ((ctrl-ID (loword wparam))
	       (nmsg (hiword wparam))
	       (hnd (make-pointer lparam 'HANDLE)))
	   (cond ((= ctrl-ID +EDITCTL_ID+)		  
		  (cond ((= nmsg *EN_CHANGE*)
			 (unless *txtedit-dirty*
			   (setwindowtext hwnd (concatenate 'string (getwindowtext hwnd) "*"))
			   (setq *txtedit-dirty* t)))
			(t
			 )))
		 ((= ctrl-ID +IDM_QUIT+)
		  (sendmessage hwnd *WM_CLOSE* 0 0))
		 ((= ctrl-ID +IDM_OPEN+)
		  (let ((pn (get-open-filename :owner hwnd :filter '(("LISP source file (*.lisp)" . "*.lisp;*.lsp")
								     ("All Files (*)" . "*")))))
		    (when pn
		      (read-file pn hwnd))))
		 ((and (= ctrl-ID +IDM_SAVE+)
		       (not (is-default-title hwnd)))
		  (save-file nil hwnd))
		 ((or (= ctrl-ID +IDM_SAVEAS+)
		      (and (= ctrl-ID +IDM_SAVE+)
			   (is-default-title hwnd)))
		  (let ((pn (get-open-filename :owner hwnd :filter '(("LISP source file (*.lisp)" . "*.lisp;*.lsp")
								     ("All Files (*)" . "*"))
					       :dlgfn #'getsavefilename :flags *OFN_OVERWRITEPROMPT*)))
		    (when pn
		      (save-file pn hwnd))))
		 ((= ctrl-ID +IDM_NEW+)
		  (setwindowtext *txtedit-edit* "")
		  (setwindowtext hwnd *txtedit-default-title*))
		 ((= ctrl-ID +IDM_CUT+)
		  (sendmessage *txtedit-edit* *WM_CUT* 0 0))
		 ((= ctrl-ID +IDM_COPY+)
		  (sendmessage *txtedit-edit* *WM_COPY* 0 0))
		 ((= ctrl-ID +IDM_PASTE+)
		  (sendmessage *txtedit-edit* *WM_PASTE* 0 0))
		 ((= ctrl-ID +IDM_UNDO+)
		  (unless (= (sendmessage *txtedit-edit* *EM_CANUNDO* 0 0) 0)
		    (sendmessage *txtedit-edit* *EM_UNDO* 0 0)))
		 ((= ctrl-ID +IDM_SELECTALL+)
		  (sendmessage *txtedit-edit* *EM_SETSEL* 0 -1))
		 ((= ctrl-ID +IDM_ABOUT+)
		  (messagebox hwnd *txtedit-about-text* "About" (logior *MB_OK* *MB_ICONINFORMATION*)))
		 (t
		   )))
	 0)
	(t
	 (defwindowproc hwnd umsg wparam lparam))
  ))

(defun register-txtedit-class ()
  (unless *txtedit-class-registered*
    (make-wndclass "SimpleTextEditor"
		   :lpfnWndProc #'txtedit-proc)
    (setq *txtedit-class-registered* t)))

(defun unregister-txtedit-class ()
  (when *txtedit-class-registered*
    (unregisterclass "SimpleTextEditor" *NULL*)
    (setq *txtedit-class-registered* nil)))

(defun txtedit (&optional fname)
  (register-txtedit-class)
  (let* ((fname-str (if fname
		      (convert-to-foreign-string (coerce fname 'simple-string))
		      *NULL*))
	 (w (createwindow "SimpleTextEditor"
			 *txtedit-default-title*
			 *WS_OVERLAPPEDWINDOW*
			 *CW_USEDEFAULT* *CW_USEDEFAULT*
			 *txtedit-width* *txtedit-height*
			 *NULL* (create-menus) *NULL* fname-str))
	 (accTable (create-accels)))
    (showwindow w *SW_SHOWNORMAL*)
    (updatewindow w)
    (event-loop :accelTable accTable :accelMain w)
    (setq *txtedit-edit* *NULL*)
    (destroyacceleratortable accTable)
    (unless (null-pointer-p fname-str)
      (free-foreign-object fname-str))
    (unregister-txtedit-class)
    nil))
