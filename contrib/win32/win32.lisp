;;; Copyright (c) 2005, Michael Goffioul (michael dot goffioul at swing dot be)
;;;
;;;   This program is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Library General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2 of the License, or (at your option) any later version.
;;;
;;;   See file '../../Copyright' for full details.
;;;
;;; FOREIGN FUNCTION INTERFACE TO MICROSOFT WINDOWS API
;;;

(defpackage "WIN32"
  (:use "COMMON-LISP" "FFI")
  (:export))

(in-package "WIN32")

(clines "#define WINVER 0x500")
(clines "#include <windows.h>")
(clines "#include <commctrl.h>")

;; Windows types

(def-foreign-type HANDLE :pointer-void)
(def-foreign-type LPCSTR :cstring)
(def-foreign-type WNDPROC :pointer-void)

;; Windows constants

(defmacro define-win-constant (name c-name &optional (c-type :int))
  `(defconstant ,name (c-inline () () ,(ffi::%convert-to-ffi-type c-type) ,c-name :one-liner t)))

(define-win-constant *TRUE* "TRUE")
(define-win-constant *FALSE* "FALSE")

(define-win-constant *WM_CLOSE* "WM_CLOSE")
(define-win-constant *WM_COMMAND* "WM_COMMAND")
(define-win-constant *WM_COPY* "WM_COPY")
(define-win-constant *WM_CREATE* "WM_CREATE")
(define-win-constant *WM_CUT* "WM_CUT")
(define-win-constant *WM_DESTROY* "WM_DESTROY")
(define-win-constant *WM_GETFONT* "WM_GETFONT")
(define-win-constant *WM_GETMINMAXINFO* "WM_GETMINMAXINFO")
(define-win-constant *WM_INITMENU* "WM_INITMENU")
(define-win-constant *WM_INITMENUPOPUP* "WM_INITMENUPOPUP")
(define-win-constant *WM_NCPAINT* "WM_NCPAINT")
(define-win-constant *WM_NOTIFY* "WM_NOTIFY")
(define-win-constant *WM_PAINT* "WM_PAINT")
(define-win-constant *WM_PASTE* "WM_PASTE")
(define-win-constant *WM_QUIT* "WM_QUIT")
(define-win-constant *WM_SETFOCUS* "WM_SETFOCUS")
(define-win-constant *WM_SETFONT* "WM_SETFONT")
(define-win-constant *WM_SIZE* "WM_SIZE")
(define-win-constant *WM_UNDO* "WM_UNDO")

(define-win-constant *WS_BORDER* "WS_BORDER")
(define-win-constant *WS_CHILD* "WS_CHILD")
(define-win-constant *WS_CLIPCHILDREN* "WS_CLIPCHILDREN")
(define-win-constant *WS_CLIPSIBLINGS* "WS_CLIPSIBLINGS")
(define-win-constant *WS_DLGFRAME* "WS_DLGFRAME")
(define-win-constant *WS_DISABLED* "WS_DISABLED")
(define-win-constant *WS_HSCROLL* "WS_HSCROLL")
(define-win-constant *WS_OVERLAPPEDWINDOW* "WS_OVERLAPPEDWINDOW")
(define-win-constant *WS_VISIBLE* "WS_VISIBLE")
(define-win-constant *WS_VSCROLL* "WS_VSCROLL")

(define-win-constant *WS_EX_CLIENTEDGE* "WS_EX_CLIENTEDGE")

(define-win-constant *WC_LISTVIEW* "WC_LISTVIEW" LPCSTR)
(define-win-constant *WC_TABCONTROL* "WC_TABCONTROL" LPCSTR)

(define-win-constant *HWND_BOTTOM* "HWND_BOTTOM" HANDLE)
(define-win-constant *HWND_NOTOPMOST* "HWND_NOTOPMOST" HANDLE)
(define-win-constant *HWND_TOP* "HWND_TOP" HANDLE)
(define-win-constant *HWND_TOPMOST* "HWND_TOPMOST" HANDLE)

(define-win-constant *SWP_DRAWFRAME* "SWP_DRAWFRAME")
(define-win-constant *SWP_HIDEWINDOW* "SWP_HIDEWINDOW")
(define-win-constant *SWP_NOMOVE* "SWP_NOMOVE")
(define-win-constant *SWP_NOOWNERZORDER* "SWP_NOOWNERZORDER")
(define-win-constant *SWP_NOREDRAW* "SWP_NOREDRAW")
(define-win-constant *SWP_NOREPOSITION* "SWP_NOREPOSITION")
(define-win-constant *SWP_NOSIZE* "SWP_NOSIZE")
(define-win-constant *SWP_NOZORDER* "SWP_NOZORDER")
(define-win-constant *SWP_SHOWWINDOW* "SWP_NOZORDER")

(define-win-constant *BS_DEFPUSHBUTTON* "BS_DEFPUSHBUTTON")
(define-win-constant *BS_PUSHBUTTON* "BS_PUSHBUTTON")

(define-win-constant *BN_CLICKED* "BN_CLICKED")

(define-win-constant *ES_AUTOHSCROLL* "ES_AUTOHSCROLL")
(define-win-constant *ES_AUTOVSCROLL* "ES_AUTOVSCROLL")
(define-win-constant *ES_LEFT* "ES_LEFT")
(define-win-constant *ES_MULTILINE* "ES_MULTILINE")

(define-win-constant *EM_CANUNDO* "EM_CANUNDO")
(define-win-constant *EM_SETSEL* "EM_SETSEL")
(define-win-constant *EM_UNDO* "EM_UNDO")
(define-win-constant *EN_CHANGE* "EN_CHANGE")

(define-win-constant *TCIF_IMAGE* "TCIF_IMAGE")
(define-win-constant *TCIF_PARAM* "TCIF_PARAM")
(define-win-constant *TCIF_RTLREADING* "TCIF_RTLREADING")
(define-win-constant *TCIF_STATE* "TCIF_STATE")
(define-win-constant *TCIF_TEXT* "TCIF_TEXT")

(define-win-constant *TCM_ADJUSTRECT* "TCM_ADJUSTRECT")
(define-win-constant *TCM_DELETEITEM* "TCM_DELETEITEM")
(define-win-constant *TCM_GETCURSEL* "TCM_GETCURSEL")
(define-win-constant *TCM_INSERTITEM* "TCM_INSERTITEM")
(define-win-constant *TCM_SETCURSEL* "TCM_SETCURSEL")
(define-win-constant *TCM_SETITEM* "TCM_SETITEM")
(define-win-constant *TCN_SELCHANGE* "TCN_SELCHANGE" :unsigned-int)

(define-win-constant *SW_HIDE* "SW_HIDE")
(define-win-constant *SW_SHOW* "SW_SHOW")
(define-win-constant *SW_SHOWNORMAL* "SW_SHOWNORMAL")

(define-win-constant *RDW_ERASE* "RDW_ERASE")
(define-win-constant *RDW_FRAME* "RDW_FRAME")
(define-win-constant *RDW_INTERNALPAINT* "RDW_INTERNALPAINT")
(define-win-constant *RDW_INVALIDATE* "RDW_INVALIDATE")
(define-win-constant *RDW_NOERASE* "RDW_NOERASE")
(define-win-constant *RDW_NOFRAME* "RDW_NOFRAME")
(define-win-constant *RDW_NOINTERNALPAINT* "RDW_NOINTERNALPAINT")
(define-win-constant *RDW_VALIDATE* "RDW_VALIDATE")
(define-win-constant *RDW_ERASENOW* "RDW_ERASENOW")
(define-win-constant *RDW_UPDATENOW* "RDW_UPDATENOW")
(define-win-constant *RDW_ALLCHILDREN* "RDW_ALLCHILDREN")
(define-win-constant *RDW_NOCHILDREN* "RDW_NOCHILDREN")

(define-win-constant *CW_USEDEFAULT* "CW_USEDEFAULT")

(define-win-constant *IDC_ARROW* "IDC_ARROW")
(define-win-constant *IDI_APPLICATION* "IDI_APPLICATION")

(define-win-constant *COLOR_BACKGROUND* "COLOR_BACKGROUND")
(define-win-constant *DEFAULT_GUI_FONT* "DEFAULT_GUI_FONT")
(define-win-constant *OEM_FIXED_FONT* "OEM_FIXED_FONT")
(define-win-constant *SYSTEM_FONT* "SYSTEM_FONT")
(define-win-constant *SYSTEM_FIXED_FONT* "SYSTEM_FIXED_FONT")

(define-win-constant *MB_HELP* "MB_HELP")
(define-win-constant *MB_OK* "MB_OK")
(define-win-constant *MB_OKCANCEL* "MB_OKCANCEL")
(define-win-constant *MB_YESNO* "MB_YESNO")
(define-win-constant *MB_YESNOCANCEL* "MB_YESNOCANCEL")
(define-win-constant *MB_ICONEXCLAMATION* "MB_ICONEXCLAMATION")
(define-win-constant *MB_ICONWARNING* "MB_ICONWARNING")
(define-win-constant *MB_ICONINFORMATION* "MB_ICONINFORMATION")
(define-win-constant *MB_ICONQUESTION* "MB_ICONQUESTION")

(define-win-constant *IDCANCEL* "IDCANCEL")
(define-win-constant *IDNO* "IDNO")
(define-win-constant *IDOK* "IDOK")
(define-win-constant *IDYES* "IDYES")

(define-win-constant *MF_BYCOMMAND* "MF_BYCOMMAND")
(define-win-constant *MF_BYPOSITION* "MF_BYPOSITION")
(define-win-constant *MF_CHECKED* "MF_CHECKED")
(define-win-constant *MF_DISABLED* "MF_DISABLED")
(define-win-constant *MF_ENABLED* "MF_ENABLED")
(define-win-constant *MF_MENUBREAK* "MF_MENUBREAK")
(define-win-constant *MF_POPUP* "MF_POPUP")
(define-win-constant *MF_SEPARATOR* "MF_SEPARATOR")
(define-win-constant *MF_STRING* "MF_STRING")
(define-win-constant *MF_UNCHECKED* "MF_UNCHECKED")

(define-win-constant *OFN_FILEMUSTEXIST* "OFN_FILEMUSTEXIST")
(define-win-constant *OFN_OVERWRITEPROMPT* "OFN_OVERWRITEPROMPT")
(define-win-constant *OFN_PATHMUSTEXIST* "OFN_PATHMUSTEXIST")
(define-win-constant *OFN_READONLY* "OFN_READONLY")

(define-win-constant *FVIRTKEY* "FVIRTKEY")
(define-win-constant *FNOINVERT* "FNOINVERT")
(define-win-constant *FSHIFT* "FSHIFT")
(define-win-constant *FCONTROL* "FCONTROL")
(define-win-constant *FALT* "FALT")

(define-win-constant *VK_F1* "VK_F1")
(define-win-constant *VK_LEFT* "VK_LEFT")
(define-win-constant *VK_RIGHT* "VK_RIGHT")

(define-win-constant *GWL_EXSTYLE* "GWL_EXSTYLE")
(define-win-constant *GWL_HINSTANCE* "GWL_HINSTANCE")
(define-win-constant *GWL_HWNDPARENT* "GWL_HWNDPARENT")
(define-win-constant *GWL_ID* "GWL_ID")
(define-win-constant *GWL_STYLE* "GWL_STYLE")
(define-win-constant *GWL_WNDPROC* "GWL_WNDPROC")

(defconstant *NULL* (make-null-pointer :pointer-void))

;; Windows structures

(def-struct WNDCLASS
	    (style :unsigned-int)
	    (lpfnWndProc WNDPROC)
	    (cbClsExtra :int)
	    (cbWndExtra :int)
	    (hInstance HANDLE)
	    (hIcon HANDLE)
	    (hCursor HANDLE)
	    (hbrBackground HANDLE)
	    (lpszMenuName :cstring)
	    (lpszClassName :cstring))
(defun make-wndclass (name &key (style 0) (lpfnWndProc nil) (cbClsExtra 0) (cbWndExtra 0) (hInstance *NULL*)
			        (hIcon (default-icon)) (hCursor (default-cursor)) (hbrBackground (default-background))
				(lpszMenuName ""))
  (with-foreign-object (cls 'WNDCLASS)
    (setf (get-slot-value cls 'WNDCLASS 'style) style
	  (get-slot-value cls 'WNDCLASS 'lpfnWndProc) *DEFAULT_WNDPROC*
	  (get-slot-value cls 'WNDCLASS 'cbClsExtra) cbClsExtra
	  (get-slot-value cls 'WNDCLASS 'cbWndExtra) cbWndExtra
	  (get-slot-value cls 'WNDCLASS 'hInstance) hInstance
	  (get-slot-value cls 'WNDCLASS 'hIcon) hIcon
	  (get-slot-value cls 'WNDCLASS 'hCursor) hCursor
	  (get-slot-value cls 'WNDCLASS 'hbrBackground) hbrBackground
	  (get-slot-value cls 'WNDCLASS 'lpszMenuName) lpszMenuName
	  (get-slot-value cls 'WNDCLASS 'lpszClassName) (string name))
    (register-wndproc (string name) lpfnWndProc)
    (registerclass cls)))
(def-struct POINT
	    (x :int)
	    (y :int))
(def-struct MSG
	    (hwnd HANDLE)
	    (message :unsigned-int)
	    (wParam :unsigned-int)
	    (lParam :int)
	    (time :unsigned-int)
	    (pt POINT))
(def-struct CREATESTRUCT
	    (lpCreateParams :pointer-void)
	    (hInstance HANDLE)
	    (hMenu HANDLE)
	    (hwndParent HANDLE)
	    (cx :int)
	    (cy :int)
	    (x :int)
	    (y :int)
	    (style :long)
	    (lpszName :cstring)
	    (lpszClass :cstring)
	    (dwExStyle :unsigned-int))
(def-struct MINMAXINFO
	    (ptReserved POINT)
	    (ptMaxSize POINT)
	    (ptMaxPosition POINT)
	    (ptMinTrackSize POINT)
	    (ptMaxTrackSize POINT))
(def-struct TEXTMETRIC (tmHeight :long) (tmAscent :long) (tmDescent :long) (tmInternalLeading :long) (tmExternalLeading :long)
	               (tmAveCharWidth :long) (tmMaxCharWidth :long) (tmWeight :long) (tmOverhang :long) (tmDigitizedAspectX :long)
		       (tmDigitizedAspectY :long) (tmFirstChar :char) (tmLastChar :char) (tmDefaultChar :char) (tmBreakChar :char)
		       (tmItalic :byte) (tmUnderlined :byte) (tmStruckOut :byte) (tmPitchAndFamily :byte) (tmCharSet :byte))
(def-struct SIZE (cx :long) (cy :long))
(def-struct RECT (left :long) (top :long) (right :long) (bottom :long))
(def-struct TITLEBARINFO (cbSize :unsigned-int) (rcTitlebar RECT) (rgstate (:array :unsigned-int 6)))
(def-struct OPENFILENAME (lStructSize :unsigned-int) (hwndOwner HANDLE) (hInstance HANDLE) (lpstrFilter LPCSTR) (lpstrCustomFilter LPCSTR)
	                 (nMaxFilter :unsigned-int) (nFilterIndex :unsigned-int) (lpstrFile LPCSTR) (nMaxFile :unsigned-int) (lpstrFileTitle LPCSTR)
			 (nMaxFileTitle :unsigned-int) (lpstrInitialDir LPCSTR) (lpstrTitle LPCSTR) (Flags :unsigned-int) (nFileOffset :unsigned-short)
			 (nFileExtension :unsigned-short) (lpstrDefExt LPCSTR) (lCustData :int) (lpfnHook HANDLE) (lpTemplateName LPCSTR)
			 (pvReserved :pointer-void) (dwReserved :unsigned-int) (FlagsEx :unsigned-int))
(def-struct ACCEL (fVirt :byte) (key :unsigned-short) (cmd :unsigned-short))
(def-struct TCITEM (mask :unsigned-int) (dwState :unsigned-int) (dwStateMask :unsigned-int)
	           (pszText :cstring) (cchTextMax :int) (iImage :int) (lParam :long))
(def-struct NMHDR (hwndFrom HANDLE) (idFrom :unsigned-int) (code :unsigned-int))

;; Windows functions

(defvar *wndproc-db* nil)
(defun register-wndproc (class-or-obj wndproc)
  (let ((entry (assoc class-or-obj *wndproc-db* :test #'equal)))
    (if entry
      (rplacd entry wndproc)
      (push (cons class-or-obj wndproc) *wndproc-db*)))
  (unless (stringp class-or-obj)
    (let ((old-proc (make-pointer (getwindowlong class-or-obj *GWL_WNDPROC*) 'HANDLE)))
      (setwindowlong class-or-obj *GWL_WNDPROC* (make-lparam *DEFAULT_WNDPROC*))
      old-proc)))
(defun get-wndproc (obj)
  (let ((entry (or (assoc obj *wndproc-db* :test #'equal)
		   (assoc (getclassname obj) *wndproc-db* :test #'equal))))
    (and entry
	 (cdr entry))))
(eval-when (compile)
  (proclaim '(si::c-export-fname win32::wndproc-proxy)))
(defun wndproc-proxy (hnd umsg wparam lparam)
  (let* ((wndproc (get-wndproc hnd)))
    (unless wndproc
      (error "Cannot find a registered Windows prodecure for object ~S" hnd))
    (funcall wndproc hnd umsg wparam lparam)))
(clines "
LRESULT CALLBACK WndProc_proxy(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return object_to_fixnum(
		win32_wndproc_proxy(
		  4,
		  ecl_make_foreign_data(make_keyword(\"POINTER-VOID\"),0,hwnd),
		  make_unsigned_integer(uMsg),
		  make_unsigned_integer(wParam),
		  make_integer(lParam)));
}
")
(defconstant *DEFAULT_WNDPROC* (c-inline () () :pointer-void "WndProc_proxy" :one-liner t))
(defun make-ID (id)
  (c-inline (id :pointer-void) (:unsigned-int :object) :object "ecl_make_foreign_data(#1,0,((void*)#0))" :one-liner t))
(setf (symbol-function 'make-handle) #'make-ID)
(defun make-wparam (hnd)
  (c-inline (hnd) (:pointer-void) :unsigned-int "(WPARAM)#0" :one-liner t))
(defun make-lparam (hnd)
  (c-inline (hnd) (:pointer-void) :unsigned-int "(LPARAM)#0" :one-liner t))
(defmacro with-cast-int-pointer ((var type &optional ptr) &body body)
  (unless ptr (setq ptr var))
  `(let ((,var (make-pointer ,ptr ',type))) ,@body))

(def-function ("ZeroMemory" zeromemory) ((Destination :pointer-void) (Length :unsigned-int)) :returning :void)
(def-function ("GetStockObject" getstockobject) ((fnObject :int)) :returning HANDLE)
(def-function ("GetTextMetrics" gettextmetrics) ((hdc HANDLE) (lptm (* TEXTMETRIC))) :returning :int)
(def-function ("GetDC" getdc) ((hWnd HANDLE)) :returning HANDLE)
(def-function ("ReleaseDC" releasedc) ((hWnd HANDLE) (hdc HANDLE)) :returning :int)
(def-function ("SelectObject" selectobject) ((hdc HANDLE) (hgdiobj HANDLE)) :returning HANDLE)
(def-function ("GetTextExtentPoint32" gettextextentpoint32) ((hdc HANDLE) (lpString :cstring) (cbString :int) (lpSize (* SIZE))) :returning :int)
(def-function ("LoadCursor" loadcursor-string) ((hnd HANDLE) (lpCursorName LPCSTR)) :returning HANDLE)
(def-function ("LoadCursor" loadcursor-int) ((hnd HANDLE) (lpCursorName :unsigned-int)) :returning HANDLE)
(defun loadcursor (hnd cur-name)
  (etypecase cur-name
    (fixnum (loadcursor-int hnd cur-name))
    (string (loadcursor-string hnd cur-name))))
(defun default-cursor () (loadcursor *NULL* *IDC_ARROW*))
(def-function ("LoadIcon" loadicon-int) ((hnd HANDLE) (lpIconName :unsigned-int)) :returning HANDLE)
(def-function ("LoadIcon" loadicon-string) ((hnd HANDLE) (lpIconName LPCSTR)) :returning HANDLE)
(defun loadicon (hnd cur-name)
  (etypecase cur-name
    (fixnum (loadicon-int hnd cur-name))
    (string (loadicon-string hnd cur-name))))
(defun default-icon () (loadicon *NULL* *IDI_APPLICATION*))
(defun default-background () (getstockobject *COLOR_BACKGROUND*))
(def-function ("GetClassName" getclassname-i) ((hnd HANDLE) (lpClassName LPCSTR) (maxCount :int)) :returning :int)
(defun getclassname (hnd)
  (with-cstring (s (make-string 64))
    (let ((n (getclassname-i hnd s 64)))
      (when (= n 0)
	(error "Unable to get class name for ~A" hnd))
      (subseq s 0 n))))
(def-function ("RegisterClass" registerclass) ((lpWndClass (* WNDCLASS))) :returning :int)
(def-function ("UnregisterClass" unregisterclass) ((lpClassName :cstring) (hInstance HANDLE)) :returning :int)
(def-function ("GetWindowLong" getwindowlong) ((hWnd HANDLE) (nIndex :int)) :returning :long)
(def-function ("SetWindowLong" setwindowlong) ((hWnd HANDLE) (nIndex :int) (dwNewLong :long)) :returning :long)
(def-function ("CreateWindow" createwindow) ((lpClassName :cstring) (lpWindowName :cstring) (dwStyle :unsigned-int) (x :int) (y :int)
					     (nWidth :int) (nHeight :int) (hWndParent HANDLE) (hMenu HANDLE) (hInstance HANDLE) (lpParam :pointer-void))
	      				    :returning HANDLE)
(def-function ("CreateWindowEx" createwindowex) ((dwExStyle :unsigned-int) (lpClassName :cstring) (lpWindowName :cstring) (dwStyle :unsigned-int)
						 (x :int) (y :int) (nWidth :int) (nHeight :int) (hWndParent HANDLE) (hMenu HANDLE) (hInstance HANDLE)
					         (lpParam :pointer-void))
	      				        :returning HANDLE)
(def-function ("DestroyWindow" destroywindow) ((hWnd HANDLE)) :returning :int)
(def-function ("ShowWindow" showwindow) ((hWnd HANDLE) (nCmdShow :int)) :returning :int)
(def-function ("UpdateWindow" updatewindow) ((hWnd HANDLE)) :returning :void)
(def-function ("RedrawWindow" redrawwindow) ((hWnd HANDLE) (lprcUpdate (* RECT)) (hrgnUpdate HANDLE) (flags :unsigned-int)) :returning :int)
(def-function ("MoveWindow" movewindow) ((hWnd HANDLE) (x :int) (y :int) (nWidth :int) (nHeight :int) (bRepaint :int)) :returning :int)
(def-function ("SetWindowPos" setwindowpos) ((hWnd HANDLE) (hWndInsertAfter HANDLE) (x :int)
					     (y :int) (cx :int) (cy :int) (uFlags :unsigned-int)) :returning :int)
(def-function ("BringWindowToTop" bringwindowtotop) ((hWnd HANDLE)) :returning :int)
(def-function ("GetWindowText" getwindowtext-i) ((hWnd HANDLE) (lpString LPCSTR) (nMaxCount :int)) :returning :int)
(defun getwindowtext (hnd)
  (let ((len (1+ (getwindowtextlength hnd))))
    (with-cstring (s (make-string len))
      (getwindowtext-i hnd s len)
      (subseq s 0 (1- len)))))
(def-function ("GetWindowTextLength" getwindowtextlength) ((hWnd HANDLE)) :returning :int)
(def-function ("SetWindowText" setwindowtext) ((hWnd HANDLE) (lpString LPCSTR)) :returning :int)
(def-function ("GetParent" getparent) ((hWnd HANDLE)) :returning HANDLE)
(def-function ("GetClientRect" getclientrect) ((hWnd HANDLE) (lpRect (* RECT))) :returning :int)
(def-function ("GetWindowRect" getwindowrect) ((hWnd HANDLE) (lpRect (* RECT))) :returning :int)
(def-function ("InvalidateRect" invalidaterect) ((hWnd HANDLE) (lpRect (* RECT)) (bErase :int)) :returning :int)
(def-function ("SetRect" setrect) ((lpRect (* RECT)) (xLeft :int) (yTop :int) (xRight :int) (yBottom :int)) :returning :int)
(def-function ("GetTitleBarInfo" gettitlebarinfo) ((hWnd HANDLE) (pti (* TITLEBARINFO))) :returning :int)
(def-function ("SetFocus" setfocus) ((hWnd HANDLE)) :returning HANDLE)
(def-function ("PostQuitMessage" postquitmessage) ((nExitCode :int)) :returning :void)
(def-function ("SendMessage" sendmessage) ((hWnd HANDLE) (uMsg :unsigned-int) (wParam :unsigned-int) (lParam :int)) :returning :int)
(def-function ("PostMessage" postmessage) ((hWnd HANDLE) (uMsg :unsigned-int) (wParam :unsigned-int) (lParam :int)) :returning :int)
(def-function ("DefWindowProc" defwindowproc) ((hWnd HANDLE) (uMsg :unsigned-int) (wParam :unsigned-int) (lParam :int)) :returning :int)
(def-function ("CallWindowProc" callwindowproc) ((wndProc HANDLE) (hWnd HANDLE) (uMsg :unsigned-int) (wParam :unsigned-int) (lParam :int)) :returning :int)
(def-function ("HIWORD" hiword) ((dWord :unsigned-int)) :returning :unsigned-int)
(def-function ("LOWORD" loword) ((dWord :unsigned-int)) :returning :unsigned-int)
(def-function ("MessageBox" messagebox) ((hWnd HANDLE) (lpText LPCSTR) (lpCaption LPCSTR) (uType :unsigned-int)) :returning :int)
(def-function ("GetOpenFileName" getopenfilename) ((lpofn (* OPENFILENAME))) :returning :int)
(def-function ("GetSaveFileName" getsavefilename) ((lpofn (* OPENFILENAME))) :returning :int)
(def-function ("GetMessage" getmessage) ((lpMsg (* MSG)) (hWnd HANDLE) (wMsgFitlerMin :unsigned-int) (wMsgFilterMax :unsigned-int)) :returning :int)
(def-function ("TranslateMessage" translatemessage) ((lpMsg (* MSG))) :returning :int)
(def-function ("DispatchMessage" dispatchmessage) ((lpMsg (* MSG))) :returning :int)
(def-function ("CreateMenu" createmenu) nil :returning HANDLE)
(def-function ("CreatePopupMenu" createpopupmenu) nil :returning HANDLE)
(def-function ("AppendMenu" appendmenu) ((hMenu HANDLE) (uFlags :unsigned-int) (uIDNewItem :unsigned-int) (lpNewItem LPCSTR)) :returning :int)
(def-function ("GetSubMenu" getsubmenu) ((hMenu HANDLE) (nPos :int)) :returning HANDLE)
(def-function ("DeleteMenu" deletemenu) ((hMenu HANDLE) (uPosition :unsigned-int) (uFlags :unsigned-int)) :returning :int)
(def-function ("RemoveMenu" removemenu) ((hMenu HANDLE) (uPosition :unsigned-int) (uFlags :unsigned-int)) :returning :int)
(def-function ("GetMenuItemCount" getmenuitemcount) ((hMenu HANDLE)) :returning :int)
(def-function ("CheckMenuItem" checkmenuitem) ((hMenu HANDLE) (uIDCheckItem :unsigned-int) (uCheck :unsigned-int)) :returning :int)
(def-function ("CreateAcceleratorTable" createacceleratortable) ((lpaccl (* ACCEL)) (cEntries :int)) :returning HANDLE)
(def-function ("TranslateAccelerator" translateaccelerator) ((hWnd HANDLE) (hAccTable HANDLE) (lpMsg (* MSG))) :returning :int)
(def-function ("DestroyAcceleratorTable" destroyacceleratortable) ((hAccTable HANDLE)) :returning :int)

(defun event-loop (&key (accelTable *NULL*) (accelMain *NULL*))
  (with-foreign-object (msg 'MSG)
    (loop for bRet = (getmessage msg *NULL* 0 0)
	  when (= bRet 0) return bRet
	  if (= bRet -1)
	        do (error "GetMessage failed!!!")
	  else
	    do (or (and (not (null-pointer-p accelTable))
			(not (null-pointer-p accelMain))
			(/= (translateaccelerator accelMain accelTable msg) 0))
		   (progn
		     (translatemessage msg)
		     (dispatchmessage msg))))))

(defun y-or-no-p (&optional control &rest args)
  (let ((s (coerce (apply #'format nil control args) 'simple-string)))
    (= (messagebox *NULL* s "ECL Dialog" (logior *MB_YESNO* *MB_ICONQUESTION*))
       *IDYES*)))

(defun get-open-filename (&key (owner *NULL*) initial-dir filter (dlgfn #'getopenfilename) 
			       (flags 0) &aux (max-fn-size 1024))
  (flet ((null-concat (x &optional y &aux (xx (if y x (car x))) (yy (if y y (cdr x))))
	   (concatenate 'string xx (string #\Null) yy)))
    (when filter
      (setq filter (format nil "~A~C~C" (reduce #'null-concat (mapcar #'null-concat filter)) #\Null #\Null)))
    (with-foreign-object (ofn 'OPENFILENAME)
      (with-cstring (fn (make-string  max-fn-size :initial-element #\Null))
        (zeromemory ofn (size-of-foreign-type 'OPENFILENAME))
	(setf (get-slot-value ofn 'OPENFILENAME 'lStructSize) (size-of-foreign-type 'OPENFILENAME))
	(setf (get-slot-value ofn 'OPENFILENAME 'hwndOwner) owner)
	(setf (get-slot-value ofn 'OPENFILENAME 'lpstrFile) fn)
	(setf (get-slot-value ofn 'OPENFILENAME 'nMaxFile) max-fn-size)
	(setf (get-slot-value ofn 'OPENFILENAME 'Flags) flags)
	(when filter
	  (setf (get-slot-value ofn 'OPENFILENAME 'lpstrFilter) filter))
	(unless (= (funcall dlgfn ofn) 0)
	  (pathname (string-trim (string #\Null) fn)))))))

(provide "WIN32")

;;; Test code

(defconstant *HELLO_ID* 100)
(defconstant *OK_ID* 101)

(defparameter hBtn nil)
(defparameter hOk nil)

(defun button-min-size (hnd)
  (let ((fnt (make-pointer (sendmessage hnd *WM_GETFONT* 0 0) :pointer-void))
	(hdc (getdc hnd))
	(txt (getwindowtext hnd)))
    (unless (null-pointer-p fnt)
      (selectobject hdc fnt))
    (with-foreign-objects ((sz 'SIZE)
			   (tm 'TEXTMETRIC))
      (gettextextentpoint32 hdc txt (length txt) sz)
      (gettextmetrics hdc tm)
      (releasedc hnd hdc)
      (list (+ (get-slot-value sz 'SIZE 'cx) 20)
	    (+ (get-slot-value tm 'TEXTMETRIC 'tmHeight) 10)))))

(defun get-titlebar-rect (hnd)
  (with-foreign-object (ti 'TITLEBARINFO)
    (setf (get-slot-value ti 'TITLEBARINFO 'cbSize) (size-of-foreign-type 'TITLEBARINFO))
    (gettitlebarinfo hnd ti)
    (let ((rc (get-slot-value ti 'TITLEBARINFO 'rcTitlebar)))
      (list (get-slot-value rc 'RECT 'left)
	    (get-slot-value rc 'RECT 'top)
	    (get-slot-value rc 'RECT 'right)
	    (get-slot-value rc 'RECT 'bottom)))))

(defun test-wndproc (hwnd umsg wparam lparam)
  (cond ((= umsg *WM_DESTROY*)
	 (postquitmessage 0)
	 0)
	((= umsg *WM_CREATE*)
	 (setq hBtn (createwindow "BUTTON" "Hello World!" (logior *WS_VISIBLE* *WS_CHILD* *BS_PUSHBUTTON*)
				  0 0 50 20 hwnd (make-ID *HELLO_ID*) *NULL* *NULL*))
	 (setq hOk (createwindow "BUTTON" "Close" (logior *WS_VISIBLE* *WS_CHILD* *BS_PUSHBUTTON*)
				 0 0 50 20 hwnd (make-ID *OK_ID*) *NULL* *NULL*))
	 (sendmessage hBtn *WM_SETFONT* (make-wparam (getstockobject *DEFAULT_GUI_FONT*)) 0)
	 (sendmessage hOk *WM_SETFONT* (make-wparam (getstockobject *DEFAULT_GUI_FONT*)) 0)
	 0)
	((= umsg *WM_SIZE*)
	 (let* ((new-w (loword lparam))
		(new-h (hiword lparam))
		(wb (- new-w 20))
		(hb (/ (- new-h 30) 2)))
	   (movewindow hBtn 10 10 wb hb *TRUE*)
	   (movewindow hOk 10 (+ 20 hb) wb hb *TRUE*))
	 0)
	((= umsg *WM_GETMINMAXINFO*)
	 (let* ((btn1-sz (and hBtn (button-min-size hBtn)))
	        (btn2-sz (and hOk (button-min-size hOk)))
	        (rc (get-titlebar-rect hWnd))
		(titleH (1+ (- (fourth rc) (second rc)))))
	   (when (and btn1-sz btn2-sz (> titleH 0))
	     (with-foreign-object (minSz 'POINT)
	       (setf (get-slot-value minSz 'POINT 'x) (+ (max (first btn1-sz) (first btn2-sz)) 20))
	       (setf (get-slot-value minSz 'POINT 'y) (+ (second btn1-sz) (second btn2-sz) 30 titleH))
	       (with-cast-int-pointer (lparam MINMAXINFO)
		 (setf (get-slot-value lparam 'MINMAXINFO 'ptMinTrackSize) minSz)))))
	 0)
	((= umsg *WM_COMMAND*)
	 (let ((n (hiword wparam))
	       (id (loword wparam)))
	   (cond ((= n *BN_CLICKED*)
		  (cond ((= id *HELLO_ID*)
			 (format t "~&Hellow World!~%"))
			((= id *OK_ID*)
			 (destroywindow hwnd))))
		 (t
		  (format t "~&Un-handled notification: ~D~%" n))))
	 0)
	(t
	 (defwindowproc hwnd umsg wparam lparam))))

(defun do-test ()
  (make-wndclass "MyClass"
    :lpfnWndProc #'test-wndproc)
  (let* ((hwnd (createwindow
	        "MyClass"
	        "ECL/Win32 test"
	        *WS_OVERLAPPEDWINDOW*
	        *CW_USEDEFAULT*
	        *CW_USEDEFAULT*
		130
		120
	        *NULL*
	        *NULL*
		*NULL*
	        *NULL*)))
    (when (si::null-pointer-p hwnd)
      (error "Unable to create window"))
    (showwindow hwnd *SW_SHOWNORMAL*)
    (updatewindow hwnd)
    (event-loop)
    (unregisterclass "MyClass" *NULL*)))

