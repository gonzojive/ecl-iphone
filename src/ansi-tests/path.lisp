;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (setf string "test-pathname.abc" symbol 'test-pathname.abc)
 test-pathname.abc)

;;pathname -mögl. Argumenttypen: pathname,string,symbol,stream
;;         -resultat: pathname

(my-assert
 (SETF PATHSTRING (PATHNAME STRING))
 #+XCL
 #S(PATHNAME SYSTEM::HOST NIL
	     SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
	     TYPE "ABC" SYSTEM::VERSION NIL)
 #+CLISP
 #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
	     :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

(my-assert
 (SETF PATHSYMBOL (PATHNAME symbol))
 #+XCL
 #S(PATHNAME SYSTEM::HOST
	     NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
	     "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL)
 #+CLISP
 #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
	     :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

(my-assert
 (SETF PATHPATH (PATHNAME PATHSYMBOL))
 #+XCL
 #S(PATHNAME SYSTEM::HOST NIL
	     SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
	     TYPE "ABC" SYSTEM::VERSION NIL)
 #+CLISP
 #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
	     :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

(my-assert
 (SETF STREAM (OPEN STRING :DIRECTION :OUTPUT)
       a nil)
 nil)

;; (SETF PATHSTREAM (PATHNAME STREAM))
;; "test-pathname.lsp"

(my-assert
 (MAPCAR (FUNCTION PATHNAMEP)
	 (LIST PATHSTRING PATHSYMBOL PATHPATH ;PATHSTREAM
	       ))
 (T T T					;T
    ))


;; funktion truename liefert filename fuer pathname oder stream
;;                   einen Pfadnamen
;;
;; (MAPCAR (FUNCTION TRUENAME) (LIST PATHSTRING PATHSYMBOL PATHPATH STREAM
;;                                                                ;PATHSTREAM
;;                                                                  ))
;;   ERROR



(my-assert
 (PARSE-NAMESTRING STRING)
 #+XCL
 #S(PATHNAME SYSTEM::HOST NIL
	     SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
	     TYPE "ABC" SYSTEM::VERSION NIL)
 #+CLISP
 #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
	     :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

(my-assert
 (PARSE-NAMESTRING SYMBOL)
 #+XCL
 #S(PATHNAME SYSTEM::HOST NIL
	     SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
	     TYPE "ABC" SYSTEM::VERSION NIL)
 #+CLISP
 #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
	     :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

#+XCL

(my-assert
 (PARSE-NAMESTRING "bab:test-pathname.abc")
 #S(PATHNAME SYSTEM::HOST NIL
	     SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2" SYSTEM::NAME
	     "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL))

#+XCL
(my-assert
 (PARSE-NAMESTRING "bab:test-pathname.abc;3")
 #S(PATHNAME SYSTEM::HOST NIL
	     SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2" SYSTEM::NAME
	     "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION 3))

(my-assert
 (PARSE-NAMESTRING PATHSTRING)
 #+XCL
 #S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
	     "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME" TYPE "ABC"
	     SYSTEM::VERSION NIL)
 #+CLISP
 #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
	     :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

(my-assert
 (PARSE-NAMESTRING "test-pathname.abc" NIL)
 #+XCL
 #S(PATHNAME SYSTEM::HOST NIL
	     SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
	     TYPE "ABC" SYSTEM::VERSION NIL)
 #+CLISP
 #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
	     :NAME "test-pathname" :TYPE "abc" :VERSION NIL))

#+XCL
(my-assert
 (PARSE-NAMESTRING "sirius::disk00$abt43:[heicking]test-pathname.abc")
 #S(PATHNAME
    SYSTEM::HOST "SIRIUS" SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "HEICKING"
    SYSTEM::NAME "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL))

#+XCL
(my-assert
 (PARSE-NAMESTRING "sirius::disk00$abt43:[heicking]test-pathname.abc" "sirius")
 #S(PATHNAME
    SYSTEM::HOST "SIRIUS" SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "HEICKING"
    SYSTEM::NAME "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL))

#+XCL
(my-assert
 (PARSE-NAMESTRING "sirius::disk00$abt43:[heicking]test-pathname.abc" "orion")
 ERROR)

(my-assert
 (PARSE-NAMESTRING "abc.123" NIL NIL :START 0 :END 5)
 #+XCL
 #S(PATHNAME SYSTEM::HOST
	     NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "ABC" TYPE
	     "1" SYSTEM::VERSION NIL)
 #+CLISP
 #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
	     :NAME "abc" :TYPE "1" :VERSION NIL))

(my-assert
 (PARSE-NAMESTRING "abc.123" NIL NIL :START 2 :END 5)
 #+XCL
 #S(PATHNAME SYSTEM::HOST
	     NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "C" TYPE "1"
	     SYSTEM::VERSION NIL)
 #+CLISP
 #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
	     :NAME "c" :TYPE "1" :VERSION NIL))

#+XCL
(my-assert
 (PARSE-NAMESTRING "babylon" NIL NIL :START 0 :END 3)
 #S(PATHNAME SYSTEM::HOST
	     NIL SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2" SYSTEM::NAME NIL TYPE
	     NIL SYSTEM::VERSION NIL))

#+XCL
(my-assert
 (PARSE-NAMESTRING "babylon" NIL NIL :START 0 :END 7)
 #S(PATHNAME SYSTEM::HOST
	     NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "BABYLON"
	     TYPE NIL SYSTEM::VERSION NIL))

#+XCL
(my-assert
 (PARSE-NAMESTRING "babylon" NIL *DEFAULT-PATHNAME-DEFAULTS* :START 0 :END 7)
 #S(PATHNAME
    SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
    "BABYLON" TYPE NIL SYSTEM::VERSION NIL))

(my-assert
 *DEFAULT-PATHNAME-DEFAULTS*
 #+XCL
 #S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE NIL
	     DIRECTORY NIL SYSTEM::NAME NIL TYPE "lsp" SYSTEM::VERSION :NEWEST)
 #+(and CLISP (or win32 os2))
 #S(PATHNAME :HOST NIL :DEVICE "C" :DIRECTORY (:RELATIVE)
	     :NAME NIL :TYPE NIL :VERSION NIL)
 #+(and CLISP unix)
 #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
	     :NAME NIL :TYPE NIL :VERSION NIL))

#+XCL
(my-assert
 (PARSE-NAMESTRING "babylon" NIL *DEFAULT-PATHNAME-DEFAULTS* :START 0 :END 3)
 #S(PATHNAME
    SYSTEM::HOST NIL SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2"
    SYSTEM::NAME NIL TYPE NIL SYSTEM::VERSION NIL))

;; (PARSE-NAMESTRING "babylon.c.c" NIL NIL :JUNK-ALLOWED T)
;; #S(PATHNAME
;; SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
;; "BABYLON" TYPE "C" SYSTEM::VERSION NIL)

;; (PARSE-NAMESTRING "babylon;c.c" NIL NIL :JUNK-ALLOWED T)
;; #S(PATHNAME
;; SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
;; "BABYLON" TYPE NIL SYSTEM::VERSION NIL)

#+XCL
(my-assert
 (PARSE-NAMESTRING "babylon;c.c" NIL NIL :JUNK-ALLOWED NIL)
 ERROR)

#+XCL
(my-assert
 (PARSE-NAMESTRING "babylon.c.c" NIL NIL :JUNK-ALLOWED NIL)
 ERROR)

#+XCL
(my-assert
 (PARSE-NAMESTRING "babylon.c;c" NIL NIL :JUNK-ALLOWED NIL)
 ERROR)

#+XCL
(my-assert
 (PARSE-NAMESTRING "babylon.c;" NIL NIL :JUNK-ALLOWED NIL)
 #S(PATHNAME
    SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
    "BABYLON" TYPE "C" SYSTEM::VERSION NIL))

#+XCL
(my-assert
 (PARSE-NAMESTRING "babylon.c;5" NIL NIL :JUNK-ALLOWED NIL)
 #S(PATHNAME
    SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
    "BABYLON" TYPE "C" SYSTEM::VERSION 5))

;; (MERGE-PATHNAME "test$$" SYMBOL 10)   ERROR
;;
;; (MERGE-PATHNAME "test$$" SYMBOL)   ERROR
;;
;; (MERGE-PATHNAME "test$$" PATH)   ERROR
;;
;; (MERGE-PATHNAME "test$$")   ERROR

#+XCL
(my-assert
 (MERGE-PATHNAMES "test$$")
 #S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
	     "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE "lsp"
	     SYSTEM::VERSION :NEWEST))

#+XCL
(my-assert
 (MERGE-PATHNAMES "test$$" SYMBOL)
 #S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
	     "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE "ABC"
	     SYSTEM::VERSION :NEWEST))

#+XCL
(my-assert
 (MERGE-PATHNAMES "test$$" SYMBOL 2)
 #S(PATHNAME SYSTEM::HOST NIL
	     SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE
	     "ABC" SYSTEM::VERSION 2))

#+XCL
(my-assert
 (MERGE-PATHNAMES "test$$" (PATHNAME SYMBOL) 2)
 #S(PATHNAME SYSTEM::HOST NIL
	     SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE
	     "ABC" SYSTEM::VERSION 2))

#+XCL
(my-assert
 (MERGE-PATHNAMES "test$$" STREAM 2)
 #S(PATHNAME SYSTEM::HOST 16 SYSTEM::DEVICE
	     "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE :ESCAPE
	     SYSTEM::VERSION 2))


;; (MERGE-PATHNAME STRING SYMBOL)   ERROR

#+XCL
(my-assert
 (MAKE-PATHNAME :NAME "a" :HOST (QUOTE ORION))
 #S(PATHNAME SYSTEM::HOST ORION
	     SYSTEM::DEVICE NIL DIRECTORY NIL SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION
	     :NEWEST))

#+XCL
(my-assert
 (DEFMACRO TEST (&REST BODY) (\` (APPLY (FUNCTION MAKE-PATHNAME) (\,@ BODY))))
 TEST)

#+XCL
(my-assert
 (setf a '(:host "sirius" :name "a"))
 (:host "sirius" :name "a"))

#+XCL
(my-assert
 (TEST A)
 #S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE NIL DIRECTORY NIL
	     SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION :NEWEST))

#+XCL
(my-assert
 (SETF A (LIST* :DEVICE "disk00$abt43" A))
 (:DEVICE "disk00$abt43" :HOST "sirius" :NAME "a"))

#+XCL
(my-assert
 (TEST A)
 #S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
	     DIRECTORY NIL SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION :NEWEST))

#+XCL
(my-assert
 (SETF A (LIST* :DIRECTORY "[heicking.comlisp]" A))
 (:DIRECTORY
  "[heicking.comlisp]" :DEVICE "disk00$abt43" :HOST "sirius" :NAME "a"))

#+XCL
(my-assert
 (TEST A)
 #S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
	     DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION
	     :NEWEST))

#+XCL
(my-assert
 (SETF A (LIST* :TYPE "raf" A))
 (:TYPE "raf" :DIRECTORY "[heicking.comlisp]"
	:DEVICE "disk00$abt43" :HOST "sirius" :NAME "a"))

#+XCL
(my-assert
 (TEST A)
 #S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
	     DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE "raf" SYSTEM::VERSION
	     :NEWEST))

#+XCL
(my-assert
 (SETF A (LIST* :VERSION 3 A))
 (:VERSION 3 :TYPE "raf" :DIRECTORY
	   "[heicking.comlisp]" :DEVICE "disk00$abt43" :HOST "sirius" :NAME "a"))

#+XCL
(my-assert
 (TEST A)
 #S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
	     DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE "raf" SYSTEM::VERSION 3))

(my-assert
 (MAPCAR (FUNCTION PATHNAMEP) (LIST PATHSYMBOL PATHPATH PATHSTRING))
 (T T T))

#+XCL
(my-assert
 (SETF PATH (TEST A))
 #S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE
	     "disk00$abt43" DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE "raf"
	     SYSTEM::VERSION 3))

#+XCL
(my-assert
 (MAPCAR (FUNCTION PATHNAME-HOST) (LIST SYMBOL STRING STREAM PATH))
 (NIL NIL NIL NIL))

#+XCL
(my-assert
 (MAPCAR (FUNCTION PATHNAME-DEVICE) (LIST SYMBOL STRING STREAM PATH))
 ("DISK00$ABT43" "DISK00$ABT43" "DISK00$ABT43" "DISK00$ABT43"))

#+XCL
(my-assert
 (MAPCAR (FUNCTION PATHNAME-DIRECTORY) (LIST SYMBOL STRING STREAM PATH))
 ("XCL.MAIN" "XCL.MAIN" "XCL.MAIN" "XCL.MAIN"))

(my-assert
 (PROGN (CLOSE STREAM) T)
 T)

#+XCL
(my-assert
 (USER-HOMEDIR-PATHNAME)
 #S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
	     "DISK00$ABT43" DIRECTORY "HEICKING" SYSTEM::NAME NIL TYPE NIL SYSTEM::VERSION
	     NIL))

(my-assert
 (PATHNAME "*.*")
 #+XCL
 #S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43"
	     DIRECTORY "HEICKING" SYSTEM::NAME "*" TYPE :WILD SYSTEM::VERSION NIL)
 #+CLISP
 #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
	     :NAME :WILD :TYPE :WILD :VERSION NIL)
 #-(or XCL CLISP)
 #P"*.*")

(my-assert
 (progn (setf file (open "nicht-vorhandenes-file.non"
			 :direction :input
			 :element-type 'character
			 :if-does-not-exist :create)) t)
 t
 "")

(my-assert
 (null (probe-file "nicht-vorhandenes-file.non"))
 NIL)

(my-assert
 (progn (close file) t)
 t)

(my-assert
 (setf file (open "nicht-vorhandenes-file.non"
		  :direction :io
		  :element-type 'string-char
		  :if-exists :error))
 error)

(my-assert
 (progn (close file) t)
 t)

(my-assert
 (null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'character
                        :if-exists :new-version)))
 nil
 "")

(my-assert
 (progn (close file) t)
 t)

(my-assert
 (null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'character
                        :if-exists :rename)))
 nil)

(my-assert
 (progn (close file) t)
 t)

(my-assert
 (null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'character
                        :if-exists :rename-and-delete)))
 nil)

(my-assert
 (progn (close file) t)
 t)

(my-assert
 (null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'character
                        :if-exists :overwrite)))
 nil)

(my-assert
 (progn (close file) t)
 t)

(my-assert
 (null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'character
                        :if-exists :append)))
 nil)

(my-assert
 (progn (close file) t)
 t)

(my-assert
 (null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'character
                        :if-exists :supersede)))
 nil)

(my-assert
 (progn (close file) t)
 t)

(my-assert
 (setf file (open "nicht-vorhandenes-file.non"
		  :direction :io
		  :element-type 'character
		  :if-exists nil))
 nil)

(my-assert
 (progn (close file) t)
 error)

(my-assert
 (setf file (open "nicht-vorhandenes-file.new"
		  :direction :io
		  :element-type 'character
		  :if-does-not-exist :error))
 error)

(my-assert
 (progn (close file) t)
 error)

(my-assert
 (null (setf file (open "nicht-vorhandenes-file.new"
                        :direction :io
                        :element-type 'character
                        :if-does-not-exist :create)))
 nil)

(my-assert
 (progn (close file) t)
 t)

(my-assert
 (null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'character
                        :if-does-not-exist nil)))
 nil)

(my-assert
 (progn (close file) t)
 t)

(my-assert
 (namestring
  (multiple-value-setq (new-name pathname truename)
    (rename-file "nicht-vorhandenes-file.non" "file.da")))
 "file.da")

(my-assert
 (namestring new-name)
 "file.da")

(my-assert
 (null pathname)
 nil)

(my-assert
 (null truename)
 nil)

(my-assert
 (progn (delete-file "test-pathname.abc") t)
 t)

(my-assert
 (progn (mapc #'delete-file (directory "nicht-vorhandenes-file.*")) t)
 t)

(my-assert
 (progn (delete-file "file.da") t)
 t)

(my-assert
 (progn
   (setf (logical-pathname-translations "clocc")
	 '(("**;*" "/usr/local/src/clocc/**/*")))
   nil)
 nil)

(my-assert
 (translate-logical-pathname "clocc:src;port;")
 #P"/usr/local/src/clocc/src/port/")
