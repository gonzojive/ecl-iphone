;
; This demo is a contribution of Grant Edwards (grante@rosemount.com)
; Modified for ECL by G. Attardi (attardi@di.unipi.it)


; Yet another "my first STk program" type thing.  This one is the "8
; queens" puzzle.  You try to figure out how to place 8 queens on a
; chessboard so that none of the queens can be taken in a single move.

; You can do it yourself (and it will make sure you follow the rules)
; or you can ask it to solve the puzzle starting with a given board
; configuration.


(in-package "TK")

(defvar queen-bitmap (concatenate 'string "@"
				  si:*system-directory* "tk/bitmaps/queen"))

; size of board (it's square)

(defvar size 8)


; Predicate that is true if the queens at p1 and p2 can't take each
; other in 1 move.  p1 and p2 are pairs of the form ( x . y ) where
; x is column and y is row (both from 0 to size-1).

(defun legal-position-pair? (p1 p2)
  (let ((x1 (car p1)) (y1 (cdr p1)) (x2 (car p2)) (y2 (cdr p2)))
    (not (or 
	  (= x1 x2) 
	  (= y1 y2) 
	  (= (abs (- x1 x2)) (abs (- y1 y2)))))))


; Predicate that is true if none of the queens in list history can
; take queen at postion new in one move.  "history" is a list of
; position pairs.  "new" is the position pair which we are testing.

(defun legal-move? (history new)
  (cond 
    ((null history) T)
    ((not (legal-position-pair? (car history) new)) NIL)
    (T (legal-move? (cdr history) new))))


; This is the procedure that solves the puzzle given a list of
; occupied squares and a list of empty rows.  It's also passed a
; continuation so that it can abort when the user asks it to stop.

; Add a legal move to history list and recurse to build up strings of
; legal moves.  The chessboard is updated as pieces are placed. When
; it reaches the required length, it waits for user to press the Next
; or Stop button. "history" is a list of pairs that denotes where
; there are already queens.  "ylist" is a list of rows that still need
; to be filled. "break" is a continuation to be called when the
; procedure is to be aborted.

(defun add-queen (history ylist break)
  (cond
    (stopPushed    (throw break NIL))
    ((null ylist)  (progn (write history)
			  (terpri)
			  (waitForNextButton)
			  (when stopPushed
			    (throw break NIL))))
    (T             (let ((newy (car ylist)))
		     (dotimes (newx size)
		       (when (legal-move? history (cons newx newy))
			 (activate-button newx newy)
			 (update)
			 (add-queen (cons (cons newx newy) history)
				    (cdr ylist) break)
			 (deactivate-button newx newy)
			 (update)))))))

; global boolean used to keep track of whether or not the user is
; allowed to rearrange the board.

(defvar userModsEnabled T)


; set up button states and solve the puzzle starting with the current
; board configuration.

(defun do-solve ()
  (setq stopPushed NIL)
  (.q.upper.solve "configure" "-state" "disabled")
  (.q.upper.stop  "configure" "-state" "normal")
  (.q.upper.clear "configure" "-state" "disabled")
  (.q.upper.quit "configure" "-state" "disabled")
  (setq userModsEnabled NIL)
  (let ((break (cons nil nil)))
    (catch break
      (add-queen (current-positions) (empty-rows) break)))
  (.q.upper.stop "configure" "-state" "disabled")
  (.q.upper.clear "configure" "-state" "normal")
  (setq userModsEnabled T)
  (.q.upper.solve "configure" "-state" "normal")
  (.q.upper.quit "configure" "-state" "normal"))


; Create two matrixes.  Each has an entry for each square on the
; board.  One matrix is Tk button procedures, the other is booleans
; that reflect whether or not the square is occupied.

(defvar board-buttons (make-array (list size size)))
(defvar board-states  (make-array (list size size)))
  

; redraw the button so that it is occupied and update the matrix of
; booleans

(defun activate-button (x y)
  (funcall (aref board-buttons y x)
	   "configure" "-relief" "raised" "-foreground" "#000")
  (setf (aref board-states y x) T))


; redraw the button so that it is empty and update the matrix of
; booleans

(defun deactivate-button (x y)
  (let* ((b (aref board-buttons y x))
	 (bg (fifth (funcall b "configure" "-background"))))
    (funcall b "configure" "-relief" "flat" "-foreground" bg)
    (setf (aref board-states y x) NIL)))

; flash a button

(defun flash-button (x y)
  (funcall (aref board-buttons y x) "flash"))


; Procedure called when the user clicks on a square in the chessboard.
; If user modifications are not enabled, then do nothing.  Otherwise
; toggle the sate of the square.  When placing a queen on a previously
; empty square, remove existing queens that could be taken by the new
; one.

(defun toggle-button (x y)
  (cond
    ((not userModsEnabled) NIL)
    ((aref board-states y x)  (deactivate-button x y))
    (t	   (activate-button x y)
	   (update)
	   (dotimes (ox size) 
	     (dotimes (oy size)
	       (when (and (aref board-states  oy ox)
			  (not (and (= x ox) (= y oy)))
			  (not (legal-position-pair? (cons x y) (cons ox oy))))
		 (flash-button ox oy)
		 (flash-button ox oy)
		 (flash-button ox oy)
		 (deactivate-button ox oy)
		 (update)))))))


; clear the board

(defun clear-board ()
  (dotimes (x size) (dotimes (y size) (deactivate-button x y))))


; Procedures to return a list of consecutive integers from start to
; end (inclusive).

(defun interval (start end)
  (do ((s start)
       (e end (1- e))
       (l () (cons e l)))
      ((> s e) l)))

(defun rinterval (start end)
  (do ((s start (1+ s))
       (e end)
       (l () (cons s l)))
       ((> s e) l)))
   

; Return a list of integers that identify the rows on the chessboard
; that are empty

(defun empty-rows ()
  (let ((empty ()))
    (dotimes (row size)
      (dotimes (col size (push row empty))
	(when (aref board-states row col)
	  (return))))
    empty))


; Return a list of pairs ( x . y ) indicating which squares are
; currently occupied.

(defun current-positions ()
  (let ((p ()))
    (dotimes (x size) 
      (dotimes (y size) 
	(when (aref board-states y x)
	  (push (cons x y) p))))
    p))


; Booleans used to detect when user presses a button 

(defvar nextOrStopPushed NIL)
(defvar stopPushed NIL)


; Procedure to wait for the user to press either the next or stop
; buttons.

(defun waitForNextButton () 
  (.q.upper.next "configure" "-state" "normal")
  (tkwait "variable" 'nextOrStopPushed)
  (.q.upper.next "configure" "-state" "disabled"))

; Create top level window

(toplevel ".q" "-class" "Queens")
(wm "title" .q "Queens")
(wm "iconname" .q "Queens")

; Define two frames.  The upper will hold control buttons, the lower
; the chessboard buttons

(frame ".q.lower")
(frame ".q.upper" "-relief" "raised" "-borderwidth" 2)


; add a frame to the lower frame for each row of sqaures on the
; chessboard and fill that row with buttons (one per square).

(dotimes (y size)
  (let ((rowframe (format NIL ".q.lower.row~a" y)))
    (frame rowframe)
    (dotimes (x size)
      (let* ((bn (format NIL "~a.b~a" rowframe x))
	     (bp (intern (button bn 
				 "-bitmap" queen-bitmap
				 "-relief" "flat"))))
	(setf (aref board-buttons y x) (symbol-function bp))
	(setf (aref board-states y x) NIL) ; if we reload file
	(let ((bg (if (oddp (+ x y)) "#bbb" "#eee")))
	  (funcall bp "configure" "-background" bg "-activebackground" "#fff"
		   "-foreground" bg))
	(bind bn "<Button-1>" `(toggle-button ,x ,y))
	(bind bn "<Any-Enter>" '())
	(bind bn "<Any-Leave>" '())
	(bind bn "<ButtonRelease-1>" '())
	(pack bn "-side" "left")
	)
      )
    (pack (intern rowframe) "-side" "bottom")
    )
  )


; add control buttons to upper frame

(button ".q.upper.quit"  "-text" "Quit" "-command" '(destroy .q))
(frame  ".q.upper.fill")
(button ".q.upper.solve" "-text" "Solve" "-command" '(do-solve))
(button ".q.upper.clear" "-text" "Clear" "-command" '(clear-board))
(button ".q.upper.next"
	"-text" "Next" 
	"-state" "disabled" 
	"-command" '(setq stopPushed NIL
		        nextOrStopPushed T))
(button ".q.upper.stop" 
	"-text" "Stop" 
	"-state" "disabled" 
	"-command" '(setq stopPushed T
		        nextOrStopPushed T))

(pack .q.upper.solve "-side" "left" "-padx" 4 "-pady" 4)
(pack .q.upper.next "-side" "left" "-padx" 4 "-pady" 4)
(pack .q.upper.stop "-side" "left" "-padx" 4 "-pady" 4)
(pack .q.upper.clear "-side" "left" "-padx" 4 "-pady" 4)
(pack .q.upper.quit "-side" "right" "-padx" 4 "-pady" 4)
(pack .q.upper.fill "-side" "right" "-fill" "x")

; arrange the two top level frames

(pack .q.upper "-side" "top" "-fill" "x")
(pack .q.lower "-side" "bottom") 
