;;
;; Hanoi - Towers of Hanoi diversion
;;
;; This program is a rewriting in ECL/tk of a program found on the net.
;; Original author is Damon A Permezel (probably fubar!dap@natinst.com)
;; Re-writing is very direct and needs much more working
;;
;; Modified for ECL by G. Attardi (attardi@di.unipi.it)

(in-package "TK")

(defvar sys:*gc-verbose* NIL)

(defvar hanoi-canvas	"")
(defvar hanoi-running 	NIL)
(defvar hanoi-stop	NIL)
(defvar previousRings   0)
(defvar max-rings 	20)
(defvar num-rings	6)
(defvar colours		'(DarkOliveGreen snow4 royalblue2 palegreen4
			  rosybrown1 wheat4 tan2 brown2 tomato3 hotpink3))

(defvar pole		(make-array 3))	     ; elts are <nRing . xPos>
(defvar ring 		(make-array (+ max-rings 1))); elts are <pole width . obj>

(defvar accel		0)
(defvar base		32)
(defvar fly-row 	32)
(defvar width-incr	12)
(defvar width-min	(* 8 width-incr))    
(defvar ring-height 	26)
(defvar ring-spacing	(/ (* 2 ring-height) 3))


;;
;; Setup the main window
;;
(defun SetupHanoi ()
  ;; Create top level window

  (toplevel ".h" "-class" "Queens")
  (wm "title" ".h" "Towers of Hanoi")
  (wm "iconname" ".h" "Towers of Hanoi")

  ;;
  ;; setup frame and main menu button
  ;;
  (label ".h.title" "-text" "Towers of Hanoi" "-bd" 4 "-fg" "RoyalBlue"
	 "-relief" "ridge")
  (frame ".h.f")
  (button ".h.f.run"  "-text" "Run"  "-command"
	  '(DoHanoi (parse-integer (funcall .h.nrframe.scale "get")) T))
  (button ".h.f.stop" "-text" "Stop" "-command" '(setq hanoi-stop 1))
  (button ".h.f.quit" "-text" "Quit" "-command" '(destroy .h))
  (pack .h.f.run .h.f.stop .h.f.quit "-fill" "x" "-side" "left" "-expand" T)

  ;;
  ;; setup next frame, for #rings slider
  ;;
  (frame ".h.nrframe" "-bd" 2 "-relief" "raised")
  (pack (label ".h.nrframe.label" "-text" "Number of Rings: " "-width" 16
	       "-anchor" "e")
	"-side" "left")
  (pack (scale ".h.nrframe.scale" "-orient" "hor" "-from" 1 "-to" max-rings
	       "-font" "fixed" "-command" "setq num-rings")
	"-side" "right" "-expand" T "-fill" "x")
  (funcall .h.nrframe.scale "set" num-rings)

  ;;
  ;; setup next frame, for speed slider
  ;;
  (frame ".h.speed-frame" "-bd" 2 "-relief" "raised")
  (pack (label ".h.speed-frame.label" "-text" "Speed: " "-width" 15
	       "-anchor" "e")
	"-side" "left")
  (pack (scale ".h.speed-frame.scale" "-orient" "hor" "-from" 1 "-to" 100
	       "-font" "fixed" "-command" "setq tk::accel")
	"-side" "right" "-expand" T "-fill" "x")
  (funcall .h.speed-frame.scale "set" 100)

  ;;
  ;; setup frame for canvas to appear in
  ;;
  (frame ".h.canv-frame" "-bd" 4 "-relief" "groove")
  (pack (canvas ".h.canv-frame.canvas" "-relief" "sunken"))
  (setq hanoi-canvas .h.canv-frame.canvas)

  ;; 
  ;; Pack evrybody
  ;;
  (pack .h.title .h.nrframe .h.speed-frame .h.canv-frame .h.f
	"-expand" T "-fill" "x")

  ;;
  ;; key bindings
  ;;
  (bind ".h" "<KeyPress-r>"       '(DoHanoi (parse-integer
					    (funcall .h.nrframe.scale "get")) T))
  (bind ".h" "<KeyPress-s>"       '(setq hanoi-stop T))
  (bind ".h" "<KeyPress-q>"       '(destroy .h))
  
  ;; 
  ;; Display tower
  ;;
  (DoHanoi num-rings NIL)
)

;;
;; DoHanoi	
;;
;; Input:
;;	n	# of rings
;;
;; setup the canvas for displaying the Hanoi simulation
;; Call hanoi if run-it is true.
;;
(defun DoHanoi (n run-it)
  (unless hanoi-running
    (setq ring-width 	(+ width-min (* n width-incr)))
    (setq wm-width	(+ (* 3 ring-width) (* 4 12)))
    (setq wm-height	(+ (* ring-spacing n) fly-row (* 2 ring-height)))


    (setq hanoi-stop 	 NIL)
    (setq hanoi-running	 T)
    (setq base 		 (- wm-height 32))

    ;;
    ;; cleanup from previous run
    ;;
    (do ((i 1 (+ i 1)))
	((> i previousRings))
      (funcall hanoi-canvas "delete" (cddr (svref ring i))))
    
    ;;
    ;; configure the canvas appropriately
    ;;
    (funcall hanoi-canvas "configure" "-width" wm-width "-height" wm-height)
    
    ;;
    ;; setup poles
    ;;
    (dotimes (i 3)
      (setf (svref pole i)
	    (cons 0 (+ (/ (* i wm-width) 3) (/ ring-width 2) 8))))
    ;;
    ;; setup rings
    ;;
    
    (dotimes (i n)
      (let* ((colour (nth (mod i 10) colours))
	     (w      (- ring-width (* i 12)))
	     (y	     (- base (* i ring-spacing)))
	     (x      (- (cdr (svref pole 0)) (/ w 2)))
	     (r      (- n i)))
      
	(setf (svref ring r)
	      (cons 0
		    (cons w
			  (funcall hanoi-canvas "create" 
				   "oval" x y (+ x w) (+ y ring-height)
				   "-fill" colour 
				   "-outline" colour
				   "-width" 12))))))
    
    (setf (svref pole 0) (cons n (cdr (svref pole 0))))
    (setq previousRings n)

    (update)
    (when run-it (Hanoi n 0 2 1))
    (setq hanoi-running NIL)))
;;
;; Hanoi: the guts of the algorithm
;;
;; Input:
;;	n	# of rings
;;	from	pole to move from
;;	to	pole to move to
;;	work	pole to aid in performing work
;;
(defun Hanoi (n from to work)
  (when (and (> n 0) (not hanoi-stop))
    (Hanoi (- n 1) from work to)
    (unless  hanoi-stop (MoveRing n to))
    (Hanoi (- n 1) work to from)))

;;
;; MoveRing:	move a ring to a new pole
;;
;; Input:
;;	n	ring number
;;	to	destination pole
;;
(defun MoveRing (n to)
  ;;
  ;; ring(n,obj) can be queried as to its current position.
  ;; Thus, we don't need to know which pole the ring is moving from.
  ;;
  (let* ((inc	  0)
	 (tox     0)
	 (toy  	  0)
	 (r  	  (cddr (svref ring n)))
	 (coords  (mapcar #'parse-integer (funcall hanoi-canvas "coords" r)))
	 (x0 	  (nth 0 coords))
	 (y0 	  (nth 1 coords))
	 (x1 	  (nth 2 coords))
	 (y1 	  (nth 3 coords)))
    
    ;;
    ;; move up to the "fly row"
    ;;
    (do ()
	((<= y0 fly-row))
      (setq inc (if (> (- y0 fly-row) accel) accel (- y0 fly-row)))
      (setq y0  (- y0 inc))
      (setq y1  (- y1 inc))
      (funcall hanoi-canvas "coords" r x0 y0 x1 y1)
      (update))

    ;;
    ;; one less ring on this pole
    ;;
    (let ((tmp (car (svref ring n))))
      (setf (car (svref pole tmp)) (- (car (svref pole tmp)) 1)))

    ;;
    ;; determine target X position, based on destination pole, and fly ring
    ;; over to new pole
    ;;
    (setq toX (- (cdr (svref pole to)) 
		 (/ (second (svref ring n)) 2)))

    (do ()
	((>= x0 toX))
      (setq inc (if (> (- toX x0) accel) accel (- toX x0)))
      (setq x0 (+ x0 inc))
      (setq x1 (+ x1 inc))
      (funcall hanoi-canvas "coords" r x0 y0 x1 y1)
      (update))

    (do ()
	((<= x0 toX))
      (setq inc (if (> (- x0 toX) accel) accel (- x0 toX)))
      (setq x0 (- x0 inc))
      (setq x1 (- x1 inc))
      (funcall hanoi-canvas "coords" r x0 y0 x1 y1)
      (update))

    ;;
    ;; determine target Y position, based on ;; rings on destination pole.
    ;;
    (setq toY (- base (* (car (svref pole to)) ring-spacing)))

    ;;
    ;; float ring down
    ;;
    (do ()
	((>= y0 toY))
      (setq inc (if (> (- toY y0) accel) accel (- toY y0)))
      (setq y0 (+ y0 inc))
      (setq y1 (+ y1 inc))
      (funcall hanoi-canvas "coords" r x0 y0 x1 y1)
      (update))

    ;;
    ;; increase destination pole usage
    ;;
    (incf (car (svref pole to)))
    (setf (car (svref ring n)) to)))

(SetupHanoi)
