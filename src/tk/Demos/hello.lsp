;; Simple Tk script to create a button that prints "Hello, world".
;; Click on the button to terminate the program.
;; 
;; The first line below creates the button, and the second line
;; arranges for packer to manage the button's geometry, centering
;; it in the application's main window.

(in-package "TK")

(button ".hello" "-text" "Hello, world" 
	"-command" '(progn
		     (format t "Hello, world~%")
		     (destroy .hello)))
(pack .hello)
