;;; section 8 structures -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;;
;;; Example 1
;;; define town structure type
;;; area, watertowers, firetrucks, population, elevation are its components
;;;
(my-assert
 (defstruct town
   area
   watertowers
   (firetrucks 1 :type fixnum)		;an initialized slot
   population
   (elevation 5128 :read-only t))	;a slot that can't be changed
 TOWN)

					;create a town instance
(my-assert
 (progn
   (setq town1 (make-town :area 0 :watertowers 0))
   t)
 t )

					;town's predicate recognizes the new instance
(my-assert
 (town-p town1)
 t)

					;new town's area is as specified by make-town
(my-assert
 (town-area town1)
 0)

					;new town's elevation has initial value
(my-assert
 (town-elevation town1)
 5128)

					;setf recognizes reader function
(my-assert
 (setf (town-population town1) 99)
 99)

(my-assert
 (town-population town1)
 99)

					;copier function makes a copy of town1
(my-assert
 (progn
   (setq town2 (copy-town town1))
   t)
 t)

(my-assert
 (= (town-population town1) (town-population town2))
 t)

					;since elevation is a read-only slot, its value can be set only
					;when the structure is created
(my-assert
 (progn
   (setq town3 (make-town :area 0 :watertowers 3 :elevation 1200))
   t)
 t)

;;;
;;; Example 2
;;; define clown structure type
;;; this structure uses a nonstandard prefix
;;;
(my-assert
 (defstruct (clown (:conc-name bozo-))
   (nose-color 'red)
   frizzy-hair-p polkadots)
 CLOWN)

(my-assert
 (progn
   (setq funny-clown (make-clown))
   t)
 t)

					;use non-default reader name
(my-assert
 (bozo-nose-color funny-clown)
 RED        )

(my-assert
 (defstruct (klown (:constructor make-up-klown) ;similar def using other
		   (:copier clone-klown) ;customizing keywords
		   (:predicate is-a-bozo-p))
   nose-color frizzy-hair-p polkadots)
 klown)

					;custom constructor now exists
(my-assert
 (fboundp 'make-up-klown)
 t)

;;;
;;; Example 3
;;; define a vehicle structure type
;;; then define a truck structure type that includes
;;; the vehicle structure
;;;
(my-assert
 (defstruct vehicle name year (diesel t :read-only t))
 VEHICLE)

(my-assert
 (defstruct (truck (:include vehicle (year 79)))
   load-limit
   (axles 6))
 TRUCK)

(my-assert
 (progn
   (setq x (make-truck :name 'mac :diesel t :load-limit 17))
   t)
 t)

					;vehicle readers work on trucks
(my-assert
 (vehicle-name x)
 MAC)

					;default taken from :include clause
(my-assert
 (vehicle-year x)
 79 )

(my-assert
 (defstruct (pickup (:include truck))	;pickup type includes truck
   camper long-bed four-wheel-drive)
 PICKUP)

(my-assert
 (progn
   (setq x (make-pickup :name 'king :long-bed t))
   t)
 t)

					;:include default inherited
(my-assert
 (pickup-year x)
 79)

;;;
;;; Example 4
;;; use of BOA constructors
;;;
(my-assert
 (defstruct (dfs-boa			;BOA constructors
	     (:constructor make-dfs-boa (a b c))
	     (:constructor create-dfs-boa
			   (a &optional b (c 'cc) &rest d &aux e (f 'ff))))
   a b c d e f)
 DFS-BOA)

					;a, b, and c set by position, and the rest are uninitialized
(my-assert
 (progn
   (setq x (make-dfs-boa 1 2 3))
   t)
 t)

(my-assert
 (dfs-boa-a x)
 1)

					;a and b set, c and f defaulted
(my-assert
 (progn
   (setq x (create-dfs-boa 1 2))
   t)
 t)

(my-assert
 (dfs-boa-b x)
 2)

(my-assert
 (eq (dfs-boa-c x) 'cc)
 t)

					;a, b, and c set, and the rest are collected into d
(my-assert
 (progn
   (setq x (create-dfs-boa 1 2 3 4 5 6))
   t)
 t)

(my-assert
 (dfs-boa-d x)
 (4 5 6))

