(use-package "CLOS")
T

(unintern '<C1>)
T

(progn
(defclass <C1> ()
  ((x :initform 0 :accessor x-val :reader get-x :writer set-x :initarg :x)
   (y :initform 1 :accessor y-val :reader get-y :writer set-y :initarg :y)))
())
NIL

(progn
(defclass <C2> (<C1>)
  ((z :initform 0 :accessor z-val :reader get-z :writer set-z :initarg :z)))
())
NIL

(defparameter a (make-instance (find-class '<C1>) :x 10))
A

(x-val a)
10

(y-val a)
1

(setf (x-val a) 20)
20

(x-val a)
20

(get-x a)
20

(set-x 10 a)
10

(x-val a)
10

(defparameter b (make-instance (find-class '<C2>) :x 10 :y 20 :z 30))
B

(x-val b)
10

(y-val b)
20

(z-val b)
30

(progn
(defgeneric f (x y)
  (:method ((x t) (y t))
    (list x y)))
(defmethod f ((i integer) (j number))
  (+ i j))
(defmethod f ((s1 string) (s2 string))
    (concatenate 'string s1 s2))
())
NIL

(f t t)
(T T)

(f 2 3)
5

(f 2 3.0)
5.0

(f 2.0 3)
(2.0 3)

(f "ab" "cd")
"abcd"

(f 1 "abc")
(1 "abc")

(progn
(defgeneric f (x y)
  (:method ((x t) (y t))
    (list x y))
  (:method ((i number) (j integer))
    (list (call-next-method) (- i j)))
  (:method ((i integer) (j number))
    (list (call-next-method) (+ i j))))
())
NIL

(f 'x 'y)
(X Y)

(f 1 2)
(((1 2) -1) 3)

(f 1 2.0)
((1 2.0) 3.0)

(f 1.0 2)
((1.0 2) -1.0)

(progn
(defgeneric g (x)
  (:method ((x null))
    (cons 'null (call-next-method)))
  (:method ((x list))
    (if (next-method-p) (cons 'list (call-next-method)) '(list$)))
  (:method ((x symbol))
    (if (next-method-p) (cons 'symbol (call-next-method)) '(symbol$))))
())
NIL

(g 'x)
(SYMBOL$)

(g '(x))
(LIST$)

(g '())
(NULL SYMBOL LIST$)

(defvar hl)
HL

(progn
(defgeneric hgen (x)
  (:method ((x integer))
    (setf hl (cons 'i-primary-1 hl))
    (call-next-method)
    (setf hl (cons 'i-primary-2 hl)))
  (:method :before ((x integer))
    (setf hl (cons 'i-before hl)))
  (:method :after ((x integer))
    (setf hl (cons 'i-after hl)))
  (:method :around ((x integer))
    (setf hl (cons 'i-around-1 hl))
    (call-next-method)
    (setf hl (cons 'i-around-2 hl)))
  (:method ((x number))
    (setf hl (cons 'n-primary-1 hl))
    (call-next-method)
    (setf hl (cons 'n-primary-2 hl)))
  (:method :before ((x number))
    (setf hl (cons 'n-before hl)))
  (:method :after ((x number))
    (setf hl (cons 'n-after hl)))
  (:method :around ((x number))
    (setf hl (cons 'n-around-1 hl))
    (call-next-method)
    (setf hl (cons 'n-around-2 hl)))
  (:method ((x t))
    (setf hl (cons 'innermost hl))))
(defun h (x)
  (setf hl '()) (hgen x) (reverse hl))
)
H

(h 'abc)
(INNERMOST)

(h 3.14)
(N-AROUND-1 N-BEFORE N-PRIMARY-1 INNERMOST N-PRIMARY-2 N-AFTER N-AROUND-2)

(h 3)
(I-AROUND-1 N-AROUND-1 I-BEFORE N-BEFORE I-PRIMARY-1 N-PRIMARY-1 INNERMOST
  N-PRIMARY-2 I-PRIMARY-2 N-AFTER I-AFTER N-AROUND-2 I-AROUND-2
)

(unintern '<C1>)
T

(progn
(defclass <C1> ()
  ((x :initform 0 :accessor x-val :initarg :x)
   (y :initform 1 :accessor y-val :initarg :y)))
())
NIL

(defparameter a (make-instance (find-class '<C1>) :x 10))
A

(defparameter b (make-instance (find-class '<C1>) :y 20 :x 10))
B

(defparameter c (make-instance (find-class '<C1>)))
C

(x-val a)
10

(y-val a)
1

(x-val b)
10

(y-val b)
20

(x-val c)
0

(y-val c)
1

(unintern '<C1>)
T

(progn
(defclass <C1> ()
  ((x :initform 0 :accessor x-val :initarg :x)
   (y :initform 1 :accessor y-val :initarg :y)))
(defmethod initialize-instance :after ((instance <C1>) &rest initvalues)
  (if (= (x-val instance) 0)
    (setf (x-val instance) (y-val instance))))
())
NIL

(x-val (make-instance (find-class '<C1>)))
1

(x-val (make-instance (find-class '<C1>) :x 10))
10

(x-val (make-instance (find-class '<C1>) :y 20))
20

(x-val (make-instance (find-class '<C1>) :x 10 :y 20))
10

(unintern '<C1>)
T

(eq (class-of ())               (find-class 'null))
T

(eq (class-of t)                (find-class 'symbol))
T

(eq (class-of 10)               (find-class #+ALLEGRO 'fixnum #-ALLEGRO 'integer))
T

(eq (class-of 10.0)             (find-class #+ALLEGRO 'single-float #-ALLEGRO 'float))
T

(eq (class-of '(a b))           (find-class 'cons))
T

(eq (class-of "abc")            (find-class 'string))
T

(eq (class-of '#(1 2))          (find-class 'vector))
T

(eq (class-of #'car)            (find-class 'function))
T

(eq (class-of #'make-instance)  (find-class 'standard-generic-function))
T

(eq (class-of '#2a((a) (b)))    (find-class 'array))
T

(eq (class-of *standard-input*) (find-class 'stream))
NIL

(eq (class-of (lambda (x) x))   (find-class 'function))
T

(eq (class-of (find-class 't)) (find-class 'built-in-class))
T

(typep "abc" (find-class 't))
T

(typep "abc" (find-class 'array))
T

(typep "abc" (find-class 'vector))
T

(typep "abc" (find-class 'string))
T

(typep "abc" (find-class 'integer))
NIL

(typep 3 (find-class 't))
T

(typep 3 (find-class 'number))
T

(typep 3 (find-class 'float))
NIL

(typep 3 (find-class 'integer))
T

(typep 3 (find-class 'string))
NIL

(typep *standard-input* (find-class 'stream))
T

#+CLISP
(defun subclassp (class1 class2)
  (clos::subclassp class1 class2)
)
#+ALLEGRO
(defun subclassp (class1 class2)
  (finalize-inheritance class1)
  (not (null (member class2 (class-precedence-list class1))))
)
#+(or CLISP ALLEGRO) SUBCLASSP

(subclassp (find-class 'number)           (find-class 't))
T

(subclassp (find-class 'integer)          (find-class 'number))
T

(subclassp (find-class 'float)            (find-class 'number))
T
