
(ACONS 'A 'B NIL)
((A . B))

(ACONS 'A 'B
       '((C . D)))
((A . B)
 (C . D))

(PAIRLIS '(A B C)
       '(1 2))
#+XCL
((B . 2)
 (A . 1))
#-XCL
ERROR

(PAIRLIS '(A B C)
       '(1 2 3))
#+(or ECLS XCL CLISP ALLEGRO)
((C . 3)
 (B . 2)
 (A . 1))
#+(or AKCL) ((A . 1) (B . 2) (C . 3))
#-(or XCL CLISP ECLS AKCL ALLEGRO) UNKNOWN

(ASSOC 'A
       '((B C)
         (A U)
         (A I)))
(A U)

(ASSOC 'A
       '((B C)
         ((A)
          U)
         (A I)))
(A I)

(ASSOC 'A
       '((B C)
         ((A)
          U)
         (A I))
       :KEY
       #'(LAMBDA (X)
                (IF (LISTP X)
                    (CAR X))))
((A)
 U)

(ASSOC 'A
       '((B C)
         A
         ((A)
          U)
         (A I))
       :KEY
       #'(LAMBDA (X)
                (IF (LISTP X)
                    (CAR X))))
#-(or GCL ECLS ALLEGRO) ((A) U)
#+(or GCL ECLS ALLEGRO) ERROR

(ASSOC 'A
       '((B C)
         A
         ((A)
          U)
         (A I))
       :KEY
       #'(LAMBDA (X)
                (IF (ATOM X)
                    X)))
#-(or GCL ECLS ALLEGRO) (A I)
#+(or GCL ECLS ALLEGRO) ERROR

(ASSOC 'A
       '((B C)
         A
         ((A)
          U)
         (A I))
       :TEST
       #'(LAMBDA (X Y)
                (IF (LISTP Y)
                    (EQL (CAR Y)
                         X))))
#-(or GCL ECLS ALLEGRO) ((A) U)
#+(or GCL ECLS ALLEGRO) ERROR

(ASSOC 'A
       '((B C)
         A
         ((A)
          U)
         (A I))
       :TEST
       #'(LAMBDA (X Y)
                (IF (ATOM Y)
                    (EQL Y X))))
#-(or GCL ECLS ALLEGRO) (A I)
#+(or GCL ECLS ALLEGRO) ERROR

(ASSOC 'A
       '((B C)
         A
         ((A)
          U)
         (A I))
       :TEST-NOT
       #'(LAMBDA (X Y)
                (IF (ATOM Y)
                    (EQL Y X))))
#-ALLEGRO (B C)
#+ALLEGRO ERROR

(ASSOC-IF 'NUMBERP
       '((A . 3)
         (3 . A)))
(3 . A)

(ASSOC-IF 'SYMBOLP
       '((A . 3)
         (3 . A)))
(A . 3)

(ASSOC-IF-NOT 'SYMBOLP
       '((A . 3)
         (3 . A)))
(3 . A)

(ASSOC-IF-NOT 'NUMBERP
       '((A . 3)
         (3 . A)))
(A . 3)

(RASSOC 'A
       '((1 . B)
         (2 . A)))
(2 . A)

(RASSOC-IF 'SYMBOLP
       '((1 . B)
         (2 . A)))
(1 . B)

(RASSOC-IF 'SYMBOLP
       '((1 . 3)
         (2 . A)))
(2 . A)

(RASSOC-IF-NOT 'SYMBOLP
       '((1 . 3)
         (2 . A)))
(1 . 3)

