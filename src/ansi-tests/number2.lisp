;;; based on v1.4 -*- mode: lisp -*-
(in-package :user)

(my-assert
 (gcd 2346026393680644703525505657 17293822570713318399)
 11)

(my-assert
 (multiple-value-list (xgcd 77874422 32223899))
 (1 -9206830 22249839))

(my-assert
 (multiple-value-list (xgcd 560014183 312839871))
 (1 77165803 -138134388))

(my-assert
 (multiple-value-list (xgcd 3 2))
 (1 1 -1))

(my-assert
 (multiple-value-list (xgcd 2 3))
 (1 -1 1))

(my-assert
 (let ((a 974507656412513757857315037382926980395082974811562770185617915360)
       (b -1539496810360685510909469177732386446833404488164283))
   (multiple-value-bind (g u v) (xgcd a b)
     (and (eql g 1) (eql g (+ (* a u) (* b v))))
     ) )
 T)

(my-assert
 (isqrt #x3FFFFFFFC000000000007F)
 #x7FFFFFFFBFF)

