#lang sicp
(define (betterguess x y)
  (/ (+ y
        (/ x y))
     2))
(define (lessthan x guess)
  (<(abs(- (* guess guess)
              x))
         0.0000000000001))
(define (gudnuff x guess)
  (if (lessthan x guess)
       guess
       (gudnuff x (betterguess x guess))))

(gudnuff 0.25 1)