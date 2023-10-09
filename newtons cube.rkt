#lang sicp
(define (calc x y)
(/ (+ (* 2 y)
        (/ x
           (* y y)))
    3)
 )
(define (isgood x y)
  (if (< (abs(- x (* y y y))) 0.001)
      y
      (isgood x (calc x y))
   )
 )
(isgood 27 1)