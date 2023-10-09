#lang sicp
(define x (list (list 1 2) (list 3 4) (list 3 4 (list 5 6))))
(define (fringe item)
  (if (null? item)
      nil
      (append (if (pair? (car item)) (fringe (car item)) (list(car item))) (fringe (cdr item)))))
x
(fringe x)