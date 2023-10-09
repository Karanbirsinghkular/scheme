#lang sicp


(define (deep-reverse item)
  (if (null? item)
      nil
      (append (deep-reverse (cdr item)) (list (if (pair? (car item)) (deep-reverse (car item)) (car item))))))

(define x (list (list 1 2) (list 3 4) (list 3 4 (list 5 6))))
x
(deep-reverse x)

