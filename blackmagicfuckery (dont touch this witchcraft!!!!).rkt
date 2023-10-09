#lang sicp
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f(f x)))))

(define three (add-1 two))


(define (plus x y)
  (lambda (f)(lambda (z) ((x f) ((y f) z)) )))

(define z (lambda (x) (+ 1 x)))
(define (print x)
  ((x z) 0))
(print (plus three two))
