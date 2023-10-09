#lang sicp
(define (product term a b next)
  (* a
     (if (>= a b)
         1
         (product term (next a) b next))))

(define (identity x)
  (x))
(define (inc x)
  (+ x 1))
(define (productfrom a b)
  (product identity a b inc))
(productfrom 1 6)