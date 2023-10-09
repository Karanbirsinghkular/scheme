#lang sicp
(define (cube x)(* x x x))

(define (sum term a next b)
(define (iter a result)
(if (>= a b)
 result
(iter (next a)(+ result (term (next a))))))
(iter a 0))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
(sum cube a inc b))

(sum-cubes 0 3)