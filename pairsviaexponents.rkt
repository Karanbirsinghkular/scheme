#lang sicp
(define (gcd a b)
(if (= b 0)
a
(gcd b (remainder a b))))

(define (expt b n)
(if (= n 0)
1
(* b (expt b (- n 1)))))

(define (find-exp z x)
  (define (div a b)(if (= (remainder a b) 0)
                   #t
                   #f))
  (if (div z x) (+ 1 (find-exp (/ z x) x))
                0))


(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))
(define (car z)
  (find-exp z 2))
(define (cdr z)
  (find-exp z 3))

(define z (cons 8 6))
(car z)
(cdr z)