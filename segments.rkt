#lang sicp
(define (print-point p)
(newline)
(display "(")
(display (x-point p))
(display ",")(display (y-point p))
(display ")"))

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (make-segment sp ep)
  (cons sp ep))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (midpoint seg)
  (make-point (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
              (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))

(define seg (make-segment (make-point -5 0)
                          (make-point  5 6)))
(print-point (midpoint seg))
  