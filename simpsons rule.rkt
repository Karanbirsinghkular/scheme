#lang sicp



(define (even x)
  (cond ((= x 0) #t)
        ((< x 0) #f)
        ((> x 0)(even (- x 1)))))
(define (iseve x)
       (even (/ (abs x) 2)))


(define (integ f a b n)
  (define (nth-y k)
  (f (+ a (* k h))))
   (define h (/ (- b a) n))
   (define (term k)
     (cond ((iseve k)  (* 2 (nth-y k)))
           ((not(iseve k))(* 4 (nth-y k)))))
  (define (sum counter) (+ (term counter)
                           (cond ((< counter (+ n 1))(sum (+ counter 1)))
                                 (else 0))))
  (* (sum 0) (/ h 3))
)

(integ (lambda (x) (-(+ (* 6 x x x)
                      x)
                      99
                      (* 5 x x)))
       5
       10
       10000)