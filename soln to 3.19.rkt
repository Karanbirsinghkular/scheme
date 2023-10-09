#lang sicp
(define (append! x y)
(set-cdr! (last-pair x) y)
x)
(define (last-pair x)
(if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
(set-cdr! (last-pair x) x)
x)

(define (count-pair x)
  (define (p x)
    (if (pair? x) 1 0))
  (apply + (map p x))
  )
(define (cycle? x)
  (define (temp x)(if (null? x)
                      #f
                      (if (eq? y (cdr x))
                          #t
                          (temp (cdr x)))))
  (define y x)
  (temp x)
  )
(define z (make-cycle (list 'a 'b 'c)))
(define w '(a b c a))
(cycle? w)


