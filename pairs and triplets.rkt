#lang sicp
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op initial (cdr seq)))))

(define (reverse sequence)
(fold-left (lambda (x y) (cons y x)) nil sequence))

(define (enumerate-interval x y)
  (cond ((> x y) nil)
        (else  (cons x
               (enumerate-interval (+ 1 x) y)))))

(define (flatmap proc seq)
(accumulate append nil (map proc seq)))

(define (generate1<=j<i<n n)
  (flatmap (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (remove item sequence)
(filter (lambda (x) (not (= x item)))
sequence))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map
              (lambda (j) (list i j))
              (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))