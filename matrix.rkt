#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))



(define (accumulate-n op init seqs)
  (define (first-pair seq)
    (if (null? seq)
        nil
        (cons (caar seq) (first-pair (cdr seq)))))
  (define (remove-first seqs)
  (if (null? seqs)
      nil
      (cons (cdar seqs)
            (remove-first (cdr seqs)))))
(if (null? (car seqs))
nil
(cons (accumulate op init (first-pair seqs))
      (accumulate-n op init (remove-first seqs)))))

(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9) ))
(define y (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define vx (list 1 2 3))
(define vy (list 1 2 3))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
(map (lambda (vm) (dot-product v vm)) m))

(define (transpose mat)
(accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
(let ((cols (transpose n)))
(map (lambda (first) (matrix-*-vector cols first)) m)))

(matrix-*-matrix x y)