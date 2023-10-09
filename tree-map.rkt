#lang sicp
(define x
(list (list 1 2)
(list 2 (list 3 4) 5)
(list 6 7)))
x

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))
(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))
(square-tree x)