#lang sicp


(define (square-list items)
(map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4))
