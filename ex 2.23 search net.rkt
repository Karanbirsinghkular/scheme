#lang sicp
(define (foreach f items)
  ((f (car items))
   (if(null? (cdr items))
     nil
     (foreach f (cdr items)))))

(foreach (lambda (x)
(newline)
(display x))
(list 57 65 67))