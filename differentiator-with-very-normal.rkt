#lang sicp
(define x (list 'a 'b))
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (find-proc item x)
  (define (iter x)
    (cond ((null? x) false)
          ((eq? item (car x)) x)
          (else (iter (cdr x)))))
  (if(= (length (cdr (iter x))) 1)
     (cadr (iter x))
     (cdr (iter x))))

(define (find item x)
    (cond ((null? x) #f)
          ((eq? item (car x)) #t)
          (else (find item (cdr x)))))

(define (find-prec item x)
  (define (iter x result)
    (cond ((null? x) false)
          ((eq? item (car x)) result)
          (else (iter (cdr x) (append result (list (car x)))))))
  (if(= (length(iter x nil)) 1)
     (car (iter x nil))
     (iter x nil)))

(define (equal? a b)
  (if (and(pair? a)(pair? b))
      (and(equal? (car a) (car b))(equal?(cdr a)(cdr b)))
      (if (or(pair? a)(pair? b))
          #f
          (eq? a b))
      ))
(define (pow m e)
  (cond((= e 1) m)
        (else  (* m
                  (pow m (- e 1))))))

(define (remove i sequence)
  (if (null? sequence)
      nil
      (append (if(= i 0) nil (list (car sequence)))
              (remove (- i 1) (cdr sequence)))))
;////////////////////////////////////////////////////////////////
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
(cond ((=number? a1 0) a2)
((=number? a2 0) a1)
((and (number? a1) (number? a2))
(+ a1 a2))
(else (list a1 '+ a2))))

(define (make-product m1 m2)
(cond ((or (=number? m1 0) (=number? m2 0)) 0)
((=number? m1 1) m2)
((=number? m2 1) m1)
((and (number? m1) (number? m2)) (* m1 m2))
(else (list m1 '* m2))))

(define (sum? x) (find '+ x))
(define (addend s) (find-prec '+ s))
(define (augend s) (find-proc '+ s))

(define (product? x) (find '* x))
(define (multiplier p) (find-prec '* p))
(define (multiplicand p) (find-proc '* p))

;(define (exp? x) (and (pair? x) (eq? (car x) '^)))
;(define (base p) (cadr p))
;(define (power p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
     ;   ((exp? exp)
    ;     (make-product (make-product (power exp)
   ;                                  (make-exp (base exp)
  ;                                             (- (power exp) 1)))
 ;                      (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))
(deriv '(2 * x * x * (x + 2)) 'x)