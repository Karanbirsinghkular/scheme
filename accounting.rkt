#lang sicp
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) true)
        (else (memq item (cdr x)))))

(define (make-accumulator sum)
  (define (dispatch add)
    (set! sum (+ add sum)))
  dispatch)

(define (make-monitored proc)
  (define called 0)
  (define (dispatch arg)
    (cond ((eq? arg 'how-many-calls) called)
          ((eq? arg 'reset-count)(set! called 0))
          (else (begin (set! called (+ 1 called))
                       (proc arg)))))
  dispatch
  )
(define (call-the-cops)
  (error "COPS ON THE WAY"))

(define (make-account balance pass)
  (define counter-for-cops 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch typed-pass m)
    (cond
      ((not (eq? typed-pass pass)) (begin (set! counter-for-cops (+ counter-for-cops 1))
                                          (if(= counter-for-cops 3)
                                             (lambda (x)
                                               (newline)
                                               (display "wrong password")
                                               (call-the-cops))
                                             (lambda (x)
                                               (newline)
                                               (display "wrong password")))))
      ((eq? typed-pass pass) (begin(set! counter-for-cops 0)
                                   (cond ((eq? m 'withdraw) withdraw)
                                         ((eq? m 'deposit) deposit)
                                       (else (error "Unknown request: MAKE-ACCOUNT"
                                                    m)))))))
  
  dispatch)

(define (make-joint-account acc acc-pass joint-pass)
  (define (dispatch typed-pass m)
    (cond
      ((not (eq? typed-pass joint-pass)) (lambda (x) (newline) (display "wrong password")))
      ((eq? typed-pass joint-pass) (cond ((eq? m 'withdraw) (acc acc-pass 'withdraw))
                                       ((eq? m 'deposit) (acc acc-pass 'deposit))
                                       (else (error "Unknown request: MAKE-JOINT-ACCOUNT"
                                                    m))))))
  
  dispatch)

(define acc (make-account 100 'secret-password))
(define joint (make-joint-account acc 'secret-password 'joint-password))
((acc 'secret-password 'withdraw) 40)
((joint 'joint-password 'withdraw) 40)
((acc 'joint-password 'withdraw) 40)
((joint 'secret-password 'withdraw) 40)