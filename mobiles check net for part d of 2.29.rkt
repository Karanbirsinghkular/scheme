#lang sicp
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define main (make-mobile
              (make-branch 5 (make-mobile
                              (make-branch 20 1)
                              (make-branch 10 2)))
              (make-branch 10 (make-mobile
                               (make-branch 5 4)
                               (make-branch 4 5)))))
(define main2 (make-mobile
               (make-branch 5 (make-mobile
                               (make-branch 20 1)
                               (make-branch 10 2)))
               (make-branch 10 (make-mobile
                                (make-branch 5 4)
                                (make-branch 4 (make-mobile (make-branch 6 3)
                                                            (make-branch 3 6)))))))

(define main3 (make-mobile
               (make-branch 20 1)
               (make-branch 10 2)))

(define (total-wieght mobile)
  (define (branch-wieght branch)
    (if (pair? (branch-structure branch))
        (mobile-weight (branch-structure branch))
        (branch-structure branch)))
  (define (mobile-weight mobile)
    (+ (branch-wieght(left-branch mobile)) (branch-wieght(right-branch mobile))))
  (mobile-weight mobile))

(define (balanced? mobile)
  (define (endbranch? branch)
    (not(pair? (branch-structure branch))))
  (define (endmobile? mobile)
    (and(endbranch? (left-branch mobile))(endbranch? (right-branch mobile))))
  (define (balanced?end mobile)
    (= (* (branch-length (left-branch mobile)) (branch-structure (left-branch mobile)))
       (* (branch-length (right-branch mobile)) (branch-structure (right-branch mobile)))))
  (define (balancedbranch? branch)
    (if(endbranch? branch)
       #t
     (if(endmobile? (branch-structure branch))
       (balanced?end(branch-structure branch))
       (balancedmobile (branch-structure branch)))))
  (define (balancedmobile mobile)
    (and (balancedbranch? (left-branch mobile)) (balancedbranch? (right-branch mobile))))

  (if(endmobile? mobile)
     (balanced?end mobile)
     (balancedmobile mobile)))

(balanced? main2)