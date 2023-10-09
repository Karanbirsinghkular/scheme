#lang sicp


(define (enumerate-interval x y)
  (cond ((> x y) nil)
        (else  (cons x
               (enumerate-interval (+ 1 x) y)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (remove i sequence)
  (if (null? sequence)
      nil
      (append (if(= i 0) nil (list (car sequence)))
              (remove (- i 1) (cdr sequence)))))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op initial (cdr seq)))))

(define (flatmap proc seq)
(accumulate append nil (map proc seq)))


(define (adjoin-position new-row k board) ;new row tells which no. is queen,k tells which column,board is the board
  (define (move cntr)
    (if(= cntr (length board))
       nil
       (cons (if (= cntr k) (list new-row) (list-ref board cntr))
             (move (+ 1 cntr)))))
  (move 0)
 )

(define (safe? k board)
  (define (safe?row t seq)
    (define seqflat (flatmap (lambda (x) x) seq))
    (not(accumulate (lambda (x y) (or x y))
                    #f
                    (map (lambda (i) (= i (list-ref seqflat t)))
                         (remove t seqflat)))))
  (define (safe?diag d seq)
    (define seqflat (flatmap (lambda (x) x) seq))
    (define (safefrom k)
      (define move (abs (- d k)))
      (not(or (= (list-ref seqflat d) (+ (list-ref seqflat k) move))
              (= (list-ref seqflat d) (- (list-ref seqflat k) move)))))
    (accumulate (lambda (x y) (and x y))
                #t
                (map (lambda(i)(safefrom i)) (remove d (enumerate-interval 0 (- (length seqflat) 1)))))
    )
  (and (safe?row k board) (safe?diag k board))
  )

(define (queens board-size)
  (define empty-board (map (lambda(x)(list x))
                           (enumerate-interval 99 (+ board-size 98))))
  (define (queen-cols k)
    (if (= k -1)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 0 (- board-size 1))))
          (queen-cols (- k 1)))
         )))
  (queen-cols (- board-size 1)))
;my answer for ex 2.43 is that above program calls (queen-cols (- k 1)) 1 time
;louis' code calls it (board-size!)*t time
(queens 5)
