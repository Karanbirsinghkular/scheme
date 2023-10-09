#lang sicp
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;(decode sample-message sample-tree)

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) true)
        (else (memq item (cdr x)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (temp result tree)
    (if (memq symbol (symbols tree))
        (if (memq symbol (symbols (left-branch tree)))
            (if (leaf? (left-branch tree))
                (append result '(0))
                (temp (append result '(0)) (left-branch tree)))
            (if (leaf? (right-branch tree))
                (append result '(1))
                (temp (append result '(1)) (right-branch tree))))
        (error "SYMBOL " symbol "NOT FOUND IN " (symbols tree))))
  (temp nil tree))

(define (generate-huffman-tree pairs)
  (car (successive-merge (make-leaf-set pairs))))

(define (successive-merge leaf-set)
  (define (temp tree set)
    (let ((p1 (car set))
          (p2 (cadr set)))
      (let ((t (make-code-tree p1 p2)))
        (if (null? (cddr set))
            (adjoin-set t (cddr set))
            (temp nil (adjoin-set t (cddr set)))))
      ))
  (temp nil leaf-set))

;sample-tree
;(generate-huffman-tree '((A 4)(b 2)(C 1)(D 1)))
(define sample-tree1 (generate-huffman-tree '((a 2)(boom 1)(get 2)(job 2)(sha 3)(wah 1)(na 16)(yip 9))))
(decode (encode '(get a job sha na na na na na na na na get a job
sha na na na na na na na na
wah yip yip yip yip yip yip yip yip yip
sha boom) sample-tree1)sample-tree1)