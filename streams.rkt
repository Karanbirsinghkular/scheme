#lang sicp

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (square x)(* x x))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              proc (map stream-cdr argstreams)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
;(define x (stream-enumerate-interval 1 5))
;(define y (stream-enumerate-interval 6 10))
;(define z (stream-enumerate-interval 11 15))
;(define res (stream-map + x y z))

(define (display-stream stream n)
  (define (temp stream n)
    (if (eq? n 'full)
                      (if (null? stream)
                          (display ")")
                          (begin(display (stream-car stream))
                                (display " ")
                                (temp (stream-cdr stream))))
                      (if (= n 0)
                          (begin (stream-car stream)
                                 (display ")"))
                          (begin(display (stream-car stream))
                                (display " ")
                                (temp (stream-cdr stream) (- n 1)))                          
                          )))
  (display "(")
  (temp stream n))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))



(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define factorials
(cons-stream 1 (mul-streams (integers-starting-from 2) factorials)))


(define (partial-sums stream)
  (cons-stream (car stream) (add-streams (stream-cdr stream) (partial-sums stream))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define S (cons-stream 1 (merge
                          (merge
                           (scale-stream (integers-starting-from 1) 2)
                           (scale-stream (integers-starting-from 1) 3))
                          (scale-stream (integers-starting-from 1) 5))))
(define (integrate-series stream)
  (div-streams stream (integers-starting-from 1)))

(define cosine-series (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(define (pow m e)
  (cond((= e 1) m)
       ((= e 0) 1)
        (else  (* m
                  (pow m (- e 1))))))

(define (sin-x x power-limit)
  (define (iter power series)
    (+ (* (pow x power) (stream-car series))
       (if (= power power-limit)
           0
           (iter (+ 1 power) (stream-cdr series)))))
  (iter 0 sine-series)
  )
(define (cos-x x power-limit)
  (define (iter power series)
    (+ (* (pow x power) (stream-car series))
       (if (= power power-limit)
           0
           (iter (+ 1 power) (stream-cdr series)))))
  (iter 0 cosine-series)
  )

(define (mul-series a b)
  (cons-stream (* (stream-car a) (stream-car b)) (add-streams (scale-stream (stream-cdr b) (stream-car a))
                                                                (mul-series (stream-cdr a) b))))

(define (invert-unit-series series)
  (define 1-less-series (stream-cdr series))
  (cons-stream 1 (scale-stream (mul-series 1-less-series (invert-unit-series series)) -1)))

(define (div-series s1 s2)
  (if (= (stream-car s2) 1)
  (mul-series s1 (invert-unit-series s2))
  (error "the 2nd series should have 1 as a constant term")))

(define tan-series (div-series sine-series cosine-series))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; Sn-1
        (s1 (stream-ref s 1)) ; Sn
        (s2 (stream-ref s 2))) ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define ones (cons-stream 1 (scale-stream ones -1)))

(define ln2 (partial-sums (cons-stream 1 (scale-stream (div-streams ones (integers-starting-from 2))
                                                       -1))))
(display-stream (partial-sums (integers-starting-from 1)) 100)