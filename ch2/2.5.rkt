#lang racket
(require "../commons.rkt")

(define (cons a b)
  (* (base-exp 2 a) (base-exp 3 b)))

(define (divide? base n)
  (= (remainder n base) 0)) 
(define (log-base base n)
  (define (log-iter count c)
    (if (not (divide? base c)) count
        (log-iter (+ count 1) (/ c base))))
  (log-iter 0 n)
  )

(define (car c)
  (log-base 2 c))


(define (cdr c)
  (log-base 3 c))