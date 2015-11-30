#lang racket
(define (sum term a next b)
  (define (sum-iter total a)
    (if (> a b)
        total
        (sum-iter (+ total (term a)) (next a))
        )
    )
  (sum-iter 0 a)
  )