#lang racket
(define (make-accumulator sum)
  (define (accumulate num)
    (set! sum (+ sum num))
    sum)
  accumulate)

(define (make-accumulator2 sum)
  (lambda (num)
    (set! sum (+ sum num))
    sum))