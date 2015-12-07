#lang racket

(require "../commons.rkt")
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))


(define (append seq1 seq2)
  (accumulate (lambda (x y) (cons x y))
              seq2 seq1))


(define (length sequence)
  (accumulate (lambda (x y) (+ y 1))  0 sequence))