#lang racket

1
(+ 1 2)
(* 2 3 4)
(* (+ 2 3) (- 5 3))
(define foo 3)
(/ foo 3)
(define (square x) (* x x))
(define (square-of-square x)
    (square (square x)))
(square-of-square 10)