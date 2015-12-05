#lang racket

(define (fringe tree)
  (define (print-leaf l)
    (unless (not (null? l)) (display l)))
  (define (print-iter t)
    (g
  (if (not (pair? tree)) (print-tree tree)
      
      