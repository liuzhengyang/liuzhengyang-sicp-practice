#lang racket

(define nil null)
(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(fringe (list 1 (list 2 3) (list 3 4 5 6 (list 7 8)) (list 1) 1))
      
      