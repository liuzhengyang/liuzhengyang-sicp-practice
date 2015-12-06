#lang racket
(require "../commons.rkt")
(define nil null)
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) 
                            rest)))))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(sum-odd-squares (list 1 (list 3 4 6) (list 7 8) 9))


(subsets (list 1 2 3))
(subsets (list 1 2))


