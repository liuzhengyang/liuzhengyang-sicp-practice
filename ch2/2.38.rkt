#lang racket
(require "../commons.rkt")
(provide fold-left)
(provide fold-right)
(define (fold-right op init lst)
  (if (null? lst) init
      (op (car lst)
          (fold-right op init (cdr lst)))))

(define (fold-left op init lst)
  (if (null? lst) init
      (fold-left op (op init (car lst)) (cdr lst))))

(fold-left / 1 '(1 2 3 4))

(define (fold-left2 op init sequence)
  (define (fold-left-iter result rest)
    (if (null? rest) result
        (fold-left-iter (op result (car rest)) (cdr rest))))
  (fold-left-iter init sequence))

(fold-left2 / 1 '(1 2 3 4))
(fold-right / 1 '(1 2 3 4))

(fold-right list nil (list 1 2 3))



(fold-left list nil (list 1 2 3))