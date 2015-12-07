#lang racket
(require "../commons.rkt")
(define (enumerate-interval low high)
  (if (> low high) nil
      (cons low (enumerate-interval (+ 1 low) high))))
(define (unique-pairs n)
  (flatmap
   (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) 
  (enumerate-interval 1 n)))


