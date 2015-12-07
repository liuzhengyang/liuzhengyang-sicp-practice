#lang racket
(define nil '())
(define (my-map proc lst)
  (if (null? lst) nil
      (cons (proc (car lst)) (my-map proc (cdr lst)))))


(define (my-max . lst)
  (display lst)
  (define (max-iter max-pre lst)
    (if (null? lst) max-pre
        (if (> max-pre (car lst))
            (max-iter max-pre (cdr lst))
            (max-iter (car lst) (cdr lst)))))
  (if (null? lst) nil 

  (max-iter (car lst) (cdr lst)))
  )
            