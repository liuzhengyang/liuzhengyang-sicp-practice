#lang racket
(require "../commons.rkt")
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
             (proc sub-tree)
             (tree-map proc sub-tree))) tree))
(define (square-tree tree)
  (tree-map square tree)
  )


(map square (list 1 2 3 4 5 6))



(define nil '())
(define (my-map proc lst)
  (if (null? lst) nil
      (cons (proc (car lst))
            (my-map proc (cdr lst)))))
(my-map square (list 1 2 3 4 5))

(define (filter predicates? lst)
  (cond ((null? lst) nil)
      ((predicates? (car lst)) (cons (car lst) (filter predicates? (cdr lst))))
      (else (filter predicates? (cdr lst)))))

(filter odd? (list 1 2 3 4 5 6  8 9))


(define (accumulate op initial sequence)
  (if (null? sequence) initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5 6))