#lang racket

(define nil '())

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 (list 4 (list 5 6 7))))) 4)

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
       (if (pair? sub-tree)
           (scale-tree2 sub-tree factor)
           (* sub-tree factor)))
       tree))

(scale-tree2 (list 1 (list 2 (list 3 (list 4 (list 5 6 7))))) 4)
      