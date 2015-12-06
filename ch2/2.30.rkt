#lang racket
(require "../commons.rkt")
(define nil '())
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(null? (list 1))
(square-tree (list 2))
(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree test-tree)

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree) (square-tree-map sub-tree)
             (square sub-tree))) tree))
(square-tree-map test-tree)


(define (tree-map fun tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree) (tree-map fun sub-tree)
             (fun sub-tree))) tree))

(define (square-tree2 tree)
  (tree-map square tree))

(square-tree2 test-tree)