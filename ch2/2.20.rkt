#lang racket

(require "../commons.rkt")
(define nil '())
(define f (lambda (x y . z) 1))

(f 1 2 3 4 5)

(define g (lambda z 2))

(define (f2 x y . z) 2)

(define (g2 . z) z)


(define (filter-list preconditions l)
  (define (filter-iter l-iter l)
    (cond ((null? l) l-iter)
          ((preconditions (car l)) (filter-iter (append l-iter (list (car l))) (cdr l)))
          (else (filter-iter l-iter (cdr l)))))
  (filter-iter (list) l)
  )
(define (same-parity first . others)
  (define remain (remainder first 2))
  (define (pass? n) (= (remainder n 2) remain))
  (filter-list pass? others))


(same-parity 4 2 3 4 5 5 6 9 10)


(define (scale-list items factor)
  (if (null? items) '()
      (cons (* factor (car items))
            (scale-list (cdr items) factor))))


(scale-list (list 1 2 3 4 ) 3)

(define (map-proc proc items)
  (if (null? items) nil
      (cons (proc (car items))
            (map-proc proc (cdr items)))))

(define (square-list items)
  (if (null? items) nil
      (cons (square (car items))
            (square-list (cdr items)))))
(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(define (square-list-iter items)
  (define (square-iter result proc input)
    (if (null? input) result
        (square-iter (append result (list (proc (car input)))) proc (cdr input))))
  (square-iter '() (lambda (x) (* x x)) items)
  )


(square-list (list 1 23 4 3 535 3523 5235))
(square-list2 (list 1 23 4 3 535 3523 5235))
(square-list-iter (list 1 23 4 3 535 3523 5235))
(map-proc (lambda (x) (* x x)) (list 1 2 3 4 5))
