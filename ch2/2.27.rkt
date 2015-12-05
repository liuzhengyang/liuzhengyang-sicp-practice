#lang racket

(define (reverse l)
  (define (reverse-iter result input)
    (if (null? input) result
        (reverse-iter (append (list (car input)) result) (cdr input))))
  (reverse-iter '() l))



(define (deep-reverse l)
  (define (other-list l)
    (if (pair? (cdr l)) (car (cdr l))
        (cdr l)))
  (cond 
      ((not (pair? l)) l)
      ((= 1 (length l)) (list (deep-reverse (car l))))
      (else (list (deep-reverse (other-list l))
            (deep-reverse (car l))))))

(cons (car (list 1 (list 2))) (cdr (list 1 (list 2))))
(define l (list (list 1 2) (list 3 4)))

(deep-reverse (list 1 2 3))
(newline)
(deep-reverse (list 2 (list 2) (list (list 3 4) 4)))
(deep-reverse (list 1 2))

(deep-reverse (list (list 1) (list 3 (list 5 4))))

(deep-reverse (list (list 1 2) (list 3 4)))

(deep-reverse (list (list 2) (list 3 4)))

(deep-reverse '((1)))


(define (my-fordr fn start lst)
  (if (null? list) start
      