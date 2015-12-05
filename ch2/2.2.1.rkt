#lang racket


(define nil '())
(cons 1 (cons 2 (cons 3 (cons 4 nil))))
(list 1 2 3 4)

(define one-through-four (list 1 2 3 4))


(define (list-ref l n)
  (if (= n 0) (car l)
      (list-ref (cdr l) (- n 1))
  ))

(define (length items)
  (define (len-iter items length)
    (if (null? items) length
        (len-iter (cdr items) (+ 1 length))))
  (len-iter items 0)
  )


;(define (append list1 list2)
 ; (if (null? list1) list2
  ;    (cons (car list1) (append (cdr list1) list2))))

;; 2.17
(define (last-pair l)
  (cond ((null? l) nil)
        ((= 1 (length l)) l)
        (else (last-pair (cdr l)))))

;; 2.18

(define (reverse l)
  (define (reverse-iter reverse-list list)
    (if (null? list) reverse-list
        (reverse-iter (append (cons (car list) nil) reverse-list) (cdr list))))
  (reverse-iter (list) l)
  )



(reverse (list 1 2 3 4 5 6 10))
(car (reverse (list 1 2 3 4 5 6 10)))

(define (