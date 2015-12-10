#lang racket
(cons 1 2)

(cons (cons 1 2) 3)

(define foo (cons 1 2))

(car foo)

(cdr foo)

(cons 1 null)

(cons 1 (cons 2 null))


(define bar (cons 1 null))

null

(define nil '())
(cons 1 (cons 2 null))
(define (enumerate-interval low high)
  (if (> low high) nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op init seq)
   (if (null? seq) init
       (op (car seq) (accumulate op init (cdr seq)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (safe? k positions)
  (define (safe-with col)
    (let ((vk (list-ref positions (- k 1)))
          (vcol (list-ref positions (- col 1))))
    (nor (= vk vcol)
         (= (- k col) (- vk vcol))
         (= (- k col) (- vcol vk)))))
  (define (safe-iter col)
    (if (= col k) true
          (if ( not (safe-with col) ) false
              (safe-iter (+ col 1)))))
  (safe-iter 1))

(define (queens board-size)
  (define (queens-cols col)
    (if (= col 0) '(())
        (filter (lambda (positions) (safe? col positions))
                (flatmap
         (lambda (positions)
           (map
            (lambda (row) (append positions (list row)))
           (enumerate-interval 1 board-size)))
         (queens-cols (- col 1))))))

  (queens-cols board-size))