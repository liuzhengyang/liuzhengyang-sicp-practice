#lang racket

(require "../commons.rkt")
; compose function
(define (compose f g)
  (lambda (x) (f (g x))))
(define (gen-list low high)
  (if (> low high) nil (cons low (gen-list (+ low 1) high))))
(define empty-board '())
(define (safe-with-col k i positions)
  (let ((value-k (list-ref positions (- k 1)))
        (value-i (list-ref positions (- i 1))))
    (nor (= value-k value-i)
         (= (abs (- i k)) (abs (- value-k value-i))))))

(define (safe? k positions)
  (define (safe-with i)
    (if (= i k) true
        (if (not (safe-with-col k i positions)) false
            (safe-with (+ i 1)))))
  (safe-with 1))

(define (adjoin-position new-row rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0) (list empty-board)
       (filter
         (lambda (positions) (safe? k positions))
           (flatmap
              (lambda (rest-of-queens)
                (map (lambda (new-row)
                  (adjoin-position new-row rest-of-queens))
                    (gen-list 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

