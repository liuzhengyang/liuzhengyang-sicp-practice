#lang racket

(require "../commons.rkt")
(define empty-board '())
(define (safe-with-col k i positions)
  (let ((vk (list-ref positions (- k 1)))
        (vi (list-ref positions (- i 1))))
    (not (or (= vk vi)
         (= (abs (- i k)) (abs (- vk vi)))))))

(define (safe? k positions)
  (define (safe-iter i)
    (if (= i k) true
        (if (not (safe-with-col k i positions)) false
            (safe-iter (+ i 1)))))
  (safe-iter 1)
  )

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0) (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
           (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)