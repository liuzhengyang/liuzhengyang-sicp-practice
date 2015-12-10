#lang racket

(require "../commons.rkt")
(define empty-board '())
(define (safe-with-col k i positions)
  (let ((valuek (list-ref positions (- k 1)))
        (valuei (list-ref positions (- i 1))))
    (nor (= valuek valuei)
         (= (abs (- i k)) (abs (- valuek valuei))))))

(define (safe? k positions)
  (define (safe-iter i)
    (if (= i k) true
        (if (not (safe-with-col k i positions)) false
            (safe-iter (+ i 1)))))
  (safe-iter 1))

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
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)
( define  ( gen-list  start  end )
   (  define  ( gen-iter  curr  result )
         ( if  ( < curr  start )  result
              ( gen-iter  ( - curr 1 )  (cons curr result ) ) ) )
   ( gen-iter  end  nil ) )


(gen-list 1 12)

(define (my-map proc seq)
  (if (null? seq) nil
      (cons (proc (car seq)) ( my-map proc (cdr seq)))))


(map (lambda (x) ( + x 1))
     (map square
          (gen-list 1 5)))
( accumulate  +  0 
     ( filter ( lambda ( x ) ( > x  0 ) )
         ( map ( lambda ( x ) ( - x  5 ) )
            (enumerate-interval 1 10 ) ) ) )

( define  ( gen-list2  start  end )
   (  define  ( gen-iter  next  result )
         ( if  ( < next  start )  result
              ( gen-iter  ( - next 1 )  (cons next result ) ) ) )
   ( gen-iter  end  nil ) )