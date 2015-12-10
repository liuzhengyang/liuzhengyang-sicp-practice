#lang racket

1
(+ 1 2)
(* 2 3 4)
(* (+ 2 3) (- 5 3))
(define foo 3)
(/ foo 3)
(define (square x) (* x x))
(define (square-of-square x)
    (square (square x)))
(square-of-square 10)


(define (abs x)
  (if (> x 0)
      x
      (- x)))

(lambda (x) (* x x))

( define  ( sum-of-squares  x  y )
  ( define  ( square  x )
    ( *  x  x ) )
  ( +  ( square  x)  ( square  y ) ) )





   ( ( (lambda  ( x )  (lambda ( y ) ( + x  y ) )) 3 ) 4)

(define nil '())
(define (accumulate op init seq )
  (if (null? seq ) init
      ( op (car seq ) (accumulate op init (cdr seq)))))

( define ( another-map proc seq )
    ( accumulate
        ( lambda  ( input  result )  ( cons ( proc input ) result ) ) 
             nil  seq ) )


( accumulate / 1 (list 1 2 3 4))
( accumulate + 0 (list 1 2 3 4))

(define (gen-list start end)
  (if (> start end) nil
      (cons start (gen-list ( + start 1 ) end))))
( define  ( flatmap  proc  seq )
    ( accumulate  append  nil 
         ( map  proc  seq ))) 

(map (lambda (x) (gen-list 1 x)) (list 1 2 3)
(flatmap (lambda (x) (gen-list 1 x)) (list 1 2 3))

