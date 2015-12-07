#lang racket

(require "../commons.rkt")
(require "2.38.rkt")

(define (reverse-left sequence)
  (fold-left (lambda (result new) (cons new result)) nil sequence))
(reverse-left '(1 2 3 4))

(define (reverse-right sequence)
  (fold-right (lambda (new result) (append result (list new))) nil sequence))

(reverse-right '(1 2 3 4))