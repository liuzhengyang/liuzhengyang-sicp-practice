#lang racket

(require "../commons.rkt")
(define (accumulate-n op initial seqs)
  (if (null? (car seqs)) nil
      (cons (accumulate op initial (map car seqs))
            (accumulate-n op initial (map cdr seqs)))))