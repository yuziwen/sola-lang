#lang racket

(provide (all-defined-out))


(define (interleave ls1 ls2)
  (cond
    [(null? ls1) ls2]
    [else (cons (car ls1)
                (interleave ls2 (cdr ls1)))]))


(define (hash-remove* h key*)
  (for/fold ([acc h])
            ([k (in-list key*)])
    (hash-remove acc k)))

(define-match-expander ->
  (λ (stx)
    (syntax-case stx ()
      [(_ proc var* ...)
       #'(app proc var* ...)])))