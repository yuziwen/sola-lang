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


;; mutable and transparent struct
(define-syntax mtstruct
  (λ (stx)
    (syntax-case stx ()
      [(_ expr* ...)
       #'(struct expr* ... #:mutable #:transparent)])))


;; the extras field in Ast

;; the global keys table
(define extras-keys-table (mutable-seteq))

(define/contract (extras-new-key! sym)
  (-> symbol? void?)
  (set-add! extras-keys-table sym))

(define extras/c (hash/c symbol? any/c))

(define/contract (newex key* val*)
  (-> (listof symbol?) (listof any/c) extras/c)
  (if (andmap (curry set-member? extras-keys-table)
              key*)
      (apply hasheq (interleave key* val*))
      (raise-syntax-error
       'newex
       "have keys not declared, please use `extras-new-key!` to declare first"
       key*)))

(define/contract (exref extras key)
  (-> extras/c symbol? any/c)
  (hash-ref extras key))

(define/contract (exset! extras key val)
  (-> extras/c symbol? any/c any/c)
  (hash-set! extras key val))
