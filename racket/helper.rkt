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

(define/contract (exref! extras key to-set)
  (-> extras/c symbol? any/c any/c)
  (hash-ref! extras key to-set))

(define/contract (exset! extras key val)
  (-> extras/c symbol? any/c any/c)
  (hash-set! extras key val))



;; Weak-ref struct
;; similar to shared_ptr in C++ to manage memory
;; it is like weak-box but references are freed immediately
;; after the object becomes unreachable
;; (by manually call weak-ref-taget-delete!
;;  to free it from global table)
;; so eliminate the need to call collect-garbage

;; hash of any object to set of -Weak-ref
(define weak-global-table (make-hasheq))


(struct -Weak-ref (obj [exist? #:mutable]))

;; field accessor
(define/contract (weak-deref ref)
  (-> -Weak-ref? any)
  (if (-Weak-ref-exist? ref)
      (-Weak-ref-obj ref)
      (void)))

;; constructor
(define (new-weak-ref obj)
  (define ref (-Weak-ref obj #t))
  (define weak-refs
    (hash-ref! weak-global-table
               obj
               (weak-seteq)))
  (set-add! weak-refs ref)
  ref)

(define (weak-ref-target-delete! obj)
  (define s (hash-ref weak-global-table obj (weak-seteq)))
  (for ([ref (in-weak-set s)])
    (set--Weak-ref-exist?! ref #f))
  (hash-remove! weak-global-table obj))
