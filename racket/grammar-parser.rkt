#lang racket

(require "./lexical-parser.rkt"
         "./helper.rkt")

(provide (all-defined-out)
         (all-from-out "./lexical-parser.rkt"
                       "./helper.rkt"))


;; grammar syntax tree

(struct Gram (raw-pos) #:transparent)

(struct Value Gram (val) #:transparent)

(struct Num Value () #:transparent)
(struct Bool Value () #:transparent)
(struct Str Value () #:transparent)
(struct Sym Value () #:transparent)


(struct Var Gram (name) #:transparent)


;; syntax structure
(struct Stx Gram () #:transparent)

(struct Fn Stx (param* body) #:transparent)

(struct Let Stx (lhs* rhs* body) #:transparent)

(struct Letrec Stx (lhs* rhs* body) #:transparent)

(struct Begin Stx (def* body*) #:transparent)

(struct If Stx (test then alt) #:transparent)

(struct Set! Stx (lhs rhs) #:transparent)

(struct Define Stx (lhs rhs) #:transparent)

(struct App Stx (fn arg*) #:transparent)


(define (reserved-word? sym)
  (memq sym '(fn let letrec begin if set! define)))

;; gram-parse :: Lex -> Gram
(define (gram-parse lex)
  (define parse gram-parse)
  ;; (define (check-identifier! x id-name pos)
  ;;   (cond
  ;;     [(symbol? x) (void)]
  ;;     [else (raise-syntax-error
  ;;            'grammar-parse
  ;;            (string-append id-name
  ;;                           " at position "
  ;;                           (pos->string pos)
  ;;                           " should be an identifier")
  ;;            x)]))

  (match lex
    [(SelfEval pos x)
     (cond
       [(number? x) (Num pos x)]
       [(boolean? x) (Bool pos x)]
       [(string? x) (Str pos x)]
       [(symbol? x) (Var pos x)]
       [else (raise-syntax-error
              'grammar-parse
              (string-append
               "self evaluate value type not supported\n "
               "at position " (pos->string pos))
              x)])]
    ;; quoted symbol
    [(Square pos
             (list (SelfEval _ 'quote)
                   (SelfEval sym-pos sym)))
     #:when (symbol? sym)
     (Sym pos sym)]

    ;; fn expression
    [(Square pos
             (list (SelfEval _ 'fn)
                   (Square p*-pos
                           (list (SelfEval p-pos* param*)
                                 ...))
                   (-> parse body)))
     #:when (andmap symbol? param*)
     (Fn pos
         (map (λ (pos p) (Var pos p))
              p-pos* param*)
         body)]

    ;; let or letrec expression
    [(Square
      pos
      (list (SelfEval _ let/rec)
            (Square _ (list (Bracket
                             _ (list (SelfEval lhs-pos* lhs*)
                                     (-> parse rhs*)))
                            ...))
            (-> parse body)))
     #:when (and (or (eq? let/rec 'let)
                     (eq? let/rec 'letrec))
                 (andmap symbol? lhs*))
     (define ctor (if (eq? let/rec 'let) Let Letrec))
     (ctor pos
           (map (λ (pos lhs) (Var pos lhs))
                lhs-pos* lhs*)
           rhs* body)]

    ;; begin expression
    [(Square pos
             (list (SelfEval _ 'begin)
                   (-> parse body*) ...))
     (define internal-def*
       (filter (λ (expr) (Define? expr))
               body*))
     (match-define (list (Define _ def-var* _) ...)
       internal-def*)
     (Begin pos def-var* body*)]

    ;; if expression
    [(Square pos
             (list (SelfEval _ 'if)
                   (-> parse test)
                   (-> parse then)
                   (-> parse alt)))
     (If pos test then alt)]

    ;; set! or define expression
    [(Square pos
             (list (SelfEval _ set/define)
                   (SelfEval lhs-pos lhs)
                   (-> parse rhs)))
     #:when (and (or (eq? set/define 'set!)
                     (eq? set/define 'define))
                 (symbol? lhs))
     (define ctor (if (eq? set/define 'set!) Set! Define))
     (ctor pos (Var lhs-pos lhs) rhs)]

    ;; function apply expression
    [(Square pos
             (list (? (λ (f)
                        (not (reserved-word?
                              (Lex-datum f))))
                      (-> parse func))
                   (-> parse arg*)
                   ...))
     (App pos func arg*)]

    [x  ;; else
     (raise-syntax-error
      'gram-parse "unrecognized syntax" x)]))

;; (define (p s) (gram-parse (lex-parse s)))