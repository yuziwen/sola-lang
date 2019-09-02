#lang racket

(require "./lexical-parser.rkt"
         "./helper.rkt")

(provide (except-out (all-defined-out) ast->s-expr p)
         ;; in lexical-parser.rkt
         pos->string lexical-parse)


;; for test convenience
(define (p in)
  (ast->s-expr
   (grammar-parse
    (lexical-parse in))))

;; abstract syntax tree

(struct Ast (raw-pos) #:transparent)

(mtstruct Value Ast (val))

(mtstruct Num Value ())
(mtstruct Bool Value ())
(mtstruct Str Value ())
(mtstruct Sym Value ())


(struct Var Ast (name) #:transparent)


;; syntax structure
(mtstruct Stx Ast ())

(mtstruct Fn Stx (param* body))

(mtstruct Let Stx (lhs* rhs* body))

(mtstruct Letrec Stx (lhs* rhs* body))

(mtstruct Begin Stx (def* body*))

(mtstruct If Stx (test then alt))

(mtstruct Set! Stx (lhs rhs))

(mtstruct Define Stx (lhs rhs))

(mtstruct App Stx (fn arg*))


(define (reserved-word? sym)
  (memq sym '(fn let letrec begin if set! define)))

;; grammar-parse :: Lex -> Ast
(define (grammar-parse lex)

  (define self grammar-parse)

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
                   (app self body)))
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
                                     (app self rhs*)))
                            ...))
            (app self body)))
     #:when (and (memq let/rec '(let letrec))
                 (andmap symbol? lhs*))
     (define ctor (if (eq? let/rec 'let) Let Letrec))
     (ctor pos
           (map (λ (pos lhs) (Var pos lhs))
                lhs-pos* lhs*)
           rhs* body)]

    ;; begin expression
    [(Square pos
             (list (SelfEval _ 'begin)
                   (app self body*) ...))
     (define internal-def*
       (filter (λ (expr) (Define? expr))
               body*))
     (match-define (list (Define _ def-var* _) ...)
       internal-def*)
     (Begin pos def-var* body*)]

    ;; if expression
    [(Square pos
             (list (SelfEval _ 'if)
                   (app self test)
                   (app self then)
                   (app self alt)))
     (If pos test then alt)]

    ;; set! or define expression
    [(Square pos
             (list (SelfEval _ set/define)
                   (SelfEval lhs-pos lhs)
                   (app self rhs)))
     #:when (and (memq set/define '(set! define))
                 (symbol? lhs))
     (define ctor (if (eq? set/define 'set!) Set! Define))
     (define result (ctor pos (Var lhs-pos lhs) rhs))
     result]

    ;; function apply expression
    [(Square pos
             (list (? (λ (f)
                        (not (reserved-word?
                              (Lex-datum f))))
                      (app self func))
                   (app self arg*)
                   ...))
     (App pos func arg*)]

    [x  ;; else
     (raise-syntax-error
      'grammar-parse "unrecognized syntax" x)]))





(define (ast->s-expr ast)

  (define str-out
    (open-output-string))
  (define (printf . arg*)
    (apply fprintf str-out arg*))

  (define (show ast)
    (match ast
      [(Num pos x) (printf "(Num:~v ~v) " pos x)]
      [(Bool pos x) (printf "(Bool:~v ~v) " pos x)]
      [(Str pos x) (printf "(Str:~v ~v) " pos x)]
      [(Sym pos x) (printf "(Sym:~v ~v) " pos x)]
      [(Var pos name) (printf "(Var:~v ~v) " pos name)]
      [(Fn pos param* body)
       (printf "(Fn:~v (" pos)
       (for ([p (in-list param*)])
         (show p))
       (printf ") ")
       (show body)
       (printf ") ")]
      [(or (Let pos lhs* rhs* body)
           (Letrec pos lhs* rhs* body))
       (printf "(~a:~v ("
               (if (Let? ast) "Let" "Letrec")
               pos)
       (for ([lhs (in-list lhs*)]
             [rhs (in-list rhs*)])
         (printf "[")
         (show lhs)
         (printf " ")
         (show rhs)
         (printf "]"))
       (printf ") ")
       (show body)
       (printf ") ")]
      [(Begin pos def* body*)
       (printf "(Begin:~v (" pos)
       (for ([def (in-list def*)])
         (show def))
       (printf ") ")
       (for ([body (in-list body*)])
         (show body))
       (printf ") ")]
      [(If pos test then alt)
       (printf "(If:~v " pos)
       (show test)
       (show then)
       (show alt)
       (printf ") ")]
      [(Set! pos lhs rhs)
       (printf "(Set!:~v " pos)
       (show lhs)
       (show rhs)
       (printf ") ")]
      [(Define pos lhs rhs)
       (printf "(Define:~v " pos)
       (show lhs)
       (show rhs)
       (printf ") ")]
      [(App pos fn arg*)
       (printf "(App:~v " pos)
       (for ([a (in-list (cons fn arg*))])
         (show a))
       (printf ") ")]))

  (show ast)

  (define str-in
    (open-input-string (get-output-string str-out)))
  (read str-in))