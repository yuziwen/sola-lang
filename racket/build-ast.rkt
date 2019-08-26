#lang racket

(require "./parser.rkt")


;; Ast structure

(struct Ast (raw-pos) ;; position in source file
  #:transparent)


;; Value ::  (raw-pos, val) -> Value
(struct Value Ast (val) #:transparent)

(struct Num Value ())
(struct Bool Value ())
(struct Str Value ())
(struct Sym Value ())



;; non-keyword-symbols are vars

;; name: symbol ;; def: Ast
(struct VarRef Ast (name def) #:transparent)

;; name: symbol ;; ref*: set of Ast
(struct VarDef Ast (name ref*) #:transparent)



;; syntax structure
(struct Stx Ast ())

;; param*: list of VarDef ;; body: Ast
;; clo*: set of VarRef
(struct Fn Stx (param* body clo*) #:transparent)

;; lhs*: list of VarDef ;; rhs*: list of Ast
;; body: Ast
(struct Local Stx (lhs* rhs* body) #:transparent)

;; Expr*: list of Ast
(struct Begin Stx (expr*) #:transparent)

;; test, then, else: Ast
(struct If Stx (test then else) #:transparent)

;; lhs: VarRef ;; rhs: Ast
(struct Set! Stx (lhs rhs) #:transparent)

;; lhs: VarDef ;; rhs: Ast
(struct Let Stx (lhs rhs) #:transparent)

;; fn: Ast ;; arg*: list of Ast
(struct App Stx (fn arg*) #:transparent)





;; Parsed -> Ast
(define (build-ast parsed)
  (match-define (Parsed raw-pos datum) parsed)
  (define (ast ctor . arg*)
    (apply ctor (cons raw-pos arg*)))

  (match datum
    [(? number? x) (ast Num x)]
    [(? boolean? x) (ast Bool x)]
    [(? string? x) (ast Str x)]
    [(list 'quote x) (ast Sym x)]
    [(list (Parsed _ 'fn)
           (Parsed param-list-pos
                   (list (Parsed param-pos* param*) ...))
           (Parsed body-pos body))
     ???]
    [(list (Parsed _ 'local)
           (Parsed _ (list (Parsed pair-pos* pair*) ...))
           (Parsed body-pos body))
     ???]
    [(list (Parsed _ 'begin)
           (Parsed expr-pos* expr*) ...)
     ???]
    [(list (Parsed _ 'if)
           (Parsed test-pos test-expr)
           (Parsed then-pos then-expr)
           (Parsed else-pos else-expr))
     ???]
    [(list (Parsed _ 'set!)
           (Parsed lhs-pos lhs)
           (Parsed rhs-pos rhs))
     ???]
    [(list (Parsed _ 'let)
           (Parsed lhs-pos lhs)
           (Parsed rhs-pos rhs))
     ???]
    [(list (Parsed fn-pos fn)
           (Parsed arg-pos* arg*) ...)
     ???]
    [(? symbol? var)
     ???]))
