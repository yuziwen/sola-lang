#lang racket

(require "./helper.rkt"
         "./grammar-parser.rkt")

(provide (all-defined-out))


;; extended Ast structures

;; def: VarDef
(mtstruct VarRef Var (def))

;; refs: VarRef-set
(mtstruct VarDef Var (refs))

(define (empty-VarRef-set) (mutable-seteq))

;; other Ast structs remain the same



;; the environment hash
(define env/c (hash/c symbol? VarDef?))

(define/contract (new-env [var* '()] [def* '()])
  (->* () ((listof symbol?) (listof VarDef?)) env/c)
  (apply hasheq (interleave var* def*)))

(define/contract (env-ext* env def*)
  (-> env/c (listof VarDef?) env/c)

  (define key* (map Var-name def*))
  (apply hash-set*
         env (interleave key* def*)))

(define/contract (env-ref env var)
  (-> env/c symbol? VarDef?)
  (hash-ref env var))



;; Ast -> Ast
(define (scoping-analyze env x)
  (define (self x)
    (scoping-analyze env x))

  (define (Var->VarDef var)
    (match var
      [(Var pos name)
       (VarDef pos name (empty-VarRef-set))]
      [var
       (raise-syntax-error
        'Var->VarDef
        "Var struct expected"
        var)]))

  (match x
    [(Value pos val) x]

    [(Var pos name)
     (define vdef (env-ref env name))
     (define vref (VarRef pos name vdef))
     (set-add! (VarDef-refs vdef) vref)
     vref]

    [(Fn pos (list (app Var->VarDef vdef*) ...) old-body)
     (define body
       (scoping-analyze (env-ext* env vdef*)
                        old-body))
     (Fn pos vdef* body)]

    [(Let pos
          (list (app Var->VarDef lhs*) ...)
          (list (app self rhs*) ...)
          (app self body))
     (Let pos lhs* rhs* body)]

    [(Letrec pos
             (list (app Var->VarDef lhs*) ...)
             old-rhs* old-body)
     (define new-env (env-ext* env lhs*))
     (define rhs* (map (curry scoping-analyze new-env)
                       old-rhs*))
     (define body (scoping-analyze new-env old-body))
     (Letrec pos lhs* rhs* body)]

    [(Begin pos
            (list (app Var->VarDef def*) ...)
            old-body*)
     (define new-env
       (env-ext* env def*))
     (define body* (map (curry scoping-analyze new-env)
                        old-body*))
     (Begin pos def* body*)]

    [(If pos
         (app self test)
         (app self then)
         (app self alt))
     (If pos test then alt)]

    [(Set! pos
           (app Var->VarDef lhs)
           (app self rhs))
     (Set! pos lhs rhs)]

    [(Define pos
       (app Var->VarDef lhs)
       (app self rhs))
     (env-set! env lhs)
     (Define pos lhs rhs)]

    [(App pos
          (app self fn)
          (list (app self arg*) ...))
     (App pos fn arg*)]))
