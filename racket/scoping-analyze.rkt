#lang racket

(require "./helper.rkt"
         "./grammar-parser.rkt")

(provide (all-defined-out)
         (all-from-out "./helper.rkt"
                       "./grammar-parser.rkt"))


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
      [(Var ex name)
       (VarDef ex name (empty-VarRef-set))]
      [var
       (raise-syntax-error
        'Var->VarDef
        "Var struct expected"
        var)]))

  (match x
    [(Value ex val) x]

    [(Var ex name)
     (define vdef (env-ref env name))
     (define vref (VarRef ex name vdef))
     (set-add! (VarDef-refs vdef) vref)
     vref]

    [(Fn ex (list (app Var->VarDef vdef*) ...) old-body)
     (define body
       (scoping-analyze (env-ext* env vdef*)
                        old-body))
     (Fn ex vdef* body)]

    [(Let ex
          (list (app Var->VarDef lhs*) ...)
          (list (app self rhs*) ...)
          (app self body))
     (Let ex lhs* rhs* body)]

    [(Letrec ex
             (list (app Var->VarDef lhs*) ...)
             old-rhs* old-body)
     (define new-env (env-ext* env lhs*))
     (define rhs* (map (curry scoping-analyze new-env)
                       old-rhs*))
     (define body (scoping-analyze new-env old-body))
     (Letrec ex lhs* rhs* body)]

    [(Begin ex
            (list (app Var->VarDef def*) ...)
            old-body*)
     (define new-env
       (env-ext* env def*))
     (define body* (map (curry scoping-analyze new-env)
                        old-body*))
     (Begin ex def* body*)]

    [(If ex
         (app self test)
         (app self then)
         (app self alt))
     (If ex test then alt)]

    [(Set! ex
           (app Var->VarDef lhs)
           (app self rhs))
     (Set! ex lhs rhs)]

    [(Define ex
       (Var v-ex name)
       (app self rhs))
     (define lhs (env-ref env name))
     ;; ensure they are `eq?`
     (unless (equal? v-ex (Ast-pos lhs))
       (raise-syntax-error
        'scoping-analyze
        "unkown error: lhs of define expression got redefined"
        x))
     (Define ex lhs rhs)]

    [(App ex
          (app self fn)
          (list (app self arg*) ...))
     (App ex fn arg*)]))
