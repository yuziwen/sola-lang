#lang racket

;; this file has been deprecated
;; and will be removed when this feature is implemented

(require (rename-in "./grammar-parser.rkt"
                    [Fn old-Fn])
         "./helper.rkt")

(provide (all-defined-out))


(module vars racket
  (require "./helper.rkt"
           racket/hash
           (rename-in "./grammar-parser.rkt"
                      [Fn old-Fn]))

  (provide/contract
   #:exists (Vars VarRef-set)
   [new-vars (->* ()
                  ((listof symbol?)
                   (listof VarRef-set))
                  Vars)]
   [vars-ref (-> Vars symbol? VarRef-set)]
   [vars-add-ref (-> Vars symbol? VarRef-set Vars)]
   [vars-add-ref* (-> Vars symbol? (listof VarRef-set) Vars)]
   [vars-union (->* (Vars) () #:rest (listof Vars)
                    Vars)]
   [vars-rem* (-> Vars (listof symbol?) Vars)]
   [vars-sub (-> Vars Vars Vars)])


  ;; def: VarDef
  (struct VarRef Var (def) #:transparent)

  ;; refs: VarRef-set
  (struct VarDef Var (refs) #:transparent)

  (define (new-VarRef-set) (mutable-seteq))
  (define (VarRef-set-union!) )

  ;; the set of vars (actually a hash table)
  ;; the keys form a set, the values are refs of those keys
  ;; vars :: (hasheq symbol -> (set of VarRef))

  (define (new-vars [var* '()] [refs* '()])
    (apply hasheq (interleave var* refs*)))

  (define (vars-ref vars sym)
    (hash-ref vars sym
              (λ () (mutable-seteq))))

  (define (vars-add-ref vars var refs)
    (cond
      [(hash-has-key? vars var)
       (hash-update vars var
                    (λ (refs2) (set-union! refs2 refs)))]
      [else (hash-set vars var refs)]))

  (define (vars-add-ref* vars var* refs*)
    (cond
      [(null? var*) vars]
      [else (vars-add-ref*
             (vars-add-ref vars (car var*) (car refs*))
             (cdr var*) (cdr refs*))]))

  (define (vars-union vars1 . vars-rest*)
    (define (union2 vars1 vars2)
      (hash-union vars1 vars2
                  ;; keys conflict
                  #:combine set-union))
    (cond
      [(null? vars-rest*) vars1]
      [else (apply vars-union
                   (union2 vars1 (car vars-rest*))
                   (cdr vars-rest*))]))

  (define (vars-rem* vars key*)
    (hash-remove* vars key*))

  (define (vars-sub vars1 vars2)
    (vars-rem* vars1 (hash-keys vars2)))

  ;; vars module end
  )

(require 'vars)




;; fvs: vars struct, free variables in body
(struct Fn old-Fn (fvs) #:transparent)

;; others remain the same


;; the environment hash
(define (new-env [var* '()] [def* '()])
  (apply hasheq (interleave var* def*)))

(define (env-ext* env var* def*)
  (apply hash-set*
         env (interleave var* def*)))

(define (env-ref env var)
  (hash-ref env var))


;; naming conventions
;; abc* :: list abc ;; abcs :: set abc or `vars` struct
;; abc+ :: hash abc

;; Parsed -> Ast
(define (build-ast parsed)

  ;; -> (values Ast vars)
  (define (build parsed env)
    (match-define (Parsed raw-pos datum) parsed)
    ;; (define (ast ctor . arg*)
    ;;   (apply ctor (cons raw-pos arg*)))

    (match datum

      ;; primitive values
      [(? number? x) (values (Num raw-pos x) (new-vars))]
      [(? boolean? x) (values (Bool raw-pos x) (new-vars))]
      [(? string? x) (values (Str raw-pos x) (new-vars))]
      [(list (Parsed _ 'quote) (Parsed _ sym))
       (values (Sym raw-pos sym) (new-vars))]

      ;; fn expression
      [(list (Parsed _ 'fn)
             (Parsed param-list-pos
                     (list (Parsed param-pos* param*) ...))
             body)
       (define param-ast*
         (map (λ (pos param)
                (VarDef pos param (mutable-seteq)))
              param-pos* param*))
       (define-values (body-ast b-fvs)
         (build body (env-ext* env param* param-ast*)))
       (for ([param (in-list param-ast*)])
         (define refs (vars-ref b-fvs
                                (VarDef-name param)))
         (set-union! (VarDef-refs param) refs))
       (define fvs-ret (vars-rem* b-fvs param*))
       ;; return
       (values (Fn raw-pos param-ast* body-ast fvs-ret)
               fvs-ret)]

      ;; local expression
      [(list (Parsed _ 'local)
             (Parsed pair-list-pos
                     (list (Parsed pair-pos* pair*) ...))
             body)
       (match-define
         (list (list (Parsed lhs-pos* lhs*)
                     rhs*)
               ...)
         pair*)
       ;; TODO: extract to a function
       (define lhs-ast*
         (map (λ (pos var) (VarDef pos var (mutable-seteq)))
              lhs-pos* lhs*))
       (define ext-env (interleave lhs* lhs-ast*))
       (define-values (body-ast b-fvs)
         (build body (env-ext* env lhs* lhs-ast*)))
       (for ([lhs (in-list lhs-ast*)])
         (define refs (vars-ref b-fvs (VarDef-name lhs)))
         (set-union! (VarDef-refs lhs) refs))
       (define-values (rhs-ast* rhs-fvs*)
         (for/lists (ast* fvs*) ;; accumulators
                    ([rhs (in-list rhs*)])
           (build rhs env)))
       (define fvs-ret (apply vars-union
                              (vars-rem* b-fvs lhs*)
                              rhs-fvs*))
       ;; return
       (values (Local raw-pos lhs-ast* rhs-ast* body-ast)
               fvs-ret)]

      ;; begin expression
      [(list (Parsed _ 'begin) expr* ...)

       (define-values (expr-ast* fvs*)
         (for/lists (e-ast* fvs*)
                    ([e (in-list expr*)])
           (build e env)))
       (values (Begin raw-pos expr-ast*)
               (apply vars-union fvs*))]

      ;; if expression
      [(list (Parsed _ 'if) test then else)

       (define-values (test-ast test-fvs) (build test env))
       (define-values (then-ast then-fvs) (build then env))
       (define-values (else-ast else-fvs) (build else env))
       (define fvs-ret
         (vars-union test-fvs then-fvs else-fvs))
       (values (If raw-pos test-ast then-ast else-ast)
               fvs-ret)]

      ;; set! expression
      [(list (Parsed _ 'set!) lhs rhs)

       (define-values (lhs-ast lhs-fvs) (build lhs env))
       (define-values (rhs-ast rhs-fvs) (build rhs env))
       (define fvs-ret (vars-union lhs-fvs rhs-fvs))
       (values (Set! raw-pos lhs-ast rhs-ast)
               fvs-ret)]

      ;; let expression
      [(list (Parsed _ 'let) lhs rhs)
       (error "let expression not implemented yet")]

      ;; function application expression
      [(list fn arg* ...)

       (define-values (ast* fvs*)
         (for/lists (ast* fvs*)
                    ([expr (in-list (cons fn arg*))])
           (build expr env)))
       (values (App raw-pos (car ast*) (cdr ast*))
               (apply vars-union fvs*))]

      ;; variable reference
      [(? symbol? var)
       (define ref (VarRef raw-pos (env-ref env var)))
       (values ref
               (new-vars (list var)
                         (list (mutable-seteq ref))))]))
  (build parsed (new-env)))
