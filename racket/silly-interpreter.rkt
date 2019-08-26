#lang racket


;; this is the interpreter written in the most direct way
;; it is used in tests to produce correct results


(define (parse expr)
  (match expr
    [(? prim-datum? x) x]
    [(? symbol? x) x]
    [`(begin ,expr* ...)
     (define let-expr*
       (filter (λ (e)
                 (and (pair? e)
                      (eq? 'let (car e))))
               expr*))
     (define let-var* (map second let-expr*))
     `(local ,(map (λ (var) `[,var '**unassigned**])
                   let-var*)
        (begin
          ,@(map parse expr*)))]
    [`(let ,var ,(app parse val))
     `(set! ,var ,val)]
    [`(fn ,param* ,(app parse body*) ...)
     `(fn ,param* (begin ,@body*))]
    [`(local ([,var* ,(app parse val*)] ...)
        ,body* ...)
     `(local ,(map (λ (var val) `[,var ,val])
                   var* val*)
        (begin ,@body*))]
    [`(if ,(app parse test)
          ,(app parse conseq)
          ,(app parse alt))
     `(if ,test ,conseq ,alt)]
    [`(set! ,var ,(app parse val))
     `(set! ,var ,val)]
    [`(,(app parse f)
       ,(app parse arg*) ...)
     `(,f ,@arg*)]))

(define (eval-core expr env)
  (define (self expr)
    (eval-core expr env))
  (match expr
    [(? prim-datum? x) x]
    [(? symbol? x) (env-lookup x env)]
    [`(fn ,param* ,body)
     (Closure param* body env)]
    [`(local ([,var* ,(app self val*)] ...)
        ,body)
     (eval-local var* val* body env)]
    [`(begin ,(app self expr*) ... ,last-expr)
     (self last-expr)]
    [`(if ,(app self test-val)
          ,conseq-expr
          ,alt-expr)
     (if test-val
         (self conseq-expr)
         (self alt-expr))]
    [`(set! ,var ,(app self val))
     (env-set! var val env)]
    [`(,(app self f)
       ,(app self arg*) ...)
     (funcall f arg*)]
    ))


(define (sola-eval expr)
  (eval-core (parse expr) init-env))


;; closure

(struct Closure (param* body env))


;; aux functions

(define (prim-datum? x)
  (or (number? x)
      (boolean? x)
      (and (pair? x)
           (eq? 'quote (car x))
           (symbol? (second x)))))

(define (eval-local var* val* body env)
  (eval-core body
             (env-extend var* val* env)))

(define (funcall f arg*)
  (match f
    [(? procedure?) (apply f arg*)] ;; primitive
    [(Closure param* body env)
     (eval-local param* arg* body env)]))


;; environment

(define init-env
  `((+ . ,(box +))
    (- . ,(box -))
    (* . ,(box *))
    (/ . ,(box /))
    (> . ,(box >))
    (< . ,(box <))
    (= . ,(box =))
    (>= . ,(box >=))
    (<= . ,(box <=))
    (not . ,(box not))))

(define (env-lookup var env)
  (match (assq var env)
    [(cons var (box '(quote **unassigned**)))
     (error-reference-void-var 'env-lookup var)]
    [(cons var (box val)) val]
    [#f (error-unbound-identifier 'env-lookup var)]))

(define (env-set! var new-val env)
  (match (assq var env)
    [(cons var val-box) (set-box! val-box new-val)]
    [#f (error-unbound-identifier 'env-set! var)]))

(define (env-extend-single var val env)
  (cons (cons var (box val)) env))

(define (env-extend var* val* env)
  (foldl env-extend-single
         env
         var* val*))

;; error

(define (error-unbound-identifier from var)
  (error from "unbound identifier: ~s" var))

(define (error-reference-void-var from var)
  (error from "reference void variable: ~s" var))
