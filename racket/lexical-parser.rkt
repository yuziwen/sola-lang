#lang racket

(provide (struct-out Pos)
         pos->string
         (struct-out Lex)
         (struct-out Square)
         (struct-out Bracket)
         (struct-out Brace)
         (struct-out SelfEval)
         lex-parse-from-s-expr
         lex-parse-from-port
         lex-parse-from-file)



;; my syntax representation

(struct Pos (line col) #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (λ (pos port mode)
       (display (pos->string pos)
                port)))])

(define (pos->string pos)
  (cond
    [(not (Pos-line pos)) ;; do not have source info
     "#f"]
    [else (string-append (number->string (Pos-line pos))
                         ":"
                         (number->string (Pos-col pos)))]))


(struct Lex (pos datum) #:transparent)

(struct Square Lex () #:transparent)
(struct Bracket Lex () #:transparent)
(struct Brace Lex () #:transparent)

(struct SelfEval Lex () #:transparent)


(define (racket-syntax->Lex r-stx)
  (define pos (Pos (syntax-line r-stx)
                   (syntax-column r-stx)))
  (match (syntax-e r-stx)
    [(? list? s)
     (define (LexNode ctor)
       (ctor pos (map racket-syntax->Lex s)))

     (case (syntax-property r-stx 'paren-shape)
       [(#f) (LexNode Square)]
       [(#\[) (LexNode Bracket)]
       [(#\{) (LexNode Brace)])]

    [s (SelfEval pos s)]))



;; complete parse function interfaces

(define (lex-parse-from-s-expr s-expr)
  (racket-syntax->Lex (datum->syntax #'() s-expr)))

(define (lex-parse-from-port port)
  (read-syntax (object-name port) port))


(define (read-file path)
  (define port (open-input-file path))
  (port-count-lines! port)
  (define lex (lex-parse-from-port port))
  (close-input-port port)
  lex)

(define (lex-parse-from-file path)
  (racket-syntax->Lex (read-file path)))
