#lang racket

(provide (struct-out Pos)
         pos->string
         (struct-out Lex)
         (struct-out Square)
         (struct-out Bracket)
         (struct-out Brace)
         (struct-out SelfEval)
         lex-parse)



;; my syntax representation

(struct Pos (line col) #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (λ (pos port mode)
       (display (pos->string pos)
                port)))])

(define (pos->string pos)
  (cond
    [(number? pos) (number->string pos)]
    [else (string-append (number->string (Pos-line pos))
                         ":"
                         (number->string (Pos-col pos)))]))

;; pos: positive-integer or Pos struct
(struct Lex (pos datum) #:transparent)

(struct Square Lex () #:transparent)
(struct Bracket Lex () #:transparent)
(struct Brace Lex () #:transparent)

(struct SelfEval Lex () #:transparent)

;; lex-parse-racket-syntax :: syntax -> Lex
(define (lex-parse-racket-syntax r-stx)
  (define pos
    (if (syntax-line r-stx)
        (Pos (syntax-line r-stx)
             (syntax-column r-stx))
        (syntax-position r-stx)))
  (match (syntax-e r-stx)
    [(? list? s)
     (define (LexNode ctor)
       (ctor pos (map lex-parse-racket-syntax s)))

     (case (syntax-property r-stx 'paren-shape)
       [(#f) (LexNode Square)]
       [(#\[) (LexNode Bracket)]
       [(#\{) (LexNode Brace)])]

    [s (SelfEval pos s)]))


;; other parse function interfaces

(define (lex-parse-from-port port)
  (lex-parse-racket-syntax (read-syntax (object-name port) port)))


(define (read-file path)
  (define port (open-input-file path))
  (port-count-lines! port)
  (define lex (lex-parse-from-port port))
  (close-input-port port)
  lex)

(define (lex-parse-from-file path)
  (lex-parse-racket-syntax (read-file path)))

(define (lex-parse in)
  (cond
    [(syntax? in) (lex-parse-racket-syntax in)]
    [(path-string? in) (lex-parse-from-file in)]
    [(port? in) (lex-parse-from-port in)]
    [else (raise-argument-error
           'lex-parse "(or syntax? path-string? port?" in)]))
