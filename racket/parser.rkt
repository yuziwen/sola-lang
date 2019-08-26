#lang racket

(provide (combine-out
          (struct-out Parsed)
          (struct-out FilePos)
          sola-parse-from-s-expr
          sola-parse-from-port
          sola-parse-from-string
          sola-parse-from-file))


;; my syntax representation

(struct Parsed (pos datum) #:transparent)
(struct FilePos (line col) #:transparent)

(define (racket-syntax->Parsed r-stx)
  (define pos (FilePos (syntax-line r-stx)
                       (syntax-column r-stx)))
  (match (syntax-e r-stx)
    [(? list? s)
     (Parsed pos (map racket-syntax->Parsed s))]
    [s  ;; raw value, not support vector etc.
     (Parsed pos s)]))





;; complete parse function interfaces

(define (sola-parse-from-s-expr s-expr)
  (racket-syntax->Parsed (datum->syntax #'() s-expr)))

(define (sola-parse-from-port port)
  (read-syntax (object-name port) port))

(define (sola-parse-from-string str)
  (define port (open-input-string str))
  (racket-syntax->Parsed (sola-parse-from-port port)))


(define (read-file path)
  (define port (open-input-file path))
  (port-count-lines! port)
  (define parsed (sola-parse-from-port port))
  (close-input-port port)
  parsed)

(define (sola-parse-from-file path)
  (racket-syntax->Parsed (read-file path)))

