#lang racket

(provide (combine-out
          (struct-out Stx)
          (struct-out FilePos)
          sola-parse-from-s-expr
          sola-parse-from-port
          sola-parse-from-string
          sola-parse-from-file))


;; my syntax representation

(struct Stx (datum pos) #:transparent)
(struct FilePos (line col) #:transparent)

(define (racket-syntax->Stx r-stx)
  (define pos (FilePos (syntax-line r-stx)
                       (syntax-column r-stx)))
  (match (syntax-e r-stx)
    [(? list? s)
     (Stx (map racket-syntax->Stx s) pos)]
    [s  ;; raw value, not support vector etc.
     (Stx s pos)]))





;; complete parse function interfaces

(define (sola-parse-from-s-expr s-expr)
  (racket-syntax->Stx (datum->syntax #'() s-expr)))

(define (sola-parse-from-port port)
  (read-syntax (object-name port) port))

(define (sola-parse-from-string str)
  (define port (open-input-string str))
  (racket-syntax->Stx (sola-parse-from-port port)))


(define (read-file path)
  (define port (open-input-file path))
  (port-count-lines! port)
  (define stx (sola-parse-from-port port))
  (close-input-port port)
  stx)

(define (sola-parse-from-file path)
  (racket-syntax->Stx (read-file path)))

