(define (read-file path)
  (define raw-input (stream-from-file path))
  (syntax->expression (run-parser (lex raw-input))))
