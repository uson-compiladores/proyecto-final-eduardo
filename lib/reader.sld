;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define-library (reader)
  (import (scheme base)
	  (scheme file)
	  (sets charset)
	  (regex)
	  (lexer)
	  (streams))
  (export read-file
	  lex
	  run-parser
	  syntax->expression
	  stream-from-file
	  LR-PARSER)
  (include "./reader/stream-from-file.scm")
  (include "./reader/r4rs-lexer.scm")
  (include "./reader/r4rs-parser.scm")
  (include "./reader/read-file.scm"))
