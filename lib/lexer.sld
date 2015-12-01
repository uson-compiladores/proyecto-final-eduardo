;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define-library (lexer)
  (import (scheme base)
	  (scheme write)
	  (streams)
	  (regex))
  (export
   make-lexer
   define-lexer)
  (include "./lexer/base.scm"))
