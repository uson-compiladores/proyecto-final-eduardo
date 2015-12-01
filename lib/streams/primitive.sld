;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define-library (streams primitive)
  (import (scheme base)
	  (scheme lazy))
  (export stream-null
	  stream-cons
	  stream?
	  stream-null?
	  stream-pair?
	  stream-car
	  stream-cdr
	  stream-lambda)
  (include "./primitive.scm"))
