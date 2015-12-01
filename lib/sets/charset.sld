;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define-library (sets charset)
  (import (scheme base)
	  (scheme char)
	  (sets))
  (export
   charset? charset-empty charset charset-int-range charset-char-range charset-int-ranges charset-char-ranges
   charset-size charset-member? charset-first charset=?
   charset-add charset-delete charset-rest
   charset-fold charset-filter charset-map
   charset->list list->charset charset->string string->charset
   charset-union charset-difference charset-intersection charset-complement
   charset-alphabet)
  (include "./charset/base.scm"))
