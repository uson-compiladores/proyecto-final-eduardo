;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define-library (variants)
  (import (scheme base))
  (export
   define-variant-type
   variant-case)
  (include "./variants/base.scm"))
