;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define-library (compiler)
  (import (scheme base))
  (export compile)
  (include "./compiler/base.scm"))
