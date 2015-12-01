;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define-library (tester)
  (import (scheme base)
	  (scheme write))
  (export
   tester)
  (include "./tester/base.scm"))
