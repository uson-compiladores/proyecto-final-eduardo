;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define-library (virtual-machine)
  (import (scheme base)
	  (scheme write)
	  (compiler))
  (export run-vm
	  run-vm-debug
	  assign!)
  (include "./virtual-machine/base.scm"))
