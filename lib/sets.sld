;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define-library (sets)
  (export
   ;; set variant type and constructors
   <oset> oset? oset-empty oset

   ;; set selectors
   oset-size oset-member? oset-min oset=?

   ;; set collection operations
   oset-add oset-delete oset-delete-min

   ;; set processing
   oset-fold oset-filter oset-map

   ;; set conversions
   oset->list list->oset
   
   ;; set operations
   oset-union oset-difference oset-intersection)
  (import (scheme base)
	  (variants))
  (include "./sets/base.scm"))
