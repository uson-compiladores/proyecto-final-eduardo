;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define-syntax define-variant-type
  (syntax-rules ()
    ((_ type-name type-predicate-name)
     (syntax-error "At least one variant must be specified"))
    ((_ type-name type-predicate-name
	(variant-name (field-name field-predicate) ...)
	...)
     (begin
       (define (type-predicate-name variant)
	 (let ((variant-names '(variant-name ...)))
	   (and (pair? variant)
		(memq (car variant) variant-names)
		#true)))
       (define type-name
	 (cons (cons '(variant-name ...) '((variant-name field-name ...) ...))
	       type-predicate-name))
       (define variant-name
	 (let ((field-names '(field-name ...))
	       (numfields   (length '(field-name ...)))
	       (predicates  (list field-predicate ...)))
	   (lambda args
	     (unless (= (length args) numfields)
	       (error "Expected different number of arguments" numfields args))
	     (for-each
	      (lambda (arg field pred)
		(unless (pred arg)
		  (error "Field doesn't satisfy predicate" (cons field arg) pred)))
	      args field-names predicates)
	     (cons 'variant-name args))))
       ...))))

(define-syntax variant-case
  (syntax-rules ()
    ((_ type-name expression . clauses)
     (let ((is-type? (cdr type-name))
	   (variant expression))
       (if (is-type? variant)
	   (variant-case-helper variant . clauses)
	   (error "Expression doesn't satisfy type predicate"
		  (cons 'type-name variant) is-type?))))))

(define-syntax variant-case-helper
  (syntax-rules (else)
    ((_ variant (else body0 body1 ...))
     (begin body0 body1 ...))
    ((_ variant ((variant-name field-name ...) body0 body1 ...))
     (if (eq? (car variant) 'variant-name)
	 (apply (lambda (field-name ...) body0 body1 ...)
		(cdr variant))
	 (error "Variant isn't handled in cases form" (car variant))))
    ((_ variant ((variant-name field-name ...) body0 body1 ...) clause ...)
     (if (eq? (car variant) 'variant-name)
	 (apply (lambda (field-name ...) body0 body1 ...)
		(cdr variant))
	 (variant-case-helper variant clause ...)))
    ((_ other ...)
     (syntax-error "Malformed variant-case form"))))
