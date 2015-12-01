;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define-syntax define-lexer
  (syntax-rules (rules delimiters atmosphere)
    ((_ name
	(rules (regex proc) ...)
	(delimiters c0 ...)
	(atmosphere atm-regex))
     (define name (make-lexer (list (cons regex proc) ...)
			      (list c0 ...)
			      atm-regex)))))

(define (make-lexer rules delims atmosphere)
  (define (iter input)
    (if (stream-null? input)
	stream-null
	(let-values (((token rest) (next-token rules delims atmosphere input)))
	  (if (stream-null? token)
	      stream-null
	      (stream-cons token (iter rest))))))
  iter)

(define (next-token rules delims atmosphere input)
  (if (empty? input)
      (values stream-null stream-null)
      (let ((c  (input-read input))
	    (cs (input-rest input)))
	(if (could-match? c atmosphere)
	    (next-token rules delims atmosphere (consume input atmosphere))
	    (let-values (((rule match rest) (try-rules rules input)))
	      (if (rule? rule)
		  (if (delimited? delims match rest)
		      (values (make-token rule match) rest)
		      (error "missing delimiter"))
		  (error "can't recognize input")))))))

(define (empty? x) (stream-null? x))

(define (input-read x) (stream-car x))

(define (input-rest x) (stream-cdr x))

(define (could-match? c regex)
  (let-values (((match rest) (regex-match regex (list c))))
    (if match #true #false)))

(define (consume input regex)
  (let-values (((match rest) (regex-match regex input)))
    rest))

(define (try-rules rules input)
  (define (iter rules rule* match* rest*)
    (if (null? rules)
	(values rule* match* rest*)
	(let* ((rule  (car rules))
	       (regex (rule-regex rule)))
	  (let-values (((match rest) (regex-match regex input)))
	    (cond ((not match) (iter (cdr rules) rule* match* rest*))
		  ((> (match-length match)
		      (match-length match*))
		   (iter (cdr rules) rule match rest))
		  (else
		   (iter (cdr rules) rule* match* rest*)))))))
  (iter rules #false #false #false))

(define (rule? x)
  (and (pair? x)
       (regex? (car x))
       (procedure? (cdr x))))

(define (make-token rule match)
  ((rule-proc rule) (list->string match)))

(define (rule-regex rule)
  (car rule))

(define (rule-proc rule)
  (cdr rule))

(define (match-length x)
  (if x (length x) -1))

(define (delimited? delims match rest)
  (or (and (not (null? match))
	   (member (car (reverse match)) delims))
      (and (not (empty? rest))
	   (member (input-read rest) delims))
      (empty? rest)))
