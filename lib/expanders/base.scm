(define (transform-begin exp1 . rest)
  (if (null? rest)
      exp1
      `((lambda (asdf) ,(apply transform-begin rest)) ,exp1)))

(define (transform-let formals . exprs)
  `((lambda ,(map car formals)
      (begin ,@exprs))
    ,@(map cadr formals)))

(define (transform-rec var expr)
  `(let ((,var 0))
     (begin
       (set! ,var ,expr)
       ,var)))

(define (transform-recur name formals . exprs)
  `((rec ,name (lambda ,(map car formals)
		 (begin ,@exprs)))
    ,@(map cadr formals)))

(define (transform-and exp1 . rest)
  (if (null? rest)
      exp1
      `(if ,exp1
	   ,(apply transform-and rest)
	   #f)))

(define (transform-or exp1 . rest)
  (if (null? rest)
      exp1
      `(if ,exp1
	   #t
	   ,(apply transform-or rest))))

(define transf
  (list (cons 'begin transform-begin)
	(cons 'let   transform-let)
	(cons 'rec   transform-rec)
	(cons 'recur transform-recur)
	(cons 'and   transform-and)
	(cons 'or    transform-or)))

(define (expand-macros expr)
  (cond ((null? expr) '())
	((pair? expr)
	 (cond ((assq (car expr) transf) =>
		(lambda (tr) (expand-macros (apply (cdr tr) (cdr expr)))))
	       (else
		(map (lambda (x) (expand-macros x)) expr))))
	(else expr)))
