(define-syntax record
  (syntax-rules ()
    ((record (var ...) val exp ...)
     (apply (lambda (var ...) exp ...) val))))

(define-syntax record-case
  (syntax-rules ()
    ((record-case exp1 (key vars exp2 ...) ... (else exp3 ...))
     (let ((r exp1))
       (cond ((eq? (car r) (quote key))
	      (record vars (cdr r) exp2 ...))
	     ...
	     (else exp3 ...))))))

(define (compile* x next)
  (cond ((symbol? x)
	 (list ':refer: x next))
	((pair? x)
	 (record-case x
	  (quote (obj) (list ':constant: obj next))
	  (lambda (vars body)
	          (list ':close: vars (compile* body '(:return:)) next))
	  (if (test then else)
	      (let ((thenc (compile* then next))
		    (elsec (compile* else next)))
		(compile* test (list ':test: thenc elsec))))
	  (set! (var x)
		(compile* x (list ':assign: var next)))
	  (call/cc (x)
		   (let ((c (list ':conti:
				  (list ':argument:
					(compile* x '(:apply:))))))
		     (if (tail? next)
			 c
			 (list ':frame: next c))))
	  (else
	   (let loop ((args (cdr x))
		      (c (compile* (car x) '(:apply:))))
	     (if (null? args)
		 (if (tail? next)
		     c
		     (list ':frame: next c))
		 (loop (cdr args)
		       (compile* (car args)
			 	 (list ':argument: c))))))))
	(else
	 (list ':constant: x next))))

(define (tail? next)
  (eq? (car next) ':return:))

(define (compile x)
  (compile* x '(:halt:)))
