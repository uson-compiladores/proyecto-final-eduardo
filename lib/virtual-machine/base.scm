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

(define env
  (list (cons (list 'cons 'car 'cdr 'null? '+ '- 'zero? '*)
	      (list  cons  car  cdr  null?  +  -  zero?  *))))

(define (vm a x e r s step debug?)
  (when debug?
    (display "VM > step ") (display step) (display " registers:\n")
    (display "  a = ") (display a) (newline)
    (display "  x = ") (display x) (newline)
    (display "  e = ") (display e) (newline)
    (display "  r = ") (display r) (newline)
    (display "  s = ") (display s) (newline))
  (record-case x
    (:halt: () a)
    (:refer: (var x)
	     (let ((val (lookup var e)))
	       (if val
		   (vm (car val) x e r s (+ step 1) debug?)
		   (error "variable not bound" var))))
    (:constant: (obj x)
		(vm obj x e r s (+ step 1) debug?))
    (:close: (vars body x)
	     (vm (closure body e vars) x e r s (+ step 1) debug?))
    (:test: (then else)
	    (vm a (if a then else) e r s (+ step 1) debug?))
    (:assign: (var x)
	      (assign! var a e)
	      (vm a x e r s (+ step 1) debug?))
    (:conti: (x)
	     (vm (continuation s) x e r s))
    (:nuate: (s var)
	     (let ((val (lookup var e)))
	       (if val
		   (vm (car val) '(:return:) e r s (+ step 1) debug?)
		   (error "variable not bound" var))))
    (:frame: (ret x)
	     (vm a x e '() (call-frame ret e r s) (+ step 1) debug?))
    (:argument: (x)
		(vm a x e (cons a r) s (+ step 1) debug?))
    (:apply: ()
	     (if (procedure? a)
		 (let ((result (apply a r)))
		   (vm result '(:return:) e '() s (+ step 1) debug?))
		 (record (body e vars) a
		   (vm a body (extend e vars r) '() s (+ step 1) debug?))))
    (:return: ()
	      (record (x e r s) s
		(vm a x e r s (+ step 1) debug?)))
    (else (error "unknown instruction" (car x)))))

(define (assign! var val env)
  (let ((old-val (lookup var env)))
    (if old-val
	(set-car! old-val val)
	(begin
	  (set-car! (car env) (cons var (car (car env))))
	  (set-cdr! (car env) (cons val (cdr (car env))))))))

(define (lookup var env)
  (let next-rib ((env env))
    (if (null? env)
	#f
	(let next-element ((vars (caar env))
			   (vals (cdar env)))
	  (cond ((null? vars)
		 (next-rib (cdr env)))
		((eq? (car vars) var)
		 vals)
		(else
		 (next-element (cdr vars) (cdr vals))))))))


(define (closure body e vars)
  (list body e vars))

(define (continuation s)
  (closure (list ':nuate: s 'v) '() '(v)))

(define (call-frame x e r s)
  (list x e r s))

(define (extend e vars vals)
  (cons (cons vars vals) e))

(define (run-vm expr)
  (vm '() (compile expr) env '() '() 0 #false))

(define (run-vm-debug expr)
  (display "VM > input expression is:\n")
  (display expr) (newline) (newline)
  (let ((compiled (compile expr)))
    (display "VM > compiled expression is:\n")
    (display compiled) (newline) (newline)
    (vm '() compiled env '() '() 0 #true)))
