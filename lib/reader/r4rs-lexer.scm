(define rx:comment (rx (seq #\; (zom (not #\newline)) #\newline)))
(define rx:whitespace* (rx (alt #\space #\newline)))
(define rx:atmosphere (rx (alt rx:whitespace* rx:comment)))

(define rx:peculiar-identifier (rx (alt #\+ #\- "...")))
(define rx:special-subsequent (rx (charset #\+ #\- #\.)))
(define rx:special-initial (rx (charset #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\~ #\_ #\^)))
(define rx:initial (rx (alt rx:alpha rx:special-initial)))
(define rx:subsequent (rx (alt rx:initial rx:num rx:special-subsequent)))
(define rx:identifier (rx (alt (seq rx:initial (zom rx:subsequent)) rx:peculiar-identifier)))
(define rx:sign (rx (opt (alt #\+ #\-))))
(define rx:number (rx (seq rx:sign rx:num (zom rx:num))))
(define rx:boolean (rx (alt "#t" "#f")))
(define rx:character-name (rx (alt "space" "newline")))
(define rx:character (rx (seq #\# #\\ (alt rx:character-name rx:any))))

(define rx:string-element (rx (alt (not (charset #\" #\\))
				   (seq #\\ #\")
				   (seq #\\ #\\))))
(define rx:string (rx (seq #\" (zom rx:string-element) #\")))

(define (strip-escape lst)
  (cond ((null? lst) '())
	((char=? (car lst) #\\) (cons #\\ (strip-escape (cdr (cdr lst)))))
	(else (cons (car lst) (strip-escape (cdr lst))))))

(define rules
  (let ((const (lambda (x) (lambda (y) (cons x '()))))
	(lex:id (lambda (y) (list 'identifier (string->symbol y))))
	(lex:bool (lambda (y) (list 'boolean (if (char=? #\t (string-ref y 1)) #t #f))))
	(lex:num (lambda (y) (list 'number (string->number y))))
	(lex:char (lambda (y) (list 'character (if (> (string-length y) 3)
						   (cond ((equal? y "#\\space") #\space)
							 ((equal? y "#\\newline") #\newline))
						   (string-ref y 2)))))
	(lex:str (lambda (y) (list 'string
				   (list->string (strip-escape
						  (string->list (substring y 1 (- (string-length y) 1)))))))))
    (list
     (cons (rx "(")        (const 'lparen))
     (cons (rx ")")        (const 'rparen))
     (cons (rx "#(")       (const 'vparen))
     (cons (rx "'")        (const 'quote))
     (cons (rx "`")        (const 'quasiquote))
     (cons (rx ",")        (const 'unquote))
     (cons (rx ",@")       (const 'unquote-splicing))
     (cons (rx ".")        (const 'dot))
     (cons rx:identifier   lex:id)
     (cons rx:boolean      lex:bool)
     (cons rx:number       lex:num)
     (cons rx:character    lex:char)
     (cons rx:string       lex:str))))

(define delims (list #\space #\newline #\( #\) #\" #\;))

(define atmosphere rx:atmosphere)

(define lex (make-lexer rules delims atmosphere))
