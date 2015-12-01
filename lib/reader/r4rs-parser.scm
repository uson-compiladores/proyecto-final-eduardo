(define GRAMMAR
  (vector '(:Start         . (:Datum))
	  '(:Datum         . (:SimpleDatum))
	  '(:Datum         . (:CompoundDatum))
	  '(:SimpleDatum   . (boolean))
	  '(:SimpleDatum   . (number))
	  '(:SimpleDatum   . (character))
	  '(:SimpleDatum   . (string))
	  '(:SimpleDatum   . (:Symbol))
	  '(:Symbol        . (identifier))
	  '(:CompoundDatum . (:List))
	  '(:CompoundDatum . (:Vector))
	  '(:List          . (lparen :ProperList))
	  '(:List          . (lparen :ImproperList))
	  '(:List          . (:Abbreviation))
	  '(:ProperList    . (rparen))
	  '(:ProperList    . (:Datum :ProperList))
	  '(:ImproperList  . (:Datum dot :Datum rparen))
	  '(:ImproperList  . (:Datum :ImproperList))
	  '(:Abbreviation  . (:AbbrevPrefix :Datum))
	  '(:AbbrevPrefix  . (quote))
	  '(:AbbrevPrefix  . (quasiquote))
	  '(:AbbrevPrefix  . (unquote))
	  '(:AbbrevPrefix  . (unquote-splicing))
	  '(:Vector        . (vparen :ProperList))))


(define (s n) (cons 'shift n))
(define (r n) (cons 'reduce n))

(define SLR-TABLE
  ;;;;;;;;;;;;;;;  bool   num    char   str    id      (      )       .     '      `      ,      ,@    #(      $          D      S      X      C      L      D*     D+     A      P      V
  (vector (vector (s 4)  (s 5)  (s 6)  (s 7)  (s 9)  (s 12) 'error 'error (s 15) (s 16) (s 17) (s 18) (s 19) 'error       1      2      8      3      10   'error 'error   13     14     11   ) ; 0
	  (vector 'error 'error 'error 'error 'error 'error 'error 'error 'error 'error 'error 'error 'error 'acc       'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 1
	  (vector (r 1)  (r 1)  (r 1)  (r 1)  (r 1)  (r 1)  (r 1)  (r 1)  (r 1)  (r 1)  (r 1)  (r 1)  (r 1)  (r 1)      'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 2
	  (vector (r 2)  (r 2)  (r 2)  (r 2)  (r 2)  (r 2)  (r 2)  (r 2)  (r 2)  (r 2)  (r 2)  (r 2)  (r 2)  (r 2)      'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 3
	  (vector (r 3)  (r 3)  (r 3)  (r 3)  (r 3)  (r 3)  (r 3)  (r 3)  (r 3)  (r 3)  (r 3)  (r 3)  (r 3)  (r 3)      'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 4
	  (vector (r 4)  (r 4)  (r 4)  (r 4)  (r 4)  (r 4)  (r 4)  (r 4)  (r 4)  (r 4)  (r 4)  (r 4)  (r 4)  (r 4)      'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 5
	  (vector (r 5)  (r 5)  (r 5)  (r 5)  (r 5)  (r 5)  (r 5)  (r 5)  (r 5)  (r 5)  (r 5)  (r 5)  (r 5)  (r 5)      'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 6
	  (vector (r 6)  (r 6)  (r 6)  (r 6)  (r 6)  (r 6)  (r 6)  (r 6)  (r 6)  (r 6)  (r 6)  (r 6)  (r 6)  (r 6)      'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 7
	  (vector (r 7)  (r 7)  (r 7)  (r 7)  (r 7)  (r 7)  (r 7)  (r 7)  (r 7)  (r 7)  (r 7)  (r 7)  (r 7)  (r 7)      'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 8
	  (vector (r 8)  (r 8)  (r 8)  (r 8)  (r 8)  (r 8)  (r 8)  (r 8)  (r 8)  (r 8)  (r 8)  (r 8)  (r 8)  (r 8)      'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 9
	  (vector (r 9)  (r 9)  (r 9)  (r 9)  (r 9)  (r 9)  (r 9)  (r 9)  (r 9)  (r 9)  (r 9)  (r 9)  (r 9)  (r 9)      'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 10
	  (vector (r 10) (r 10) (r 10) (r 10) (r 10) (r 10) (r 10) (r 10) (r 10) (r 10) (r 10) (r 10) (r 10) (r 10)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 11
	  (vector (s 4)  (s 5)  (s 6)  (s 7)  (s 9)  (s 12) (s 22) 'error (s 15) (s 16) (s 17) (s 18) (s 19) 'error       23     2      8      3      10     20     21     13     14     11   ) ; 12
	  (vector (r 13) (r 13) (r 13) (r 13) (r 13) (r 13) (r 13) (r 13) (r 13) (r 13) (r 13) (r 13) (r 13) (r 13)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 13
	  (vector (s 4)  (s 5)  (s 6)  (s 7)  (s 9)  (s 12) 'error 'error (s 15) (s 16) (s 17) (s 18) (s 19) 'error       24     2      8      3      10   'error 'error   13     14     11   ) ; 14
	  (vector (r 19) (r 19) (r 19) (r 19) (r 19) (r 19) 'error 'error (r 19) (r 19) (r 19) (r 19) (r 19) (r 19)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 15
	  (vector (r 20) (r 20) (r 20) (r 20) (r 20) (r 20) 'error 'error (r 20) (r 20) (r 20) (r 20) (r 20) (r 20)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 16
	  (vector (r 21) (r 21) (r 21) (r 21) (r 21) (r 21) 'error 'error (r 21) (r 21) (r 21) (r 21) (r 21) (r 21)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 17
	  (vector (r 22) (r 22) (r 22) (r 22) (r 22) (r 22) 'error 'error (r 22) (r 22) (r 22) (r 22) (r 22) (r 22)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 18
	  (vector (s 4)  (s 5)  (s 6)  (s 7)  (s 9)  (s 12) (s 22) 'error (s 15) (s 16) (s 17) (s 18) (s 19) 'error       31     2      8      3      10     25   'error   13     14     11   ) ; 19
	  (vector (r 11) (r 11) (r 11) (r 11) (r 11) (r 11) (r 11) (r 11) (r 11) (r 11) (r 11) (r 11) (r 11) (r 11)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 20
	  (vector (r 12) (r 12) (r 12) (r 12) (r 12) (r 12) (r 12) (r 12) (r 12) (r 12) (r 12) (r 12) (r 12) (r 12)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 21
	  (vector (r 14) (r 14) (r 14) (r 14) (r 14) (r 14) (r 14) (r 14) (r 14) (r 14) (r 14) (r 14) (r 14) (r 14)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 22
	  (vector (s 4)  (s 5)  (s 6)  (s 7)  (s 9)  (s 12) (s 22) (s 27) (s 15) (s 16) (s 17) (s 18) (s 19) 'error       23     2      8      3      10     26     28     13     14     11   ) ; 23
	  (vector (r 18) (r 18) (r 18) (r 18) (r 18) (r 18) (r 18) (r 18) (r 18) (r 18) (r 18) (r 18) (r 18) (r 18)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 24
	  (vector (r 23) (r 23) (r 23) (r 23) (r 23) (r 23) (r 23) (r 23) (r 23) (r 23) (r 23) (r 23) (r 23) (r 23)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 25
	  (vector (r 15) (r 15) (r 15) (r 15) (r 15) (r 15) (r 15) (r 15) (r 15) (r 15) (r 15) (r 15) (r 15) (r 15)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 26
	  (vector (s 4)  (s 5)  (s 6)  (s 7)  (s 9)  (s 12) 'error 'error (s 15) (s 16) (s 17) (s 18) (s 19) 'error       29     2      8      3      10   'error 'error   13     14     11   ) ; 27
	  (vector (r 17) (r 17) (r 17) (r 17) (r 17) (r 17) (r 17) (r 17) (r 17) (r 17) (r 17) (r 17) (r 17) (r 17)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 28
	  (vector 'error 'error 'error 'error 'error 'error (s 30) 'error 'error 'error 'error 'error 'error 'error     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 29
	  (vector (r 16) (r 16) (r 16) (r 16) (r 16) (r 16) (r 16) (r 16) (r 16) (r 16) (r 16) (r 16) (r 16) (r 16)     'error 'error 'error 'error 'error 'error 'error 'error 'error 'error ) ; 30
	  (vector (s 4)  (s 5)  (s 6)  (s 7)  (s 9)  (s 12) (s 22) 'error (s 15) (s 16) (s 17) (s 18) (s 19) 'error       31     2      8      3      10     26   'error   13     14     11   ) ; 31
	  ))

(define sym:col
  '((boolean . 0) (number . 1) (character . 2) (string . 3) (identifier . 4) (lparen . 5) (rparen . 6) (dot . 7) (quote . 8) (quasiquote . 9) (unquote . 10) (unquote-splicing . 11) (vparen . 12) ($ . 13)
    (:Datum . 14) (:SimpleDatum . 15) (:Symbol . 16) (:CompoundDatum . 17) (:List . 18) (:ProperList . 19) (:ImproperList . 20) (:Abbreviation . 21) (:AbbrevPrefix . 22) (:Vector . 23)))

(define (ref state sym)
  (vector-ref (vector-ref SLR-TABLE state)
	      (cond ((assq sym sym:col) => cdr)
		    (else (error "missing table entry")))))

(define (drop n lst)
  (if (zero? n)
      lst
      (drop (- n 1) (cdr lst))))

(define (take n lst)
  (if (zero? n)
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (LR-PARSER input stack i gold)
  (let ((tok (if (stream-null? input) '($) (stream-car input)))
	(tos (car stack)))
    (let ((entry (ref tos (car tok))))
      (if (pair? entry)
	  (if (eq? (car entry) 'shift)
	      (LR-PARSER (stream-cdr input) (cons (cdr entry) stack) (+ i 1) (cons tok gold))
	      (let* ((prod   (vector-ref GRAMMAR (cdr entry)))
		     (var    (car prod))
		     (n      (length (cdr prod)))
		     (stack* (drop n stack))
		     (gold+  (take n gold))
		     (gold*  (drop n gold)))
		(LR-PARSER input (cons (ref (car stack*) var) stack*) (+ i 1) (cons (cons var (reverse gold+)) gold*))))
	  (if (eq? entry 'acc)
	      (car gold)
	      (error "something very very wrong happened :S"))))))

(define (run-parser input)
  (LR-PARSER input '(0) 0 '()))

(define (syntax->expression stx)
  (define (value-of var)
    (case (car var)
      ((boolean number character string identifier)
       (list-ref var 1))
      ((:Datum :SimpleDatum :Symbol :CompoundDatum)
       (value-of (list-ref var 1)))
      ((:List)
       (value-of (if (eq? (car (list-ref var 1)) 'lparen)
		     (list-ref var 2)
		     (list-ref var 1))))
      ((:Vector)
       (list->vector (value-of (list-ref var 2))))
      ((:ProperList)
       (if (= (length (cdr var)) 1)
	   '()
	   (cons (value-of (list-ref var 1))
		 (value-of (list-ref var 2)))))
      ((:ImproperList)
       (if (= (length (cdr var)) 4)
	   (cons (value-of (list-ref var 1))
		 (value-of (list-ref var 3)))
	   (cons (value-of (list-ref var 1))
		 (value-of (list-ref var 2)))))
      ((:AbbrevPrefix) (car (list-ref var 1)))
      ((:Abbreviation) (list (value-of (list-ref var 1)) (value-of (list-ref var 2))))))
  (value-of stx))
