;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015


;;; 
;;; BEGIN UTILITY PROCEDURES
;;;

(define (natural? x) (and (integer? x) (not (negative? x))))

;;;
;;; END UTILITY PROCEDURES
;;;


(define-variant-type <regex> regex?
  (nil:)				; null regex
  (eps:)				; empty string
  (set: (cs charset?))			; character set
  (seq: (r1 regex?)			; sequence of regexps
	(r2 regex?))
  (alt: (r1 regex?)			; alternation of regexps
	(r2 regex?))
  (zom: (r  regex?))			; zero or more occurrences of a regexp (kleene closure)
  (and: (r1 regex?)			; and of regexps (like intersection)
	(r2 regex?))
  (not: (r  regex?)))			; complement of a regex


;;; smart constructors
(define (make-nil) (nil:))

(define (make-eps) (eps:))

(define (make-set cs) (set: cs))

(define (make-seq r1 r2)
  (variant-case <regex> r1
    ((nil:) (make-nil))
    ((eps:) r2)
    ((seq: r1a r1b) (make-seq r1a (make-seq r1b r2)))
    (else
     (variant-case <regex> r2
       ((nil:) (make-nil))
       ((eps:) r1)
       (else (seq: r1 r2))))))

(define (similar? r1 r2)
  (variant-case <regex> r1
    ((set: cs1)
     (variant-case <regex> r2
       ((set: cs2)
	(charset=? cs1 cs2))
       (else #false)))
    (else
     (variant-case <regex> r2
       ((set: cs2) #false)
       (else (equal? r1 r2))))))

(define (make-alt r1 r2)
  (if (similar? r1 r2)
      r1
      (variant-case <regex> r1
	((nil:) r2)
	((eps:)
	 (variant-case <regex> r2
	   ((nil:) r1)
	   ((zom: r2a) r2)
	   ((not: r2a)
	    (variant-case <regex> r2a
	      ((nil:) r2)
	      (else (alt: r1 r2))))
	   (else (alt: r1 r2))))
	((set: cs1)
	 (variant-case <regex> r2
	   ((nil:) r1)
	   ((set: cs2) (make-set (charset-union cs1 cs2)))
	   ((not: r2a)
	    (variant-case <regex> r2a
	      ((nil:) r2)
	      (else  (alt: r1 r2))))
	   (else (alt: r1 r2))))
	((alt: r1a r1b)
	 (variant-case <regex> r2
	   ((nil:) r1)
	   ((not: r2a)
	    (variant-case <regex> r2a
	      ((nil:) r2)
	      (else (make-alt r1a (make-alt r1b r2)))))
	   (else (make-alt r1a (make-alt r1b r2)))))
	((zom: r1a)
	 (variant-case <regex> r2
	   ((nil:) r1)
	   ((eps:) r1)
	   ((not: r2a)
	    (variant-case <regex> r2a
	      ((nil:) r2)
	      (else (alt: r1 r2))))
	   (else (alt: r1 r2))))
	((not: r1a)
	 (variant-case <regex> r1a
	   ((nil:) r1)
	   (else
	    (variant-case <regex> r2
	      ((nil:) r1)
	      ((not: r2a)
	       (variant-case <regex> r2a
		 ((nil:) r2)
		 (else (alt: r1 r2))))))))
	(else
	 (variant-case <regex> r2
	   ((nil:) r1)
	   ((not: r2a)
	    (variant-case <regex> r2a
	      ((nil:) r2)
	      (else (alt: r1 r2))))
	   (else (alt: r1 r2)))))))

(define (make-zom r)
  (variant-case <regex> r
    ((nil:) (make-eps))
    ((eps:) (make-eps))
    ((zom: s) r)
    (else (zom: r))))

(define (make-and r1 r2)
  (if (similar? r1 r2)
      r1
      (variant-case <regex> r1
	((nil:) (make-nil))
	((eps:)
	 (variant-case <regex> r2
	   ((nil:) (make-nil))
	   ((zom: r2a) r2)
	   ((not: r2a)
	    (variant-case <regex> r2a
	      ((nil:) r1)
	      (else (and: r1 r2))))
	   (else (and: r1 r2))))
	((set: cs1)
	 (variant-case <regex> r2
	   ((nil:) (make-nil))
	   ((set: cs2)
	    (let ((result (charset-intersection cs1 cs2)))
	      (if (zero? (charset-size result))
		  (make-nil)
		  (make-set result))))
	   ((not: r2a)
	    (variant-case <regex> r2a
	      ((nil:) r1)
	      (else  (and: r1 r2))))
	   (else (and: r1 r2))))
	((and: r1a r1b)
	 (variant-case <regex> r2
	   ((nil:) (make-nil))
	   ((not: r2a)
	    (variant-case <regex> r2a
	      ((nil:) r1)
	      (else (make-and r1a (make-and r1b r2)))))
	   (else (make-and r1a (make-and r1b r2)))))
	((zom: r1a)
	 (variant-case <regex> r2
	   ((nil:) (make-nil))
	   ((eps:) (make-eps))
	   ((not: r2a)
	    (variant-case <regex> r2a
	      ((nil:) r1)
	      (else (and: r1 r2))))
	   (else (and: r1 r2))))
	((not: r1a)
	 (variant-case <regex> r1a
	   ((nil:) r2)
	   (else
	    (variant-case <regex> r2
	      ((nil:) (make-nil))
	      ((not: r2a)
	       (variant-case <regex> r2a
		 ((nil:) r1)
		 (else (and: r1 r2))))))))
	(else
	 (variant-case <regex> r2
	   ((nil:) (make-nil))
	   ((not: r2a)
	    (variant-case <regex> r2a
	      ((nil:) r1)
	      (else (and: r1 r2))))
	   (else (and: r1 r2)))))))

(define (make-not r)
  (variant-case <regex> r
    ((nil:) (make-set charset-alphabet))
    ((set: cs) (make-set (charset-complement cs)))
    ((not: s) r)
    (else (not: r))))

(define (make-seqn r n)
  (if (<= n 0)
      (make-eps)
      (make-seq r (make-seqn r (- n 1)))))

(define (make-seqtn r n)
  (if (<= n 0)
      (make-eps)
      (make-alt (make-eps)
		(make-seq r
			  (make-seqtn r (- n 1))))))

(define (make-seqntm r n m)
  (if (and (>= n 0) (>= m n))
      (make-seq (make-seqn r n)
		(make-seqtn r (- m n)))
      (error "in making a sequence of n to m regular expressions: n has to be a non-negative integer and m >= n" (cons n m))))

(define (regex-nullable r)
  (variant-case <regex> r
    ((nil:) (make-nil))
    ((eps:) (make-eps))
    ((set: cs) (make-nil))
    ((seq: ra rb) (make-and (regex-nullable ra) (regex-nullable rb)))
    ((alt: ra rb) (make-alt (regex-nullable ra) (regex-nullable rb)))
    ((zom: s) (make-eps))
    ((and: ra rb) (make-and (regex-nullable ra) (regex-nullable rb)))
    ((not: s)
     (if (regex-nullable? s) (make-nil) (make-eps)))))

(define (regex-nullable? r)
  (variant-case <regex> (regex-nullable r)
    ((eps:) #true)
    ((nil:) #false)))

(define (regex-derive r c)
  (if (equal? c (make-eps))
      r
      (variant-case <regex> r
	((nil:) (make-nil))
	((eps:) (make-nil))
	((set: cs)
	 (if (charset-member? c cs)
	     (make-eps)
	     (make-nil)))
	((seq: ra rb)
	 (make-alt (make-seq (regex-derive ra c) rb)
		   (make-seq (regex-nullable ra) (regex-derive rb c))))
	((alt: ra rb)
	 (make-alt (regex-derive ra c)
		   (regex-derive rb c)))
	((zom: s)
	 (make-seq (regex-derive s c)
		   (make-zom s)))
	((and: ra rb)
	 (make-and (regex-derive ra c)
		   (regex-derive rb c)))
	((not: s)
	 (make-not (regex-derive s c))))))

(define-syntax rx
  (syntax-rules (eps nil seq alt opt zom oom nom ztn ntm and not)
    ((rx eps) (make-eps))
    ((rx nil) (make-nil))
    ((rx (seq r)) (rx r))
    ((rx (seq r1 r2 ...)) (make-seq (rx r1) (rx (seq r2 ...))))
    ((rx (alt r)) (rx r))
    ((rx (alt r1 r2 ...)) (make-alt (rx r1) (rx (alt r2 ...))))
    ((rx (zom r)) (make-zom (rx r)))
    ((rx (and r)) (rx r))
    ((rx (and r1 r2 ...)) (make-and (rx r1) (rx (and r2 ...))))
    ((rx (not r)) (make-not (rx r)))
    ((rx (opt r)) (make-alt (rx r) (make-eps)))
    ((rx (oom r)) (make-seq (rx r) (make-zom (rx r))))
    ((rx (nom n r)) (make-seq (make-seqn (rx r) n) (make-zom (rx r))))
    ((rx (ztn n r)) (make-seqtn (rx r) n))
    ((rx (ntm n m r)) (make-seqntm (rx r) n m))
    ((rx other) (regex-other other))))

(define (make-list-seq lst)
  (if (null? lst)
      (make-eps)
      (make-seq (regex-other (car lst)) (make-list-seq (cdr lst)))))

(define (regex-other obj)
  (cond ((charset? obj)
	 (make-set obj))
	((char? obj)
	 (make-set (charset obj)))
	((string? obj)
	 (make-list-seq (string->list obj)))
	(else obj)))

(define (regex->list r)
  (variant-case <regex> r
    ((nil:) '())
    ((eps:) 'eps)
    ((set: cs)
     (if (= 1 (charset-size cs))
	 (car (charset->list cs))
	 (cons 'alt (charset->list cs))))
    ((seq: ra rb)
     (variant-case <regex> rb
       ((seq: rba rbb) `(seq ,(regex->list ra) ,@(cdr (regex->list rb))))
       (else `(seq ,(regex->list ra) ,(regex->list rb)))))
    ((alt: ra rb)
     (variant-case <regex> rb
       ((alt: rba rbb) `(seq ,(regex->list ra) ,@(cdr (regex->list rb))))
       (else `(seq ,(regex->list ra) ,(regex->list rb)))))
    ((zom: s) (list '* (regex->list s)))
    ((and: ra rb)
     (variant-case <regex> rb
       ((and: rba rbb) `(and ,(regex->list ra) ,@(cdr (regex->list rb))))
       (else `(and ,(regex->list ra) ,(regex->list rb)))))
    ((not: s) (list 'not (regex->list s)))))


(define regex-match
  (case-lambda
   ((r obj) (regex-match r obj 0 -1))
   ((r obj n) (regex-match r obj n -1))
   ((r obj n m)
    (cond ((string? obj) (regex-match-string r obj n m))
	  ((stream? obj) (regex-match-stream r obj n m))
	  ((list? obj)   (regex-match-list r obj n m))
	  (else "regex-match doesn't support matching objects like" obj)))))

(define regex-match-list
  (case-lambda
   ((r lst) (regex-match-list r lst 0 -1))
   ((r lst n) (regex-match-list r lst n -1))
   ((r lst n m) (regex-match-list* r lst n m))))

(define regex-match-string
  (case-lambda
   ((r str) (regex-match-string r str 0 -1))
   ((r str n) (regex-match-string r str n -1))
   ((r str n m) (regex-match-string* r str n m))))

(define regex-match-stream
  (case-lambda
   ((r strm) (regex-match-stream r strm 0 -1))
   ((r strm n) (regex-match-stream r strm n -1))
   ((r strm n m) (regex-match-stream* r strm n m))))

(define (regex-match-list* r lst n m)
  (let loop ((index 0)
	     (regex r)
	     (input lst)
	     (accum '())
	     (rest  lst)
	     (match (if (regex-nullable? r) '() #false)))
    (cond ((or (= index m)
	       (equal? regex (make-nil))
	       (null? input))
	   (if match
	       (values (reverse match) rest)
	       (values #false lst)))
	  ((< index n)
	   (loop (+ index 1) regex (cdr input) accum rest match))
	  (else
	   (let* ((char (car input))
		  (regex* (regex-derive regex char)))
	     (if (regex-nullable? regex*)
		 (let ((accum* (cons char accum)))
		   (loop (+ index 1) regex* (cdr input) accum* (cdr input) accum*))
		 (loop (+ index 1) regex* (cdr input) (cons char accum) rest match)))))))

(define (regex-match-string* r str n m)
  (let-values (((match rest) (regex-match-list* r (string->list str) n m)))
    (values match (list->string rest))))

(define (regex-match-stream* r strm n m)
  (let loop ((index 0)
	     (regex r)
	     (input strm)
	     (accum '())
	     (rest  strm)
	     (match (if (regex-nullable? r) '() #false)))
    (cond ((or (= index m)
	       (equal? regex (make-nil))
	       (stream-null? input))
	   (if match
	       (values (reverse match) rest)
	       (values #false strm)))
	  ((< index n)
	   (loop (+ index 1) regex (stream-cdr input) accum rest match))
	  (else
	   (let* ((char (stream-car input))
		  (regex* (regex-derive regex char)))
	     (if (regex-nullable? regex*)
		 (let ((accum* (cons char accum)))
		   (loop (+ index 1) regex* (stream-cdr input) accum* (stream-cdr input) accum*))
		 (loop (+ index 1) regex* (stream-cdr input) (cons char accum) rest match)))))))

(define rx:ascii (rx charset-alphabet))
(define rx:any rx:ascii)

(define rx:nonl (rx (not (alt #\return #\newline))))

(define rx:lower-case (rx (charset-char-range #\a #\z)))
(define rx:lower rx:lower-case)

(define rx:upper-case (rx (charset-char-range #\A #\Z)))
(define rx:upper rx:upper-case)

(define rx:alphabetic (rx (alt rx:lower rx:upper)))
(define rx:alpha rx:alphabetic)

(define rx:numeric (rx (charset-char-range #\0 #\9)))
(define rx:num rx:numeric)

(define rx:alphanumeric (rx (alt rx:alpha rx:num)))
(define rx:alphanum rx:alphanumeric)
(define rx:alnum rx:alphanumeric)

(define rx:punctuation (rx (string->charset "!\"#%&'()*,-./:;?@[\\_{}")))
(define rx:punct rx:punctuation)

(define rx:symbol (rx (string->charset "$+<=>^`|~")))

(define rx:graphic (rx (alt rx:alnum rx:punct rx:symbol)))
(define rx:graph rx:graphic)

(define rx:whitespace (rx (alt (integer->char 32)
			       (integer->char 9)
			       (integer->char 10)
			       (integer->char 12)
			       (integer->char 13))))
(define rx:white rx:whitespace)
(define rx:space rx:whitespace)

(define rx:printing (rx (alt rx:graph rx:space)))
(define rx:print rx:printing)

(define rx:control (rx (charset-int-range 0 31)))
(define rx:cntrl rx:control)

(define rx:hex-digit (rx (alt rx:num
			      (charset-char-ranges '(#\a . #\f) '(#\A . #\F)))))
(define rx:xdigit rx:hex-digit)
