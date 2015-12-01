;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define (lt a b) (char<? a b))

(define (same? a b) (char=? a b))

(define (charset? x) (oset? x))

(define (charset-empty) (oset-empty))

(define (charset . args)
  (apply oset `(,lt ,@args)))

(define (charset-int-range a b)
  (if (and (integer? a) (integer? b) (>= b a))
      (let loop ((set (charset-empty))
		 (i a))
	(if (> i b) set (loop (charset-add set (integer->char i)) (+ i 1))))
      (error "charset-int-range must take two positive integers a and b, where a <= b" (cons a b))))

(define (charset-char-range a b)
  (if (and (char? a) (char? b) (char>=? b a))
      (charset-int-range (char->integer a) (char->integer b))
      (error "charset-char-range must take two characters a and b, where a char<=? b" (cons a b))))

(define (charset-int-ranges . ranges)
  (define (aux ranges)
    (cond ((null? ranges)
	   (charset-empty))
	  ((and (pair? (car ranges))
		(integer? (caar ranges))
		(integer? (cdar ranges))
		(>= (cdar ranges) (caar ranges)))
	   (let ((a (caar ranges)) (b (cdar ranges)))
	     (charset-union (charset-int-range a b) (aux (cdr ranges)))))
	  (else
	   (error "charset-int-ranges: ranges must be pairs of positive integers (a . b) where a <= b"
		  (car ranges)))))
  (aux ranges))

(define (charset-char-ranges . ranges)
  (define (char-ranges->int-ranges ranges)
    (cond ((null? ranges) '())
	  ((and (pair? (car ranges))
		(char? (caar ranges))
		(char? (cdar ranges))
		(char>=? (cdar ranges) (caar ranges)))
	   (let ((a (caar ranges)) (b (cdar ranges)))
	     (cons (cons (char->integer a) (char->integer b))
		   (char-ranges->int-ranges (cdr ranges)))))
	  (else
	   (error "charset-char-ranges: ranges must be pairs of characters (a . b) where a char<=? b"
		  (car ranges)))))
  (apply charset-int-ranges (char-ranges->int-ranges ranges)))

(define (charset-size set) (oset-size set))

(define (charset-member? c set)
  (oset-member? c set lt))

(define (charset-first set)
  (oset-min set))

(define (charset=? set1 set2)
  (oset=? set1 set2 lt same?))

(define (charset-add set c)
  (oset-add set c lt))

(define (charset-delete set c)
  (oset-delete set c lt))

(define (charset-rest set)
  (oset-delete-min set lt))

(define (charset-fold f base set)
  (oset-fold f base set))

(define (charset-filter pred set)
  (oset-filter pred set lt))

(define (charset-map f set)
  (oset-map f set lt))

(define (charset->list set)
  (oset->list set lt))

(define (list->charset lst)
  (list->oset lst lt))

(define (charset->string set)
  (list->string (charset->list set)))

(define (string->charset str)
  (list->charset (string->list str)))

(define (charset-union set1 set2)
  (oset-union set1 set2 lt))

(define (charset-difference set1 set2)
  (oset-difference set1 set2 lt))

(define (charset-intersection set1 set2)
  (oset-intersection set1 set2 lt))

(define charset-alphabet (charset-int-range 0 127))

(define (charset-complement set)
  (charset-difference charset-alphabet set))
