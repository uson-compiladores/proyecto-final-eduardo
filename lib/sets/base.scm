;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

;;; 
;;; BEGIN UTILITY PROCEDURES
;;;

;;; if the procedure call is not an error, the argument is a scheme object
(define (object? x) #t)

;;;
;;; END UTILITY PROCEDURES
;;; 

(define-variant-type <oset> oset?
  (oset-empty)
  (oset-node (value object?)
	     (count integer?)
	     (left  oset?)
	     (right oset?)))

(define (oset-size set)
  (variant-case <oset> set
    ((oset-empty)
     0)
    ((oset-node value count left right)
     count)))

(define (oset-node* value left right)
  (oset-node value (+ 1 (oset-size left) (oset-size right)) left right))

(define (oset-member? x set lt)
  (variant-case <oset> set
    ((oset-empty) #false)
    ((oset-node v c l r)
     (cond ((lt x v) (oset-member? x l lt))
	   ((lt v x) (oset-member? x r lt))
	   (else    #true)))))

(define (oset-min set)
  (variant-case <oset> set
    ((oset-empty) (error "The ordered set is empty"))
    ((oset-node v c l r)
     (variant-case <oset> l
       ((oset-empty) v)
       (else         (oset-min l))))))

(define (oset=? set1 set2 lt same?)
  (variant-case <oset> set1
    ((oset-empty)
     (variant-case <oset> set2
       ((oset-empty) #true)
       (else         #false)))
    (else
     (variant-case <oset> set2
       ((oset-empty) #false)
       (else
	(let ((m1 (oset-min set1))
	      (m2 (oset-min set2)))
	  (if (same? m1 m2)
	      (oset=? (oset-delete-min set1 lt)
		      (oset-delete-min set2 lt) lt same?)
	      #false)))))))

(define (oset-cons value left right lt)
  (let ((ln (oset-size left))
	(rn (oset-size right)))
    (cond ((< (+ ln rn) 2)
	   (oset-node* value left right))
	  ((> rn (* 4 ln))
	   (variant-case <oset> right
	     ((oset-node x y rl rr)
	      (let ((rln (oset-size rl))
		    (rrn (oset-size rr)))
		(if (< rln rrn)
		    (single-left-rotation value left right)
		    (double-left-rotation value left right))))))
	  ((> ln (* 4 rn))
	   (variant-case <oset> left
	     ((oset-node x y ll lr)
	      (let ((lln (oset-size ll))
		    (lrn (oset-size lr)))
		(if (< lrn lln)
		    (single-right-rotation value left right)
		    (double-right-rotation value left right))))))
	  (else
	   (oset-node* value left right)))))

(define (single-left-rotation value left right)
  (variant-case <oset> right
    ((oset-node right-value _ right-left right-right)
     (oset-node* right-value
		 (oset-node* value left right-left)
		 right-right))))

(define (double-left-rotation value left right)
  (variant-case <oset> right
    ((oset-node right-value _ right-left right-right)
     (variant-case <oset> right-left
       ((oset-node right-left-value _ right-left-left right-left-right)
	(oset-node* right-left-value
		    (oset-node* value left right-left-left)
		    (oset-node* right-value right-left-right right-right)))))))

(define (single-right-rotation value left right)
  (variant-case <oset> left
    ((oset-node left-value _ left-left left-right)
     (oset-node* left-value
		 left-left
		 (oset-node* value left-right right)))))

(define (double-right-rotation value left right)
  (variant-case <oset> left
    ((oset-node left-value _ left-left left-right)
     (variant-case <oset> left-right
       ((oset-node left-right-value _ left-right-left left-right-right)
	(oset-node* left-right-value
		    (oset-node* left-value left-left left-right-left)
		    (oset-node* value left-right-right right)))))))

(define (oset-add set x lt)
  (variant-case <oset> set
    ((oset-empty)        (oset-node* x (oset-empty) (oset-empty)))
    ((oset-node value _ left right)
     (cond ((lt x value) (oset-cons value (oset-add left x lt) right lt))
	   ((lt value x) (oset-cons value left (oset-add right x lt) lt))
	   (else         (oset-node* x left right))))))

(define (oset-delete set x lt)
  (variant-case <oset> set
    ((oset-empty) (oset-empty))
    ((oset-node value _ left right)
     (cond ((lt x value) (oset-cons value (oset-delete left x lt) right lt))
	   ((lt value x) (oset-cons value left (oset-delete right x lt) lt))
	   (else         (oset-delete-aux left right lt))))))

(define (oset-delete-aux left right lt)
  (variant-case <oset> left
    ((oset-empty) right)
    (else
     (variant-case <oset> right
       ((oset-empty) left)
       (else         (oset-cons (oset-min right) left (oset-delete-min right lt) lt))))))

(define (oset-delete-min set lt)
  (variant-case <oset> set
    ((oset-empty) (error "The set is empty"))
    ((oset-node value _ left right)
     (variant-case <oset> left
       ((oset-empty) right)
       ((oset-node w x y z)
	(oset-cons value (oset-delete-min left lt) right lt))))))

(define (oset-fold f base set)
  (variant-case <oset> set
    ((oset-empty) base)
    ((oset-node value _ left right)
     (oset-fold f (f value (oset-fold f base right)) left))))

(define (oset-filter pred set lt)
  (let loop ((set1 (oset-empty))
	     (set2 set))
    (variant-case <oset> set2
      ((oset-empty) set1)
      (else
       (let ((m (oset-min set2)))
	 (if (pred m)
	     (loop (oset-add set1 m lt) (oset-delete-min set2 lt))
	     (loop set1 (oset-delete-min set2 lt))))))))

(define (oset-map f set lt)
  (let loop ((set1 (oset-empty))
	     (set2 set))
    (variant-case <oset> set2
      ((oset-empty) set1)
      (else
       (let ((m (oset-min set2)))
	 (loop (oset-add set1 (f m) lt)
	       (oset-delete-min set2 lt)))))))

(define (oset->list set lt)
  (variant-case <oset> set
    ((oset-empty) '())
    (else (cons (oset-min set) (oset->list (oset-delete-min set lt) lt)))))

(define (list->oset lst lt)
  (if (null? lst)
      (oset-empty)
      (oset-add (list->oset (cdr lst) lt) (car lst) lt)))

(define (oset-union set1 set2 lt)
  (oset-fold (lambda (elm elms) (oset-add elms elm lt)) set1 set2))

(define (oset-difference set1 set2 lt)
  (oset-fold (lambda (elm elms) (oset-delete elms elm lt)) set1 set2))

(define (oset-intersection set1 set2 lt)
  (oset-difference set2 (oset-difference set2 set1 lt) lt))

(define (oset lt . args)
  (list->oset args lt))
