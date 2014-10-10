#lang scheme

(define reverse-pair
  (lambda (l)
    (cons (cdr l) (car l))))

(define member?
  (lambda (l1 l2)
    (cond ((null? l2)
           #f)
    ((equal? l1 (car l2))
             '#t)
     (else
      (member? l1 (cdr l2))))))

(define symmetric
  (lambda (l)
    (define helper
      (lambda (l1 l2)
     (cond((null? l2)
          '#T)
     ((null? l1)
           '#T)
     ((member? (reverse-pair (car l1)) l2)
      (helper (cdr l1) l2))
     (else 
      #F))))
    (helper l l)))






(define relation1 '((a . b) (z . z) (d . c) (c . d) (b . c) (c . b) (b . a)))
(define relation2 '((a . b) (c . d) (b . c) (c . b) (b . a)))
(define relation3 '((a . a) (b . a) (b . b) (b . b) (a . b) (a . b)))
(symmetric '())
(symmetric relation1) 
(symmetric relation2) 
(symmetric relation3)
;eval (reverse-pair '(a . b))