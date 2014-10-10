;;; Coursework 2
;;; Question 1: successors.scm
;;; Name: Mohammad Ali Khan
;;; Email: mak1g11@soton.ac.uk

;;; Solution:

(define successors 
  (lambda (entity graph)
    (helper entity (filter graph))))

(define helper
  (lambda (e l)
    (cond((null? l) '())
    ((equal? (derivation-src (car l)) e)
     (cons (car l) (helper e (cdr l))))
    (else (helper e (cdr l))))))

(define filter
 (lambda (l)
  (cond ((null? l) '())
        ((equal? 'wasDerivedFrom (caar l))
        (cons (car l)
        (filter (cdr l))))
        (else (filter (cdr l))))))


