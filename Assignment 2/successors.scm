;;; Coursework 2
;;; Question 1: successors.scm
;;; Name: Mohammad Ali Khan
;;; Email: mak1g11@soton.ac.uk

;;; Solution:

(define derivation-src caddr)

(define successors 
  (lambda (entity graph)
    (find-successors entity (filter graph))))

(define find-successors
  (lambda (e l)
    (cond((null? l) '())
    ((equal? (derivation-src (car l)) e)
     (cons (car l) (find-successors e (cdr l))))
    (else (find-successors e (cdr l))))))

(define filter
 (lambda (l)
  (cond ((null? l) '())
        ((equal? 'wasDerivedFrom (caar l))
        (cons (car l)
        (filter (cdr l))))
        (else (filter (cdr l))))))

;;; In my solution, I am using two different 
;;; functions. One of them filters a list and
;;; gives me all the statements in the form of
;;; a list. The other function goes through it
;;; and and returns all the ones which have the
;;; entity as a source. derivation-src function
;;; has also been included as it is used.