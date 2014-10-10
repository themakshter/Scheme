;;; Coursework 2
;;; Question 3: make-derivation-tree.scm
;;; Name: Mohammad Ali Khan
;;; Email: mak1g11@soton.ac.uk

;;; Solution:

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

(define derivation-src caddr)
(define derivation-dst cadddr)

(define make-derivation-tree
  (lambda (entity graph)
    (if(null? (successors entity graph)) (list entity)
       (cons entity 
             (derivation-trees (successors entity graph) graph)))))
  
(define derivation-trees
  (lambda (l g)
    (if(null? l)
       '()
       (cons (cons (derivation-dst (car l)) 
                   (derivation-trees (successors (derivation-dst (car l)) g) g))
             (derivation-trees (cdr l) g)))))

;;; This function has a base case that the entity given has 
;;; no successors. In this case, only the entity will be 
;;; returned. Otherwise, a helper function will be called 
;;; and be given the whole graph and also the list of the 
;;; current entity's successors. In the helper function, the 
;;; base case is checked for if the successors of the given 
;;; entity are empty or not. In such cases, an empty list is
;;; returned. Otherwise,it further recurses on each derivation's
;;; (first adding the destination) destination, before adding all 
;;; the different branches together. 