;;; Coursework 2
;;; Question 5: make-derivation-tree-with-sharing.scm
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

(define derivation-trees-with-sharing
  (lambda (l g)
    (let ((trees '()))
      (letrec ((make-trees(lambda (l g)
        (if(null? l)
           '()
           (let ((sucs (successors (derivation-dst (car l)) g)))
           (if(not(subtree? trees (list (cons (derivation-dst (car l)) (derivation-trees-with-sharing sucs g)))))
              (begin
                (set! trees (append (list (cons (derivation-dst (car l)) 
                                                      (derivation-trees-with-sharing sucs g))) trees))
                (cons (car trees) (derivation-trees-with-sharing (cdr l) g)))
              (cons (subtree trees (cons (cons (derivation-dst (car l)) 
                                               (derivation-trees-with-sharing sucs g)) 
                                         (derivation-trees-with-sharing (cdr l) g))) 
                    (derivation-trees-with-sharing (cdr l) g))))))))
        (make-trees l g)))))

(define subtree?
  (lambda (l t)
    (cond ((null? l) #f)
          (else (if (equal? (car l) t)
                    #t
                    (subtree (cdr l) t))))))
    
(define subtree
  (lambda (l t)
    (if (equal? (car l) t)
                    (car l)
                    (subtree (cdr l) t))))
  
(define make-derivation-tree-with-sharing
  (lambda (entity graph)
    (if(null? (successors entity graph)) (list entity)
       (cons entity (derivation-trees-with-sharing (successors entity graph) graph)))))

;;; In this, I have done something similar to Question 3 but instead, now in my helper
;;; function, I need to keep a variable which has the list of sub-trees which have 
;;; already been seen. This way, instead of consing a new tree, I just add the one 
;;; which has already been created. Therefore, it preserves sharing. This code isn't
;;; working fully but it works for many cases. If you can improve it to work for all 
;;; cases, do let me know in feedback.