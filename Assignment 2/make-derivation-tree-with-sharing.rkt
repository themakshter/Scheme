(define graph1
  '((wasDerivedFrom d1 e2 e1)
    (entity e0)
    (entity e1)
    (entity e2)
    (entity e3)
    (entity e4)
    (entity e5)
    (wasDerivedFrom d2 e3 e2)
    (wasDerivedFrom d4 e4 e5)
    (wasDerivedFrom d3 e4 e3)
    (wasDerivedFrom d5 e5 e0)
    (wasDerivedFrom d0 e1 e0)))

(define graph2
  '((entity e0)
    (entity e1)
    (entity e2)
    (entity e3)
    (entity e4)
    (wasDerivedFrom d0 e1 e0)
    (wasDerivedFrom d1 e2 e1)
    (wasDerivedFrom d2 e3 e2)
    (wasDerivedFrom d3 e4 e3)
    (wasDerivedFrom d4 e3 e1)
    (wasDerivedFrom d5 e3 e0)))

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
       (cons entity (derivation-trees (successors entity graph) graph)))))

(define derivation-trees
  (lambda (l g)
    (if(null? l)
       '()
       (cons (cons (derivation-dst (car l)) (derivation-trees (successors (derivation-dst (car l)) g) g))(derivation-trees (cdr l) g)))))

(define derivation-trees-with-sharing
  (lambda (l g)
    (let ((trees '()))
      (letrec ((make-trees(lambda (l g)
        (if(null? l)
           '()
           (if(not(subtree? trees (list (cons (derivation-dst (car l)) (derivation-trees-with-sharing (successors (derivation-dst (car l)) g) g)))))
              (begin
                (set! trees (append trees (list (cons (derivation-dst (car l)) (derivation-trees-with-sharing (successors (derivation-dst (car l)) g) g)))))
                ;(display trees)
                (cons (cons (derivation-dst (car l)) (derivation-trees-with-sharing (successors (derivation-dst (car l)) g) g)) (derivation-trees-with-sharing (cdr l) g)))
              (cons (subtree trees (cons (cons (derivation-dst (car l)) (derivation-trees-with-sharing (successors (derivation-dst (car l)) g) g)) (derivation-trees-with-sharing (cdr l) g))) (derivation-trees-with-sharing (cdr l) g)))))))
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


(make-derivation-tree-with-sharing 'e3 graph2)
(define tree-label car)
(define tree-children cdr)

(define tree1 (make-derivation-tree 'e3 graph2))
(define tree2 (make-derivation-tree 'e3 graph2))
(eq? (cadr (tree-children tree2))
(car (tree-children (car (tree-children tree2)))))
(equal? tree1 tree2)
