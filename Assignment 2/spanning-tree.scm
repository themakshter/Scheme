;;; Coursework 2
;;; Question 4: spanning-tree.scm
;;; Name: Mohammad Ali Khan
;;; Email: mak1g11@soton.ac.uk

;;; Solution:

(define tree-label car)
(define tree-children cdr)

(define spanning-tree
  (lambda (tree)
    (let ((visited '()))
      (letrec ((span (lambda (t)
          (cond
            ((null? t) '())
            ((pair? (tree-label t)) (if(symbol? (car (tree-label t)))
                                       (if(check visited (car (tree-label t)))
                                          (append (span (cdr (tree-label t))) 
                                                  (span (tree-children t)))
                                          (cons (span (cons (car (tree-label t)) 
                                                            (cdr (tree-label t)))) 
                                                (span (tree-children t))))
                                       (cons 
                                        (cons (span (car (tree-label t))) 
                                              (span (cdr (tree-label t)))) 
                                        (span (tree-children t))))) 
            ((null? (tree-label t)) '())
            (else
             (if(not (check visited (tree-label t) ))
                (begin
                (set! visited (cons (tree-label t) visited))
                (append (list (tree-label t)) (span (tree-children t))))
                (span (tree-children t)))
                )))))
      (span tree)))))

(define check
   (lambda (l e)
     (cond
      ((not (pair? l))(equal? l e))
      (else (or (check (car l) e) (check (cdr l) e))))))

;;; The function defined inside the main function is the main one.
;;; In it, a few things are check. First, a list of visited nodes
;;; is initialised. After that, on the base case,if the tree is 
;;; empty, empty list returned.A  node is checked if it is contained
;;; in the visited list. If it is, we move on to the rest of the tree.
;;; Otherwise, we add it to the list of visited nodes and cons it with
;;; the rest of the list. We also check if what we are on is not a list.
;;; If it is, we compare the label against the visited nodes and see if
;;; it isn't there; this way we can ignore whole sub-tree which have been
;;; visited.