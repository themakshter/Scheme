
(define tree-e0-g1 '(e0))
(define tree-e1-g1 '(e1 (e0)))
(define tree-e2-g1 '(e2 (e1 (e0))))
(define tree-e3-g1 '(e3 (e2 (e1 (e0)))))
(define tree-e4-g1 '(e4 (e3 (e2 (e1 (e0)))) (e5 (e0))))
(define tree-e5-g1 '(e5 (e0)))
(define tree-e0-g2 '(e0))
(define tree-e1-g2 '(e1 (e0)))
(define tree-e2-g2 '(e2 (e1 (e0))))
(define tree-e3-g2 '(e3 (e2 (e1 (e0))) (e1 (e0)) (e0)))
(define tree-e4-g2 '(e4 (e3 (e2 (e1 (e0))) (e1 (e0)) (e0))))
(define tree-e0-g3 '(e0 (e1 (e3 (e5) (e6 (e5))) (e4 (e5) (e3 (e5) (e6 (e5))) (e6 (e5)))) (e2 (e3 (e5) (e6 (e5))) (e4 (e5) (e3 (e5) (e6 (e5))) (e6 (e5)))) (e5)))
(define tree-e1-g3 '(e1 (e3 (e5) (e6 (e5))) (e4 (e5) (e3 (e5) (e6 (e5))) (e6 (e5)))))
(define tree-e2-g3 '(e2 (e3 (e5) (e6 (e5))) (e4 (e5) (e3 (e5) (e6 (e5))) (e6 (e5)))))
(define tree-e3-g3 '(e3 (e5) (e6 (e5))))
(define tree-e4-g3 '(e4 (e5) (e3 (e5) (e6 (e5))) (e6 (e5))))
(define tree-e5-g3 '(e5))
(define tree-e6-g3 '(e6 (e5)))
(define tree-e8-g4 '(e8 (e4 (e2 (e0)) (e5 (e1 (e0))) (e6 (e2 (e0)) (e3 (e0)) (e0)) (e0)) (e9 (e1 (e0)) (e5 (e1 (e0))) (e10 (e5 (e1 (e0))))) (e10 (e5 (e1 (e0)))) (e11 (e6 (e2 (e0)) (e3 (e0)) (e0)) (e12 (e13 (e14 (e15))) (e14 (e15))) (e13 (e14 (e15))) (e15)) (e12 (e13 (e14 (e15))) (e14 (e15))) (e5 (e1 (e0))) (e6 (e2 (e0)) (e3 (e0)) (e0)) (e0) (e7)))

(define tree1 '(e3 (e2 (e1 (e0))) (e1 (e0)) (e0)))

(define tree-label car)
(define tree-children cdr)

(define spanner
  (lambda (tree)
    (let ((visited '()))
      (letrec ((span (lambda (l t)
          (cond
            ((null? t) '())
            ((pair? (tree-label t)) (if(symbol? (car (tree-label t)))
                                       (if(checker l (car (tree-label t)))
                                          (append (span l (cdr (tree-label t))) (span l (cdr t)))
                                          (cons (span l (cons (car (tree-label t)) (cdr (tree-label t)))) (span l (cdr t))))
                                       (cons (cons (span l (car (tree-label t))) (span l (cdr (tree-label t)))) (span l (cdr t))))) 
            ((null? (tree-label t)) '())
            (else
             (if(not (checker l (tree-label t) ))
                (begin
                ;(set! visited (cons (tree-label t) visited))
                (cons (tree-label t) (span  (cons (tree-label t) l) (cdr t)))
                (display l)
                )
                (span (cdr t)))
                )))))
      (span '() tree)))))


(define spanning-tree
  (lambda (t)
   (helper t '())))


 (define checker
   (lambda (l e)
     (cond
      ((not (pair? l))(equal? l e))
      (else (or (checker (car l) e) (checker (cdr l) e))))))
 
 
(define helper
  (lambda (t l)
    (cond((null? t) l)
         ((list? (car t))(if(symbol? (caar t))
                            (helper (cdr t) (helper (cdar t) (cons l (list (check l (caar t)))))) 
                            (helper (cdr t) (helper (cdar t) (helper (caar t) (append l (list (check l (caar t)))))))))
         (else (helper (cdr t) (list (check l (car t))))))))

(define check
  (lambda (t e)
    (cond((null? t) e)
         ((null? e) '())          
    ((list? (car t)) (if (or (equal? (check (car t) e) '()) (equal? (check (cdr t) e) '()))
                         '()
                         e))
    ((equal? (car t) e) '())
    (else (check (cdr t) e)))))


(define hack
  (lambda (l)
    (if(null? (car l)) (hack (cdr l))
       l)))


;(check '() 'e3)
;(cadr '((e0)))
(spanner '(e1 (e0) (e0)))
;(spanning-tree tree-e4-g1) ;'(e4 (e3 (e2 (e1 (e0)))) (e5))
;(spanning-tree tree-e3-g2) ;'(e3 (e2 (e1 (e0))))
;(spanning-tree tree-e4-g2) ;'(e4 (e3 (e2 (e1 (e0)))))                                                             
;(spanning-tree tree-e0-g3) ;'(e0 (e1 (e3 (e5) (e6)) (e4)) (e2))                                                
;(spanning-tree tree-e1-g3) ;'(e1 (e3 (e5) (e6)) (e4))                                                               
;(spanning-tree tree-e2-g3) ;'(e2 (e3 (e5) (e6)) (e4))                                                           
;(spanning-tree tree-e3-g3) ;'(e3 (e5) (e6))                                                                       
;(spanning-tree tree-e4-g3) ;'(e4 (e5) (e3 (e6)))                                                                   
;(spanning-tree tree-e8-g4) ;'(e8 (e4 (e2 (e0)) (e5 (e1)) (e6 (e3))) (e9 (e10)) (e11 (e12 (e13 (e14 (e15))))) (e7)) 





