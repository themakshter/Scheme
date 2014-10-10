#lang scheme

(define transpose
  (lambda (l)
    (cond ((null? l)
           '())
     ((null? (car l))
   '())
     (else
      (cons (map car l) (transpose (map cdr l)))))))


;(transpose '((a b) (c d) (e f)))
;(transpose '((1 2 3) (4 5 6) (7 8 9)))