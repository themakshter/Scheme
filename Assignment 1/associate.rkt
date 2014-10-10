#lang scheme
(define associate
  (lambda (l v)
    (define helper
      (lambda (l v n)
        (if (null? l)
           '()
        (cons (cons (car l) (vector-ref v n)) (helper (cdr l) v (+ n 1))))))
        (helper l v 0)))

;eval (associate '(a b c) '#(10 20 30))