#lang scheme

(define error #f)
(call-with-current-continuation (lambda (k)
				  (set! error
					(lambda error-arguments
					  (display ">>>> ERROR ")
					  (newline)
					  (k error-arguments)))
				  'done)) 

(define triangular
  (lambda (n)
    (cond((= n 0)
     0)
    ((< n 0)
    'ERROR)
    (else
    (+ (triangular (- n 1)) n )))))

;eval (triangular 1)
;eval (triangular 3)
;eval (triangular 4)
;eval (triangular 5)
;eval (triangular -1)


