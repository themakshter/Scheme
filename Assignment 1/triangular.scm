;;; Coursework 1
;;; Question 1
;;; Name: Mohammad Ali Khan Email: mak1g11@soton.ac.uk
;;;
;;;

;;; solution:

(define triangular
  (lambda (n)
    (cond((= n 1) 1)
    ((< n 0) (error "ERROR: Must be a natural number!"))
    (else (+ (triangular (- n 1)) n )))))

;;;It is assumed that the input will always be a number. The 
;;; function, triangular has a base case, checking if the number
;;; is 1 or not. If it is, the number returned is 1. If the number
;;; is less than this, an error is produced. If greater, the number
;;;is added to the recursive function called on the same number after
;;; it is decremented by 1.
;;;