;;; Coursework 1
;;; Question 2
;;; Name: Mohammad Ali Khan Email: mak1g11@soton.ac.uk
;;;
;;;


(define associate
  (lambda (l v)
    (define helper
      (lambda (l v n)
        (if (null? l)
         '()
        (cons (cons (car l) (vector-ref v n)) (helper (cdr l) v (+ n 1))))))
    (helper l v 0)))



;;; It is assumed that the inputs wil always be a list and vector 
;;; and they will be of the same size. For this function associate,
;;; I had one issue: how to cons the part of the list to the vector.
;;; The list is easy because of the car and cdr fucntions but the 
;;; vector doesn't have that and instead has vector-ref. Hence, I 
;;; need to keep a count of which position we were at regarding the
;;; vector, which needed an extra parameter of a number. This number
;;; would be started as a 0 at the beginning and be used in vector-ref.
;;; The base case would be when the list is null, and an empty list is
;;; returned. Otherwise, the car of l and the vector reference of the 
;;; number of the vector element we are at, and recursively, the function
;;; is called on the cdr of the list and the vector,with the index for the
;;; vector ref incremented.