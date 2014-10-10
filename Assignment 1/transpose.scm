;;; Coursework 1
;;; Question 3
;;; Name: Mohammad Ali Khan Email: mak1g11@soton.ac.uk
;;;
;;;

(define transpose
  (lambda (l)
    (cond ((null? l)
           '())
     ((null? (car l))
   '())
     (else
      (cons (map car l) (transpose (map cdr l)))))))

;;; For this, I decided to use an in-built function to help 
;;; me, namely, map. Calling map on the car of each element 
;;; in the list gives me the first bit,and then, I call 
;;; transpose on the map of the cdr of the list. This way, 
;;; the first bit from each list is removed and map can be 
;;; called on the rest of it in the recursive case. One thing
;;; I had to look for are null elements, like () () () in a list
;;; and such. Once I get here, I return an empty list, else 
;;; problems are caused and and error is produced.