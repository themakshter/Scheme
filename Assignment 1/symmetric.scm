;;; Coursework 1
;;; Question 3
;;; Name: Mohammad Ali Khan Email: mak1g11@soton.ac.uk
;;;
;;;


(define reverse-pair
  (lambda (l)
    (cons (cdr l) (car l))))

(define member?
  (lambda (l1 l2)
    (cond ((null? l2) #f)
    ((equal? l1 (car l2)) #t)
     (else (member? l1 (cdr l2))))))

(define symmetric
  (lambda (l)
    (define helper
      (lambda (l1 l2)
        (cond((null? l2) '#T)
        ((null? l1) '#T)
        ((member? (reverse-pair (car l1)) l2) (helper (cdr l1) l2))
        (else  #F))))
    (helper l l)))



;;; To achieve what is required of the function symmetric, I had to use
;;; the help of two other functions in addition to a helper function The
;;; first other function I use is reverse-pair, which gives me the opposite
;;; pair of the one I call this funtion on. I needed this to check for the
;;; opposite pair of a specific one in the list. The second function I used
;;; was member?, which gave returned a true or false depending on if a 
;;; particular pair was present in the list or not. The last function I used
;;; to assist me was a helper function, which is given two instances of the
;;; list given as parameters, one to change, other one to remain the same. I
;;; go through the first list, take each member of it, see if its reverse-pair
;;; exists in the list with member? If I get a false for any one, I return a 
;;; false. Otherwise, I call the fucntion on the next pair in the list. If the
;;; list becomes null, it means that all have opposites, and hence return true.