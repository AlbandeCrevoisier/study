;; Structure & Interpretation of Computer Programs


(define true #t)
(define false #f)


;; Exercise 1.1
;; 10
;; 12
;; 8
;; 3
;; 6
;; undefined
;; undefined
;; 19
;; false
;; 4
;; 16
;; 6
;; 16


;; Exercise 1.2
(/ (+ 5
      4
     (- 2
	 (- 3
	    (+ 6
	       (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))


;; Exercise 1.3
(define (sum-of-squares a b)
  (+ (* a a) (* b b)))

(define (f a b c)
  (cond ((and (< a b) (< a c))
	 (sum-of-squares b c))
	((and (< b a) (< b c))
	 (sum-of-squares a c))
	(else (sum-of-squares a b))))
