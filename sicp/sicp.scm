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
(define (square x)
  (* x x))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (f a b c)
  (cond ((and (< a b) (< a c))
	 (sum-of-squares b c))
	((and (< b a) (< b c))
	 (sum-of-squares a c))
	(else (sum-of-squares a b))))


;; Exercise 1.4
;; When b>0, the procedure uses the + operator, otherwise the - one.
;; This results in a + |b|.


;; Exercise 1.5
;; With applicative-order evaluation: infinite-loop
;; With normal-order evaluation: 0


;; Exercise 1.6
;; Because of applicative-order evaluation, all the parameters will be
;; evaluated before calling new-if, including the `else' call to
;; `sqrt-iter', thus creating an infinite loop.
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))
