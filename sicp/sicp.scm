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

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

;; Another possibility would be for both the then- & else- clause to be
;; read as the body associated with the first predicate of the cond.
;; In this case, `guess' would just pass, & then the call to `sqrt-iter'
;; would be made, hence creating an infinite loop. Let's check it out:
(new-if (= 1 1)
	2
	(display 3))

;; This prints `32', confirming the latter explaination.


;; Exercise 1.7
;; In the case of very small numbers, 0.001 represents too big of a
;; bound to be of any use:
(sqrt 0.000001)

;; This yields 0.03, a clearly wrong result.

;; In big numbers, the difference between the squared guess & the number
;; might never fall under the given threshold since precision is
;; arbitrary.
(sqrt 999999999999999)

;; This does not terminate with a threshold of 0.0000000001 .


;; Exercise 1.8
(define (cube x)
  (* x x x))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess))
	(* 2 guess))
     3))

(define (cube-root-iter guess x)
  (if (< (abs (- x (cube guess))) 0.001)
      guess
      (cube-root-iter (improve-cube guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))


;; Exercise 1.9
;;
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; This is clearly a linear recursive process.
;;
;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
;; This is clearly an iterative process.
