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

;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
;; This is clearly an iterative process.


;; Exercise 1.10

;; (A 1 10)
;; (A 0 (A 1 9))
;; (* 2 (A 1 9))
;; => (* 2 (* 2 ... (* 2 (A 1 1))...)
;; 2^10 = 1024

;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 (* 2 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 (* 2 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 (* 2 4)))
;; (A 1 (A 0 8))
;; (A 1 (* 2 8))
;; (A 1 16)
;; 2^16 = 65 536

;; (A 3 3)
;; Too long to be done by hand.
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))
;; Also 65 536.

;; f: n |-> 2n
;; g: n |-> 2^n
;; h: n |-> (2^n)^n


;; TODO: challenge of counting the change in coins using an iterative
;; process.


;; Exercise 1.11
(define (recursive-f n)
  (if (< n 3)
      n
      (+ (recursive-f (- n 1))
	 (* 2 (recursive-f (- n 2)))
         (* 3 (recursive-f (- n 3))))))

(define (iterative-f n)
  (if (< n 3)
      n
      (f-iter 0 1 2 (- n 3))))
(define (f-iter a b c n)
  (if (= n 0)
      (+ c (* 2 b) (* 3 a))
      (f-iter b
	      c
	      (+ c (* 2 b) (* 3 a))
	      (- n 1))))


;; Exercise 1.12
(define (pascal n k)
  (if (or (= k 1) (= k n))
      1
      (+ (pascal (- n 1) (- k 1))
	 (pascal (- n 1) k))))


;; Exercise 1.13

;; Let's proceed by induction:
;; n = 0: trivial.
;; n = 1: (1 + sqrt(5) - (1 - sqrt(5))) / 2 = 1.
;; n = 2: develop the square, it reduces to 4 sqrt(5), so 1.
;; n > 3: let's assume the equation holds true for n-1 & n-2.
;; (phi^n - psi^n) / sqrt(5) = (phi^n-2 (1 + 2 sqrt(5) + 5) / 4
;;                            - psi^n-2 (1 - 2 sqrt(5) + 5) / 4)
;;                            / sqrt(5)
;;  = ((phi^n-2(1 + (1 + sqrt(5)) / 2)
;;   - (psi^n-2(1 - (1 - sqrt(5)) / 2)) / sqrt(5)
;;  = (phi^n-2 - psi^n-2) / sqrt(5) + (phi^n-1 - psi^n-1) / sqrt(5)
;;  = fib(n-2) + fib(n-1)
;;  = fib(n). QED



;; Exercise 1.14
;;   11
;;  /  \
;; 10  !10
;; |   /  \
;; 1  5   !5
;;   / \   |
;;  5  !5  1
;; /   /   |
;; 1  1  ...
;;    |    1
;;    1
;;    |
;;  ...
;;    1
;; Theta(n^n-coins)


;; Exercise 1.15
(define (sine angle)
  (define (p x)
    (display 'p)
    (- (* 3 x) (* 4 (cube x))))
  (if (< (abs angle) 0.1)
      angle
      (p (sine (/ angle 3.0)))))

;; a. p is applied 5 times.
;; b. Theta(n)


;; Exercise 1.16
(define (iterative-exp b n)
  (exp-iter b n 1))

(define (exp-iter b n a)
  (cond ((= n 0) a)
	((odd? n)
	 (exp-iter b (- n 1) (* a b)))
	(else
	 (exp-iter (square b) (/ n 2) a))))



;; Exercise 1.17
(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))
(define (fast-mult a b)
  (cond ((= b 0) 0)
	((odd? b) (+ a (fast-mult a (- b 1))))
	(else (fast-mult (double a) (halve b)))))
