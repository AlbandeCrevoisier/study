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
	3)

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


;; Exercise 1.18
(define (iterative-mult a b)
  (mult-iter a b 0))

(define (mult-iter a b c)
  (cond ((= b 0) c)
	((odd? b) (mult-iter a (- b 1) (+ c a)))
	(else (mult-iter (double a) (halve b) c))))


;; Exercise 1.19
;; p' = p^2 + q^2
;; q' = 2pq + q^2
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q))
		   (+ (* 2 p q) (square q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))


;; Exercise 1.20
;; Normal-order evaluation:
;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; (gcd (remainder 40 (remainder 206 40))
;;      (remainder (remainder 206 40)
;;                 (remainder 40 (remainder 206 40))))
;; (gcd (remainder (remainder 206 40)
;;                 (remainder 40 (remainder 206 40)))
;;      (remainder (remainder 40 (remainder 206 40))
;;                 (remainder (remainder 206 40)
;;                            (remainder 40 (remainder 206 40)))))
;; (gcd (remainder (remainder 40 (remainder 206 40))
;;                 (remainder (remainder 206 40)
;;                            (remainder 40 (remainder 206 40))))
;;      (remainder (remainder (remainder 206 40)
;;                            (remainder 40 (remainder 206 40)))
;;                 (remainder (remainder 40 (remainder 206 40))
;;                            (remainder (remainder 206 40)
;;                                       (remainder 40
;;                                                  (remainder 206
;;                                                             40))))))
;; remainder ends up being called 19 times.

;; Applicative-order evaluation: 5 times.


;; Exercise 1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

;; (smallest-divisor 199) 199
;; (smallest-divisor 1999) 1999
;; (smallest-divisor 19999) 7


;; Exercise 1.22
(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (time->seconds (current-time))))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (time->seconds (current-time)) start-time))))

(define (report-prime elapsed-time)  (display " *** ")
  (display elapsed-time))

(define (search-for-primes l u)
  (define (search-iter n)
    (timed-prime-test n)
    (if (< (+ n 2) u)
	(search-iter (+ n 2))))
  (if (odd? l)
      (search-iter l)
      (search-iter (+ l 1))))

;; Three smallest primes larger than:
;; 1000: 1009, 1013, 1019, computed in 1.4e-4
;; 10,000: 10,007, 10,009, 10,037, computed in 4.5e-4
;; 100,000: 100,003, 100,019, 100,043, computed in 1.5e-3
;; 1,000,000: 1,000,003, 1,000,033, 1,000,037, computed in 4.5e-3
;; The increase in computation does follow about sqrt(10) for a tenfold
;; increase in steps, which agrees with the approximation of a run in
;; time proportionnal to the number of steps.


;; Exercise 1.23
(define (smallest-divisor n)
  (define (next a)
    (if (= a 2) 3 (+ a 2)))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor (next test-divisor)))))
  (find-divisor 2))

;; New computation times:
;; 1009: 1.2e-4
;; 10,007: 2.7e-4
;; 100,003: 8e-4
;; 1,000,003: 2.7e-3
;; The descrease is not exactly half, which is to be expected in such an
;; approximative measure.


;; Exercise 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (time->seconds (current-time)) start-time))))

;; Computing times, with times=4:
;; 1009: 7e-4
;; 10,007: 9.6e-4
;; 100,003: 1.1e-3
;; 1,000,003: 1.3e-3
;; It takes more time than expected for smaller numbers. The assumption
;; here is that generating random numbers is slowing down the whole
;; execution more than using a mathemacaly suboptimal algorithm.


;; Exercise 1.25
;; In this case, we would only use the remainder at the end, so on
;; rather big numbers. In expmod, we use the properties of the modulo
;; to save computation by immediately reducing numbers to their modulo.


;; Exercise 1.26
;; Applicative-order evaluation means that expmod will be called twice,
;; once for each parameter of the `*' operation. When using square, it
;; only gets called once.


;; Exercise 1.27
(define (carmichael? n)
  (define (iter a)
    (cond ((= a n) true)
	  ((= (expmod a n n) a)
	   (iter (+ a 1)))
	  (else	false)))
  (iter 2))

;; All the mentionned numbers are indeed Carmichael numbers: 561, 1105,
;; 1729, 2465, 2821, and 6601.


;; Exercise 1.28
;; Return 0 if there is a nontrivial square root of 1 modulo m.
(define (expmod-MR-test a m)
  (if (and (not (= a 1))
           (not (= a (- m 1)))
           (= (remainder (square a) m)
              1))
      0
      (remainder (square a) m)))

(define (expmod-MR base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
         (expmod-MR-test (expmod-MR base (/ exp 2) m) m))
	(else
	 (remainder (* base (expmod-MR base (- exp 1) m))
		    m))))

(define (MR-test n)
  (define (try-it a)
    (not (= (expmod-MR a (- n 1) n)
            0)))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime-MR? n times)
  (cond ((= times 0) true)
	((MR-test n) (fast-prime-MR? n (- times 1)))
	(else false)))


;; Exercise 1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-integral f a b n)
  (define (inc-h x) (+ x (/ (- b a) n)))
  (define (term x)
    (cond ((or (= x a)
               (= x b))
           (f x))
          ((odd? (/ (- x a)
                    (/ (- b a) n)))
           (* 4 (f x)))
          (else
           (* 2 (f x)))))
  (/ (* (/ (- b a) n)
        (sum term a inc-h b))
      3.0))

;; Let's integrate `cube' from 0 to 1 with each method:
;; - integral
;;   * 0.01:  .24998750000000042
;;   * 0.001: .249999875000001
;; - simpson-integral
;;   * 100:   .25
;;   * 1000:  .25
;; Simpson's method is more accurate (also checked on an approximation of the
;; log to be sure that the benefit was not only due to the computation being
;; done only on rationnal numbers until the very end, which naturaly leads to
;; fewer approximations.


;; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter x result)
    (if (> x b)
        result
	(iter (next x) (+ result (term x)))))
  (iter a 0))


;; Exercise 1.31
;; a.
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial x)
  (if (= x 1)
      1
      (* x (factorial (- x 1)))))

(define (id x) x)

(define (inc-2 x) (+ x 2))

(define (pi-approx n)
  (if (even? n)
      (* 2.0
	 n
	 (/ (square (product id 2 inc-2 (- n 2)))
	    (square (product id 3 inc-2 (- n 1)))))
      (* 2.0
	 (/ (square (product id 2 inc-2 (- n 1)))
	    (* n (square (product id 2 inc-2 (- n 2))))))))

;; b.
(define (product-iter term a next b)
  (define (iter x res)
    (if (> x b)
	res
	(iter (next x) (* res (term x)))))
  (iter a 1))


;; Exercise 1.32
;; a.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

;; sum: (accumulate + 0 term a next b)
;; product: (accumulate * 1 term a next b)

;; b.
(define (acc-iter combiner null-value term a next b)
  (define (iter x res)
    (if (> x b)
	res
	(iter (next x)
	      (combiner res (term x)))))
  (iter a null-value))


;; Exercise 1.33
(define (filtered-accumulate combiner pass-filter? null-value term a next b)
  (cond ((> a b)
	 null-value)
	((pass-filter? a)
	 (combiner (term a)
		   (filtered-accumulate combiner pass-filter? null-value
					term (next a) next b)))
	(else
	 (filtered-accumulate combiner pass-filter? null-value
			      term (next a) next b))))

;; a.
(define (inc x) (+ x 1))

(define (sum-sq-primes a b)
  (filtered-accumulate + prime? 0 square a inc b))

;; b.
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relative-primes? a b)
  (= (gcd a b) 1))

(define (prod-relative-primes n)
  (define (pass-filter? x)
    (relative-primes? x n))
  (filtered-accumulate * pass-filter? 1 id 1 inc n))


;; Exercise 1.34
;; (f f): since `2' is not a procedure, it will fail at `(2 2)'.


;; Exercise 1.35
(define (fixed-point f first-guess tolerance)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;; Phi is the solution of the equation: x^2 = x + 1. Since x = 0 is
;; not a solution, we can rewrite it as: x = 1 + 1/x, which shows that
;; phi is a fixed point of the application: x |-> 1 + 1/x.
(define (phi tolerance)
  (fixed-point (lambda (x) (+ 1 (/ 1.0 x)))
	       1.0
	       tolerance))


;; Exercise 1.36
(define (verbose-fixed-point f first-guess tolerance)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))


;; Exercise 1.37
(define (cont-frac n d k)
  (define (recurse i)
    (if (= i (+ k 1))
	0
	(/ (n i)
	   (+ (d i)
	      (recurse (+ i 1))))))
  (recurse 1))
;; Only 12 iterations are required to achieve a 4-decimal precision.

(define (cont-frac-iter n d k)
  (define (iter i res)
    (if (= i 0)
	res
	(iter (- i 1)
	      (/ (n i)
		 (+ (d i)
		    res)))))
  (iter k 0))


;; Exercise 1.38
(define (one x) 1)
(define (euler-e k)
  (define (D_i i)
    (if (= (modulo i 3) 2)
	(* 2 (+ 1 (quotient i 3)))
	1))
  (+ 2.0 (cont-frac one D_i k)))


;;Exercise 1.39
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
	x
	(- (square x))))
  (define (d i)
    (+ (* 2 (- i 1))
       1))
  (cont-frac n d k))


;; Exercise 1.40
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform f)
  (lambda (x)
    (- x (/ (f x) ((deriv f) x)))))

(define (newtons-method f guess)
  (fixed-point (newton-transform f) guess 0.001))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))


;; Exercise 1.41
(define (double-proc f)
  (lambda (x)
    (f (f x))))

;; (((double (double double)) inc) 5) = 21
