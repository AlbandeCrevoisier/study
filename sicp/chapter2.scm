;; Structure & Interpretation of Computer Programs
;; Chapter 2 - Bulding Abstractions with Data

(load "chapter1.scm")


(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat q)
  (newline)
  (display (numer q))
  (display " / ")
  (display (denom q)))

(define (add-rat a b)
  (make-rat (+ (* (numer a) (denom b))
               (* (denom a) (numer b)))
            (* (denom a) (denom b))))
(define (sub-rat a b)
  (make-rat (- (* (numer a) (denom b))
               (* (denom a) (numer b)))
            (* (denom a) (denom b))))
(define (mult-rat a b)
  (make-rat (* (numer a) (numer b))
            (* (denom a) (denom b))))
(define (div-rat a b)
  (make-rat (* (numer a) (denom b))
            (* (denom a) (numer b))))
(define (equal-rat? a b)
  (= (* (numer a) (denom b))
     (* (denom a) (numer b))))


;; Exercise 2.1
;; (gcd n d) has the same sign as d, hence (/ d g) is always positive.
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


;; Exercise 2.2
(define (make-segment p q) (cons p q))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s)))
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))


;; Exercise 2.3
(define (make-rectangle p q) (cons p q))
(define (minx-rect r)
  (min (x-point (car r))
       (x-point (cdr r))))
(define (miny-rect r)
  (min (y-point (car r))
       (y-point (cdr r))))
(define (maxx-rect r)
  (max (x-point (car r))
       (x-point (cdr r))))
(define (maxy-rect r)
  (max (y-point (car r))
       (y-point (cdr r))))

(define (make-rect-alt p q)
  (cons (min (x-point p) (x-point q))
    (cons (min (y-point p) (y-point q))
      (cons (max (x-point p) (x-point q))
        (cons (max (y-point p) (y-point q)) '())))))
(define (minx-rect-alt r)
  (car r))

(define (perimeter-rect r)
  (* 2 (+ (- (maxx-rect r)
             (minx-rect r))
          (- (maxy-rect r)
             (miny-rect r)))))
(define (area-rect r)
  (* (- (maxx-rect r)
        (minx-rect r))
     (- (maxy-rect r)
        (miny-rect r))))


;; Exercise 2.4
(define (new-cons x y)
  (lambda (m) (m x y)))
(define (new-car z)
  (z (lambda (p q) p)))
(define (new-cdr z)
  (z (lambda (p q) q)))


;; Exercise 2.5
;; 2^a.3^b is a decomposition in prime numbers, & thus lends itself to
;; defining unique pairs of natural numbers.
(define (n-pow a n)
  (cond ((= n 0) 1)
        ((= n 1) a)
        (else (* a (n-pow a (- n 1))))))

(define (p-cons a b)
  (* (n-pow 2 a) (n-pow 3 b)))
(define (p-car p)
  (if (= (remainder p 2) 0)
      (+ 1 (p-car (/ p 2)))
      0))
(define (p-cdr p)
  (if (= (remainder p 3) 0)
      (+ 1 (p-cdr (/ p 3)))
      0))
