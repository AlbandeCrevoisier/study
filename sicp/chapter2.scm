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


;; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))


;; Exercise 2.7
(define (make-inter l u) (cons l u))
(define (lower i) (car i))
(define (upper i) (cdr i))

(define (add-inter x y)
  (make-inter (+ (lower x) (lower y))
              (+ (upper x) (upper y))))
(define (mult-inter x y)
  (let ((p1 (* (lower x) (lower y)))
        (p2 (* (lower x) (upper y)))
        (p3 (* (upper x) (lower y)))
        (p4 (* (upper x) (upper y))))
    (make-inter (min p1 p2 p3 p4)
                (max p1 p2 p3 p4))))
(define (div-inter x y)
  (mult-inter x (make-inter (/ 1.0 (upper y))
                            (/ 1.0 (lower y)))))


;; Exercise 2.8
(define (sub-inter x y)
  (add-inter x (make-inter (- (upper y))
                           (- (lower y)))))


;; Exercise 2.9
;; (w (+ x y))
;; (- (upper (+ x y)) (lower (+ x y)))
;; (- (+ (upper x) (upper y)) (+ (lower x) (lower y)))
;; (+ (- (upper x) (lower x)) (- (upper y) (lower y)))
;; (+ (w x) (w y))
;; However, let's consider I [0, 1] & I' [1, 2]: their width is 1, but
;; if the width of I * I is 1, that of I * I' is 2.


;; Exercise 2.10
(define (new-div-inter x y)
  (if (= (lower y) (upper y))
      (error "Cannot divide by zero.")
      (mult-inter x (make-inter (/ 1.0 (upper y))
                                (/ 1.0 (lower y))))))


;; Exercise 2.11
(define (new-mult-inter x y)
  (let ((lx (lower x))
        (ux (upper x))
        (ly (lower y))
        (uy (upper y)))
    (cond ((and (< lx 0) (< ux 0) (< ly 0) (< uy 0))
           (make-inter (* ux uy) (* lx ly)))
          ((and (< lx 0) (< ux 0) (< ly 0) (> uy 0))
           (make-inter (* lx uy) (* lx ly)))
          ((and (< lx 0) (< ux 0) (> ly 0) (> uy 0))
           (make-inter (* lx uy) (* ux ly)))
          ((and (< lx 0) (> ux 0) (< ly 0) (< uy 0))
           (make-inter (* ux ly) (* lx ly)))
          ((and (< lx 0) (> ux 0) (> ly 0) (> uy 0))
           (make-inter (* lx uy) (* ux uy)))
          ((and (> lx 0) (> ux 0) (< ly 0) (< uy 0))
           (make-inter (* ux ly) (* lx uy)))
          ((and (> lx 0) (> ux 0) (< ly 0) (> uy 0))
           (make-inter (* ux ly) (* ux uy)))
          ((and (> lx 0) (> ux 0) (> ly 0) (> uy 0))
           (make-inter (* lx ly) (* ux uy)))
          ((and (< lx 0) (> ux 0) (< ly 0) (> uy 0))
           (let ((p1 (* lx ly))
                 (p2 (* lx uy))
                 (p3 (* ux ly))
                 (p4 (* ux uy)))
             (make-inter (min p1 p2 p3 p4)
                         (max p1 p2 p3 p4)))))))


(define (make-center-width c w)
  (make-inter (-c w) (+ cw)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


;; Exercise 2.12
(define (make-center-percent c pc)
  (make-center-width c (* pc c)))
(define (percent i)
  (/ (width i) (center i)))


;; Exercise 2.13
;; lxy = (cx - wx/2)(cy - wy/2)
;;     = cx.cy - cx.wy/2 - wx.cy/2 + wx.wy/2
;; lxy ~= cx.cy - (cx.wy + cy.wx)/2
;; wxy ~= cx.wy + cy.wx
;; pcxy ~= (cx.wy + cy.wx)/(cx.cy)
;;      ~= wy/cy + wx/cx
;; pcxy ~= pcy + pcx
