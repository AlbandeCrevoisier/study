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


(define (par1 r1 r2)
  (div-inter (mult-inter r1 r2)
                (add-inter r1 r2)))
(define (par2 r1 r2)
(let ((one (make-inter 1 1)))
  (div-inter one
             (add-inter (div-inter one r1)
                        (div-inter one r2)))))


;; Exercise 2.14
;; r = [999, 1001]
;; (par1 r r) = (498.501998 . 501.502002)
;; (par2 r r) = (499.5 . 500.5)
;; `par2' always has a better precision.


;; Exercise 2.15
;; Eva is indirectly correct: what matters is the number of operations
;; that affect the precision. 1/x does not affect the precision, hence
;; par2 only has one precision-damaging operation when par1 has two.
;; Avoiding repeating a number does help in minimising the number of
;; precision-damaging operations.


;; Exercise 2.16
;; See 2.15: it stems from the number of precision-damaging operations.
;; Trying to reduce an operation to its optimal precision computation
;; sounds terrifyingly hard: there are infinitely many equivalent
;; formulations of an operation...


;; Exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))


;; Exercise 2.18
(define (reverse l)
  (define (iter l ret)
    (if (null? l)
        ret
        (iter (cdr l) (cons (car l) ret))))
  (iter l '()))


;; Exercise 2.19
;; Count change: count the ways of getting a given amount of change.
(define (cc amount coin-values)
  (define (no-more? cv) (null? cv))
  (define (except-first-denomination cv) (cdr cv))
  (define (first-denomination cv) (car cv))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount (first-denomination coin-values))
                 coin-values)))))


;; Exercise 2.20
(define (same-parity x . l)
  (define (iter l ret)
    (cond ((null? l) ret)
          ((or (and (odd? x) (odd? (car l)))
               (and (even? x) (even? (car l))))
           (iter (cdr l) (cons (car l) ret)))
          (else
            (iter (cdr l) ret))))
  (iter (reverse l) '()))


;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))
(define (square-list-map items)
  (map square items))


;; Exercise 2.22
;; (cons '(1 2 3) 4) => ((1 2 3) 4).
;; Instead, Louis could reverse his items.


;; Exercise 2.23
(define (for-each proc args)
  (cond ((null? args) #t)
        (else
          (proc (car args))
          (for-each proc (cdr args)))))


;; Exercise 2.24
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else
          (+ (count-leaves (car x))
             (count-leaves (cdr x))))))


;; Exercise 2.25
;; car/cdr 7 in:
;; (1 3 (5 7) 9) => cadaddr
;; ((7)) => caar
;; (1 (2 (3 (4 (5 (6 7)))))) => cadadadadadadr


;; Exercise 2.26
;; x: (1 2 3)
;; y: (4 5 6)
;; (append x y) => (1 2 3 4 5 6)
;; (cons x y) => ((1 2 3) 4 5 6)
;; (list x y) => ((1 2 3) (4 5 6))


;; Exercise 2.27
(define (deep-reverse ll)
  (define (iter ll ret)
    (cond ((null? ll) ret)
          ((not (pair? (car ll)))
           (iter (cdr ll) (cons (car ll) ret)))
          (else
            (iter (cdr ll)
                  (cons (deep-reverse (car ll))
                        ret)))))
  (iter ll '()))


;; Exercise 2.28
(define (fringe tree)
    (cond ((null? tree) '())
          ((not (pair? tree))
           (list tree))
          (else
            (append (fringe (car tree))
                    (fringe (cdr tree))))))


;; Exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (right-branch mobile)
  (cadr mobile))
(define (left-branch mobile)
  (car mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (+ (weight-branch (left-branch mobile))
     (weight-branch (right-branch mobile))))
(define (weight-branch branch)
  (if (not (pair? (branch-structure branch)))
      (branch-structure branch)
      (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (or (not (pair? mobile))
      (and (= (* (branch-length (left-branch mobile))
		 (weight-branch (left-branch mobile)))
	      (* (branch-length (right-branch mobile))
		 (weight-branch (right-branch mobile))))
	   (balanced? (branch-structure (left-branch mobile)))
	   (balanced? (branch-structure (right-branch mobile))))))
