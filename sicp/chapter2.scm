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

;; d.
;; cadr => cdr, only difference (in the selectors).


;; Exercise 2.30
(define (square-tree tree)
  (map (lambda (sub-tree)
   (if (pair? sub-tree)
       (square-tree sub-tree)
       (square sub-tree)))
       tree))

(define (square-tree-dumb tree)
  (cond ((null? tree) '())
  ((not (pair? tree))
   (square tree))
  (else
   (cons (square-tree-dumb (car tree))
         (square-tree-dumb (cdr tree))))))


;; Exercise 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
   (if (pair? sub-tree)
       (tree-map proc sub-tree)
       (proc sub-tree)))
       tree))


;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s))))
  (append rest (map (lambda (x) (cons (car s) x)) rest)))))
;; Partition the subsets on wether or not they have (car s) in them.


;; Exercise 2.33
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
              (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (new-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))


;; Exercise 2.34
(define (horner-eval x coeffs)
  (accumulate (lambda (this-coeff higher-term) (+ (* higher-term x) this-coeff))
              0
              coeffs))


;; Exercise 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (fringe t))))


;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (line) (dot-product line v)) m))
(define (transpose m)
  (accumulate-n cons '() m))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (line) (matrix-*-vector cols line)) m)))


;; Exercise 2.38
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
;; op has to be commutative for fold-left & fold-right to be
;; equivalent.


;; Exercise 2.39
(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))


;; Exercise 2.40
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))


;; Exercise 2.41
(define (sum-to-n-triplets n)
  (filter (make-sum-checker n) (unique-triplets n)))
(define (make-sum-checker n)
  (lambda (triplet)
    (= n (+ (car triplet) (cadr triplet) (caddr triplet)))))
(define (unique-triplets n)
  (flatmap (lambda (i)
             (map (lambda (pair) (cons i pair))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))


;; Exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda(new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))
(define (safe? k positions)
  (if (<= k 1)
      #t
      (let ((row (caar positions)))
        (null? (filter (lambda (pos)
                         (let ((i (car pos))
                               (j (cdr pos)))
                           (or (= row i)
                               (= row (+ i (- k j)))
                               (= row (- i (- k j))))))
                       (cdr positions))))))


;; Exercise 2.43
;; Louis' algo will run in about O(board-size ^ board-size),
;; whereas the give one runs in about O(board-size ^ 2).


;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((up-painter (up-split painter (- n 1))))
        (below painter (beside up-painter up-painter)))))


;; Exercise 2.45
(define (split outer inner)
  (lambda (painter)
    (outer painter (inner painter painter))))


;; Exercise 2.46
(define make-vect cons)
(define x-vect car)
(define y-vect cdr)

(define (add-vect v w)
  (make-vect (+ (x-vect v) (x-vect w))
             (+ (y-vect v) (y-vect w))))
(define (sub-vect v w)
  (make-vect (- (x-vect v) (x-vect w))
             (- (y-vect v) (y-vect w))))
(define (scale-vect scale v)
  (make-vect (* scale (x-vect v))
             (* scale (y-vect v))))


;; Exercise 2.47
(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define origin-frame-cons car)
(define edge1-frame-cons cadr)
(define edge2-frame-cons cddr)

(define make-frame list)
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)


;; Exercise 2.48
(define make-segment list)
(define start-segment car)
(define end-segment cadr)


;; Exercise 2.49
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (x-vect v) (edge1-frame frame))
                (scale-vect (y-vect v) (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda(segment)
        (draw-line
          ((frame-coord-map frame)
           (start-segment segment))
          ((frame-coord-map frame)
           (end-segment segment))))
      segment-list)))

(define (outline frame)
  ((segments->painter
     (list
       (make-segment (make-vect 0 0) (make-vect 0 1))
       (make-segment (make-vect 0 1) (make-vect 1 1))
       (make-segment (make-vect 1 1) (make-vect 1 0))
       (make-segment (make-vect 1 0) (make-vect 0 0))))
   frame))
(define (X frame)
  ((segments->painter
     (list
       (make-segment (make-vect 0 0) (make-vect 0 1))
       (make-segment (make-vect 0 1) (make-vect 1 1))
       (make-segment (make-vect 1 1) (make-vect 1 0))
       (make-segment (make-vect 1 0) (make-vect 0 0))))
   frame))
(define (diamond frame)
  ((segments->painter
     (list
       (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
       (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
       (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
       (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))
   frame))


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                   new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin)))))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter
              painter1
              (make-vect 0.0 0.0)
              split-point
              (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter
              painter1
              (make-vect 0.0 0.0)
              split-point
              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


;; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))


;; Exercise 2.51
(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
(define (below-long-version painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom (transform-painter (make-vect 0 0)
                                           (make-vect 1 0)
                                           split-point))
          (paint-top (transform-painter split-point
                                        (make-vect 1 0.5)
                                        (make-vect 0 1))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))


;; Exercise 2.52
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((top-left (up-split painter (- n 1)))
            (bottom-right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner)))))
(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-hiroz
                                  flip-vert rotate180)))
  (combine4 (corner-split painter n))))


(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


;; Exercise 2.53
;; (list 'a 'b 'c) => (a b c)
;; (list (list 'george)) => ((george))
;; (cdr '((x1 x2) (y1 y2))) => ((y1 y2))
;; (cadr '((x1 x2) (y1 y2))) => (y1 y2)
;; (pair? (car '(a short list))) => #f
;; (memq 'red '((red shoes) (blue socks))) => #f
;; (memq 'red '(red shoes blue socks)) => (red shoes blue socks)


;; Exercise 2.54
(define (equal? a b)
  (cond ((and (number? a) (number? b))
         (= a b))
        ((and (atom? a) (atom? b))
         (eq? a b))
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        (else #f)))


;; Exercise 2.55
;; 'foo is extended in: (quote foo), hence ''foo is extended in:
;; (quote (quote foo)), which yields the list (quote foo).


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))))
        (else
          (error "Unknown expression type: DERIV" exp))))

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))
(define addend cadr)
(define augend caddr)
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* a1 a2))
        (else (list '* m1 m2))))
(define (product? e)
  (and (pair? e) (eq? (car e) '*)))
(define multiplier cadr)
(define multiplicand caddr)
(define (=number? exp num) (and (number? exp) (= exp num)))


;; Exercise 2.56
(define (make-exponentiation b e)
  (cond ((=number? b 0) 0)
        ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (pow b e))
        (else (list '** b e))))
(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))
(define base cadr)
(define exponent caddr)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp) (- (exponent exp) 1))
                       (deriv (base exp) var)))
        (else
          (error "Unknown expression type: DERIV" exp))))


;; Exercise 2.57
(define (make-sum . s)
  (let ((sum-numbers (fold-left + 0 (filter number? s)))
        (sum-symbols (filter (lambda (x) (not (number? x))) s)))
    (cond ((null? sum-symbols) sum-numbers)
          ((= sum-numbers 0) (append '(+) sum-symbols))
          (else (append (list '+ sum-numbers) sum-symbols)))))
(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))
(define addend cadr)
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (append '(+) (cddr s))))
(define (make-product . p)
  (let ((prod-numbers (fold-left * 1 (filter number? p)))
        (prod-symbols (filter (lambda (x) (not (number? x))) p)))
    (cond ((null? prod-symbols) prod-numbers)
          ((= prod-numbers 0) 0)
          ((= prod-numbers 1)
           (if (null? (cdr prod-symbols))
               (car prod-symbols)
               (append '(*) prod-symbols)))
          (else (append (list '* prod-numbers) prod-symbols)))))
(define (product? e)
  (and (pair? e) (eq? (car e) '*)))
(define multiplier cadr)
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (append '(*) (cddr p))))


;; Exercise 2.58
;; Binary infix operations:
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (sum? e)
  (and (pair? e) (eq? (cadr e) '+)))
(define addend car)
(define augend caddr)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* a1 a2))
        (else (list m1 '* m2))))
(define (product? e)
  (and (pair? e) (eq? (cadr e) '*)))
(define multiplier car)
(define multiplicand caddr)

;; Set as an unordered list.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;; My version of this, also in Theta(n**2).
(define (my-intersection-set s1 s2)
  (filter (lambda (x) (element-of-set? x s1)) s2))


;; Exercise 2.59
(define (union-set set1 set2)
  (append set1 (filter (lambda (x) (not (element-of-set? x set1))) set2)))


;; Exercise 2.60
;; Sets as lists with duplicates.
;; element-of-sets remains the same.
(define (adjoin-set x set)
  (cons x set))
(define (union-set set1 set2)
  (append set1 set2))
;; intersection-set does not change.
