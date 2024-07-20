;; 4·VII·2024
;; Computational process manipulate data as directed by programs.
;; Lisp (Scheme) treats procedures as data, blurring the lines & unlocking
;; powerful techniques.

(+ 137 349)  ;; is a combination: the parentheses denote procedure application &
;; contain a list of expressions.

(define size 2) ;; names a variable size of value 2.
;; Definitions are one of special forms in that their evaluation does not
;; result in the application of the operator to the operands: they are not
;; combinations.

(define (square x) (* x x)) ;; compound procedure
;; The body can be a sequence of expressions, the value of which is that of
;; the last expression.
(define (sequence-expr x) (+ x 2) (- x 2))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; Substitution Model for procedure application: evaluate all expressions,
;; then evaluate the operator with the operands value instead of its formal
;; parameters.
;; This model follows the applicative-order evaluation; the normal-order
;; evaluation would be to first expand all expressions, then reduce them.
;; This latter model evaluates arguments as many times as they appear in
;; combinations, & is thus inefficient.

;; TODO 1.1.6 p22 (50 in pdf)
