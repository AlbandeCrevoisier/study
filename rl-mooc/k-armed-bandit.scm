;k-armed bandits form the classical reinforcement learning toy problem.
;See Julia code for a preatier description.

(import (chibi math stats) (slib charplot))

;10-armed bandit
(define (make-bandit)
 (let ((q-star (random-sample 10 (normal-distribution))))
  (lambda (a) (random-pick (normal-distribution (vector-ref q-star a))))))

;ε-greedy
(define (ε-greedy-pick-action ε action-value-estimates)
 (if (explore? ε)
     (random-action (vector-length action-value-estimates))
     (let ((l (vector->list action-value-estimates)))
      (argmax l (apply max l)))))

(define (explore? ε) (< (random-pick (uniform-distribution 0 1)) ε))

(define (random-action number-of-actions)
  (exact (floor (random-pick (uniform-distribution
                               0
                               (- number-of-actions 0.0001))))))

;I should probably write it from scratch to avoid using max & argmax.
(define (argmax l m)
  (if (= m (car l))
      0
      (+ 1 (argmax (cdr l) m))))

(define (ε-greedy-learn! action reward action-value-estimates action-counts)
 (let ((q (vector-ref action-value-estimates action))
       (n (vector-ref action-counts action)))
  (begin
   (vector-set! action-value-estimates
                action
                ;new <- old + (reward - old)/n
                (if (= n 0)
                    reward
                    (+ q (/ (- reward q) n))))
   (vector-set! action-counts action (+ n 1)))))

;experiment
(define (run-experiment n-steps bandit ε )
 (let ((action-value-estimates (make-vector 10 0))
       (action-counts (make-vector 10 0)))
  (define (run-iter steps)
    (if (= steps 0)
        '()
        (let ((action (ε-greedy-pick-action ε action-value-estimates)))
         (let ((reward (bandit action)))
          (begin
           (ε-greedy-learn! action reward action-value-estimates action-counts)
           (cons (list steps reward) (run-iter (- steps 1))))))))
  (run-iter n-steps)))

;run experiment & plot result
(define (run n-steps ε)
 (plot (run-experiment n-steps (make-bandit) ε) "Steps" "Rewards"))
