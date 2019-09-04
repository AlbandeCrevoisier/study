(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (lat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

(define (member? a lat)
  (cond
    ((null? lat) #f)
    (else (or (eq? (car lat) a)
              (member? a (cdr lat))))))

(define (rember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) a) (cdr lat))
    (else (cons (car lat) (rember a (cdr lat))))))

(define (member* a l)
  (cond
    ((null? l) #f)
    ((atom? (car l))
     (or (eq? (car l) a)
         (member* a (cdr l))))
    (else (or (member* a (car l))
              (member* a (cdr l))))))

(define (eqan? a1 a2)
  (cond
    ((and (number? a1) (number? a2))
     (= a1 a2))
    ((or (number? a1) (number? a2))
     #f)
    (else (eq? a1 a2))))

(define (eqal? s1 s2)
  (cond
    ((and (atom? s1) (atom? s2))
     (eqan? s1 s2))
    ((or (atom? s1) (atom? s2))
     #f)
    (else (eqlist? s1 s2))))
