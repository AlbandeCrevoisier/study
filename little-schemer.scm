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

(define (rember s l)
  (cond
    ((null? l) '())
    ((eqal? (car l) s) (cdr l))
    (else (cons (car l) (rember s (cdr l))))))

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

(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    (else (and (eqal? (car l1) (car l2))
               (eqlist? (cdr l1) (cdr l2))))))

(define (sero? n)
  (null? n))

(define (edd1 n)
  (cons '() n))

(define (zub1 n)
  (cdr n))

(define (plus n m)
  (cond
    ((sero? m) n)
    (else (edd1 (plus n (zub1 m))))))

(define (set? lat)
  (cond
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat)))))

(define (makeset lat)
  (cond
    ((null? lat) '())
    ((member? (car lat) (cdr lat))
     (makeset (cdr lat)))
    (else (cons (car lat) (makeset (cdr lat))))))

(define (subset? s1 s2)
  (cond
    ((null? s1) #t)
    (else (and (member? (car s1) s2)
               (subset? (cdr s1) s2)))))

(define (eqset? s1 s2)
  (and (subset? s1 s2) (subset? s2 s1)))

(define (intersect? s1 s2)
  (cond
    ((or (null? s1) (null? s2)) #f)
    (else (or (member? (car s1) s2)
              (intersect? (cdr s1) s2)))))

(define (intersect s1 s2)
  (cond
    ((or (null? s1) (null? s2)) '())
    ((member? (car s1) s2)
     (cons (car s1) (intersect (cdr s1) s2)))
    (else (intersect (cdr s1) s2))))

(define (union s1 s2)
  (cond
    ((null? s1) s2)
    ((member? (car s1) s2)
     (union (cdr s1) s2))
    (else (cons (car s1) (union (cdr s1) s2)))))

(define (intersectall l-set)
  (cond
    ((null? (cdr l-set)) (car l-set))
    (else (intersect (car l-set)
                     (intersectall (cdr l-set))))))

(define (a-pair? x)
  (cond
   ((atom? x) #f)
   ((null? x) #f)
   ((null? (cdr x)) #f)
   ((null? (cdr (cdr x))) #t)
   (else #f)))

(define (first p)
  (car p))

(define (second p)
  (cadr p))

(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define (firsts l)
  (cond
   ((null? l) '())
   (else (cons (caar l) (firsts (cdr l))))))
