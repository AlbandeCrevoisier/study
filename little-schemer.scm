(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

; lat: list of atoms
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

; rel: relation, a list of pairs.
(define (fun? rel)
  (set? (firsts rel)))

(define (revpair p)
  (build (second p) (first p)))

(define (revrel rel)
  (cond
   ((null? rel) '())
   (else (cons (revpair (car rel))
	       (revrel (cdr rel))))))

(define (inj? fun)
  (fun? (revrel fun)))

(define (rember-f test? a l)
  (cond
   ((null? l) '())
   ((test? (car l) a) (cdr l))
   (else (cons (car l) (rember-f test? a (cdr l))))))

; col: collector
(define (multirember&co a lat col)
  (cond
   ((null? lat)
    (col '() '()))
   ((eq? (car lat) a)
    (multirember&co
     a
     (cdr lat)
     (lambda (newlat seen)
       (col newlat (cons (car lat) seen)))))
   (else
    (multirember&co
     a
     (cdr lat)
     (lambda (newlat seen)
       (col (cons (car lat) newlat) seen))))))

(define (multiinsertLR new oldL oldR lat)
  (cond
   ((null? lat) '())
   ((eq? (car lat) oldL)
    (cons new
	  (cons oldL
		(multiinsertLR new oldL oldR (cdr lat)))))
   ((eq? (car lat) oldR)
    (cons oldR
	  (cons new
		(multiinsertLR new oldL oldR (cdr lat)))))
   (else (cons (car lat)
	       (multiinsertLR new oldL oldR (cdr lat))))))

(define (add1 x)
  (+ x 1))

(define (multiinsertLR&co new oldL oldR lat col)
  (cond
   ((null? lat)
    (col '() 0 0))
   ((eq? (car lat) oldL)
    (multiinsertLR&co new oldL oldR
		      (cdr lat)
		      (lambda (newlat L R)
			(col (cons new (cons oldL newlat)) (add1 L) R))))
   ((eq? (car lat) oldR)
    (multiinsertLR&co new oldL oldR
		      (cdr lat)
		      (lambda (newlat L R)
			(col (cons oldR (cons new newlat)) L (add1 R)))))
   (else (multiinsertLR&co new oldL oldR
			   (cdr lat)
			   (lambda (newlat L R)
			     (col (cons (car lat) newlat) L R))))))

(define (evens-only* l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((even? (car l))
      (cons (car l)
	    (evens-only* (cdr l))))
     (else (evens-only* (cdr l)))))
   (else (cons (evens-only* (car l))
	       (evens-only* (cdr l))))))

(define (evens-only*&co l col)
  (cond
   ((null? l)
    (col '() 1 0))
   ((atom? (car l))
    (cond
     ((even? (car l))
      (evens-only*&co (cdr l)
		      (lambda (newlat P S)
			(col (cons (car l) newlat)
			     (* (car l) P)
			     S))))
     (else (evens-only*&co (cdr l)
			   (lambda (newlat P S)
			     (col newlat
				  P
				  (+ (car l) S)))))))
   (else (evens-only*&co (car l)
			 (lambda (al ap as)
			   (evens-only*&co (cdr l)
					   (lambda (bl bp bs)
					     (col (cons al bl)
						  (* ap bp)
						  (+ as bs)))))))))

(define (sub1 a)
  (- a 1))

(define (pick n lat)
  (cond
   ((eq? n 1) (car lat))
   (else (pick (sub1 n) (cdr lat)))))

(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

; unnatural recursion: does not recure on a part of lat
; partial function: might not terminate
; sorn: symbol or number
(define (keep-looking a sorn lat)
  (cond
   ((number? sorn)
    (keep-looking a (pick sorn lat) lat))
   (else (eq? sorn a))))

(define (shift p)
  (build (first (first p))
    (build (second (first p))
      (second p))))

; pora: pair or atom
(define (align pora)
  (cond
   ((atom? pora) pora)
   ((a-pair? (first pora))
    (align (shift pora)))
   (else (build (first pora)
     (align (second pora))))))

(define (weight* pora)
  (cond
   ((atom? pora) 1)
   (else (+ (* (weight* (first pora)) 2)
            (weight* (second pora))))))

(define (shuffle pora)
  (cond
   ((atom? pora) pora)
   ((a-pair? (first pora))
    (shuffle (revpair pora)))
   (else (build (first pora) (shuffle (second pora))))))

; Beware, for here begins the road to the applicative-order Y combinator.
; We are trying to write length without define.
(define (eternity x)
  (eternity x))

; returns length_0
(lambda (length)
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l)))))))

; returns length_less_or_eq_1
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

; returns length_less_or_eq_1
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 ((mk-length eternity) (cdr l))))))))

; length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 ((mk-length mk-length) (cdr l))))))))

; wrap (mk-length mk-length) in a function:
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1
	     ((lambda (x)
		((mk-length mk-length) x))
	      (cdr l))))))))

; make length appear
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

; extract length
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
	    ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

; applicative-order Y combinator
(define (Y le)
  ((lambda (f) f f)
   (lambda (f)
     (le (lambda (x) ((f f) x))))))

; entry: pair of set & list of same size
(define new-entry build)

(define (mk-entry s l)
  (cond
   ((> (length s) (length l))
    (mk-entry (cdr s) l))
   ((< (length s) (length l))
    (mk-entry s (cdr l)))
   (else (new-entry s l))))

(define (lookup-in-entry name entry entry-f)
  (lookup-in-entry-help
   name
   (first entry)
   (second entry)
   entry-f))

(define (lookup-in-entry-help name names values entry-f)
  (cond
   ((null? names) (entry-f name))
   ((eq? (car names) name)
    (car values))
   (else (lookup-in-entry-help name
           (cdr names)
           (cdr values)
           entry-f))))

(define extend-table cons)

(define (lookup-in-table name table table-f)
  (cond
   ((null? table) table-f)
   (else (lookup-in-entry name
	   (car table)
	   (lambda (name)
	     (lookup-in-table name (cdr table) table-f))))))

(define (expression-to-action e)
  (cond
   ((atom? e) (atom-to-action e))
   (else (list-to-action e))))

(define (atom-to-action a)
  (cond
   ((or (number? a)
        (eq? a #t)
        (eq? a #f)
        (eq? a 'cons)
        (eq? a 'car)
        (eq? a 'cdr)
        (eq? a 'null?)
        (eq? a 'eq?)
        (eq? a 'atom?)
        (eq? a 'zero?)
        (eq? a 'add1)
        (eq? a 'sub1)
        (eq? a 'number?))
    *const)
   (else *identifier)))

(define (list-to-action e)
  (cond
   ((atom? (car e))
    (cond
     ((eq? (car e) 'quote) *quote)
     ((eq? (car e) 'lambda) *lambda)
     ((eq? (car e) 'cond) *cond)))
   (else *application)))

(define (value e)
  (meaning e '()))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*const e table)
  (cond
   ((number? e) e)
   ((eq? e #t) #t)
   ((eq? e #f) #f)
   (else (build 'primitive e))))

(define text-of second)

(define (*quote e table)
  (text-of e))
