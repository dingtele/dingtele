#lang typed/scheme

(define (expt b n)
  (fast-expt b n 1))

(define (fast-expt b n product)
    (cond ((= n 0) product)
          ((even? n) (fast-expt (* b b) (/ n 2) product))
          (else (fast-expt b (- n 1) (* product b)))))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))

  (define (term a)
    (+ (f a) (* 4 f (+ a h))) (f (+ a (* 2 h))))
  (define (next a)
    (+ a (* 2 h)))
  (* (sum term a next (- b (* 2 h)))
      (/ h 3)))

  (define (sum term a next b)
    (define (iter a result)
      (if (= a b)
          result
          (iter (next a) (+ result (term a)))))
    (iter a 0))
(define (accumulator combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (accumulator combiner null-value term (next a) next b))))
  (iter a 0))

;; recursive
(define (accumulator2 combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a) (accumulator2 combiner null-value term (next a) next b))))
;; iterative
(define (accumulator1 combiner null-value term a next b)
  (define (iteraor a res)
    ((if (> a b) res
          (iteraor (next a) (combiner (term a) res)))))
  (iteraor a null-value))

  (define (f x y)
  ((lambda (a b)
  (+ (* x (square a))(* y b)(* a b)))
  (+ 1 (* x y))(- 1 y)))

  (define (f x y)
    (let ((a (+ 1 (* x y)))
          (b (- 1 y)))
         (+ (* x (square a))(* y b)(* a b))))


(newtons-method (cubic a b c)
                1)
(define (cubic a b c)
  (lambda (x) 
          (+  (* x (square x))
              (* a
                (square x))
              (* b x)
              c)))

(((double (double double)) inc) 5)
((compose square inc) 6)
(define (compose f g)
  (lambda (x) (f (g x))))

((repeated square 2) 5)

(define (repeated f times)
  (if (times > 0)
    (compose f (repeated f (- times 1)))
    (lambda (x) x)))

(define (repeated f times)
  (define (ite res i) 
      (if (< i 1)
          res
          (ite (compose f res) (- i 1)))))

(define dx 0.00001)
(define (smooth f)
  (lambda (x) 
    (/ (+ (f (- x dx))
        (f x)
        (f (+ x dx)))
        3)))
(define (n-fold-smooth f n)
  ((repeated smooth n) f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))(* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (denom-sign (if (> d 0) 1 (- 1))))
    (cons (* (/ n g) denom-sign)
          (* (/ d g) denom-sign))))

(define (make-segment sp ep)
  (cons sp ep))
(define (start-segment l)
  (car l))
(define (end-segment l)
  (cdr l))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (midpoint-segment segment)
  (define (average a b) (/ (+ a b) 2.0))
 (let ((sp (start-segment segment))
        (ep (end-segment segment)))
      (make-point (average (x-point sp) (x-point ep))
                  (average (y-point sp) (y-point ep)))))

(define (rectangle-1 p1 p2)
  (cons p1 p2))

(define (selector-1 rectangle-1)
  (car rectangle-1))
(define (selector-2 rectangle-1)
(cdr rectangle-1))

(define (segment-length idx1 idx2)
  (abs (- idx1 idx2)))
(define (x-length rec)
  (segment-length (x-point (selector-1 rec)) (x-point (selector-2 rec))))
(define (y-length rec)
  (segment-length (y-point (selector-1 rec)) (y-point (selector-2 rec))))

(define (perimiter rectangle-1)
  (* (+ (x-length rectangle-1)
      (y-length rectangle-1))
      2))

(define (area rectangle-1)
  (* (x-length rectangle-1)
      (y-length rectangle-1)))

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(define (cons-product x y)
  (* (expt 2 x)
     (expt 3 y)))
(define (car-prod z)
  (define (ite-car cnt)
   (if (= (remainder z 2) 0)
     (ite-car (/ z 2) (+ cnt 1))
     cnt))
     (ite-car 0))

(define (make-interval a b) (cons a b))
(defne (lower-bound interval)
 (car interval))
(defne (upper-bound interval)
 (cdr interval))

(define (add-interval x y)
(make-interval (+ (lower-bound x) (lower-bound y))
(+ (upper-bound x) (upper-bound y))))

(define (div-interval x y)
(mul-interval
x
(make-interval (/ 1.0 (upper-bound y))
(/ 1.0 (lower-bound y)))))

(define (last-pair alist)
	(define (ite the-list cnt)
		(if (= cnt (- (length alist) 1))
			the-list
		(ite (cdr the-list) (+ cnt 1))))
	(ite alist 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define nil '()) 
  
 (define (reverse items) 
   (if (null? (cdr items)) 
       items 
       (append (reverse (cdr items)) 
               (cons (car items) nil)))) 
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (same-parity . z)
	(define (find-parity input even-or-odd)
		(if (null? input)
			(list)
			(if (even-or-odd (car input))
				(cons (car input) (find-parity (cdr input) even-or-odd))
				(find-parity (cdr input) even-or-odd))))
	(if (even? (car z))
		(find-parity z even?)
		(find-parity z odd?)))


(define (square-list items)
	(if (null? items) 
		nil
		(cons (square (car items))(square-list (cdr items)))))

(define (square-list items)(map square items))

(define (for-each proc list)
	(if (null? list)
		#f
		(begin (proc (car list))
				(for-each proc (cdr list)))))

(define (deep-reverse x)
  (if (pair? x)
      (append (deep-reverse (cdr x)) (list (deep-reverse (car x))))
      x))

(define (fringe l)
  (define (ite l res)
    (cond ((null? l)
           res)
          ((not (pair? l)) (cons l res))
          (else (append (ite (car l) res) (ite (cdr l) res)))
          ))
  (ite l '()))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-mobile left right)(list left right))
(define (make-branch length structure)(list length structure))
(define (left-branch-selector mobile)
  (car mobile))
(define (right-branch-selector mobile)
  (car (cdr mobile)))
(define (branch-length-selector branch)
  (car branch))
(define (branch-structure-selector branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (define (ite s t)
    (cond ((number? s)
            (+ s t))
          (else (total-weight s))))
  (if ((not (pair? mobile)) mobile)
      (+  (ite (branch-structure-selector (left-branch-selector mobile)) 0)
      (ite (branch-structure-selector (right-branch-selector mobile)) 0))))

(define (torque branch)
  (*  (branch-length-selector) (totalweight (branch-structure-selector branch))))
(define (isBalanced? mobile)
  (if (not (pair? mobile))
      true
      (and (= (torque (left-branch-selector mobile)) (torque (right-branch-selector mobile)))
            (isBalanced (branch-structure-selector (left-branch-selector mobile)))
            (isBalanced (branch-structure-selector (right-branch-selector mobile))))))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((pair? tree)
              (cons (square-tree (car tree))
                    (square-tree (cdr tree))))
        (else (* tree tree))))

(define (square-tree-2 tree)
  (map (lambda (x) 
                (if (pair? x)
                    (square-tree-2 x)
                    (* x x)) tree)))
   
(define (tree-map function tree)
  (map (lambda (x) 
              (if (pair? x))
                  (tree-map function x)
                  (function x)) 
        tree))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (x)
                                  (cond (car s) x) rest))))))

(define (map p sequence)
  (accumulate (lambda (x y)
                      (cons (p x) y))
              nil 
              sequence))
(define (append seq1 seq2)
  (accumulate cons seq1 seq2))
(define (length sequence)
  (accumulate (lambda (x y)
                      (+ y 1)) 0 sequence))

(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms)
                        (+ (* higher-terms x) this-coeff))
                0
                coefficient-sequence))

(define (count-leaves t)
  (accumulate +
              0 
              (map (lambda(x) 
                    (cond ((null? x) 0)
                          ((pair? x) (count-leaves x))
                          (else 1)))
                    t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x) seqs)))
            (accumulate-n op init (map (lambda (x) (cdr x) seqs))))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) 
        result
        (iter (op result (car rest))(cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence) ;; fold-right
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence))))) 

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
  
(define (reverse sequence)
  (fold-left (lambda (x y) (list y) x) nil sequence))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
            (cons (car sequence)
                  (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (permutations s)
  (if (null? s)
      '()
      (flatten (lambda(x) 
                  (map (lambda(P)(cons x p)) (permutations (remove x s))))
                s)))

(define (enumerate-elm low high) 
  (if (> low high)
      nil
      (cons low (enumerate-elm (+ low 1) high))))

(define (unique-pairs n)
  (flatmap (lambda (x) 
              (map (lambda(y) 
                          (list x y))
                    (enumerate-elm 1 (- x 1))))
      (enumerate-elm 1 n)))
(define (prime-sum? p)
  (prime? (+ (car p) (cdr p)))) 

(define (make-pairs p)
  (list (car p) (cdr p) (+ (car p) (cdr p))))

 (define (prime-sum-pairs n) 
  (map make-pairs (filter prime-sum? (unique-pairs n))))

(define (ordered-triples n)
  (flatmap (lambda (x) 
              (flatmap (lambda(y) 
                          (map (lambda(z)(list x y z))
                                          (enumerate-interval 1 (- y 1))))
                        (enumerate-interval 1 (- x 1))))
      (enumerate-interval 1 n)))

 (define (find-ordered-triples n s) 
  (filter (lambda(x)(= (accumulate + 0 x) s)) (ordered-triples n)))

(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list empty-board)
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
					(lambda (rest-of-queens)
						(map (lambda (new-row)
								(adjoin-position new-row k rest-of-queens))
							(enumerate-interval 1 board-size)))
					(queen-cols (- k 1))))))
	(queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
	(cons (list new-row k) rest-of-queens))

(define empty-board
	())
(define (safe? k positions)
	(if (= )))

;;what those old greeks, whom one must also credit with a little knowledge of philosophy, took to be the task of a whole lifetime, doubt not being a skill one acquires in days and weeks; what the old veteran warrior achieved after keeping the balance of doubt in the face of all inveiglement, fearlessly reject the certainties of sense and thought, incorruptibly defying the selfish anxieties and the wheedling of sympathies

(flatmap
	(lambda (rest-of-queens)
		(map (lambda (new-row)
				(cons (list new-row 2) rest-of-queens))
			(enumerate-elm 1 4)))
	'(((1 1)) ((2 1)) ((3 1)) ((4 1))))

	((1 1)) ((2 1)) ((3 1)) ((4 1))

'(((1 2) ((1 1)) ((2 1)) ((3 1)) ((4 1)))
  ((2 2) ((1 1)) ((2 1)) ((3 1)) ((4 1)))
  ((3 2) ((1 1)) ((2 1)) ((3 1)) ((4 1)))
  ((4 2) ((1 1)) ((2 1)) ((3 1)) ((4 1))))

  (define (equals? a b)
    (cond ((not (pair? a b)) (eq? a b))
          ((and (pair? a) (pair? b))(and (equals? (car a) (car b)) (equals? (cdr a) (cdr b))))
          (else false)))


        (define (deriv exp var)(cond ((number? exp) 0)((variable? exp) (if (same-variable? exp var) 1 0))((sum? exp) (make-sum (deriv (addend exp) var)(deriv (augend exp) var)))((product? exp)(make-sum(make-product (multiplier exp)(deriv (multiplicand exp) var))(make-product (deriv (multiplier exp) var)(multiplicand exp))))(else(error "unknown expression type: DERIV" exp))))


;; constructor
(define (make-vect x y)
  (cons x y))
;; selector
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
;;arithmetic
(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2))
                              (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2))
                              (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v1 s) (make-vect (* (xcor-vect v1) s)
                              (* (ycor-vect v1) s)))

(define (make-frame origin edge1 edge2) 
  (cons origin (cons edge1 edge2)))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (cddr f))
(define (origin-frame f) (car f))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
                      (draw-line((frame-coord-map frame)(start-segment segment))((frame-coord-map frame)(end-segment segment)))) 
              segment-list)))

(define (make-segment v1 v2) ())
(define (start-segment) ())
(define (end-segment) ())

(define (base-frame f) (edg2-frame f))
(define (left-frame f) (edge1-frame f))
(define (right-frame f) ())
(define (outline-painter) ())

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
    (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                 (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (transform-painter painter origin corner1 corner2)
    (lambda (frame)
        (let ((m (frame-coord-map frame)))
            (let ((new-origin (m origin)))
                (painter (make-frame new-origin
                                    (sub-vect (m corner1) new-origin)
                                    (sub-vect (m corner2) new-origin)))))))

(define (element-of-set? x set)
(cond ((null? set) false)
((equal? x (car set)) true)
(else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((element-of-sets? (car set1) set2) (union-set (cdr set1) set2))
          (else (union-set (cdr set1) (cons (car set1) set2)))))


(define install-get-record
  (define (records file) (cadr file))
  (define (get-cus-record name records))
    (cond ((null? file) "ERROR")
          ((eq? name (get-cust-name (car (records file))))
              (car records))
          (else (get-cus-record name (cdr records))))
  (define (get-cust-name record)
    (car record)))