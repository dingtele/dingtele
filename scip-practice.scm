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
  (+  (ite (branch-structure-selector (left-branch-selector mobile)) 0)
      (ite (branch-structure-selector (right-branch-selector mobile)) 0)))