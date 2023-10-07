(define (ripple-carry-adder a-list b-list c s-list)
    (let ((c-in-list (map (lambda(x)(make-wire)) a))))
        (map full-adder a-list 
                        b-list 
                        c-in-list
                        s-list
                        (cons c c-in-list)))

(define (ripple-carry-adder-2 a-list b-list c-in s-list)
    (define (iterator a-list b-list c s-list c-out)
        (full-adder (car a) (car d) c (car s) c-out)
           (iterator (cdr a-list) (cdr b-list) c-out (cdr s) (make-wire)))
    (iterator a-list b-list c-in s-list (make-wire)))