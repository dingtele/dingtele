(define (averager a b c)
     (let ((v (make-connector))
            (u (make-connector)))
        (adder a b v)
        (multiplier v u c)
        (constant 0.5 v)))

 (define A (make-connector)) 
 (define B (make-connector)) 
 (define C (make-connector)) 
 (averager A B C) 
  
 (set-value! A 100 'user) 
 (set-value! B 0 'user) 
 ; 50 