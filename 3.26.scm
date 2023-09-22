(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))



(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (let ((record 
             (assoc (car key) (cdr local-table))))
        (if record
            ¡]cond (= (car key) (en
       
            (lookup ((record 
                   (assoc key-2  
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr! 
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))
