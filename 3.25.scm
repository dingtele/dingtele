(define (lookup key table)
    (let ((record (assoc key (cdr table))))
        (if record
            (cdr record) 
            false)))

(define (assoc key records)
    (cond ((null? records) false)
            ((equal? key (caar records)) 
                (car records))
            (else (assoc key (cdr records)))))

(define (lookup key-1 key-2 table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (cdr record)
                     false))
            false)))

(define (lookup keys table)
    (if(null? keys) 
            (cdr table)
        (else (let ((record (assoc (car keys) (cdr table))))
            (if record
                (lookup (cdr keys)(cdad table))
                #f)))))