;E2.54
(define (myequal? a b)
    (cond 
        ((or (null? a) (null? b)) (cond
            ((and (null? a) (null? b)) #t)
            (else #f)))
        ((atom? a) (cond 
            ((atom? b) (eq? a b))
            (else #f)))
        (else (cond
            ((atom? b) #f)
            (else (and 
                (myequal? (car a) (car b))
                (myequal? (cdr a) (cdr b))))))))

;E2.55
;Why  (eq? (car '('a)) (cadr '''a)) is #f?