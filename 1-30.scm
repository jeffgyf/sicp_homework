;E2.82
;assume (get op) returns list of argument type lists
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let 
            ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (let 
                    ((coercions (get-coercions type-tags (get op))))
                    (let 
                        ((converted-args (apply-coercions args coercions)))
                        (apply-generic op converted-args)))))))


(define (get-coercions type-tags required-types)
    (cond 
        ((null? type-tags) '())
        (else 
            (let 
                ((coercion (get-coercion (car type-tags) (car required-types))))
                (cond 
                    (coercion (cons coercion (get-coercions (cdr type-tags) (cdr required-types))))
                    (else (error "get-coercions" "No coercion for" (list (car type-tags) (car required-types)))))))))