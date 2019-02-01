;E2.82
;assume (get op) returns list of argument type lists
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let 
            ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (let 
                    ((coercions (try-get-coercions type-tags (get op))))
                    (let 
                        ((converted-args (apply-coercions args coercions)))
                        (apply-generic op converted-args)))))))


(define (try-get-coercions type-tags types-list)
    (cond 
        ((null? types-list) (error "try-get-coercions" "No coercion for" (list (car type-tags) (car required-types))))
        ((get-coercions type-tags (car types-list)))
        (else (try-get-coercions type-tags (cdr types-list)))))

(define (get-coercions type-tags required-types)
    (cond 
        ((null? type-tags) '())
        (else 
            (let 
                ((coercion (get-coercion (car type-tags) (car required-types))))
                (cond 
                    (coercion (cons coercion (get-coercions (cdr type-tags) (cdr required-types))))
                    (else #f))))))

(define (apply-coercions args coercions)
    (cond 
        ((null? args) '())
        (else 
            (cons 
                ((car coercions) (car args)) 
                (apply-coercions (cdr args) (cdr coercions))))))