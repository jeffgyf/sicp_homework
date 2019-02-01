;E2.83
;integer
(put 'raise 'scheme-number (lambda (i)
    ((get 'make 'rational) i 1)))
;rational
(put 'raise 'rational (lambda (r)
    ((get 'make 'real) (/ (numer r) (denom r)))))
;real
(put 'raise 'real (lambda (r) 
    ((get 'make=from-real-imag 'complex) r 0)))

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let 
            ((proc (get op type-tags)))
            (if proc
            (apply proc (map contents args))
                (if (= (length args) 2)
                    (let 
                        ((type1 (car type-tags))
                        (type2 (cadr type-tags))
                        (a1 (car args))
                        (a2 (cadr args)))
                        (let 
                            ((t1 (try-raise a1 type2))
                            (t2 (try-raise a2 type1)))
                            (cond 
                                (t1 (apply-generic op t1 a2))
                                (t2 (apply-generic op a1 t2))
                                (else
                                    (error "No method for these types" (list op type-tags))))))
                (error "No method for these types"
                (list op type-tags)))))))
(define (try-raise d type)
    (cond 
        ((eq? (type-tag d) type) d)
        (else 
            (let 
                ((raise-func) (get 'raise (type-tag d)))
                (cond 
                    (raise-func (try-raise (raise-func (contents d))))
                    (else #f))))))