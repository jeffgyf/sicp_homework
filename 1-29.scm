;E2.78
(define (attach-tag type-tag contents)
    (cond 
        ((number? contents) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (if (number? datum)
            'scheme-number
            (error "type-tag" "bad tagged datum" datum))))

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (if (number? datum)
            datum
            (error "type-tag" "bad tagged datum" datum))))

;2.79-2.80
(define (equ? d1 d2)
    (apply-generic 'equ? d1 d2))
(define (=zero? d) 
    (apply-generic '=zero? d))
;scheme-number-package
(put 'equ? '(scheme-number scheme-number) =)
(put '=zero? '(scheme-number) 
    (lambda (n)
        (= n 0)))
;rational-package
(put 'equ? '(rational rational) 
    (lambda (r1 r2)
        (and 
            (= (numer r1) (numer r2))
            (= (denom r1) (denom r2)))))
(put '=zero? '(rational) 
    (lambda (r)
        (and 
            (= (numer r1) 0))))
;complex-package
(put 'equ '(complex complex) 
    (lambda (c1 c2)
        (and 
            (= (magnitude c1) (magnitude c2))
            (= (angle c1) (angle c2)))))
(put '=zero '(complex) 
    (lambda (c)
        (and 
            (= (magnitude c) 0))))

