;2.85
;complex
(put 'project 'complex (lambda (c)
    ((get 'make 'real) (real-part c))))
;real
(put 'project 'real (lambda (r) 
    ((get 'make 'scheme-number) (round r))

;rational
(put 'project 'rational (lambda (r)
    ((get 'make 'scheme-number)  (/ (numer r) (denom r)))))

    
(define (drop d)
    (let 
        ((project (get 'project (list (tag d))))
        (cond 
            (project 
                (let 
                    ((projected (project d)))
                    (cond 
                        ((equ? (raise projected) d) (drop projected))
                        (else d))))
            (else d)))))

;2.86
(define (install-complex-package)
    ;; imported procedures from rectangular and polar packages
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))
    ;; internal procedures
    (define (add-complex z1 z2)
        (make-from-real-imag (apply-generic 'add (real-part z1) (real-part z2))
        (apply-generic 'add (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag (apply-generic 'sub (real-part z1) (real-part z2))
        (- (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
        (make-from-mag-ang (apply-generic 'mul (magnitude z1) (magnitude z2))
        (apply-generic 'add (angle z1) (angle z2))))
    (define (div-complex z1 z2)
        (make-from-mag-ang (apply-generic 'div (magnitude z1) (magnitude z2))
        (- (angle z1) (angle z2))))
    ;; interface to rest of the system
    (define (tag z) (attach-tag 'complex z))
        (put 'add '(complex complex)
            (lambda (z1 z2) (tag (add-complex z1 z2))))
        (put 'sub '(complex complex)
            (lambda (z1 z2) (tag (sub-complex z1 z2))))
        (put 'mul '(complex complex)
            (lambda (z1 z2) (tag (mul-complex z1 z2))))
        (put 'div '(complex complex)
            (lambda (z1 z2) (tag (div-complex z1 z2))))
        (put 'make-from-real-imag 'complex
            (lambda (x y) (tag (make-from-real-imag x y))))
        (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)