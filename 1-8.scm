;E2.56
(define (deriv expr var)
    (cond 
        ((number? expr) 0)
        ((variable? expr) 
            (if (same-variable? expr var)
                1
                0))
        ((sum? expr) (make-sum
            (deriv (addend expr) var)
            (deriv (augend expr) var)))
        ((product? expr) (make-sum
            (make-product (deriv (multiplier expr) var) (multiplicand expr))
            (make-product (multiplier expr) (deriv (multiplicand expr) var))))
        ((exp? expr) (make-product
            (make-product 
                (expn expr)
                (make-exp (base expr) (make-sum (expn expr) -1)))
            (deriv (base expr) var)))
        (else (error "deriv" "unknown type"))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) 
    (cond 
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((eq? a1 0) a2)
        ((eq? a2 0) a1)
        ((or (sum? a1) (sum? a2))
            (cond 
                ((and (sum? a1) (sum? a2)) (append a1 (cdr a2)))
                ((sum? a2) (cons '+ (cons a1 (cdr a2))))
                ((sum? a1) (append a1 (list a2)))))
        (else (list '+ a1 a2))))

(define (make-product m1 m2) 
    (cond 
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((or (eq? m1 0) (eq? m2 0)) 0)
        ((eq? m1 1) m2)
        ((eq? m2 1) m1)
        ((or (product? m1) (product? m2))
            (cond 
                ((and (product? m1) (product? m2)) (append m1 (cdr m2)))
                ((product? m2) (cons '* (cons m1 (cdr m2))))
                ((product? m1) (append m1 (list m2)))))
        (else (list '* m1 m2)))
)

(define (make-exp base expn)
    (cond 
        ((or (eq? base 0) (eq? base 1)) base)
        ((eq? expn 0) 1)
        ((eq? expn 1) base)
        (else (list '^ base expn))))

(define (exp? x)
    (and (pair? x) (eq? (car x) '^)))

(define (base exp)
    (cadr exp))

(define (expn exp)
    (caddr exp))

(define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) 
    (cond
        ((> (length s) 3) (cons '+ (cddr s)))
        (else (caddr s))))

(define (product? x)
    (and (pair? x) (eq? (car x) '*)))
    
(define (multiplier p) (cadr p))

(define (multiplicand p) 
    (cond
        ((> (length p) 3) (cons '* (cddr p)))
        (else (caddr p))))