(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

;-------------------------------
(define (operator exp) (cadr exp))
(define (operands exp) (cdr exp))

                
(define (deriv expr var)
    (cond 
        ((number? expr) 0)
        ((variable? expr) 
            (if (same-variable? expr var)
                1
                0))
        (else ((get 'deriv (list (operator expr))) expr var))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) 
    (cond 
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((eq? a1 0) a2)
        ((eq? a2 0) a1)
        (else (list a1 '+ a2))))

(define (make-product m1 m2) 
    (cond 
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((or (eq? m1 0) (eq? m2 0)) 0)
        ((eq? m1 1) m2)
        ((eq? m2 1) m1)
        (else (list m1 '* m2)))
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
    (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) 
    (cond
        ((> (length s) 3) (cddr s))
        (else (caddr s))))

(define (product? x)
    (and (pair? x) (eq? (cadr x) '*)))
    
(define (multiplier p) (car p))

(define (multiplicand p) 
    (cond
        (else (caddr p))))

(define install-deriv-package
    (lambda () 
        (define (sum-deriv expr var)
            (make-sum
                (deriv (addend expr) var)
                (deriv (augend expr) var)))
        (define (prod-deriv expr var)
            (make-sum
                (make-product (deriv (multiplier expr) var) (multiplicand expr))
                (make-product (multiplier expr) (deriv (multiplicand expr) var))))
        (define (exp-deriv expr var)
            (make-product
                (make-product 
                    (expn expr)
                    (make-exp (base expr) (make-sum (expn expr) -1)))
                (deriv (base expr) var)))
        (put 'deriv '(+) sum-deriv)
        (put 'deriv '(*) prod-deriv)
        (put 'deriv '(^) exp-deriv)))