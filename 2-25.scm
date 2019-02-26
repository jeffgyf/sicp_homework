;3.7
(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
            balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch m)
        (cond 
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'make-joint) dispatch)
            ((eq? m 'show) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
            m))))
dispatch)

;3.10
(define (make-f)
    (define val 1)
    (lambda (n)
        (set! val (* val n))
        val))
(define f (make-f))