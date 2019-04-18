

;3.48
(define (make-account balance id)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
            balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (let ((protected (make-serializer)))
        (define (dispatch m)
            (cond 
                ((eq? m 'withdraw) protected-withdraw)
                ((eq? m 'deposit) protected-deposit)
                ((eq? m 'balance) balance)
                ((eq? m 'id) id)
                ((eq? m 'serializer) balance-serializer)
                (else (error "Unknown request -- MAKE-ACCOUNT"m))))
            dispatch))

(define (serialized-exchange account1 account2)
    (let 
        ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (id1 (account1 'id))
        (id2 (account2 'id)))
        (if (> id1 id2)
            ((serializer1 (serializer2 exchange)) account1 account2)
            ((serializer2 (serializer1 exchange)) account1 account2))))

;3.50
(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                (cons proc (map stream-cdr argstreams))))))
