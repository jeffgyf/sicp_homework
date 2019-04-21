;E3.38-E3.46 书面
(define (enum a b c)
    (cond 
        ((or (< a 0) (< b 0) (< c 0)) 0)
        ((and (zero? a) (zero? b) (zero? c)) 1)
        (else (+ (enum (- a 1) b c) (enum a (- b 1) c) (enum a b (- c 1))) )))

;E.47
;a
(define (make-semaphore n)
    (define mutex (make-mutex))
    (define (the-semaphore m)
        (mutex 'acquire)
        (cond 
            ((eq? m 'acquire) 
                (cond
                    ((> n 0) 
                        (set! n (- n 1))
                        #f)
                    (else 
                        (mutex 'release)
                        (the-semaphore 'acquire)) ;retry
                            ((eq? m 'release) (set! n (+ n 1)))
            (else (error "undefined op" "the-semaphore" m)))
        (mutex 'release)
    the-semaphore)))))

;b
(define (make-semaphore n)
    (define cell (list n))
    (define (the-semaphore m)
        (cond 
            ((eq? m 'acquire) 
                (if (test-and-set! cell) (the-semaphore 'acquire))
            ((eq? m 'release) (increment! cell))
            (else (error "undefined op" "the-semaphore" m)))
        (mutex 'release)
    the-semaphore)))))

(define (increment! cell);this function should be atomic
    (set-car! cell (+ (car cell) 1)))

(define (test-and-set! cell);this function should be atomic
    (cond 
        ((> (car cell) 0) 
            (set-car! cell (- (car cell) 1))
            #f)
        (else #t)))
