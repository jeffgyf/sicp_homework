;E3.76
(define (make-smooth input-stream)
    (cons-stream 
        (stream-car input-stream)
        (scale-stream (add-streams input-stream (stream-cdr input-stream)) 0.5)))

(define smooth-data 
    (make-smooth sense-data))

(define zero-crossings
    (stream-map sign-change-detector smooth-data (stream-cdr smooth-data)))

;E3.77
(define (integral delayed-integrand initial-value dt)
    (cons-stream initial-value
        (if (stream-null? integrand)
            the-empty-stream
            (let ((integrand (force delayed-integrand)))
                ((integral (delay (stream-cdr integrand))
                    (+ (* dt (stream-car integrand))
                    initial-value)
                    dt)))))

;E3.78
(define (solve-2nd a b y0 dy0 dt)
    (define y (integral (delay dy) y0 dt))
    (define dy (integral (delay ddy) dy0 dt))
    (define ddy (add-streams (scale-stream a dy) (scale-stream b y)))
    y))

;E3.79
(define (solve-2nd f y0 dy0 dt)
    (define y (integral (delay dy) y0 dt))
    (define dy (integral (delay ddy) dy0 dt))
    (define ddy (stream-map f y dy))
    y)


;E3.81
(define (generate-rand requests)
    (define rand
        (cons-stream 
            init
            (stream-map
                (lambda (m r)
                    (cond 
                        ((eq? m 'generate) update-random(r))
                        ((eq? m 'reset) init)
                        (else (error xxx))))
                (stream-cdr requests)
                rand)))
    rand)

;E3.82
(define (estimate-integral P x1 x2 y1 y2)
    (define random-pairs
        (cons-stream 
            (list (random-in-range x1 x2) (random-in-range y1 y2))
            random-pairs))
    (define check
        (stream-map
            P
            random-pairs))
    (define integral pass total series
        (cond 
            ((stream-car series) 
                (cons-stream 
                    (/ (+ pass 1) (+ total 1))
                    (integral (+ pass 1) (+ total 1) (stream-cdr series))))
            (else 
                (cons-stream 
                    (/ pass (+ total 1))
                    (integral pass (+ total 1) (stream-cdr series))))))
    (integral 0 0 check))