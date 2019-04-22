;3.54
(define factorials
    (cons-stream 1 (mul-streams factorials integers)))

;3.55
(define (partial-sums s)
    (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

;3.56
(define (merge-streams s1 s2)
    (cond 
        ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
            (let 
                ((s1car (stream-car s1))
                (s2car (stream-car s2)))
                (cond 
                    ((< s1car s2car)
                        (cons-stream s1car (merge-streams (stream-cdr s1) s2)))
                    ((> s1car s2car)
                        (cons-stream s2car (merge-streams s1 (stream-cdr s2))))
                    (else
                        (cons-stream s1car
                            (merge-streams (stream-cdr s1)
                            (stream-cdr s2)))))))))
;E3.59
(define S
    (cons-stream 1 (merge-streams (scale-stream S 2) (merge-streams (scale-stream S 3) (scale-stream S 5)))))

(define (integrate-series s)
    (div-streams s integers))

(define exp-series
    (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
    (cons-stream 1 (integrate-series sine-series)))

(define sine-series
    (cons-stream 0 (integrate-series (scale-stream cosine-series -1))))


;E3.60
(define (mul-series s1 s2)
    (let 
        ((scar1 (stream-car s1))
        (scar2 (stream-car s2)))
        (cons-stream 
            (* scar1 scar2)
            (add-streams 
                (add-streams 
                    (scale-stream (stream-cdr s1) scar2)
                    (scale-stream (stream-cdr s2) scar1))
                (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))))))

(add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series))

;E3.61
(define (invert-unit-series s)
    (cons-stream 1 (scale-stream (mul-series (stream-cdr s) (invert-unit-series s)) -1)))

;E3.62
(define (div-series s1 s2)
    (cond 
        ((= 0 (stream-car s2)) (error "div-series" "divide by 0"))
        (else 
            (mul-series
                s1
                (scale-stream 
                    (invert-unit-series (scale-stream s2 (/ 1(stream-car s2))))
                    (stream-car s2))))))

(define tan-series
    (div-series sine-series cosine-series))