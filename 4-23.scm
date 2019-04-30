;3.66
;(x,x)=2^x-1
;(x,y)=2^x-1+2^x(y-x)-2^(x-1) where y>x

(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave
            (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
                (pairs (stream-cdr s) (stream-cdr t)))))

