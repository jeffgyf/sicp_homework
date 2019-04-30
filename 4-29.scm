;3.67
(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave
            (interleave
                (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t))
                (stream-map (lambda (x) (list x (stream-car t)))
                    (stream-cdr s))
                    )
        (pairs (stream-cdr s) (stream-cdr t)))))

;3.69
(define (triples i j k)
    (let 
        ((pjk (stream-map (lambda (x) (cons (stream-car i) x)) (pairs j k))))
        (cons-stream
            (stream-car pjk)
            (interleave
                (stream-cdr pjk)
                (triples (stream-cdr i) (stream-cdr j) (stream-cdr k))))))

(define pythagorean
    (stream-filter 
        (lambda (s)
            (let 
                ((i (car s))
                (j (cadr s))
                (k (caddr s)))
                (= (+ (* i i) (* j j)) (* k k))))
        (triples integers integers integers)))

(define (get-iterator s)
    (lambda ()
        (define r (stream-car s))
        (set! s (stream-cdr s))
        r))

;3.70
(define (merge-weighted s1 s2 W)
    (cond 
        ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
            (let 
                ((s1car (stream-car s1))
                (s2car (stream-car s2)))
                (cond 
                    ((<= (W s1car) (W s2car))
                        (cons-stream s1car (merge-weighted (stream-cdr s1) s2 W)))
                    (else
                        (cons-stream s2car (merge-weighted s1 (stream-cdr s2) W)))
                    )))))

(define (weighted-pairs s t W)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (merge-weighted
            (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
            (weighted-pairs (stream-cdr s) (stream-cdr t) W)
            W)))

;a
(weighted-pairs integers integers (lambda (s) (apply + s)))
;b
(weighted-pairs 
    integers 
    integers 
    (lambda (s) 
        (let 
            ((i (car s))
            (j (cadr s)))
            (+ (* 2 i) (* 3 j) (* 5 i j)))))

;3.71
(define pre-ramanujan-series 
        (weighted-pairs 
            integers 
            integers 
            (lambda (s) 
                (let 
                    ((i (car s))
                    (j (cadr s)))
                    (+ (* i i i) (* j j j))))))
(define foo
    (lambda (s) 
                (let 
                    ((i (car s))
                    (j (cadr s)))
                    (+ (* i i i) (* j j j)))))
(define ramanujan-series
    ((lambda ()
        (define (find-next s pre)
            (define s1 (stream-car s))
            (let 
                ((cur (foo s1)))
                (begin
                    (cond 
                        ((= cur pre)
                            (cons-stream 
                                cur
                                (find-next (stream-cdr s) 0)))
                        (else (find-next (stream-cdr s) cur))))))
        (find-next pre-ramanujan-series 0))))

;3.72
;pass