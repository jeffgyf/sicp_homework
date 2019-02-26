;3.1
(define (make-accumulator ivalue)
    (define value ivalue)
    (lambda (n)
        (set! value (+ value n))
        value))

;3.2
(define (make-monitored f)
    (define cnt 0)
    (lambda (m)
        (cond 
            ((eq? m 'how-many) cnt)
            ((eq? m 'reset) (set! cnt 0))
            (else 
                (set! cnt (+ cnt 1))
                (f m)))))

;3.6
(define (rand-update x)
    (mod (* x 10007) 9973))

(define rander
    (let 
        ((x 1))
        (lambda (m)
            (cond 
                ((eq? m 'generate) (set! x (rand-update x))
                                    x)
                ((eq? m 'reset) (lambda (value)
                                (set! x value)))))))
                                
(define (rand m)
    (rander m))