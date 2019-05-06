(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ exp1 exp2) (cons exp1 (lambda () exp2)))))

(define (memo-proc proc)
    (let ((already-run? #f) (result #f))
        (lambda ()
            (if (not already-run?)
                (begin (set! result (proc))
                (set! already-run? #t)
                result)
                result))))

(define stream-car
  (lambda (s)
    (car s))) 

(define stream-cdr
  (lambda (s)
    (force (cdr s)))) 

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))



(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                (cons proc (map stream-cdr argstreams))))))

(define the-empty-stream '())

(define stream-null? null?)

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream
            low
            (stream-enumerate-interval (+ low 1) high))))


(define (display-line x)
    (display x)
    (newline))

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin 
            (proc (stream-car s))
            (stream-for-each proc (stream-cdr s)))))

(define (display-stream s n)
    (cond 
        ((> n 0)
            (display (stream-car s))
            (display " ")
            (display-stream (stream-cdr s) (- n 1)))))

(define (stream-filter pred s)
    (cond 
        ((stream-null? s) the-empty-stream)
        ((pred (stream-car s)) (cons-stream (stream-car s) (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (stream-enumerate-interval a b)
    (cond 
        ((> a b) the-empty-stream)
        (else (cons-stream a (stream-enumerate-interval (+ a 1) b)))))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define ones
    (cons-stream 1 ones))

(define integers 
    (cons-stream 1 (add-streams integers ones)))

(define (mul-streams s1 s2)
    (stream-map * s1 s2))

(define (div-streams s1 s2)
    (stream-map / s1 s2))


(define (partial-sums s)
    (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

(define (scale-stream s n)
    (stream-map (lambda (e) (* e n)) s))

(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
            (interleave s2 (stream-cdr s1)))))

(define (integral integrand initial-value dt)
    (define int
        (cons-stream 
            initial-value
            (add-streams 
                (scale-stream integrand dt)
                int)))
    int)