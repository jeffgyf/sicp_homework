(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ exp1 exp2) (cons exp1 (lambda () exp2)))))

(define (memo-proc proc)
    (let ((already-run? false) (result false))
        (lambda ()
            (if (not already-run?)
                (begin (set! result (proc))
                (set! already-run? true)
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



(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
        (stream-map proc (stream-cdr s)))))

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

(define (show x)
    (display-line x)
    x)

(define showx
    ((lambda ()
        (define x 1)
        (lambda ()
            (set! x (+ x 1))
            (show x)
            x)
        )))