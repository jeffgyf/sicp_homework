;E2.50
(define (flip-horiz painter)
    (transform-painter
        painter
        (make-vect 1 0)
        (make-vect 0 0)
        (make-vect 1 1)))

(define (rotate-180 painter)
    (transform-painter
        painter
        (make-vect 1 1)
        (make-vect 0 1)
        (make-vect 1 0)))


(define (rotate-270 painter)
    (transform-painter
        painter
        (make-vect 0 1)
        (make-vect 0 0)
        (make-vect 1 1)))

;E2.51
(define (below1 painter1 painter2)
    (define split-point (make-vect 0 0.5))
    (lambda (frame)
        (((transform-painter painter1
            (make-vect 0 0)
            (make-vect 1 0)
            split-point) frame)
        ((transform-painter 
            split-point
            (make-vect 1 0.5)
            (make-vect 0 1)) frame))))

(define (below2 painter1 painter2)
    (define p (rotate-90 (flip-vert painter1)))
    (lambda (frame)
        ((p (beside (p painter1) (p painter2)) frame))))