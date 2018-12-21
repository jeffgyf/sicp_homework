;E2.46
(define (make-vect x y) (list x y))

(define xcor-vect car)

(define ycor-vect cadr)

(define (add-vect v1 v2)
    (make-vect 
        (+ (xcor-vect v1) (xcor-vect v2)) 
        (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
        (make-vect 
            (- (xcor-vect v1) (xcor-vect v2)) 
            (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
            (make-vect 
                (* (xcor-vect v1) s) 
                (* (ycor-vect v1) s)))


;E2.47
;1.
(define orgin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)
;2.
(define orgin-frame car)
(define edge1-frame cdar)
(define edge2-frame caddr)

;E2.48
(define (make-seg start end)
    (list start end))

(define start-seg car)
(define end-seg cadr)

;E2.49
;1.
((segments->painter 
    '(
        ((0 0) (0 1))
        ((0 0) (1 0))
        ((0 1) (1 1))
        ((1 0) (1 1)))) frame)
;2.
((segments->painter 
    '(
        ((0 0) (1 1))
        ((0 1) (1 0)))) frame)
;3.
((segments->painter 
    '(
        ((0.5 0) (1 0.5))
        ((1 0.5) (0.5 1))
        ((0.5 1) (0 0.5))
        ((0 0.5) (0.5 0)))) frame)

