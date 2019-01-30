;E2.75
(define (make-from-mag-ang r theta)
    (define (dispatch op)
        (cond 
            ((eq? op 'real-part) (* r (cos theta)))
            ((eq? op 'imag-part) (* r (sin theta)))
            ((eq? op 'magnitude) r)
            ((eq? op 'angle) theta)
            (else (error "make-from-mag-ang" "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
    dispatch)
