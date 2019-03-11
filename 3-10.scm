;3.29
(define (or-gate a1 a2 output)
    (define r1 (make-wire))
    (define r2 (make-wire))
    (define s (make-wire))
    (inverter a1 r1)
    (inverter a2 r2)
    (and-gate r1 r2 s)
    (inverter s output)
    'ok)