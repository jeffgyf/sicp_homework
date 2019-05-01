;3.73

(define (RC R C dt)
    (define (RC-series v0 i)
        (define series
            (cons-stream v0
                (add-streams
                    series
                    (scale-stream
                        i
                       (/ dt C)))))
        (add-streams series (scale-stream i R)))
    RC-series)