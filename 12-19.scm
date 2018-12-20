;E2.44
(define (up-split painter n)
    (cond
        ((= n 0) painter)
        (else (below painter (beside (up-split painter (- n 1)) (up-split painter (- n 1)))))))

;E2.45
(define (split grow_dir split_dir)
    (lambda (painter n)
        (define small ((split grow_dir split_dir) painter (- n 1)))
        (grow_dir painter (split_dir small small))))