;E2.40, 2.41
(define (enum_n n)
    (define (fun n)
        (cond 
            ((= n 0) '())
            (else (cons n (fun (- n 1))))))
    (reverse (fun n)))

(define (accumulate op initial s)
    (cond 
        ((null? s) initial)
        (else (op (car s) (accumulate op initial (cdr s))))))

(define (flat s)
    (accumulate append '() s))

(define (flatmap proc seq)
    (flat (map proc seq)))

(define (unique_pairs n)
    (flat
        (map 
            (lambda (i)
                (map 
                    (lambda (j)
                        (list j i))
                    (enum_n (- i 1)))) 
            (enum_n n))))

(define (triple n s)
    (filter 
        (lambda (t)
            (and 
                (> (caddr t) (cadr t))
                (<= (caddr t) n)))
        (map 
            (lambda (p) 
                (append p (list (- s (+ (car p) (cadr p))))))
            (unique_pairs n))))