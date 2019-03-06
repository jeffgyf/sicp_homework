;3.17
(define (count-pairs l)
    (define counted_list '())
    (define (count l)
        (cond 
            ((not (pair? l)) 0)
            ((memq l counted_list) 0)
            (else 
                (set! counted_list (cons l counted_list))
                (+ (count (car l)) (count (cdr l)) 1))))
    (count l))

;3.18
(define (cycle? l)
    (define checked_list '())
    (define (check l)
        (cond 
            ((null? l) #f)
            ((memq l checked_list) #t)
            (else
                (set! checked_list (cons (car l) checked_list)) 
                (check (cdr l)))))
    (check l))

;3.19初见杀面试问题
(define (cycle? l)
    (define (check p1 p2)
        (cond 
            ((or (null? p2) (null? (cdr p2))) #f)
            ((eq? p1 p2) #t)
            (else (check (cdr p1) (cddr p2)))))                
    (cond
        ((null? l) #f)
        (else (check l (cdr l)))))