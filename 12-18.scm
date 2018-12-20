;E2.42
(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list '())
            (filter
                (lambda (positions) (safe? positions))
                (flatmap
                    (lambda (rest-of-queens)
                        (map (lambda (new-row)
                            (adjoin-position new-row k rest-of-queens))
                            (enum_n board-size)))
                (queen-cols (- k 1))))))
(queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
    (let 
        ((p (list k new-row)))
        (cons p rest-of-queens)))

(define (safe? positions)
    (cond 
        ((null? positions) #t)
        (else (and (check_safe (car positions) (cdr positions)) (safe? (cdr positions))))))

(define (check_safe p positions)
    (cond 
        ((null? positions) #t)
        (else (and (check_p_safe p (car positions)) (check_safe p (cdr positions))))))

(define (check_p_safe p1 p2)
    (cond 
        ((= (cadr p1) (cadr p2)) #f)
        ((= (abs (/ (- (car p1) (car p2)) (- (cadr p1) (cadr p2)))) 1) #f)
        (else #t)))

(define (accumulate op initial s)
        (cond 
            ((null? s) initial)
            (else (op (car s) (accumulate op initial (cdr s))))))
    
(define (flat s)
    (accumulate append '() s))
    
(define (flatmap proc seq)
    (flat (map proc seq)))

(define (enum_n n)
    (define (fun n)
        (cond 
            ((= n 0) '())
            (else (cons n (fun (- n 1))))))
    (reverse (fun n)))