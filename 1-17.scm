(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (make-leaf symbol weight)
    (list 'leaf symbol weight))
(define (leaf? o)
    (eq? (car o) 'leaf))
(define (symbol-leaf o) (cadr o))
(define (weight-leaf o) (caddr o))

(define (symbol o)
    (cond 
        ((leaf? o) (list (symbol-leaf o)))
        (else (caddr o))))

(define (weight o)
    (cond 
        ((leaf? o) (weight-leaf o))
        (else (cadddr o))))

(define (make-code-tree left right)
    (list
        left
        right
        (append (symbol left) (symbol right))
        (+ (weight left) (weight right))))

;E2.67
(define sample-tree
    (make-code-tree (make-leaf 'A 4)
        (make-code-tree
            (make-leaf 'B 2)
            (make-code-tree (make-leaf 'D 1)
                (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;ADABBCA

;E2.68
(define (elementof? s l)
    (cond 
        ((null? l) #f)
        ((eq? s (car l)) #t)
        (else (elementof? s (cdr l)))))

(define (encode-symbol s tree)
    (define (encode-s s tree)
        (cond 
            ((leaf? tree) '())
            ((elementof? s (symbol (left-branch tree)))
                (cons 0 (encode-s s (left-branch tree))))
            (else 
                (cons 1 (encode-s s (right-branch tree))))))
    (cond 
        ((elementof? s (symbol tree)) (encode-s s tree))
        (else (error 'encode-symbol "symbol not exist" s))))

(define (encode l tree)
    (cond 
        ((null? l) '())
        (else (append 
            (encode-symbol (car l) tree)
            (encode (cdr l) tree)))))