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

(define (adjoin-set x set)
    (cond 
        ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else 
            (cons (car set)
            (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let 
            ((pair (car pairs)))
            (adjoin-set 
                (make-leaf 
                    (car pair) ; symbol
                    (cadr pair)) ; frequency
                (make-leaf-set (cdr pairs))))))            

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

;E2.69
(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
    (cond 
        ((null? (cdr leaf-set)) (car leaf-set))
        (else 
            (let 
                ((l1 (car leaf-set))
                (l2 (cadr leaf-set)))
                (let 
                    ((l (make-code-tree l1 l2)))
                    (successive-merge (adjoin-set l (cddr leaf-set))))))))
;(encode '(A D A B B C A) (generate-huffman-tree '((A 4) (B 2) (D 1) (C 1))))

;E2.70
(define rock-tree
    (generate-huffman-tree '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1))))
(define rock-messages
    '((get a job) (sha na na na na na na na na) (get a job) (sha na na na na na na na na) (wah yip yip yip yip yip yip yip yip yip) (sha boom)))
(define (encode-msgs msgs tree)
    (cond
        ((null? msgs) '())
        (else 
            (cons 
                (encode (car msgs) tree)
                (encode-msgs (cdr msgs) tree)))))

;(encode-msgs rock-messages rock-tree)
;85bits huffman
;108bits fixed length
;lower limit 2.3*36=82.8bits