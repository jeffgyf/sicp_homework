(define (element-of-set? x set)
    (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
;O(n)

(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))
;O(n)

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
            (cons (car set1)
                (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;O(n^2)

;E2.59
(define (union-set set1 set2)
    (cond 
        ((null? set1) set2)
        ((element-of-set? (car set1) set2)
            (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))
;O(n^2)

;E2.60
(define (element-of-dset? x set)
    (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-dset? x (cdr set)))))
;O(n)

(define (adjoin-dset x set)
    (cons x set))
;O(1)

(define (remove-from-set x set)
    (cond 
        ((null? set) '())
        ((equal? x (car set)) 
            (cdr set))
        (else (cons (car set) (remove-from-set x (cdr set))))))
;O(n)

(define (intersection-dset set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
            (cons (car set1)
                (intersection-dset (cdr set1) (remove-from-set (car set1) set2))))
        (else (intersection-dset (cdr set1) set2))))
;O(n^2)

(define (union-dset set1 set2)
    (cond 
        ((null? set1) set2)
        ((element-of-set? (car set1) set2)
            (cons (car set1) (union-set (cdr set1) (remove-from-set (car set1) set2))))
        (else (cons (car set1) (union-set (cdr set1) set2)))))
;O(n^2)
;useful when not only existance but quantity also matters

;2.61
(define (element-of-oset? x set)
    (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-oset? x (cdr set)))))
        
(define (intersection-oset set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                (cons x1
                    (intersection-oset (cdr set1)
                    (cdr set2))))
                ((< x1 x2)
                    (intersection-oset (cdr set1) set2))
                ((< x2 x1)
                    (intersection-oset set1 (cdr set2)))))))

(define (adjoin-oset x set)
    (cond 
        ((null? set) (cons x set))
        ((> x (car set)) (adjoin-oset x (cdr set)))
        ((< x (car set)) (cons x set))
        (else set)))
;O(n/2)

(define (union-oset set1 set2)
    (cond 
        ((null? set1) set2)
        ((null? set2) set1)
        (else (let 
            ((x1 (car set1))
            (x2 (car set2)))
            (cond 
                ((= x1 x2) (cons x1 (union-oset (cdr set1) (cdr set2))))
                ((> x1 x2) (cons x2 (union-oset set1 (cdr set2))))
                ((< x1 x2) (cons x1 (union-oset (cdr set1) set2))))))))
;O(m+n)

;E2.65
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))
(define (tree->list tree)
    (define (add-tree-to-list tree l)
        (cond 
            ((null? tree) l)
            (else 
                (add-tree-to-list 
                    (left-branch tree)
                    (cons 
                        (entry tree) 
                        (add-tree-to-list (right-branch tree) l))))))
    (add-tree-to-list tree '()))

(define (list->tree elements)
    (define (partial-tree elts n)
    (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
    (let ((left-result (partial-tree elts left-size)))
    (let ((left-tree (car left-result))
    (non-left-elts (cdr left-result))
    (right-size (- n (+ left-size 1))))
    (let ((this-entry (car non-left-elts))
    (right-result (partial-tree (cdr non-left-elts)
    right-size)))
    (let ((right-tree (car right-result))
    (remaining-elts (cdr right-result)))
    (cons (make-tree this-entry left-tree right-tree)
    remaining-elts))))))))
    (car (partial-tree elements (length elements))))

(define (union-tset set1 set2)
    (let 
        ((oset1 (tree->list set1))
        (oset2 (tree->list set2)))
        (list->tree (union-oset oset1 oset2))))

(define (intersection-tset set1 set2)
    (let 
        ((oset1 (tree->list set1))
        (oset2 (tree->list set2)))
        (list->tree (intersection-oset oset1 oset2))))