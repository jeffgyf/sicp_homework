(define global-array '())
(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))
(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (apply-generic op . args)
    (let 
        ((type-tags (map type-tag args)))
        (let 
            ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "No method for these types" "APPLY-GENERIC" (list op type-tags))))))

(define (attach-tag type-tag contents)
    (cons type-tag contents))
(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum" "TYPE-TAG" datum)))
(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum" "CONTENTS" datum)))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
    (define (make-poly variable term-list)
        (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (same-variable? v1 v2)
        (define (variable? x) (symbol? x))
        (and (variable? v1) (variable? v2) (eq? v1 v2)))
    ;; representation of terms and term lists
    (define (adjoin-term term term-list)
        (if (=zero? (coeff term))
            term-list
            (cons term term-list)))
    (define (the-empty-termlist) '())
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list) (null? term-list))
    (define (make-term order coeff) (list order coeff))
    (define (order term) (car term))
    (define (coeff term) (cadr term))
    (define (add-poly p1 p2)
        (if 
            (same-variable? (variable p1) (variable p2))
            (make-poly 
                (variable p1)
                (add-terms 
                    (term-list p1)
                    (term-list p2)))
            (error "Polys not in same var" "ADD-POLY" (list p1 p2))))
    (define (mul-poly p1 p2)
        (if 
            (same-variable? (variable p1) (variable p2))
            (make-poly 
                (variable p1)
                (mul-terms 
                    (term-list p1)
                    (term-list p2)))
            (error "Polys not in same var" "MUL-POLY" (list p1 p2))))
    (define (add-terms L1 L2)
        (cond 
            ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else (let 
                ((t1 (first-term L1)) (t2 (first-term L2)))
                (cond 
                    ((> (order t1) (order t2)) 
                        (adjoin-term
                            t1 
                            (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2)) 
                        (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                (else
                    (adjoin-term
                        (make-term 
                            (order t1)
                            (add (coeff t1) (coeff t2)))
                        (add-terms 
                            (rest-terms L1)
                            (rest-terms L2)))))))))
    (define (mul-terms L1 L2)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms 
                (mul-term-by-all-terms (first-term L1) L2)
                (mul-terms (rest-terms L1) L2))))
    (define (mul-term-by-all-terms t1 L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let 
                ((t2 (first-term L)))
                (adjoin-term
                    (make-term 
                        (+ (order t1) (order t2))
                        (mul (coeff t1) (coeff t2)))
                    (mul-term-by-all-terms t1 (rest-terms L))))))
    (define (zero? p)
        (define (allzero? tlist)
            (cond 
                ((null? tlist) #t)
                (else (and (=zero? (car tlist)) (allzero? (cdr tlist))))))
        (allzero? (map coeff (term-list p))))
    (define (pneg p)
        (define (allneg tlist)
            (cond 
                ((null? tlist) '())
                (else (cons 
                    (make-term (order (car tlist))(neg (coeff (car tlist))))
                    (allneg (cdr tlist))))))
        (tag (make-poly (variable p) (allneg (term-list p)))))
    ;; interface to rest of the system
    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
    (put 'neg '(polynomial) pneg)
    (put '=zero? '(polynomial) zero?)
'done)

(define (=zero? d)
    (cond 
        ((number? d) (zero? d))
        (else (apply-generic '=zero? d))))

(define (add d1 d2)
    (cond 
        ((and (number? d1) (number? d2)) (+ d1 d2))
        (else (apply-generic 'add d1 d2))))

(define (mul d1 d2)
    (cond 
        ((and (number? d1) (number? d2)) (* d1 d2))
        (else (apply-generic 'mul d1 d2))))

(define (neg d)
    (cond 
        ((number? d) (- d))
        (else (apply-generic 'neg d))))

(define (sub d1 d2)
    (add d1 (neg d2)))

(install-polynomial-package)
(define p1 ((get 'make 'polynomial) 'x '((1 1) (0 5))))