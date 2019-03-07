;3.21
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
    (if 
        (empty-queue? queue)
        (error "front-queue" "FRONT called with an empty queue" queue)
        (car (front-ptr queue))))
(define (insert-queue! queue item)
    (let 
        ((new-pair (cons item '())))
        (cond 
            ((empty-queue? queue)
                (set-front-ptr! queue new-pair)
                (set-rear-ptr! queue new-pair)
                (print-queue queue))
            (else
                (set-cdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                (print-queue queue)))))
(define (delete-queue! queue)
    (cond 
        ((empty-queue? queue)
            (error "delete-queue!" "DELETE! called with an empty queue" queue))
        (else
            (set-front-ptr! queue (cdr (front-ptr queue)))
    (print-queue queue))))
;;;;;;;;;;;;
(define (print-queue queue)
    (front-ptr queue))

;3.22
(define (make-queue)
    (define front-ptr '())
    (define rear-ptr '())
    (define (empty?) (null? front-ptr))
    (define (print) front-ptr)
    (define (front) 
        (cond 
            ((empty?) (error "front" "queue is EMTPY"))
            (else (car front-ptr))))
    (define (push e)
        (define tmp (cons e '()))
        (if 
            (empty?)
            (set! front-ptr tmp)
            (set-cdr! rear-ptr tmp))
        (set! rear-ptr tmp)
        (print))
    (define (pop)
        (cond 
            ((empty?) (error "pop" "queue is EMPTY"))
            (else 
                (set! front-ptr (cdr front-ptr))
                (print))))
    (define (dispatch m)
        (cond 
            ((eq? m 'front) front)
            ((eq? m 'push) push)
            ((eq? m 'pop) pop)
            (else (error "queue" "no such op"))))
    dispatch)

;3.23需要双向链表
;写之前想好要写什么！！！