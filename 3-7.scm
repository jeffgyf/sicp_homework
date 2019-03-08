;3.23需要双向链表
(define (make-node e)
    (define val e)
    (define prev '())
    (define next '())
    (define (set-prev! p)
        (set! prev p))
    (define (set-next! n)
        (set! next n))
    (define (dispatch m)
        (cond 
            ((eq? m 'prev) prev)
            ((eq? m 'next) next)
            ((eq? m 'val) val)
            ((eq? m 'set-prev!) set-prev!)
            ((eq? m 'set-next!) set-next!)
            (else (error "node" "no such op" m))))
    dispatch)

(define (make-deque)
    (define front-ptr '())
    (define rear-ptr '())
    (define (empty?) (null? front-ptr))
    (define (print)
        (define (traverse node)
            (cond 
                ((null? node) '())
                (else (cons (node 'val) (traverse (node 'next))))))
        (cond 
            ((empty?) '())
            (else 
                (traverse front-ptr))))
    (define (front) 
        (cond 
            ((empty?) (error "front" "queue is EMTPY"))
            (else (front-ptr val))))
    (define (rear) 
        (cond 
            ((empty?) (error "rear" "queue is EMTPY"))
            (else (rear-ptr val))))
    (define (push-back e)
        (define tmp (make-node e))
        (cond 
            ((empty?) (set! front-ptr tmp))
            (else
                ((tmp 'set-prev!) rear-ptr) 
                ((rear-ptr 'set-next!) tmp)))
        (set! rear-ptr tmp)
        (print))
    (define (push-front e)
        (define tmp (make-node e))
        (cond 
            ((empty?) (set! rear-ptr tmp))
            (else
                ((front-ptr 'set-prev!) tmp) 
                ((tmp 'set-next!) front-ptr)))
        (set! front-ptr tmp)
        (print))
    (define (pop-back)
        (cond 
            ((empty?) (error "pop-back" "queue is EMPTY"))
            (else 
                (set! rear-ptr (rear-ptr 'prev))
                (cond
                    ((not (empty?))((rear-ptr 'set-next!) '())))
                (print))))
    (define (pop-front)
        (cond 
            ((empty?) (error "pop-front" "queue is EMPTY"))
            (else 
                (set! front-ptr (front-ptr 'next))
                (cond
                    ((not (empty?))((front-ptr 'set-prev!) '())))
                (print))))
    (define (dispatch m)
        (cond 
            ((eq? m 'front) (front-ptr 'val))
            ((eq? m 'push-back) push-back)
            ((eq? m 'push-front) push-front)
            ((eq? m 'pop-back) pop-back)
            ((eq? m 'pop-front) pop-front)
            (else (error "queue" "no such op"))))
    dispatch)
;不要边写边想，想好过程再写
;边界判断！

;3.24
;初始化接受same-key?函数

;3.25
;多key表

;3.26
;二叉树索引

;3.27
;作图