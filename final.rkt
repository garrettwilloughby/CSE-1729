;question 1

(define (make-stack)
  (let ((stack '())) ;; internal stack variables
    (define (empty?) (null? stack)) ;; stack methods
    (define (push x) (set! stack (cons x stack)))
    (define (pop)
    (let ((top (car stack)))
      (begin (set! stack (cdr stack))
           top)))
    (define (top) (car stack))
    (define (dispatcher method)
      (cond ((eq? method 'top) top)
        ((eq? method 'pop) pop)
        ((eq? method 'push) push)
        ((eq? method 'empty) empty?)))
    dispatcher))

;question 2

(define (value n) (car n))
(define (next n) (cdr n))

(define (make-queue)
  (let ((head '())
        (tail '()))
  (define (empty?) (null? head)) ;; queue methods
  (define (enqueue x)
    (let ((new-node (cons x '())))
      (begin
      (if (empty?)
          (set! head new-node)
          (set-cdr! tail new-node))
      (set! tail new-node))))
  (define (dequeue)
    (let ((return (value head)))
      (if (eq? head tail)
          (begin (set! head '())
                 (set! tail '())
                 return)
          (begin (set! head (next head))
                 return))))
    (define (front) (car head))
 (define (dispatcher method)
  (cond ((eq? method 'empty) empty?)
        ((eq? method 'enqueue) enqueue)
        ((eq? method 'dequeue) dequeue)
        ((eq? method 'front) front)))
  dispatcher))

;question 3

(define (make-tree value left right) (list value left right))
(define (value tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))

(define (make-set is-equal? before?)
  (let ((tree '())) ;; internal variables
  (define (empty?) (null? tree)) ;; ADT methods
  (define (element? x)
(define (search n)
  (cond ((null? tree)
         #f)
        ((= (car tree) x)
         #t)
        ((< x (value n)) (search (left n)))
        ((> x (value n)) (search (right n)))))
    (search tree))    
  (define (insert x)
    (define (helper tree)
    (cond ((null? tree) (make-tree x '() '()))
          ((< x (value tree)) (begin (set-cdr! (pointers n)
                                               (helper (left tree)))
                                     tree))
          ((> x (value tree)) (begin (set-cdr! (pointers n)
                                               (helper (right tree)))
                                     tree))))
          (set! tree (helper tree)))
  (define (dispatcher method)
  (cond ((eq? method 'insert) insert)
      ((eq? method 'element?) element?)
      ((eq? method 'get) get)
      ((eq? method 'list-elements) list-elements)
      ((eq? method 'empty) empty?)))
  dispatcher))

