(define (make-tree value left right)
(list value left right))
(define (value T) (car T))
(define (right T) (caddr T))
(define (left T) (cadr T))


(define (mtree x T)
  (cond ((null? T) T)
        ;if empty want to return same value incase list is empty
        ((make-tree (* x (value T)) (mtree x (left T)) (mtree x (right T))))))
        ;only multiply first value then recursively go through each side of the tree                


;(mtree 3 '(12 (6 (3 (2 (1 () ())
                       ;()) (5 (4 () ()) ()))
                 ;(9 (7 () (8 () ())) (10 () (11 () ()))))
             ; (18 (15 (13 () (14 () ())) ())(21 (19 ()
                                               ;     (20 () ()))
                                              ;  (27 (25 () ()) (29 () ()))))))


(define (list-interator l)
  (let ((list l))
    (define (has-next?) (not (null? (cdr list))))
    (define (next) (cdr list))
    (define (current) (car list))
    (define (to-begin) (set! list l))
    (define (to-end) (if (not (has-next?))
                         (current)
                           (begin (next)
                                  (to-end))))
                                 
    (lambda (method)
      (cond
        ((eq? method 'current) current)
        ((eq? method 'to-begin) to-begin)
        ((eq? method 'next) next)
        ((eq? method 'has-next?) has-next?)
        ((eq? method 'to-end) to-end)))))


(define (make-stack)
  (let ((stack '()))
    (define (empty?) (null? stack))
    (define (push x) (set! stack (cons x stack)))
    (define (pop) (let ((element (car stack)))
                    (begin (set! stack (cdr stack)))
                    element))
    (define (top) (car stack))
    (lambda (method)
      (cond ((eq? method 'top) top)
            ((eq? method 'pop) pop)
            ((eq? method 'push) push)
            ((eq? method 'empty?) empty?)))))


      



(define (numlist n)
  (if (< n 10) n
      (append (list (numlist (floor (/ n 10)))) (list (modulo n 10)))))


(define (squaresum L)
  (if (null? L)
      0
      (+ (expt (car L) 2) (squaresum (cdr L)))))

(define (element? x)
  (define (search n)
  (cond ((null? n) #f)
        ((= (car n) x) #t)
        ((< (value n) x) (search (right n)))
        ((> (value n) x) (search (left n)))))
  (search tree))

(or (= 1 1) (= 1 2))
         

;example of begin function

(begin (display "I hate ")
       (display "Scheme ")
       (+ 1 1))

(define z 12)
(begin (set! z 13)
       (* z 2))


(define (new x y)
  (if (= x 0)
      '()
      (begin
        (set! y 2)
           (cons (+ x y) (new (- x 1) y )))))
(new 4 1)

(define (square x) (* x x))

(define (squ s)
  (if (null? s)
      '()
      (cons (square (car s))
            (delay (squ (force (cdr s)))))))

(squ '(222 2 2 2))


(define (stream s)
  (if (null? s)
      '()
      (cons (factorial (car s))

            (delay (stream (rest s))))))

(define (bst-min T)
  (if (null? (left T))
    (value T)
(bst-min (left T))))

(define (bst-max T)
  (if (null? (right T))
    (value T)
(bst-max (right T))))

(define (bst-ceil v T)
  ;;This version returns -1 if there is no element smaller than v
  ;;Prelim 2 question allows one to presume there is at least on element less than v
  (cond ((null? T) -1)
      ((= v (value T)) (if (null? (right T)) -1 (bst-min (right T))))
((and (not (null? (right T)))
      (< (value T) v)
      (> (bst-min (right T)) v)) (bst-min (right T)))
((and (not (null? (left T)))
(> (value T) v)
(<= (bst-max (left T)) v)) (value T))
((< v (value T)) (if (null? (left T)) (value T) (bst-ceil v (left T))))
(else (bst-ceil v (right T)))))

