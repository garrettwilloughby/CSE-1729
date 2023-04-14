;question 1

(define (make-clock h m)
  (let ((otime (+ (* h 60) m)))


(define (get-time)
(let ((hour (modulo (floor (/ otime 60)) 12))
  (min (floor (/ (modulo otime 60) 10)))
  (min2 (modulo (modulo otime 60) 10)))

  (string-append
 (if (= hour 0) "12"
 (number->string hour)) ":"
     (number->string min)
     (number->string min2)
(if (< (modulo (floor (/ otime 60)) 24) 12)
    " AM\n"
    " PM\n"
     ))))

(define (tick)
  (begin
       (set! otime
          (+ otime 1))))


  
(define (get-mil)
  (let ((hour1 (floor (/ (modulo (floor (/ otime 60)) 24) 10)))
        (hour (modulo (modulo (floor (/ otime 60)) 24) 10))
  (min (floor (/ (modulo otime 60) 10)))
  (min2 (modulo (modulo otime 60) 10)))
(string-append
  (number->string hour1)
   (number->string hour)
   ":"
   (number->string min)
     (number->string min2)
     "\n"
     )))

(lambda (method)
   (cond ((eq? method 'time) get-time)
         ((eq? method 'tick) tick)
         ((eq? method 'military) get-mil)))))



;question 2

(define (make-stack)
  (let ((S '())) ;; internal stack variables
    (define (empty?)
      (null? S)) ;; stack methods
    (define (push x)
      (set! S (cons x S)))
    (define (pop)
      (let ((pop2 (car S)))
                    (begin (set! S (cdr S))
                           pop2)))
    (define (top) (car S))
    (lambda (method)
      (cond ((eq? method 'top) top)
            ((eq? method 'pop) pop)
            ((eq? method 'push) push)
            ((eq? method 'empty?) empty?)))))

;question 3

(define (eval-postfix p)

  (let ((postfix p)
      (OPS (make-stack)))

(define (helper2)
  (cond ((null? postfix) '())
        ((number? (car postfix)) (begin ((OPS 'push) (car postfix))
                                        (set! postfix (cdr postfix))
                                        (helper2)))
        (else (begin (helper (car postfix)) (set! postfix (cdr postfix))
                     (helper2)))))

(define (helper x)
  (cond ((eq? x #\+)
     ((OPS 'push)
      (+ ((OPS 'pop)) ((OPS 'pop)))))

      ((eq? x #\-)
      ((OPS 'push))
      (let ((b ((OPS 'pop)))
           (a ((OPS 'pop))))
        (- a b)))

      ((eq? x #\*)
     ((OPS 'push)
      (* ((OPS 'pop)) ((OPS 'pop)))))
      
    ((eq? x #\/) 
      ((OPS 'push)
      (let ((b ((OPS 'pop)))
           (a ((OPS 'pop))))
        (/ a b))))
    
    ((eq? x #\^)
     ((OPS 'push)
      (let ((b ((OPS 'pop)))
           (a ((OPS 'pop))))
        (expt a b))))))
    
(begin (helper2)
       ((OPS 'top)))))

(eval-postfix (list 23 15 (string-ref (symbol->string '+) 0)))

    
;question 4

(define (make-tree value left right) (list value left right))
(define (value tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))



(define (dfs T)
  (let ((stack (make-stack)))
       (define (helper L)
(cond (((stack 'empty?)) L)
      ;check if empty
      ((begin (let ((T ((stack 'pop))))
      (if (not (null? (right T)))
          ((stack 'push) (right T)))
                ;push right
      (if  (not (null? (left T)))
              ((stack 'push) (left T)))
                ;push left
             (helper (append L (list (value T)))))))))
    ;then transverse through tree once on stack
    (begin ((stack 'push) T)
           (helper '()))))



(dfs '(1 (2 (4 (8 () ()) (9 () ())) (5 (10 () ()) (11 () ()))) (3 (6 (12 () ()) (13 () ())) (7 (14 () ()) (15 () ())))))


;question 5

(define (make-queue)
  (let ((head '())
        (tail '()))
(define (value n) (car n))
(define (next n) (cdr n))
(define (empty?) (null? head))
(define (front) (value head))
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
(lambda (method)
  (cond ((eq? method 'empty?) empty?)
        ((eq? method 'enqueue) enqueue)
        ((eq? method 'dequeue) dequeue)
        ((eq? method 'front) front)))))


(define (bfs T)
  (let ((q (make-queue)))
    (define (helper)
      (cond ((not ((q 'empty?))) 
        (let ((dq ((q 'dequeue))))
  (begin (cond ((not (null? (left dq)))
      ((q 'enqueue) (left dq))))
   (cond ((not (null? (right dq)))
       ((q 'enqueue) (right dq))))
       (cons (value dq) (helper)))))
    (else '())))
       (begin ((q 'enqueue) T)
           (helper))))

      
(bfs '(1 (2 (4 (8 () ()) (9 () ())) (5 (10 () ()) (11 () ()))) (3 (6 (12 () ()) (13 () ())) (7 (14 () ()) (15 () ())))))
  




