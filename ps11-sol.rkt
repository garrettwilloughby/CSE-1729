(define (head stream) (car stream))
(define (rest stream) (force (cdr stream)))


(define (take s k)
  (if (= k 0)
      '()
  (cons (head s)
        (delay (take (rest s) (- k 1))))))




(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define the-empty-stream '())

(define (take s k)
  (cond ((= k 0) the-empty-stream)
        (#t (cons (car s) (take (stream-cdr s) (- k 1))))))

(take '(1 2 3 4 5 6 7) 5) 