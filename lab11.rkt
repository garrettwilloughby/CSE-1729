(define (head s) (car s))
(define (rest s) (force (cdr s)))

(define (lucas-stream)
  (define (helper a b)
    (cons a 
          (delay (helper b (+ a b)))))
(helper 1 3))


(define (lucas-pseudoprime p)
  (define (helper k l)
    (if (= k 1)
       (head l)
        (helper (- k 1) (rest l))))
  (let ((iterate (lucas i (lucas-stream))))
    (= (modulo (- iterate 1) i) 0)))
  
  
(define (ll-stream)
  (define (helper i)
    (cons i
          (delay (helper (- (expt i 2) 2)))))
  (helper 4))

