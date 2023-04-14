(define (huffman freqs)
  (define (htree-leaf letter weight) (list 'leaf weight letter))
  (define (htree-node t0 t1) (list 'internal (+ (htree-weight t0) (htree-weight t1)) t0 t1))
  (define (htree-weight t) (cadr t))
  ...)



;question 1

(define (get-count x)
  (define (helper a)
  (cond ((null? a) 0)
        (append (cons (car a) 1) (cons (helper (cdr a)) 1))))
  (helper (string->list x)))


  
(get-count "Hello")