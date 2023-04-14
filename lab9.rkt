(define (create-heap v H1 H2)
  (list v H1 H2))

(define (h-min H) (car H))
(define (left H) (cadr H))
(define (right H) (caddr H))

(define (combine f Ha Hb)
  (cond ((null? Ha) Hb)
        ((null? Hb) Ha)
        ((f (h-min Ha) (h-min Hb))
         (create-heap (h-min Ha)
                      Hb
                      (combine f (left Ha) (right Ha))))
         (else (create-heap (h-min Hb)
                      Ha
                      (combine f (left Hb) (right Hb))))))

(define (heap-remove f H)
  (combine f (left H) (right H)))

(define (heap-insert f x H)
  (if (null? H)
      (create-heap x '() '())
      (if (f x (h-min H))
          (create-heap x
                       (right H)
                       (heap-insert f (h-min H) (left H)))



          (create-heap (h-min H)
                       (right H)
                       (heap-insert f x (left H))))))



;question 1a

(define (equalize-heaps H)
  (let ((H1 (cdar H))
        (H2 (cddr H))
        (H1c (caar H))
        (H2c (cadr H)))
        
    
  (cond ((<= (abs (- H2c H1c)) 1) H)
        ((> H1c H2c) (equalize-heaps (cons (cons (- H1c 1) (heap-remove > H1)) (cons (+ H2c 1) (heap-insert < (h-min H1) H2)))))
        ((< H1c H2c) (equalize-heaps (cons (cons (+ H1c 1) (heap-insert > (h-min H2) H1)) (cons (- H2c 1) (heap-remove < H2))))))))

;question 1b

(define (add-number x H)
  (let ((H1 (cdar H))
        (H2 (cddr H))
        (H1c (caar H))
        (H2c (cadr H)))
    (define (help x H1 H2)
  (cond ((< x (h-min H1)) (cons (cons (+ H1c 1) (heap-insert > x H1)) (cons H2c H2)))
        ((> x (h-min H1)) (cons (cons H1c H1) (cons (+ H2c 1) (heap-insert < x H2))))
        )
      )
    (equalize-heaps (help x H1 H2))
    )
  )



;question 1c

(define (get-median H)
  (let ((H1 (cdar H))
        (H2 (cddr H))
        (H1c (caar H))
        (H2c (cadr H)))
    (cond ((= H1c H2c) (/ (+ (car H1) (car H2)) 2))
          ((> H1c H2c) (car H1))
          ((< H1c H2c) (car H2)))))
  
