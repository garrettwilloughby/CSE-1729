;question 1

(define (merge a b)
  (cond ((null? a) b)
        ((null? b) a)
     
        ((< (car a) (car b)) (cons (car a) (merge (cdr a) b)))
        ((cons (car b) (merge a (cdr b))))))


(merge (list 1 3 5 9 10) (list 0 2 4 10 12))

;question 2



(define (smallest l)
    (define (smaller a b) (if (< a b)
                              a
                              b))
    (if (null? (cdr l))
        (car l)
        (min (car l) (smallest (cdr l)))))

  (define (remove x l)
    (if (null? l)
        l
        (if (equal? x (car l))
            (cdr l)
            (cons (car l) (remove x (cdr l))))))

(define (mergeSort l)
  (define (odd l)
    (if (null? l)
        '()
        (if (null? (cdr l))
            (list (car l))
            (cons (car l) (odd (cddr l))))))
  (define (even l)
    (if (null? l)
        '()
        (if (null? (cdr l))
            '()
            (cons (cadr l) (even (cddr l))))))
  (define (split l)
    (cons (odd l) (cons (even l) '())))
  (if (null? l)
      l
      (if (null? (cdr l))
          l
          (if (null? (cdr l))
              l
              (merge (mergeSort (car (split l)))
                     (mergeSort (cadr (split l))))))))





(mergeSort (list 1 6 7 3 9 0 2))

;question 3

(define (ins x l)
  (cond ((null? l) (list x))
        ((< x (car l)) (cons x l))
        ((cons (car l) (ins x (cdr l))))))
      
(ins 2 (list 1 3 4 5))

;question 4

(define (insSort l)
  (cond ((null? l) '())
    ((ins (car l) (insSort (cdr l))))))
    
  
    
  

(insSort (list 3 5 1 6 9 0 2 7))


;question 5a

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
(op (car sequence) (fold-right op initial (cdr sequence)))))




;question 5b

(define (fold-left op initial sequence)
  (if (null? sequence)
      initial
  (fold-left op (op initial (car sequence)) (cdr sequence))))


;question 5c

(define (my-map p sequence)
  (fold-right (lambda (x y) (cons (p x) y)) '() sequence))


;question 5d

(define (my-append seq1 seq2) (fold-right cons seq2 seq1))


(my-append (list 1 2 1) (list 10 11 12))

;question 5e

(define (my-length sequence) (fold-right (lambda (a b) (+ b 1)) 0 sequence))

(my-length (list 1 2 1 11 12))

;question 5f

(define (reverse-r sequence)
(fold-right (lambda (x y) (append y (list x))) '() sequence))



;question 5g

(define (reverse-l sequence)
(fold-left (lambda (x y) (cons y x)) '() sequence))


;question 5h

(define (horner-eval x coefficient-list)
  (fold-right (lambda (a b) (+ a (* b x))) 0 coefficient-list))

(horner-eval 2 (list 1 3 0 5 0 1))


(define (prime? n)
  (define (divides a b) (= (modulo b a) 0))
  (define (smooth k)
    (and (>= k 2)
         (or (divides k n)
             (smooth (- k 1)))))
  (and (> n 1)
       (not (smooth (floor (sqrt n))))))


(define (explode x)
  (cond ((< x 10) (list x))
        ((append (explode (floor (/ x 10)))
                (list (- x (* 10 (floor (/ x 10)))))))))

(define (implode l)
  (define (ihelper l i)
    (if (null? l)
        0
        (+ (* (car l) (expt 10 i))
           (ihelper (cdr l) (+ i 1)))))
  (ihelper (reverse l) 0))

(define (remove l)
  (if (null? (cdr l))
    (list)
(cons (car l) (remove (cdr l)))))

;question 6ai


(define (left-truncatable-prime? p)
  (let* ((prime (prime? p)))
(cond ((and prime (< p 10)) #t)
      ((not prime) #f)
      (else (let* ((a (explode p)))
              (cond ((= (cadr a) 0) #f)
                    (else (left-truncatable-prime? (implode (cdr a))))))))))

(left-truncatable-prime? 915)
      
;question 6aii

(define (find sequence test n)
  (define (find-aux x a)
    (let ((fx (sequence x)))
      (if (test fx)
             (if (= (+ a 1) n)
                 fx
                   (find-aux (+ x 1) (+ a 1)))
             (find-aux (+ x 1) a))))
    (find-aux 1 0))



(define (nth-left-trunc-prime n)
  (find (lambda (x) x) left-truncatable-prime? n))
  
;question 6bi

(define (right-truncatable-prime? p)
  (let ((prime (prime? p)))
  (cond ((and prime (< p 10)) #t)
      ((not prime) #f)
(else (right-truncatable-prime? (implode (remove (explode p))))))))



;question 6bii

(define (nth-right-trunc-prime n)
  (find (lambda (x) x) right-truncatable-prime? n))

;question 6ci

(define (two-sided-prime? p)
  (and
   (left-truncatable-prime? p)
   (right-truncatable-prime? p)
       ))



;question 6cii

(define (nth-two-sided-prime n)
  (find (lambda (x) x) two-sided-prime? n))


  