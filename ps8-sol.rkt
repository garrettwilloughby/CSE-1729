(define (make-tree value left right) (list value left right))
(define (value tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))
(define empty-tree? null?)


;question 1

(define (tree-map T f)
  (cond ((null? T) '())
      ((make-tree (f (value T))
      (tree-map (left T) f)
      (tree-map (right T) f)
      ))))


(tree-map (list 15 (list) (list)) (lambda (x) (* x x)))

;question 2

(define (tree-equal? T1 T2)
  (cond ((and (null? T1) (null? T2)) #t)          
        ((or (null? T1) (null? T2)) #f)
            ((eq? (value T1) (value T2)))
            ((and (tree-equal? (left T1) (left T2))
                 (tree-equal? (right T1) (right T2))))
            
            (else #f)))
        
  

(tree-equal? (list 15 (list) (list)) (list))

;question 3a

(define (insert-list L T)
  (define (insert v T)
  (cond ((null? T) (make-tree v '() '()))
        ((< v (value T)) (make-tree (value T)
                                    (insert v (left T))
                                    (right T)))
        ((make-tree (value T) (left T) (insert v (right T))))))

  (cond ((null? L) T)
        ((insert-list (cdr L)
                     (insert (car L) T)))))


        


(insert-list (list) (list 15 (list) (list)))

;question 3b

(define (sort-extract T)
  (if (null? T)
      '()
      (append (sort-extract (left T))
              (list (value T))
              (sort-extract (right T)))))

;question 3c

(define (tree-sort l)
  (define (insert-list L T)
  (define (insert v T)
  (cond ((null? T) (make-tree v '() '()))
        ((< v (value T)) (make-tree (value T)
                                    (insert v (left T))
                                    (right T)))
        ((make-tree (value T) (left T) (insert v (right T))))))

  (cond ((null? L) T)
        ((insert-list (cdr L)
                     (insert (car L) T)))))
(define (sort-extract T)
  (if (null? T)
      '()
      (append (sort-extract (left T))
              (list (value T))
              (sort-extract (right T)))))
  (sort-extract (insert-list L '())))



;question 4

(define (maximum l)
  (if (null? (right l))
      (car l)
      (max (car l) (maximum (right l)))))

(define (delete-value v T)
  (cond ((null? T) '())
        ((< (value T) v) (make-tree (value T) (left T) (delete-value v (right T))))
        ((> (value T) v) (make-tree (value T) (delete-value v (left T)) (right T)))
        ((null? (left T)) (right T))
(else (make-tree (maximum (left T)) (delete-value (maximum (left T)) (left T)) (right T)))))
  

;question 5

(define (arithvalue T)
  (cond ((eq? (value T) #\+) (+ (arithvalue (left T)) (arithvalue (right T))))
        ((eq? (value T) #\*) (* (arithvalue (left T)) (arithvalue (right T))))
        ((eq? (value T) #\-) (* (arithvalue (left T)) -1))
        ((eq? (value T) #\/) (/ 1  (arithvalue (left T))))
       
        (else (value T))))
        
                   
(define example (list #\+ (list #\*
                                (list 4 '() '())
                                (list 5 '() '()))
                      (list #\+
                            (list #\/ (list 6 '() '()) '())
                            (list 7 '() '()))))             






;question 6a


(define (prepare x)
(cond ((number? x) (number->string x))
((char? x) (string x))))

(define (tree-map T f)
  (cond ((null? T) '())
      ((make-tree (f (value T))
      (tree-map (left T) f)
      (tree-map (right T) f)
      ))))


(define (arith-prefix T)
  (define (helper T)
  (cond ((null? T) "")
        ((string-append (value T) (helper (left T)) (helper (right T))))))


         (helper (tree-map T prepare)))


        
;question 6b

(define (arith-postfix T)
  (define (helper T)
    (cond ((null? T) "")
          ((string-append (helper (left T)) (helper (right T)) (value T)))))
  (helper (tree-map T prepare)))
                          


;question 6c

(define (arith-infix T)
   (cond ((null? T) "")
     ((eq? (value T) #\+) (string-append "(" (arith-infix (left T)) "+"
                                             (arith-infix (right T)) ")"))
        ((eq? (value T) #\*) (string-append "(" (arith-infix (left T)) "*"
                                             (arith-infix (right T)) ")"))
        ((eq? (value T) #\-) (string-append "-" "(" (arith-infix (left T)) ")"))
        ((eq? (value T) #\/) (string-append "/" "(" (arith-infix (left T)) ")"))
        (else (number->string (value T)))))
       



(arith-infix example)


