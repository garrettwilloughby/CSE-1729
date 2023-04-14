;question 1

(define (zip a b)
  (cond ((or (null? a) (null? b))
      '() )
      ((cons (cons (car a) (car b))
            (zip (cdr a) (cdr b))))))


(zip (list 1 2 3) (list 4 5 6))

;question 2

(define (unzip l)
  (if (null? l)
      '(().()) 
      (cons (cons (car (car l)) (car (unzip (cdr l))))
            (cons (cdr (car l)) (cdr (unzip (cdr l)))))))

       
     
            
              

(unzip (list (cons 1 4) (cons 2 5) (cons 3 6)))

;question 3a

(define (list-sum l)
  (if (null? l)
      0
      (+ (car l) (list-sum (cdr l)))))

;question 3b

(define (average l)
  (/ (list-sum l) (length l)))

;question 3c

(define (var-map x)
 (map (lambda (y) (expt (- y (average x)) 2)) x))


(var-map (list 1 2 3 4 5))

;question 3d

(define (stdev l)
  (sqrt (average (var-map l))))




;question 3e

(define (map2 f X Y)
  (if (or (null? X) (null? Y))
      '()
   (cons (f (car X) (car Y))
   (map2 f (cdr X) (cdr Y)))))

(map2 (lambda (x y) (* x y)) (list 2 2 2) (list 1 2 3))


;question 3f

(define (covar-elements X Y)
  (map2 (lambda (a b) (* (- a (average X)) (- b (average Y)))) X Y))
        


(covar-elements (list 1 2 3) (list 4 5 6))


;question 3g

(define (pearson X Y)
  (/ (average (covar-elements X Y))
     (* (stdev X) (stdev Y))))

(pearson (list 10 10 10 10 9) (list 10 10 5 1000 1))

;question 4a


(define (best-fit X Y)
  (let* ((r (pearson X Y))
         (a (* r (/ (stdev Y) (stdev X))))
         (b (- (average Y) (* a (average X)))))
    (cons a b)))

             
(best-fit (list 1 2 3) (list 4 5 6))

;question 4b

(define (best-fit-fn pX pY)
  (let* ((r (pearson pX pY))
         (a (* r (/ (stdev pY) (stdev pX))))
         (b (- (average pY) (* a (average pX)))))
    (lambda (x) (+ (* a x) b))))


(best-fit-fn (list 160 180 200 220 240 260 280) (list 126 103 82 75 78 40 20))

