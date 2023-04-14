;1a


(define (fizzbuzz x)
  (cond ((and (= (modulo x 5) 0) (= (modulo x 3) 0) ) "fizzbuzz" )
        (( = (modulo x 3) 0) "fizz")
        (( = (modulo x 5) 0) "buzz")
        (display x)
))




;1b

(define (fizz x)
  (if (= (modulo x 3) 0 ) "fizz")


  )



(define (buzz x)
   (if (= (modulo x 5) 0 ) "buzz")

  )

(define (fizzbuzz2 x)
(cond
  ((and (= (modulo x 5) 0) (= (modulo x 3) 0) ) "fizzbuzz" )
  ((= (modulo x 3) 0) (fizz x))
  ((= (modulo x 5) 0) (buzz x))
  
  (display x)
  ))





;2


(define (piecewise x)
  (cond ((> x (* 2 3.142)) (- x (* 2 3.142)))
        ((< x (* -1 3.142)) (- (* -1 x) (3.142)))
        ((sin x))))



;3

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (add n m)
   (cond ((> n 0) (+ (dec n) (inc m)))
      ((= n 0) m)))

;4

(define (mult n m)
(if (<= n 0)
    0
   (+ m (mult (dec n) m))))

;5

(define (power b n)
 (if (= n 0)
     1

  (mult 1 (mult b (power b (dec n))))

  ))




;6

;(define (raise x n)
  ;(cond ((= n 0)
      ;1)
  ;((= (modulo n 2) 0) (expt (expt x (/ n 2)) 2))

  ;((= (modulo n 2) 1) ( * (expt (expt x (floor (/ n 2))) 2)))))
  


(define (raise x n)
  (cond ((<= n 0) 1)

  ((= (modulo n 2) 1) (mult x (raise (mult x x) (floor (/ n 2)))))
  ((= (modulo n 2) 0) (raise (mult x x) (floor (/ n 2))))

  
))


(raise 2 2)
;7




(define (pmult x y)
(cond ((= x 1) y)
      ((= x 0) 0)
      ((= y 0) 0)
      ((= (modulo x 2) 0) (pmult (floor(/ x 2)) (* y 2)))
      (else (+ y (pmult (floor(/ x 2)) (* y 2))))
  ))
    
    



;8a

(define (sumEven n)
    (if (= n 0)
        0
    (if (= (modulo n 2) 0) (+ n (sumEven (- n 2)))
     
       (+ (- n 1) (sumEven (- n 3))))))


;8b 


(define (sumOdd n)
  (if (<= n 0)
      0
    (if (= (modulo n 2) 1) (+ n (sumOdd (- n 2)))
     
     (+ (- n 1) (sumOdd (- n 3))))))


;9


(define (h-product k)
(if (= k 1)
    1

(* (- 1 (/ 1 k)) (h-product (- k 1)))))

;10

(define (divides a b) (= 0 (modulo b a)))

(define (divisors n) (divisors-upto n n))
(define (divisors-upto n k)
  
    (cond ((= k 0) 0)
  ((= n 0) 0)
  ((= k 1) 1)
  ((divides k n) (+ 1 (divisors-upto n (- k 1))))
  ((divisors-upto n (- k 1)))))

(divisors-upto 8 8)



;11


(define (subfact n)
  (cond ((= n 0) 1)
   ((= n 1) 0)
   ((> n 1) 
      (* (- n 1) (+ (subfact(- n 1))
         (subfact(- n 2))))
   )
  ))


;12


(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))
      ))

(define (new-cos x n) 
  (cond ((= n 0)
       1)

  ((+ (* (expt -1 n) (/ (expt x (* 2 n)) (factorial (* 2 n))))
     (new-cos x (- n 1)))
  )))

(new-cos 0 2)
(new-cos 1 3)
(new-cos (/ 3.1414592 2) 2)
(new-cos (/ 3.1414592 2) 4)
(new-cos (/ 3.1414592 2) 8)

  