;problem 1

(define (geom-series-np2 n)
  (if (= n 0)
      1
      (+ (/ 1 (expt 2 n))
         (geom-series-np2 (- n 1)))))








;problem 2

(define (num-digits n)
  (if (< n 10)
      1
      (+ (num-digits (/ n 10)) 1))


  )



;problem 3a

(define (a n)
  (if (= 1 n)
      2
      (* (a (- n 1)) 2)))


;problem 3b

(define (num-ancestors n)
(if (= n 1)
    2
    (+ (a n) (num-ancestors (- n 1)))
  ))


;problem 4


(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))
      )
  )

(define (factorial k)
  (if (= k 0)
      1
      (* k (factorial (- k 1)))
      )
  )


(define (n-choose-k n k)
  (if (< n k)
      0
      (/ (factorial n) (* (factorial(- n k)) (factorial k)))))



;problem 5

(define (pascals-triangle n k)
  (cond ((> 0 k) 0)
        ((> k n) 0)
        ((and (= n 0) (= k 0)) 1)
        ((+ (pascals-triangle (- n 1) (- k 1))

      (pascals-triangle (- n 1) k))


      )))




(pascals-triangle 0 0)
(pascals-triangle 1 1)
(pascals-triangle 1 0)
(pascals-triangle 10 4)
(pascals-triangle 100 2)
