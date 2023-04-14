( define (f x )
   (define ( g y ) (+ x y ))
   ( let (( x 100)
          ( y 200))
      ( g (+ x 10))))
(f 1000)



( let (( x 10)
       ( y 20)
       ( z 40))
   ( let (( x (+ y 1))
          ( y (+ x 2))
( z ( lambda ( z ) (* z y ))))
   ( z y )))



(define (tester x)
  (if (= x 10) #t)
  (= x 10))

(tester 1000)

(define (test-upto n k)
  (cond ((= k 0) #f)
        ((= k (sqrt n)) #t)
        (else (test-upto n (- k 1)))))

(define (perfect-square? n)
  (if (= n 0)
      #f)
  (test-upto n n))

(perfect-square? 100)



