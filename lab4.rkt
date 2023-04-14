;question 1

(define (windchill W F)
  (let ((a (sqrt W)))
    (+ 1.05
       (* 0.93 F)
       (- (* 3.65 W))
       (* 3.62 a)
       (* .103 F a)
       (* 0.0439 (expt W 2))
       )))

;question 2

(define (pressure h)
  (let ((p 101325)
        (T 288.15)
        (L 0.0065)
        (g 9.80665)
        (M 0.0289644)
        (R 8.31447))
    (let
        ((power (/ (* g M) (* R L)))
        (base (- 1 (/ (* L h) T))))

(* p (expt base power)))))



;question 3

(define (square x) (* x x))
(define (inc x) (+ x 1))



(define (compose f g)
 (define (chelp x)
     (f (g x)))
chelp)


((compose square inc) 6)


;question 4

(define (repeated f n)
    (if (= n 0)
        (f 0)
       (compose f (repeated f (- n 1)))))




(repeated (lambda (x) (* x x)) 8)
(repeated (lambda (x) (* x x)) 16)   
(repeated (lambda (x) (+ x 1)) 4)
((repeated square 2) 5)




