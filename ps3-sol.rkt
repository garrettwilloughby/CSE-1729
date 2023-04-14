;question 1a

(define (harmonic n)
  (if (= n 1)
      1
      (+ (/ 1 n) (harmonic (- n 1)))))

;question 1b


(define (Eulerest n)


  (abs (- (harmonic n) (log n))))

;question 2

(define (prime? n)
(define (divisor? k) (= 0 (modulo n k)))
(define (divisors-upto k)
(and (> k 1)
1
(or (divisor? k) (divisors-upto (- k 1)))))
(not (divisors-upto (- n 1))))

(define (count-primes m)
  (cond ((= m 1) 0)
        ((prime? m) (+ 1 (count-primes (- m 1))))
        ((count-primes (- m 1)))))


;question 3

(define (rel-prime a b)
  (define (divides-both d)
     (and (= 0 (modulo a d))
          (= 0 (modulo b d))))
  
(define (divisor-upto k)
   (and (> k 1)
        (or (divides-both k)
        (divisor-upto (- k 1)))))
(not (divisor-upto (min a b))))



(define (count-rel-prime m)

  (define (over-a a b)
    (cond ((= a 1) 1)
            ((rel-prime a b) (+ 1 (over-a (- a 1) b)))
            ((over-a (- a 1) b ))))
  (cond ((= m 1) 1)
  ((+ (over-a m m)
      (count-rel-prime (- m 1))))))


;question 4a

(define (lucas n)
  (cond ((= n 0) 2)
        ((= n 1) 1)
        ((> n 1) (+ (lucas (- n 1))
                           (lucas(- n 2))))))
;question 4b

(define (Lucas-ratio n)
(/ (lucas n) (lucas (- n 1))))


;question 4c



(define (fast-Lucas-help n k luc-a luc-b)
(if (= n k)
    luc-a
    (fast-Lucas-help n (+ k 1) (+ luc-a luc-b) luc-a)))
    

(define (fast-Lucas n) (fast-Lucas-help n 1 1 2))


(define (rec-call-lucas k)
    (cond ((= k 1)  0)
          ((= k 2)  2)
          ((= k 3)  4)
          ((= k 4)  8)
          ((= k 5)  14)
          ((= k 6)  24)
    ))


(define (rec-call-fast-lucas-helper k)
    (cond ((= k 1)  0)
          ((= k 2)  1)
          ((= k 3)  2)
          ((= k 4)  3)
          ((= k 5)  4)
          ((= k 6)  5)
    ))






;question 5a

(define (golden n)
  (cond ((= n 0) 1)
        ((+ 1 (/ 1 (golden (- n 1)))))))``



;question 5b

(define (golden-sqrt n)
  (cond ((= n 0) 1)
        ((> n 0) (sqrt (+ 1 (golden-sqrt (- n 1)))))))



;question 6

(define (interval-sum m n)
(cond ((= m n) m)
((even? m) (+ m (interval-sum (+ m 1) (- n 1)) n))
((+ m (interval-sum (+ m 1) n )))))



(define (explain-interval-sum)
  (define a "One can never do  an induction on both inputs at once.")
  (define b "The base case isn't quite right. It needs to be updated to account for the two inductive calls.")
  (define c "The inductive case should be adding three things together.")
  (define d "The predicate to recognize the base case is wrong. One can go from m > n to m < n without ever seeing n = m.")
  (define e "I have no idea.")
  d)





;question 7

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((and (> m 0) (= n 0)) (ack (- m 1) 1))
        ((and (> m 0) (> n 0)) (ack (- m 1) (ack m (- n 1))))))

;problem 8 




(define (catalan n)
  (define (catalanh n k)
    (cond ((< k 2) 1)
          (else (* (/ (+ n k) k)
                   (catalanh n (- k 1))
                   ))))
  (catalanh n n))



  

;want to find the numbers between n and k, n never changes.

;problem 9
  (define pi-approx
    (let ((a (* 0.5 (+ 23 (* 4 (sqrt 34)))))
         (b (* 0.5 (+ (* 19 (sqrt 2)) (* 7 (sqrt 17)))))
         (c (+ 429 (* 304 (sqrt 2))))
         (d (* 0.5 (+ 627 (* 442 (sqrt 2))))))

     (let ((u (* (expt (+ a (sqrt (- (* a a) 1))) 2)
                 (expt (+ b (sqrt (- (* b b) 1))) 2)
                 (+ c (sqrt (- (* c c) 1)))
                 (+ d (sqrt (- (* d d) 1)))))) (/ (log (+ (expt (* 2 u) 6) 24)) (sqrt 3502))



              )))

;question 10

(define (gauss-legendre tol)
(define (ghelp a b t p)

  (let ((a (/ (+ a b) 2))
        (b (sqrt (* a b)))
        (t (- t (* p (expt (- a (/ (+ a b) 2)) 2))))
        (p (* 2 p)))
       (cond ((< (abs (- b a)) tol) (/ (expt (+ a b) 2) (* 4 t)))
  ((ghelp a b t p)))))
(ghelp 1 (/ 1 (sqrt 2)) (/ 1 4) 1))
  



  

  




