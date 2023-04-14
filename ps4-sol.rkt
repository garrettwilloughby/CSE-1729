;question 1


(define (harmonic a)
  
(define (term-h a)
  (/ 1 a))
  
(define (next-h b)
  (+ b 1))
  
(define (sum term-h a next-h b)
  (if (> a b)
      0
      (+ (term-h a)
         (sum term-h (next-h a) next-h b))))
  (sum term-h 1 next-h a))



;question 2a

(define (product term a next b)
  (if (> a b)
      1
(* (term a)
   (product term (next a) next b))))





;question 2b



(define (wallis-pi n)
(define (term-w a) (* (/ (* 2 n) (- (* 2 n) 1)) (/ (* 2 n) (- (* 2 n) 1))))
(define (next-w b) (+ 1 b))

(define (product term-w a next-w b)
  (if (> a b)
      1
      (* (term-w a)
         (product term-w (next-w a) next-w b))))
  (product term-w 1 next-w n)
  )


;question 3

(define (frac-sum f g n)
  (define (frac-h f g n k)
    (cond ((> n k) 0)
          ((= 0 (g n)) (frac-h f g (+ n 1) k))
          ((+ (/ (f n) (g n)) (frac-h f g (+ n 1) k)))))

  (frac-h f g (- n) n))
  



  

(frac-sum (lambda (x) x) (lambda (x) x) 10)



;question 4

(define (der f h)
  (define (der-h x)
  (/ (-  (f (+ x h)) (f x)) h))
  der-h)


(define (der-n f n h)
  (cond ((= n 1) (der f h))
  ((der f h) (der-n (der f h) (- n 1) h))))




;question 5

(define (newton f x n)
  (define deriv (der f .01))
  (define (newton-h x h)
    (let ((new-x (- x (/ (f x) (deriv x))))) 
    (cond ((= h 0) x)
          ((newton-h new-x (- h 1))))))
     (newton-h x n))



;question 6

(define (sum-term term a b)
(if (> a b)
0
(+ (term a)
   
(sum-term term (+ a 1) b))))

(sum-term (lambda (x) (* x x)) 1 10)


(define (simpson-integrate f a b n)
(let ((deltax (/ (- b a) n)))
    (define (y i) (f (* (+ a i) deltax)))
  (define (helper i)
  (cond ((= i 0) (y i))
          ((= n i)  (y i))
          ((even? i) (* (y i) 2))
          ((odd? i) (* (y i) 4))))
      (* (/ deltax 3) (sum-term helper 0 n))))



(simpson-integrate (lambda (x) x) 0 1 10)
(simpson-integrate (lambda (x) (* x x x x)) 0 1 100)
(simpson-integrate (lambda (x) (* x x)) 0 1 100)












