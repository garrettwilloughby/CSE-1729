;question 1a

(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
(combiner (term a)
  (accumulate combiner null-value term (next a) next b))))



;question 1b


(define (catalan n)
  (define (combiner n) *)
  (define (term k) (/ (+ n k) k))
  (define (next n) (+ n 1))

  (define (accumulate combiner null-value term a next b)
    (if (> a b) null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))
  (accumulate * 1 term 2 next n))

;question 1c

(define (leibniz-pi n)
  (define (term n) (* (expt (- 1) n) (/ 1 (- 1 (* 2 n)))))
  (define (next n) (+ n 1))

    (define (accumulate combiner null-value term a next b)
    (if (> a b) null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))
    (* 4 (accumulate + 0 term 1 next n)))

(leibniz-pi 4)

;question 1d

(define (accumulate-tr combiner null-value term a next b)
  (define (achelp n val)
    (if (> n b) val
        (achelp (next n) (combiner (term n) val))))
  (achelp a null-value))

;doing math in variable 

(accumulate-tr  + 0 (lambda (x) (- (* 2 x) 1)) 1 (lambda (x) (+ x 1)) 500)
;should return 250000

;question 1e

(define (fact n)
(define (term n) (* n 1))
(define (next n) (+ n 1))

         (define (accumulate-tr combiner null-value term a next b)
  (define (achelp n val)
    (if (> n b) val
        (achelp (next n) (combiner (term n) val))))
  (achelp a null-value))
  
  (accumulate-tr * 1 term 1 next n))


;question 1f

(define (e-to-x x n)
  (define (term n) (/ (expt x n) (fact n)))
  (define (next n) (+ n 1))

  (accumulate-tr + 0 term 0 next n))

(e-to-x 10 100)

;question 2


(define (encode p)
  (cond ((not (= (car p) (max (car p) (cdr p)))) (+ (expt (cdr p) 2) (car p)))
        (else (+ (expt (car p) 2) (car p) (cdr p))))) 

(define (decode z)
  (cond ((< (- z (expt (floor (sqrt z)) 2)) (floor (sqrt z)))
         (cons (- z (expt (floor (sqrt z)) 2)) (floor (sqrt z))))
        (else (cons
               (floor (sqrt z))
               (- z (expt (floor (sqrt z)) 2) (floor (sqrt z)))))))

;question 3a

(define (sub-complex c d)
  (cons (- (car c) (car d)) (- (cdr c) (cdr d))))

;question 3b

(define (div-complex c d)
  (let* ((a (car c))
        (b (cdr c))
        (c (car d))
        (d (cdr d)))
    (cons (/ (+ (* a c) (* b d)) (+ (expt c 2) (expt d 2)))
          (/ (- (* b c) (* a d)) (+ (expt c 2) (expt d 2)))
          )))

;question 4ai

(define (sum-quadratic-roots a b c)
  (div-complex (sub-complex (cons 0 0) b) a))

;question 4aii

(define (prod-quadratic-roots a b c)
  (div-complex c a))
  
;question 4bi

(define (sum-cubic-roots a b c d)
  (div-complex (sub-complex (cons 0 0) b) a))

;question 4bii

(define (sum-pairs-cubic-roots a b c d)
  (div-complex c a))

;question 4biii

(define (prod-cubic-roots a b c d)
  (div-complex (sub-complex (cons 0 0) d) a))


