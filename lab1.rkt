(define (usd-to-euro x)(* x .82))
(define (euro-to-yen x)(* x 126.01))
(define (usd-to-yen x)(euro-to-yen (usd-to-euro x)))


(define e 2.71828)
(define (tanh x) ( / (- (expt e (* 2 x)) 1) ( + (expt e (* 2 x)) 1)))

(define (det2x2 a b c d) (- (* a d)(* b c)))
(det2x2 2 -4 -6 12)

(define (invertible? a b c d)(not (= 0 (det2x2 a b c d))))
(invertible? 2 -4 -6 12)

(define (prod-inv-direct? a1 b1 c1 d1 a2 b2 c2 d2) (invertible? (+ (* a1 a2) (* b1 c2)) (+ (* c1 a2) (* d1 c2)) (+ (* a1 b2) (* b1 d2)) (+ (* c1 b2) (* d1 d2))))

(define (prod-inv-indirect? a1 b1 c1 d1 a2 b2 c2 d2) (not (= 0 (* (det2x2 a2 b2 c2 d2) (det2x2 a1 b1 c1 d1)))))

