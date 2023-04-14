(define (harmonic n)
  (if (= n 1)
      1
      ((/ 1 n) (harmonic (+ n 1)))))

(harmonic 10)