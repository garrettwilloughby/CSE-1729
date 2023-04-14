;question 1

(define (fact n)
  (let ((fact 1)
        (count 0))
    (define (helper)
      (cond ((= count n) 'done)
            (else
             (set! count (+ count 1))
             (set! fact (* fact count))
                   (helper))))
            (helper)
            fact))

(fact 4)


;question 2

(define (new-account initial-balance)
  (let ((balance initial-balance))
    (define (deposit f)
      (set! balance (+ balance f))
      balance)
    (define (withdraw f)
      (cond ((> f balance) "Insufficient funds")
            (else
             (set! balance (- balance f))
             balance)))
    (define (bal-inq) balance)
    (lambda (method)
      (cond ((eq? method 'deposit) deposit)
            ((eq? method 'withdraw) withdraw)
            ((eq? method 'balance-inquire) bal-inq)))))