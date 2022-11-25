(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (not (even? n)))

(define (square n)
  (* n n))

(define (exp b n)
  (define (exp-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (exp-iter a (square b) (/ n 2)))
          ((odd? n) (exp-iter (* a b) b (- n 1)))))
  (exp-iter 1 b n))
