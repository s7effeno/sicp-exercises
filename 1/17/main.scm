(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (not (even? n)))

(define (* a b)
  (cond ((= b 0) 0)
	((even? b) (double (* a (halve b))))
	((odd? b) (+ a (* a (- b 1))))))
