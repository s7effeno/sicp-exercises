(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (not (even? n)))

(define (* a b)
  (define (*-iter acc a b)
    (cond ((= b 0) acc)
	  ((even? b) (*-iter acc (double a) (halve b)))
	  ((odd? b) (*-iter (+ acc a) a (- b 1)))))
