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

(define (cons a b)
  (* (exp 2 a) (exp 3 b)))

(define (car z)
    (if (= (remainder z 2) 0)
        (+ 1 (car (/ z 2)))
        0))

(define (cdr z)
    (if (= (remainder z 3) 0)
        (+ 1 (car (/ z 3)))
        0))

(display (car (cons 3 2)))
(newline)
(display (cdr (cons 3 2)))
