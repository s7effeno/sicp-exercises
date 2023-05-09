(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (identity x) x)
(define (repeated f n)
  (if (= n 0)
    identity
    (compose f (repeated f (- n 1)))))

(define (square x) (* x x))
(display ((repeated square 2) 5))
