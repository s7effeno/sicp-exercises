(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (identity x) x)
(define (repeated f n)
  (if (= n 0)
    identity
    (compose f (repeated f (- n 1)))))

(define (smooth f)
  (define (avg-3 a b c)
    (/ (+ a b c) 3))
  (define dx 0.00001)
  (lambda (x)
    (avg-3 (f (- x dx))
           (f x)
           (f (+ x dx)))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (identity x) x)
(define (repeated f n)
  (if (= n 0)
    identity
    (compose f (repeated f (- n 1)))))

(define (fold-smooth f n)
  ((repeated smooth n) f))
