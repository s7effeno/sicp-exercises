(define (cont-frac n d k)
  (define (recurse i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (recurse (+ i 1))))))
  (recurse 1))

(define (e k)
  (define (n i) 1.0)
  (define (d i)
    (if (= (remainder i 3) 2)
        (* (/ (+ i 1) 3) 2)
        1.0))
  (+ (cont-frac n d k) 2.0))

(display (e 50))
