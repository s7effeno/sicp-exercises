(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) acc)))))
  (iter k 0))

(define (square x) (* x x))
(define (nth-odd n) (- (* n 2) 1))
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (square x))))
  (define (d i) (nth-odd i))
  (- 0 (cont-frac n d k)))
