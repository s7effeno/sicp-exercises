(define (square x) (* x x))

(define (average x y z)
  (/ (+ x y z) 3))

(define (improve guess x)
  (average (/ x (square guess))
           guess
           guess))

(define (good-enough? guess x)
  (= (improve guess x) guess))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (cbrt-iter (improve guess x) x)))

(define (cbrt x) (cbrt-iter 1.0 x))

(cbrt 8)
