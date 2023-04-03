(define (square x) (* x x))
(define (cube x) (* x (square x)))

(define (odd? n)
  (= (remainder n 2) 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc a) (+ a 1))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (simpson-multiplier k)
    (if (or (= k n) (= k 0))
        1
        (* 2 (+ 1 (remainder k 2)))))
  (define (simpson-term k) (* (y k) (simpson-multiplier k)))
  (if (odd? n)
      (simpson f a b (+ n 1))
      (sum simpson-term (y 0) inc n)))

(display (simpson cube 0 1 100.0))
(newline)
(display (simpson cube 0 1 1000.0))
(newline)
