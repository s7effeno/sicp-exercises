(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (sqrt x) (sqrt-iter 1.0 x))

; number too small:
(sqrt 0.000001)
; evaluates to 0.031260655525445276
; correct value: 0.001 

; number too big:
; (sqrt 99999999999999999999999)
; hangs... the guess will never be good-enough because of the limited precision



(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess))
     (* guess 0.001)))

(sqrt 0.000001)
(sqrt 99999999999999999999999)

; this was better, maybe we could ask even a little more



(define (good-enough? guess x)
  (= (improve guess x) guess))

(sqrt 0.000001)
(sqrt 99999999999999999999999)
