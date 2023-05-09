(define (iterative-improve improve good-enough?)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (square x) (* x x))
(define (average x y) 
  (/ (+ x y) 2))
(define (sqrt guess x)
  (define tolerance 0.001)
  ((iterative-improve (lambda (guess)
                       (average guess (/ x guess)))
                     (lambda (guess)
                       (< (abs (- (square guess) x))
                          tolerance))) guess))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (iterative-improve f close-enough)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(display (sqrt 1.0 625))
