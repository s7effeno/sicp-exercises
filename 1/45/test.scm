(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define (sqrt x)
  (fixed-point-of-transform 
   (lambda (y) (/ x y))
   average-damp
   1.0))

(define (square x) (* x x))
(define (cbrt x)
  (fixed-point-of-transform 
   (lambda (y) (/ x (square y)))
   average-damp
   1.0))

(define (cube x) (* x (square x)))
(define (qrtrt x)
  (fixed-point-of-transform 
   (lambda (y) (/ x (cube y)))
   (compose average-damp average-damp)
   1.0))

(define (quartic x) (* x (cube x)))
(define (qntrt x)
  (fixed-point-of-transform 
   (lambda (y) (/ x (quartic y)))
   (compose average-damp average-damp)
   1.0))

(define (quintic x) (* x (quartic x)))
(define (sxrt x)
  (fixed-point-of-transform 
   (lambda (y) (/ x (quintic y)))
   (compose average-damp average-damp)
   1.0))

(define (sextic x) (* x (quintic x)))
(define (sprt x)
  (fixed-point-of-transform 
   (lambda (y) (/ x (sextic y)))
   (compose average-damp average-damp)
   1.0))

(define (septic x) (* x (sextic x)))
(define (octrt x)
  (fixed-point-of-transform 
   (lambda (y) (/ x (septic y)))
   (compose average-damp (compose average-damp average-damp))
   1.0))

(display (sqrt 4))
(newline)
(display (cbrt 8))
(newline)
(display (qrtrt 16))
(newline)
(display (qntrt 32))
(newline)
(display (sxrt 64))
(newline)
(display (sprt 128))
(newline)
(display (octrt 256))

; it seems like we need floor(log_2(n)) chained average-damp to calculate the nth root of x
