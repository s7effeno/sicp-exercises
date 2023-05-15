(define (apply-percent x p) (/ (* x p) 100))

(define (make-interval a b) (cons a b))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))

(define (make-center-percent c p)
  (let ((p (apply-percent c p)))
    (make-interval (- c p) (+ c p))))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (percent i)
  (let ((c (center i)))
    (* (/ (- (upper-bound i) c) c) 100)))
