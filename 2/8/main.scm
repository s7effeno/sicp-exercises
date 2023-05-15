(define (make-interval a b) (cons a b))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (lower-bound y))
                               (- (upper-bound y)))))

(display (lower-bound (sub-interval
                        (make-interval 5 10)
                        (make-interval 3 7))))
(newline)
(display (upper-bound (sub-interval
                        (make-interval 5 10)
                        (make-interval 3 7))))
