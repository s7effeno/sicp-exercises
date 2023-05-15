(define (make-interval a b) (cons a b))

(define (lower-bound z) (car z))

(define (upper-bound z) (cdr z))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (in-interval i x)
  (and (>= x (lower-bound i)) (<= x (upper-bound i))))

(define (div-interval x y)
  (if (in-interval y 0)
    (error "divison by 0")
    (mul-interval x 
                  (make-interval 
                   (/ 1.0 (upper-bound y)) 
                   (/ 1.0 (lower-bound y))))))
