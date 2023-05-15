(define (make-interval a b) (cons a b))

(define (lower-bound z) (car z))

(define (upper-bound z) (cdr z))

(define (display-interval x)
  (display "[")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display "]"))

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (if (> lx 0)
        (if (> ly 0)
            (make-interval (* lx ly) (* ux uy)) ; ++++
            (if (> uy 0)
                (make-interval (* ux ly) (* ux uy)) ; ++-+
                (make-interval (* ux ly) (* lx uy)))) ; ++--
        (if (> ux 0)
            (if (> ly 0)
                (make-interval (* lx uy) (* ux uy)) ; -+++
                (if (> uy 0)
                    (make-interval (min (* lx uy) (* ux ly)) ; -+-+
                                   (max (* lx ly) (* ux uy)))
                    (make-interval (* ux ly) (* lx ly)))) ; -+--
            (if (> ly 0)
                (make-interval (* lx uy) (* ux ly)) ; --++
                (if (> uy 0)
                    (make-interval (* lx uy) (* lx ly)) ; ---+
                    (make-interval (* ux uy) (* lx ly)))))))) ; ----

 (define (mul-interval x y) 
         (let            ((p1 (* (lower-bound x) (lower-bound y))) 
                          (p2 (* (lower-bound x) (upper-bound y))) 
                          (p3 (* (upper-bound x) (lower-bound y))) 
                          (p4 (* (upper-bound x) (upper-bound y)))) 
                 (make-interval 
                         (min p1 p2 p3 p4) 
                         (max p1 p2 p3 p4)))) 


(define (display-interval x)
  (display "[")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display "]")
  (newline))

(display-interval (mul-interval (make-interval 2 3) (make-interval 4 5)))
(display-interval (mul-interval (make-interval 2 3) (make-interval (- 4) 5)))
(display-interval (mul-interval (make-interval (- 2) 3) (make-interval 4 5)))
(display-interval (mul-interval (make-interval 2 3) (make-interval (- 5) (- 4))))
(display-interval (mul-interval (make-interval (- 2) 3) (make-interval 4 5)))
(display-interval (mul-interval (make-interval (- 2) 3) (make-interval (- 4) 5)))
(display-interval (mul-interval (make-interval (- 2) 3) (make-interval (- 5) (- 4))))
(display-interval (mul-interval (make-interval (- 3) (- 2)) (make-interval 4 5)))
(display-interval (mul-interval (make-interval (- 3) (- 2)) (make-interval (- 4) 5)))
(display-interval (mul-interval (make-interval (- 3) (- 2)) (make-interval (- 5) (- 4))))
(newline)

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

(display-interval (mul-interval (make-interval 2 3) (make-interval 4 5)))
(display-interval (mul-interval (make-interval 2 3) (make-interval (- 4) 5)))
(display-interval (mul-interval (make-interval (- 2) 3) (make-interval 4 5)))
(display-interval (mul-interval (make-interval 2 3) (make-interval (- 5) (- 4))))
(display-interval (mul-interval (make-interval (- 2) 3) (make-interval 4 5)))
(display-interval (mul-interval (make-interval (- 2) 3) (make-interval (- 4) 5)))
(display-interval (mul-interval (make-interval (- 2) 3) (make-interval (- 5) (- 4))))
(display-interval (mul-interval (make-interval (- 3) (- 2)) (make-interval 4 5)))
(display-interval (mul-interval (make-interval (- 3) (- 2)) (make-interval (- 4) 5)))
(display-interval (mul-interval (make-interval (- 3) (- 2)) (make-interval (- 5) (- 4))))
