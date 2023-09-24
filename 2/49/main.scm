(define (make-vect x y)
  (cons x y))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))

(define outline->painter
  (segments->painter (list (make-segment (make-vect 0 0)
                                         (make-vect 0 1))
                           (make-segment (make-vect 0 1)
                                         (make-vect 1 1))
                           (make-segment (make-vect 1 1)
                                         (make-vect 1 0))
                           (make-segment (make-vet 1 0)
                                         (make-vect 0 0)))))

(define x->painter
  (segments->painter (list (make-segment (make-vect 0 0)
                                         (make-vect 1 1))
                           (make-segment (make-vect 1 0)
                                         (make-vect 0 1)))))

(define diamond->painter
  (segments->painter (list (make-segment (make-vect 0 0.5)
                                         (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1)
                                         (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5)
                                         (make-vect 0.5 0.5))
                           (make-segment (make-vect 0.5 0.5)
                                         (make-vect 0 0.5)))))

(define wave->painter
  ; TODO
  (segments->painter (list)))
