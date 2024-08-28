(define (make-accumulator amount)
  (lambda (increase) (begin
                       (set! amount (+ amount increase))
                       amount)))

(define A (make-accumulator 5))
(A 10)
(A 10)
