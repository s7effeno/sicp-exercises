(define nil (list))

(define (reverse items)
  (define (iter items ret)
    (if (null? items)
      ret
      (iter (cdr items) (cons (car items) ret))))
  (iter items nil))

(display (reverse (list 1 4 9 16 25)))
