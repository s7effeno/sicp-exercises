(define nil (list))

(define (reverse items)
  (define (iter items ret)
    (if (null? items)
      ret
      (iter (cdr items) (cons (car items) ret))))
  (iter items nil))

(define (deep-reverse items)
  (define (iter items ret)
    (if (null? items)
      ret
      (iter (cdr items) (cons (if (pair? (car items))
                                (deep-reverse (car items))
                                (car items))
                              ret))))
  (iter items nil))

(define x 
  (list (list 1 2) (list 3 4)))

(display (reverse x))
(newline)
(display (deep-reverse x))
