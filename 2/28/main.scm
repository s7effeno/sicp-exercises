(define nil (list))

(define (fringe tree)
  (define (iter tree items)
    (if (pair? tree)
      (iter (car tree) (iter (cdr tree) items))
      (if (null? tree)
        items
        (cons tree items))))
  (iter tree nil))

(define x 
  (list (list 1 2) (list 3 4)))

(display (fringe x))
(newline)
(display (fringe (list x x)))
