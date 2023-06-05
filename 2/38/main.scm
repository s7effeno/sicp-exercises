; `op` should respect the associative property for`fold-right`
; and `fold-left` to produce ; the same values for any sequence

(define nil (list))
ve 
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op
                      initial
                      (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(display (fold-right / 1 (list 1 2 3))) 
; (/ 1 (fold-right / 1 (2 3)))
; (/ 1 (/ 2 (fold-right / 1 (3))))
; (/ 1 (/ 2 (/ 3 (fold-right / 1 ()))))
; (/ 1 (/ 2 (/ 3 1)))
; 3/2
(newline)
(display (fold-left / 1 (list 1 2 3)))
; (iter 1 (1 2 3))
; (iter (/ 1 1) (2 3))
; (iter 1 (2 3))
; (iter (/ 1 2) (3))
; (iter 1/2 (3))
; (iter (/ 1/2 3) ())
; (iter 1/6 nil)
; 1/6
(newline)
(display (fold-right list nil (list 1 2 3)))
; (list 1 (fold-right list nil (2 3)))
; (list 1 (list 2 (fold-right list nil (3))))
; (list 1 (list 2 (list 3 (fold-right list nil ()))))
; (list 1 (list 2 (list 3 nil)))
; (1 (2 (3 ())))
(newline)
(display (fold-left list nil (list 1 2 3)))
; (iter nil (1 2 3))
; (iter (list nil 1) (2 3))
; (iter (() 1) (2 3))
; (iter (list (() 1) 2) (3))
; (iter ((() 1) 2) (3))
; (iter (list ((() 1) 2) 3) ())
; (iter (((() 1) 2) 3) ())
; (((() 1) 2) 3)
