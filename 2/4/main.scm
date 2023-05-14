(define (cons x y) 
  (lambda (m) (m x y)))

(define (car z) 
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(display (car (cons 0 1)))
(newline)
(display (cdr (cons 0 1)))

; (car (cons x y))
; (car (lambda (m) (m x y)))
; ((lambda (m) (m x y)) (lambda (p q) p))
; (lambda (p q) p) x y)
; (lambda (x y) x)
; x

; (cdr (cons x y))
; (cdr (lambda (m) (m x y)))
; ((lambda (m) (m x y)) (lambda (p q) q))
; (lambda (p q) q) x y)
; (lambda (x y) y)
; y
