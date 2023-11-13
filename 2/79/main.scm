(define (install-scheme-number-package)
  ; ...
  (put 'equ? '(scheme-number scheme-number) =))

(define (install-rational-number-package)
  ; ...
  (define (equ? x y)
    (= 0 (- (* (numer x) (denom y))
            (* (numer y) (denom x)))))
  (put 'equ? '(rational-number rational-number) equ?))

(define (install-complex-number-package)
  ; ...
  (define (equ? x y)
    (and (= (real-part x) (real-part y))
         (= (imag-part x) (imag-part y))))
  (put 'equ? '(complex-number complex-number) equ?))

; generic arithmetic package:
(define (equ? x y)
  (apply-generic 'equ? x y))
