(define (install-scheme-number-package)
  ; ...
  (define (=zero? x)
    (= 0 x))
  (put '=zero? '(scheme-number) =zero?))

(define (install-rational-number-package)
  ; ...
  (define (=zero? x)
    (= 0 (numer x)))
  (put 'equ? '(rational-number) =zero?))

(define (install-complex-number-package)
  ; ...
  (define (=zero? x)
    (= 0 (real-part x) (imag-part x)))
  (put '=zero? '(complex-number) =zero?))

; generic arithmetic package:
(define (=zero? x)
  (apply-generic '=zero? x))
