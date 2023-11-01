(define (base e)
  (cadr e))
(define (exponent e)
  (caddr e))
(define (make-exponentiation b e)
  (cond ((= e 0) 1)
        ((= e 1) b)
        (else (list '** b e))))
(define (install-exponentiation-package)
  (define (deriv-exponentiation exp var)
    (make-product
      (make-product
        (exponent exp)
        (make-exponentiation
          (base exp)
          (make-sum (exponent exp) '-1)))))
  (put 'deriv '(**) deriv-exponentiation))
