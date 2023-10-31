(define (install-sum-package)
  (define (make-sum a1 a2) (list a1 '+ a2))
  (define (addend s) (car s))
  (define (augend s) (caddr s))
  (define (deriv-sum exp var)
   (make-sum (deriv (addend exp) var)
             (deriv (augend exp) var)))
  (put 'deriv ('+) deriv-sum))

(define (install-product-package)
  (define (make-product m1 m2) (list m1 '* m2))
  (define (multiplier p) (car p))
  (define (multiplicand p) (caddr p))
  (define (deriv-product exp var)
    (make-sum
      (make-product
        (multiplier exp)
        (deriv (multiplicand exp) var))
      (make-product
        (deriv (multiplier exp) var)
        (multiplicand exp))))
  (put 'deriv ('*) deriv-product))
