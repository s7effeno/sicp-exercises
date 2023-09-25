(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2) (list a1 '+ a2))

(define (make-product m1 m2) (list m1 '* m2))

(define (sum? x)
  (and (pair? (cdr x)) (or (eq? (cadr x) '+)
                       (sum? (cddr x)))))

(define (addend s)
  (if (eq? (cadr s) '+)
      (car s)
      (list (car s)
            (cadr s)
            (addend (cddr s)))))

(define (augend s)
  (if (eq? (cadr s) '+)
      (if (null? (cdddr s))
          (caddr s)
          (cddr s))
      (augend (cddr s))))

(define (product? x)
  (and (pair? (cdr x))
       (not (sum? x))
       (or (eq? (cadr x) '*)
           (product? (cddr x)))))

(define (multiplier p)
  (if (eq? (cadr p) '*)
      (car p)
      (list (car p)
            (cadr p)
            (multiplier (cddr p)))))

(define (multiplicand p)
  (if (eq? (cadr p) '*)
      (if (null? (cdddr p))
          (caddr p)
          (cddr p))
      (multiplicand (cddr p))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(display (deriv '(x + 3 * (x + y + 2)) 'x))
