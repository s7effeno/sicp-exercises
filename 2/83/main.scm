(define (integer->rational n)
  (make-rat (contents n) 1))

(put 'raise '(integer) integer->rational)

(define (rational->real n)
  (let ((n (contents n)))
    (make-real (/ (numer n)
                  (denom n)))))

(put 'raise '(rational) rational->real)

(define (real->complex n)
  (make-complex-from-real-imag
    (contents n) 0))

(put 'raise '(real) rational->complex)

(define (raise n)
  (apply-generic 'raise n))
