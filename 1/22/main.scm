(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n
                    (- (runtime)
                       start-time))))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (find-primes-range lower upper)
  (define (iter-odds n)
    (cond ((<= n upper) (timed-prime-test n)
                        (iter-odds (+ n 2)))))
  (iter-odds (if (odd? lower) lower (+ lower 1))))

(find-primes-range 1000 1019)
(newline)
(find-primes-range 10000 10037)
(newline)
(find-primes-range 100000 100043)
(newline)
(find-primes-range 1000000 1000037)
(newline) 
(find-primes-range 1000000000 1000000021)
(newline) 
(find-primes-range 10000000000 10000000061)
(newline) 
(find-primes-range 100000000000 100000000057)
(newline) 
(find-primes-range 1000000000000 1000000000063)

; 1009 *** 0.
; 1013 *** 0.
; 1019 *** 0.
; 
; 10007 *** 0.
; 10009 *** 0.
; 10037 *** 0.
; 
; 100003 *** 0.
; 100019 *** .01
; 100043 *** 0.
; 
; 1000003 *** 0.
; 1000033 *** 0.
; 1000037 *** 0.
; 
; 1000000007 *** 3.9999999999999994e-2
; 1000000009 *** .04000000000000001
; 1000000021 *** .04000000000000001
; 
; 10000000019 *** .09000000000000002
; 10000000033 *** .08999999999999997
; 10000000061 *** .08999999999999997
; 
; 100000000003 *** .30000000000000004
; 100000000019 *** .29000000000000004
; 100000000057 *** .29000000000000004
; 
; 1000000000039 *** .8900000000000001
; 1000000000061 *** .9199999999999999
; 1000000000063 *** .8999999999999999
;
; My computer is too fast to find a difference in time for the ranges
; {[133, 1e3 + 19], [1e4, 1e4 + 37], [1e5, 1e5 + 43], [1e6, 1e6 + 37]}
; Starting with primes near 1e(7+k), k in N = {0, 1, 2, ...}, a linear
; growth of 3 ~ sqrt(10) can be seen, and this is what we expected
