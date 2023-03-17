(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (define (next)
    (if (= test-divisor 3)
        3
        2))
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor (next))))))

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

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(newline)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(newline)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(newline)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)
(newline)
(timed-prime-test 1000000007)
(timed-prime-test 1000000009)
(timed-prime-test 1000000021)
(newline)
(timed-prime-test 10000000019)
(timed-prime-test 10000000033)
(timed-prime-test 10000000061)
(newline)
(timed-prime-test 100000000003)
(timed-prime-test 100000000019)
(timed-prime-test 100000000057)
(newline)
(timed-prime-test 1000000000039)
(timed-prime-test 1000000000061)
(timed-prime-test 1000000000063)

;; Before
;
; 1009 *** 0.
; 1013 *** 0.
; 1019 *** 0.
; 
; 10007 *** 0.
; 10009 *** 0.
; 10037 *** 0.
; 
; 100003 *** 0.
; 100019 *** 0.
; 100043 *** 0.
; 
; 1000003 *** 0.
; 1000033 *** 0.
; 1000037 *** 0.
; 
; 1000000007 *** .03
; 1000000009 *** .03
; 1000000021 *** .03
; 
; 10000000019 *** .09000000000000002
; 10000000033 *** .09999999999999998
; 10000000061 *** .09000000000000002
; 
; 100000000003 *** .3
; 100000000019 *** .29000000000000004
; 100000000057 *** .2999999999999998
; 
; 1000000000039 *** .9500000000000002
; 1000000000061 *** .9300000000000002
; 1000000000063 *** .9199999999999999

;; Now
;
; 1009 *** 0.
; 1013 *** 0.
; 1019 *** 0.
; 
; 10007 *** 0.
; 10009 *** 0.
; 10037 *** 0.
; 
; 100003 *** 0.
; 100019 *** 0.
; 100043 *** 0.
; 
; 1000003 *** 0.
; 1000033 *** 0.
; 1000037 *** .01
; 
; 1000000007 *** .02
; 1000000009 *** 1.9999999999999997e-2
; 1000000021 *** .03
; 
; 10000000019 *** .07
; 10000000033 *** .06
; 10000000061 *** 6.0000000000000026e-2
; 
; 100000000003 *** .19999999999999996
; 100000000019 *** .20999999999999996
; 100000000057 *** .20000000000000007
; 
; 1000000000039 *** .63
; 1000000000061 *** .6200000000000001
; 1000000000063 *** .6299999999999999

; We can notice the time hasn't exactly halved, rather there's a ratio of 3/2
; between the previous version and the current one.
; I don't know the exact reason behind it but the `next` procedure is more
; complicated than a mere increment, it requires a test as well, so 
; the "amount of incremets" has halved but every increment requires a test.
