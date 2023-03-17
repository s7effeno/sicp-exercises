(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 500))

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

; 1009 *** .01
; 1013 *** 9.999999999999998e-3
; 1019 *** 1.0000000000000002e-2
; 
; 10007 *** 1.0000000000000002e-2
; 10009 *** 9.999999999999995e-3
; 10037 *** 1.0000000000000009e-2
; 
; 100003 *** 9.999999999999995e-3
; 100019 *** 9.999999999999995e-3
; 100043 *** 2.0000000000000004e-2
; 
; 1000003 *** 9.999999999999995e-3
; 1000033 *** 1.0000000000000009e-2
; 1000037 *** .01999999999999999
; 
; 1000000007 *** 2.0000000000000018e-2
; 1000000009 *** .03
; 1000000021 *** .01999999999999999
; 
; 10000000019 *** .03
; 10000000033 *** 3.0000000000000027e-2
; 10000000061 *** .02999999999999997
; 
; 100000000003 *** 3.0000000000000027e-2
; 100000000019 *** .02999999999999997
; 100000000057 *** 3.0000000000000027e-2
; 
; 1000000000039 *** .03999999999999998
; 1000000000061 *** .03999999999999998
; 1000000000063 *** 3.0000000000000027e-2
;
; This results are kind of what was expected (to some degree), it is observable that
; (in some cases) as log(n) doubles, the time somewhat doubles
