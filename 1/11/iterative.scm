(define (f n)
  (define (f-iter n mul-n mul-n-1 mul-n-2)
    (if (< n 3)
        ;; works for negative values too
        (+ (* n mul-n)
           (* 1 mul-n-1)) ;++ at the end of the process n-1 = 1 since n=2
           ;++ also, n-3 is not needed since it's 0
        (f-iter (- n 1)
                (+ mul-n mul-n-1)
                (+ (* mul-n 2) mul-n-2)
                (* mul-n 3))))
  (f-iter n 1 0 0))
