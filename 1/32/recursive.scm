(define (accumulate combiner null-value term a next b)
  (define (recurse a)
    (if (> a b)
        null-value
        (combiner (term a)
                  (recurse (next a)))))
  (recurse a))
