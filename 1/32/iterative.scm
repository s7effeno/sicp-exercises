(define (accumulate combiner null-value term a next b)
  (define (iter a accumulator)
    (if (> a b)
        accumulator
        (iter (next a)
              (combiner (term a) accumulator))))
  (iter a null-value))
