(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (inc a) (+ a 1))
(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(define (even? n) (= (remainder n 2) 0))

(define (pi n)
  (define (pi-term n)
    (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))
  (* 4 (product pi-term 1 inc n)))
