(define (square x) (* x x))
(define (sum_of_squares x1 x2) (+ (square x1) (square x2)))

(define (max a b)
  (if (> a b) a b))

(define (min a b)
  (if (= a (max a b)) b a))

(define (sum_largest_2_numbers_squares a b c)
  (sum_of_squares (max a b) (max (min a b) c)))

(define n1 3)
(define n2 4)
(define n3 5)

(sum_largest_2_numbers_squares n1 n2 n3)
