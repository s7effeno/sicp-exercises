(define (square x) (* x x))
(define (sum_of_squares x1 x2) (+ (square x1) (square x2)))

(define (smallest_3 x1 x2 x3)
  (if (< x1 x2)
      (if (< x1 x3) 
          x1
          x3)
      (if (< x2 x3)
          x2
          x3)))

(define (sum_largest_2_numbers_squares x1 x2 x3)
  (cond
   ((= x1 (smallest_3 x1 x2 x3))
    (sum_of_squares x2 x3))
   ((= x2 (smallest_3 x1 x2 x3))
    (sum_of_squares x1 x3))
   ((= x3 (smallest_3 x1 x2 x3))
    (sum_of_squares x1 x2))))

(define n1 3)
(define n2 4)
(define n3 5)

(sum_largest_2_numbers_squares n1 n2 n3)
