(define nil (list))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else  (filter predicate
                       (cdr sequence)))))

(define (any predicate sequence)
  (if (null? sequence)
      #f
      (or (predicate (car sequence))
          (any predicate (cdr sequence)))))

(define (nth n sequence)
  (if (= n 1)
    (car sequence)
    (nth (- n 1) (cdr sequence))))

(define (take n sequence)
  (if (= n 0)
    nil
    (cons (car sequence)
          (take (- n 1) (cdr sequence)))))

(define (queens board-size)
  (define (make-position row column)
    (cons row column))
  (define (get-row position) (car position))
  (define (get-col position) (cdr position))

  (define (safe? k positions)
    (define (same-row? p1 p2)
      (= (get-row p1) (get-row p2)))

    ; this redundant way sucks but at the moment
    ; I can't think of any better way
    (define (same-diag-down? p1 p2)
      (= (get-row p2)
         (+ (get-row p1)
            (- (get-col p2)
               (get-col p1)))))

    (define (same-diag-up? p1 p2)
      (= (get-row p1)
         (+ (get-row p2)
            (- (get-col p2)
               (get-col p1)))))

    (not (any (lambda (position)
                (let ((last (nth k positions)))
                  (or (same-row? position
                                 last)
                      (same-diag-down? position
                                       last)
                      (same-diag-up? position
                                     last))))
              (take (- k 1) positions))))

  (define (adjoin-position row column positions)
    (append positions (list (make-position row column))))

  (define empty-board nil)

  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(display (queens 4))
