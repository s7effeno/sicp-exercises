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

(define (triples-sum n s)
  (filter
    (lambda (t) (= (+
                     (car t)
                     (cadr t)
                     (caddr t))
                   s))
    (flatmap (lambda (i)
               (flatmap (lambda (j)
                          (map (lambda (k)
                                 (list i j k))
                               (enumerate-interval 1 n)))
                        (enumerate-interval 1 n)))
             (enumerate-interval 1 n))))

(display (triples-sum 10 6))
