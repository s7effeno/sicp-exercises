(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
            (define (level bot top)
              (cond ((equal? (type-tag bot)
                             (type-tag top))
                     bot)
                    ; presume raise returns false when the
                    ; arg can't be raised to avoidboilerplate
                    ((raise (type-tag bot))
                     (level (raise (type-tag bot))
                            top))
                    (else #f)))
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((a1->a2 (level a1 a2))
                      (a2->a1 (level a2 a1)))
                  (cond (a1->a2
                         (apply-generic 
                          op a1->a2 a2))
                        (a2->a1
                         (apply-generic 
                          op a1 a2->a1))
                        (else
                         (error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
