(define (split split-main split-smaller)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split split-main split-smaller) painter
                                      (- n 1))))
          (split-main painter
                      (split-smaller smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))
