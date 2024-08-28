(define (make-monitored proc)
  (let ((count 0))
      (lambda (param)
        (if (eq? param 'how-many-calls?)
          count
          (begin (set! count (+ count 1))
                 (proc param))))))

(define s (make-monitored sqrt))
(s 100)
(s 100)
(s 100)
(s 'how-many-calls?)
