(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (let ((square-area (* (- x2 x1)
                        (- y2 y1))))
    (display (monte-carlo trials p))
    (* square-area (monte-carlo trials p))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(estimate-integral (lambda ()
                     (let ((x (random-in-range -1.0 1.0))
                           (y (random-in-range -1.0 1.0)))
                       (not (> (+ (* x x) (* y y)) 1))))
                   -1
                   1
                   -1
                   1
                   100000)
