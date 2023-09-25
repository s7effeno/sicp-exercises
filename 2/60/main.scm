; stays the same
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; check removed => Θ(1)
(define (adjoin-set x set)
  (cons x set))

; append => Θ(n)
(define (union-set set1 set2)
  (append set1 set2))

; stays the same
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

; You should use this implementation when:
; * you need to store duplicates
; * you don't care about duplicates
; * you know set1 ∩ set2 = ø
; * you know you are adding a new element
