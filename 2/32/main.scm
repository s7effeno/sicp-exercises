(define nil (list))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (item)
                       (cons (car s) item))
                     rest)))))

(display (subsets (list 1 2 3)))

; (car s) is the first element of the set s, (cdr s)
; are the remaining elements.
; (lambda (item) (cons (car s) item)) combines (car s)
; with every other subset of the remaining set, in other words
; given a set s, if we add one element n to s, generating s U {n}
; the list of the subsets will be such that
; (subsets s U {n}) = (subsets s) U {{n} U subset for all subset in (subsets s)}
; Since (subsets nil) exists, we can build up from there recursively.
