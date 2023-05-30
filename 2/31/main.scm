(define nil (list))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square x) (* x x))

(define (tree-map proc tree)
  (map (lambda (tree)
         (if (pair? tree)
           (tree-map proc tree)
           (proc tree)))
       tree))

(define (square-tree tree) 
  (tree-map square tree))

(display (square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))))
