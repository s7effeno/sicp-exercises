(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right)) (+ (weight left) (weight right))))

(define (successive-merge nodes)
  (define (adjoin-node x nodes)
    (if (or (null? nodes)
            (< (weight x) (weight (car nodes))))
        (cons x nodes)
        (cons (car nodes)
              (adjoin-node x (cdr nodes)))))
  (if (= (length nodes) 1)
      nodes
      (let ((merged (make-code-tree (car nodes)
                                    (cadr nodes)))
            (rest (cddr nodes)))
        (successive-merge (adjoin-node merged rest)))))

(define (make-leaf-set pairs)
  (list (make-leaf 'H 1)
        (make-leaf 'G 1)
        (make-leaf 'F 1)
        (make-leaf 'E 1)
        (make-leaf 'D 1)
        (make-leaf 'C 1)
        (make-leaf 'B 3)
        (make-leaf 'A 8)))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(display (generate-huffman-tree '()))
