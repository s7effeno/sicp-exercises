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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (successive-merge nodes)
  (display nodes)
  (newline)
  (if (= (length nodes) 1)
      nodes
      (let ((merged (make-code-tree (car nodes)
                                    (cadr nodes)))
            (rest (cddr nodes)))
        (successive-merge (adjoin-set merged rest)))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)
                    (cadr pair))
         (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree) (eq? (symbol-leaf tree)
                                symbol))
         '())
        ((element-of-set? symbol
                          (symbols (left-branch tree)))
         (cons '0 (encode-symbol symbol
                                 (left-branch tree))))
        ((element-of-set? symbol
                          (symbols (right-branch tree)))
         (cons '1 (encode-symbol symbol
                                 (right-branch tree))))
        (else (error "bad symbol: ENCODE-SYMBOL"))))

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

(display (length (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA
          GET A JOB SHA NA NA NA NA NA NA NA NA
          WAH YIP YIP YIP YIP
          YIP YIP YIP YIP YIP
          SHA BOOM)
          (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2)
                                   (NA 16) (SHA 3) (YIP 9) (WAH 1))))))
