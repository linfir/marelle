;; Many records live across collections
(define-record-type item (fields n (mutable twice)))
(define (build i acc)
  (if (= i 0) acc (build (- i 1) (cons (make-item i (* 2 i)) acc))))
(define items (build 20000 '()))
(for-each (lambda (it) (item-twice-set! it (+ (item-twice it) (item-n it)))) items)
(fold-left (lambda (s it) (+ s (item-twice it))) 0 items)
