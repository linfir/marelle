;; SRFI 1's reduce, which Chez lacks
(define (reduce f default l)
  (if (null? l)
      default
      (fold-left (lambda (acc x) (f x acc)) (car l) (cdr l))))
