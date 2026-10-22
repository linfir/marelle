;; A pair kept across collections holds the fresh objects stored into it
(define p (cons '() '()))
(let loop ((i 0))
  (when (< i 20000)
    (set-car! p (list i (make-vector 3 i)))
    (set-cdr! p (if (pair? (cdr p)) (cadr p) (car p)))
    (loop (+ i 1))))
p
