(library (utils)
  (export acons split-last read-all round-up-even proc-counter)
  (import (chezscheme))


  (define proc-counter
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        n)))


  (define (acons k v alist)
    (cons (cons k v) alist))


  (define (round-up-even n)
    (if (even? n) n (+ n 1)))


  ;; x must be a non-empty list
  (define (split-last x)
    (let loop ((x x)
               (acc '()))
      (if (null? (cdr x))
          (values (reverse acc) (car x))
          (loop (cdr x) (cons (car x) acc)))))


  (define (read-all)
    (let loop ((acc '()))
      (let ((x (read)))
        (if (eof-object? x)
            (reverse acc)
            (loop (cons x acc)))))))
