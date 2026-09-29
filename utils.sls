(library (utils)
  (export acons split-last read-all round-up-even make-counter proc-counter map-in-order)
  (import (chezscheme))


  (define (make-counter)
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        n)))


  (define proc-counter (make-counter))


  (define (acons k v alist)
    (cons (cons k v) alist))


  ;; Like map, but processes the list left to right
  (define (map-in-order f lst)
    (let loop ((lst lst)
               (acc '()))
      (if (null? lst)
          (reverse acc)
          (loop (cdr lst) (cons (f (car lst)) acc)))))


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
