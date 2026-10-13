(library (utils)
  (export make-counter acons map-in-order adjoin non-empty-list?
          split-last read-all)
  (import (chezscheme))


  (define (make-counter)
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        n)))


  (define (acons k v alist)
    (cons (cons k v) alist))


  ;; Like map, but processes the list left to right
  (define (map-in-order f lst)
    (let loop ((lst lst)
               (acc '()))
      (if (null? lst)
          (reverse acc)
          (loop (cdr lst) (cons (f (car lst)) acc)))))


  (define (adjoin x xs)
    (if (memq x xs) xs (cons x xs)))


  (define (non-empty-list? x)
    (and (pair? x) (list? x)))


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
