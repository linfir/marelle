(library (utils)
  (export make-counter acons map-in-order adjoin non-empty-list?
          split-last read-all mangle)
  (import (chezscheme))


  (define (make-counter)
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        n)))


  (define (acons k v alist)
    (cons (cons k v) alist))


  ;; Like map, but processes the lists left to right
  (define (map-in-order f lst . lsts)
    (let loop ((lsts (cons lst lsts))
               (acc '()))
      (if (null? (car lsts))
          (reverse acc)
          (loop (map cdr lsts) (cons (apply f (map car lsts)) acc)))))


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
            (loop (cons x acc))))))


  (define mangle-names
    '(("<=" . "le") (">=" . "ge") ("->" . "to")
      ("?" . "p") ("!" . "x") ("+" . "add") ("-" . "sub") ("*" . "mul") ("/" . "div")
      ("%" . "rem") ("=" . "eq") ("<" . "lt") (">" . "gt")))


  ;; An assembler symbol for x: letters, digits and _ kept, a - or . between two
  ;; words becomes _, other characters get short names (? p, <= le, else xHH), parts joined with _:
  ;; pair? => pair_p, char<=? => char_le_p, x.5 => x_5; not injective (a-b, a_b)
  (define (mangle x)
    (define (word-char? c)
      (or (char<=? #\a c #\z) (char<=? #\A c #\Z) (char<=? #\0 c #\9) (char=? c #\_)))
    (define (op-parts s)
      (if (string=? s "")
          '()
          (let ((entry (find (lambda (e)
                               (let ((n (string-length (car e))))
                                 (and (<= n (string-length s))
                                      (string=? (substring s 0 n) (car e)))))
                             mangle-names)))
            (if entry
                (cons (cdr entry) (op-parts (substring s (string-length (car entry))
                                                       (string-length s))))
                (cons (format "x~x" (char->integer (string-ref s 0)))
                      (op-parts (substring s 1 (string-length s))))))))
    (let loop ((cs (string->list (symbol->string x)))
               (parts '())
               (word? #f))
      (if (null? cs)
          (let ((parts (reverse parts)))
            (fold-left (lambda (acc p) (string-append acc "_" p)) (car parts) (cdr parts)))
          (let* ((w? (word-char? (car cs)))
                 (run (let take ((cs cs))
                        (if (and (pair? cs) (eq? (word-char? (car cs)) w?))
                            (cons (car cs) (take (cdr cs)))
                            '())))
                 (rest (list-tail cs (length run)))
                 (s (list->string run)))
            (cond
             (w? (loop rest (cons s parts) #t))
             ((and word? (member s '("-" ".")) (pair? rest))
              (loop rest parts #f))
             (else (loop rest (append (reverse (op-parts s)) parts) #f))))))))
