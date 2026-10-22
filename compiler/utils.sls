(library (utils)
  (export make-counter acons map-in-order for-each-idx adjoin non-empty-list?
          split-last span span-reverse append-reverse read-all mangle
          dump-passes dump-pass dumped add-printer! printable
          compile-error compile-errorf with-entry)
  (import (chezscheme))


  ;; Gives start + 1, start + 2, ...
  (define make-counter
    (case-lambda
     (() (make-counter 0))
     ((n)
      (lambda ()
        (set! n (+ n 1))
        n))))


  (define (acons k v alist)
    (cons (cons k v) alist))


  ;; Like map, but processes the lists left to right
  (define (map-in-order f lst . lsts)
    (let loop ((lsts (cons lst lsts))
               (acc '()))
      (if (null? (car lsts))
          (reverse acc)
          (loop (map cdr lsts) (cons (apply f (map car lsts)) acc)))))


  ;; Call f with the index and the elements at that index of each list,
  ;; indices starting at i
  (define (for-each-idx f i xs . yss)
    (unless (null? xs)
      (apply f i (car xs) (map car yss))
      (apply for-each-idx f (+ i 1) (cdr xs) (map cdr yss))))


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


  ;; Split lst after its longest prefix of elements satisfying pred.
  ;; Returns the prefix reversed, and the rest of lst
  (define (span-reverse pred lst)
    (let loop ((lst lst)
               (acc '()))
      (if (and (pair? lst) (pred (car lst)))
          (loop (cdr lst) (cons (car lst) acc))
          (values acc lst))))


  ;; (append (reverse rev-head) tail)
  (define (append-reverse rev-head tail)
    (if (null? rev-head)
        tail
        (append-reverse (cdr rev-head) (cons (car rev-head) tail))))


  ;; Like span-reverse, but the prefix is in order (SRFI-1 span)
  (define (span pred lst)
    (let-values (((rev-prefix rest) (span-reverse pred lst)))
      (values (reverse rev-prefix) rest)))


  (define (read-all)
    (let loop ((acc '()))
      (let ((x (read)))
        (if (eof-object? x)
            (reverse acc)
            (loop (cons x acc))))))


  (define absent (list 'absent))


  ;; (thunk)'s value, with key bound to value in the hashtable t while it runs; t then has key's
  ;; previous entry again, or none.
  ;; Hashtables as scoped environments: an entry made for a walk of a subterm is undone after it.
  (define (with-entry t key value thunk)
    (let ((old (hashtable-ref t key absent)))
      (hashtable-set! t key value)
      (let ((result (thunk)))
        (if (eq? old absent)
            (hashtable-delete! t key)
            (hashtable-set! t key old))
        result)))


  (define dump-passes '(front letrec closure resolve assign cps simplify cse frames compile copy-prop reg-alloc asm peephole program))


  ;; The pass whose output dump! prints, else #f
  (define dump-pass (make-parameter #f))


  ;; (pred . f) pairs: a record of pred is printed as (f record)
  (define printers '())


  (define (add-printer! pred f)
    (set! printers (cons (cons pred f) printers)))


  ;; x with each record of a printer replaced by what it is printed as
  (define (printable x)
    (cond ((pair? x) (cons (printable (car x)) (printable (cdr x))))
          ((vector? x) (list->vector (map printable (vector->list x))))
          ((find (lambda (p) ((car p) x)) printers) => (lambda (p) ((cdr p) x)))
          (else x)))


  ;; error and errorf, the records printed as in dumps
  (define (compile-error who msg . irritants)
    (apply error who msg (map printable irritants)))


  (define (compile-errorf who fmt . args)
    (apply errorf who fmt (map printable args)))


  ;; xs, printed after head to the console when pass is the dumped one, one per line; a
  ;; function's blocks one instruction per line
  (define (dumped pass head xs)
    (when (eq? pass (dump-pass))
      (let ((port (console-output-port))
            (head (printable head))
            (xs (printable xs)))
        (fprintf port "(~s" (car head))
        (for-each (lambda (x) (fprintf port " ~s" x)) (cdr head))
        (for-each (lambda (x)
                    (if (and (eq? (car head) 'function) (eq? (car x) 'block))
                        (begin
                          (fprintf port "\n  (block ~s" (cadr x))
                          (for-each (lambda (i) (fprintf port "\n    ~s" i)) (cddr x))
                          (display ")" port))
                        (fprintf port "\n  ~s" x)))
                  xs)
        (display ")\n\n" port)))
    xs)


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
