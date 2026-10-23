;; The optimizations of a fun, between check-cps of the converter's output and the frames pass:
;; the passes in order, each with its --pass name and check-cps after it.
;; SCHEME_OPT in the environment, a comma-separated list, disables some: -name a pass, none all
;; of them.

(library (opt)
  (export optimize optimize-unit)
  (import (except (chezscheme) bytevector-copy!)
          (utils)
          (host)
          (cps)
          (simplify)
          (cse)
          (inline)
          (contify)
          (cells)
          (lift))


  ;; (name . pass), (pass def fun) a fun; each name is also in dump-passes
  (define passes
    (list (cons 'cells (lambda (def fun) (cells-to-params fun)))
          (cons 'simplify simplify)
          (cons 'cse cse)
          ;; The branches cse decided, and what they used
          (cons 'resimplify simplify)))


  (define pass-names (map car passes))


  ;; The passes on a whole unit, (name . pass), (pass defs funs reoptimize) the funs, #f
  ;; for a def's that went
  (define unit-passes
    (list (cons 'lift lift-unit)
          (cons 'contify contify-unit)
          (cons 'inline inline-unit)))


  ;; The names of the passes SCHEME_OPT disables, once read
  (define disabled #f)


  (define (disabled-passes)
    (unless disabled
      (set! disabled (parse-opt (get-environment-variable "SCHEME_OPT"))))
    disabled)


  (define (parse-opt s)
    (cond
     ((or (not s) (string=? s "")) '())
     ((string=? s "none") (append pass-names (map car unit-passes)))
     (else
      (map (lambda (item)
             (let ((name (and (> (string-length item) 1)
                              (char=? (string-ref item 0) #\-)
                              (string->symbol (substring item 1 (string-length item))))))
               (unless (and name (or (memq name pass-names) (assq name unit-passes)))
                 (compile-errorf 'optimize "invalid SCHEME_OPT item ~s, expected none or -PASS, PASS one of ~s"
                                 item (append pass-names (map car unit-passes))))
               name))
           (split-commas s)))))


  (define (split-commas s)
    (let loop ((i 0) (start 0) (items '()))
      (cond
       ((= i (string-length s))
        (reverse (cons (substring s start i) items)))
       ((char=? (string-ref s i) #\,)
        (loop (+ i 1) (+ i 1) (cons (substring s start i) items)))
       (else
        (loop (+ i 1) start items)))))


  ;; A disabled pass leaves the fun as it is, also in its dump
  (define (optimize def fun)
    (let ((disabled (disabled-passes)))
      (fold-left (lambda (fun pass)
                   (let ((fun (if (memq (car pass) disabled) fun ((cdr pass) def fun))))
                     (dumped (car pass) (list-head fun 3) (cdddr fun))
                     (check-cps fun)
                     fun))
                 fun
                 passes)))


  ;; The unit passes on defs and their optimized funs, in the same order, a fun they change optimized again (not dumped), each pass on the defs left;
  ;; each fun dumped once, after them all, under the name unit. Gives the funs, #f for a def
  ;; that went.
  (define (optimize-unit defs funs)
    (let* ((disabled (disabled-passes))
           (reoptimize (lambda (def fun)
                         (parameterize ((dump-pass #f))
                           (optimize def fun))))
           (table (make-eq-hashtable)))
      (for-each (lambda (def fun) (hashtable-set! table def fun)) defs funs)
      (for-each (lambda (pass)
                  (unless (memq (car pass) disabled)
                    (let ((live (filter (lambda (def) (hashtable-ref table def #f)) defs)))
                      (for-each (lambda (def fun) (hashtable-set! table def fun))
                                live
                                ((cdr pass) live (map (lambda (def) (hashtable-ref table def #f)) live)
                                 reoptimize)))))
                unit-passes)
      (map (lambda (def)
             (let ((fun (hashtable-ref table def #f)))
               (when fun
                 (dumped 'unit (list-head fun 3) (cdddr fun)))
               fun))
           defs))))
