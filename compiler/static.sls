;; A compilation's static objects: immutable data laid out at compile time, outside the heap,
;; which the collector never scans. Each object has its entry and its label, referenced as the
;; label, or plus 7 for an immutable pair (a list's pairs in one entry); strings and symbols are
;; shared, and a symbol the library has is the library's, so symbols stay unique.

(library (static)
  (export static-data new-static-data place-static! static-data-entries static-symbol-entries
          add-library-symbol!)
  (import (chezscheme)
          (utils)
          (defs)
          (output)
          (parse))


  ;; entries: the entries, last first; strings: string to reference; symbols: symbol to
  ;; reference, the library's too; own: this unit's symbols, (symbol . reference), last first
  (define-record-type state
    (fields (mutable entries) strings symbols (mutable own)))


  ;; The compilation's state, else #f
  (define static-data (make-parameter #f))


  (define (new-static-data)
    (make-state '() (make-hashtable string-hash string=?) (make-eq-hashtable) '()))


  ;; The library's static symbol name, at the reference label, for a program
  (define (add-library-symbol! name label)
    (hashtable-set! (state-symbols (static-data)) name label))


  ;; The reference to the static object equal to x, or its immediate's word
  (define (place-static! x)
    (cond
     ((pair? x) (place-list x))
     ((null? x) (immediate-value ''()))
     ((string? x) (place-string x))
     ((symbol? x) (place-symbol x))
     ((vector? x) (place-vector x))
     ((bytevector? x) (place-bytevector x))
     ((and (exact-rational? x) (not (fixnum-value? x))) (intern-number! x))
     ((or (fixnum-value? x) (boolean? x) (char? x)) (immediate-value x))
     (else (compile-error 'place-static! "unsupported static datum" x))))


  ;; The entries of the static objects, and the table of the unit's symbols, name its label:
  ;; their count, then each
  (define (static-data-entries name)
    (let ((own (reverse (state-own (static-data)))))
      `(,@(reverse (state-entries (static-data)))
        (rodata ,name (quad ,(length own)) ,@(map (lambda (s) `(quad ,(cdr s))) own)))))


  ;; The interface's entries for the unit's symbols, (static-symbol name label)
  (define (static-symbol-entries)
    (map (lambda (s) `(static-symbol ,(car s) ,(cdr s)))
         (reverse (state-own (static-data)))))


  (define (new-label)
    (let ((label (mangle (fresh-name 'static))))
      (add-symbol! label 'static)
      label))


  (define (add-entry! entry)
    (state-entries-set! (static-data) (cons entry (state-entries (static-data)))))


  (define (immediate-value x)
    (cadadr (immediate x)))


  ;; Along the spine, so a long list does not nest: its pairs in one entry
  (define (place-list x)
    (let* ((label (new-label))
           (words (let loop ((x x) (i 0))
                    (let ((car-word (place-static! (car x))))
                      (if (pair? (cdr x))
                          (cons* car-word `(+ ,label ,(+ (* 8 (+ i 2)) 7)) (loop (cdr x) (+ i 2)))
                          (list car-word (place-static! (cdr x))))))))
      (add-entry! `(rodata ,label ,@(map (lambda (w) `(quad ,w)) words)))
      `(+ ,label 7)))


  ;; The bytes as little-endian words, the last padded with zeros
  (define (byte-words bytes)
    (if (null? bytes)
        '()
        (let ((n (min 8 (length bytes))))
          (cons (fold-right (lambda (b w) (+ b (bitwise-arithmetic-shift-left w 8)))
                            0 (list-head bytes n))
                (byte-words (list-tail bytes n))))))


  ;; ASCII, [header, nchars, bytes...], else UTF-8, [header, nchars, nbytes, cache-index,
  ;; cache-offset, bytes...] (lib/strings.ss), writable since indexing writes its cache
  (define (place-string s)
    (let ((strings (state-strings (static-data))))
      (or (hashtable-ref strings s #f)
          (let* ((bytes (bytevector->u8-list (string->utf8 s)))
                 (units (byte-words bytes))
                 (label (new-label)))
            (add-entry! (if (= (length bytes) (string-length s))
                            `(rodata ,label (quad ,immutable-ascii-string-type) (quad ,(string-length s))
                                     ,@(map (lambda (w) `(quad ,w)) units))
                            `(words ,label ,immutable-utf8-string-type ,(string-length s) ,(length bytes)
                                    0 0 ,@units)))
            (hashtable-set! strings s label)
            label))))


  (define (place-symbol x)
    (let ((symbols (state-symbols (static-data))))
      (or (hashtable-ref symbols x #f)
          (let* ((name (symbol->string x))
                 (label (new-label)))
            (hashtable-set! symbols x label)
            (state-own-set! (static-data) (cons (cons x label) (state-own (static-data))))
            (add-entry! `(rodata ,label (quad ,symbol-type) (quad ,(static-symbol-hash name))
                                 (quad ,(place-string name))))
            label))))


  (define (place-vector v)
    (let* ((elements (map place-static! (vector->list v)))
           (label (new-label)))
      (add-entry! `(rodata ,label (quad ,immutable-vector-type) (quad ,(vector-length v))
                           ,@(map (lambda (w) `(quad ,w)) elements)))
      label))


  (define (place-bytevector bv)
    (let ((label (new-label)))
      (add-entry! `(rodata ,label (quad ,immutable-bytevector-type) (quad ,(bytevector-length bv))
                           ,@(map (lambda (w) `(quad ,w)) (byte-words (bytevector->u8-list bv)))))
      label))


  (define (fixnum-value? x)
    (and (integer? x) (exact? x) (<= (- (expt 2 60)) x (- (expt 2 60) 1))))


  ;; The hash of the string s, as string-hash-word (lib/symbols.ss) computes it at run time
  (define (static-symbol-hash s)
    (fold-left (lambda (h c) (modulo (* (+ h (char->integer c) 1) #x9E3779B97F4A7C15) (expt 2 64)))
               0 (string->list s))))
