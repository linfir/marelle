;; Strings, immutable. The header is the kind, chosen by content: ASCII, [header, nchars,
;; bytes...], when every char is below 128, else UTF-8, [header, nchars, nbytes, cache-index,
;; cache-offset, bytes...]; nchars, nbytes and the cache are raw words, the bytes padded with
;; zeros to a whole word. A UTF-8 string caches the byte offset of the last char indexed, so
;; indexing near it is fast (utf8-offset). UTF-32, [header, nchars, units...], 4 bytes per char,
;; only comes from static data; 51 is kept for a later kind. ASCII and UTF-8 strings share their
;; bytes' encoding, so building one writes UTF-8 bytes (put-utf8-range!). The static strings of
;; embedded-datum have the immutable types, 64 more (header bit 9): every test of the kind masks
;; that bit, and a future mutator must check the mutable types exactly

;; Exactly the eight words (48 to 51 and 112 to 115) << 3: the GC also reads a pair's car as a
;; header
(define (string-header? (raw h))
  (declare unsafe inline)
  (raw= (bit-and h (raw -537)) ascii-string-type))

;; The kind of the string header h, mutable or not
(define (ascii-header? (raw h))
  (declare unsafe inline)
  (raw= (bit-and h (raw -513)) ascii-string-type))

(define (utf8-header? (raw h))
  (declare unsafe inline)
  (raw= (bit-and h (raw -513)) utf8-string-type))

(define (string? x)
  (declare unsafe inline public)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (string-header? (load (as-raw x) (raw 0)))))

(define (check-string s who)
  (declare inline (result void))
  (unless (string? s)
    (argument-error who 'string s)))

(define (string-nchars s)
  (declare unsafe inline (result raw))
  (load (as-raw s) (raw 1)))

(define (ascii-string? s)
  (declare unsafe inline)
  (ascii-header? (load (as-raw s) (raw 0))))

(define (utf8-string? s)
  (declare unsafe inline)
  (utf8-header? (load (as-raw s) (raw 0))))

;; The address of the units (bytes, or UTF-32 codes) of the string at p
(define (string-units (raw p))
  (declare unsafe inline (result raw))
  (raw+ p (if (utf8-header? (load p (raw 0))) (raw 40) (raw 16))))

;; The bytes of a UTF-8 string
(define (utf8-nbytes-of s)
  (declare unsafe inline (result raw))
  (load (as-raw s) (raw 2)))

;; The words of the string at p
(define (string-words (raw p))
  (declare unsafe inline (result raw))
  (cond
   ((ascii-header? (load p (raw 0))) (raw+ (raw 2) (raw>>u (raw+ (load p (raw 1)) (raw 7)) (raw 3))))
   ((utf8-header? (load p (raw 0))) (raw+ (raw 5) (raw>>u (raw+ (load p (raw 2)) (raw 7)) (raw 3))))
   (else (raw+ (raw 2) (raw>>u (raw+ (load p (raw 1)) (raw 1)) (raw 1))))))

;; A string of the fixnum nchars chars, whose UTF-8 encoding has the fixnum nbytes bytes, zero:
;; ASCII if they are equal, else UTF-8; to fill before the next allocation
(define (make-text nchars nbytes)
  (declare unsafe inline)
  (let ((p (alloc (if (eq? nchars nbytes)
                      (raw+ (raw 2) (raw>>u (raw+ (fixnum-value nbytes) (raw 7)) (raw 3)))
                      (raw+ (raw 5) (raw>>u (raw+ (fixnum-value nbytes) (raw 7)) (raw 3)))))))
    (store p (raw 0) (if (eq? nchars nbytes) ascii-string-type utf8-string-type))
    (store p (raw 1) (fixnum-value nchars))
    (unless (eq? nchars nbytes)
      (store p (raw 2) (fixnum-value nbytes)))
    (as-val p)))

;; The length of the UTF-8 sequence the valid lead byte b starts
(define (lead-length (raw b))
  (declare unsafe inline (result raw))
  (cond
   ((raw<u b (raw 128)) (raw 1))
   ((raw<u b (raw 224)) (raw 2))
   ((raw<u b (raw 240)) (raw 3))
   (else (raw 4))))

;; n extended by the UTF-8 continuation bytes [i, end) at p
(define (continue-utf8 (raw p) (raw i) (raw end) (raw n))
  (declare unsafe noalloc (result raw))
  (if (raw= i end)
      n
      (continue-utf8 p (raw+ i (raw 1)) end (bit-or (raw<< n (raw 6)) (bit-and (load8 p i) (raw 63))))))

;; The code of the valid UTF-8 sequence at p
(define (decode-at (raw p))
  (declare unsafe inline (result raw))
  (let ((b (load8 p (raw 0))))
    (if (raw<u b (raw 128))
        b
        ;; The lead byte's payload: 255 >> (len + 1) is 31, 15 or 7
        (continue-utf8 p (raw 1) (lead-length b) (bit-and b (raw>>u (raw 255) (raw+ (lead-length b) (raw 1))))))))

;; The byte offset of char i, at most nchars, of the UTF-8 string s: from the nearest of its
;; start, its end and the cached char, stepping over lead bytes forward or continuation bytes
;; backward; i is cached next
(define (utf8-offset s (raw i))
  (declare unsafe noalloc (result raw))
  (let* ((p (as-raw s))
         (bytes (raw+ p (raw 40)))
         (ci (load p (raw 3)))
         (dc (if (raw<u i ci) (raw- ci i) (raw- i ci)))
         (de (raw- (load p (raw 1)) i)))
    (let ((o (let walk ((c (cond
                            ((and (raw<=u dc i) (raw<=u dc de)) ci)
                            ((raw<=u i de) (raw 0))
                            (else (load p (raw 1)))))
                        (o (cond
                            ((and (raw<=u dc i) (raw<=u dc de)) (load p (raw 4)))
                            ((raw<=u i de) (raw 0))
                            (else (load p (raw 2))))))
               (cond
                ((raw<u c i) (walk (raw+ c (raw 1)) (raw+ o (lead-length (load8 bytes o)))))
                ((raw<u i c)
                 (let back ((o (raw- o (raw 1))))
                   (if (raw= (bit-and (load8 bytes o) (raw 192)) (raw 128))
                       (back (raw- o (raw 1)))
                       (walk (raw- c (raw 1)) o))))
                (else o)))))
      (store p (raw 3) i)
      (store p (raw 4) o)
      o)))

(define (utf8-string-code s (raw i))
  (declare unsafe noalloc (result raw))
  (decode-at (raw+ (raw+ (as-raw s) (raw 40)) (utf8-offset s i))))

;; The code of char i of the string s
(define (string-code s (raw i))
  (declare unsafe inline (result raw))
  (cond
   ((ascii-string? s) (load8 (as-raw s) (raw+ i (raw 16))))
   ((utf8-string? s) (utf8-string-code s i))
   (else (load32 (as-raw s) (raw+ (raw<< i (raw 2)) (raw 16))))))

;; The bytes of the UTF-8 encoding of the chars [start, end) of s
(define (utf8-range-nbytes s (raw start) (raw end))
  (declare unsafe noalloc (result raw))
  (cond
   ((ascii-string? s) (raw- end start))
   ((utf8-string? s)
    (let ((from (utf8-offset s start)))
      (raw- (utf8-offset s end) from)))
   (else
    (let char-next ((i start)
                    (n (raw 0)))
      (if (raw= i end)
          n
          (char-next (raw+ i (raw 1)) (raw+ n (utf8-nbytes (string-code s i)))))))))

;; Stores the UTF-8 encoding of the chars [start, end) of s at p; returns its length
(define (put-utf8-range! (raw p) s (raw start) (raw end))
  (declare unsafe noalloc (result raw))
  (cond
   ((ascii-string? s)
    (copy-bytes! p (raw+ (as-raw s) (raw+ start (raw 16))) (raw- end start))
    (raw- end start))
   ((utf8-string? s)
    (let* ((from (utf8-offset s start))
           (n (raw- (utf8-offset s end) from)))
      (copy-bytes! p (raw+ (raw+ (as-raw s) (raw 40)) from) n)
      n))
   (else
    (let char-next ((i start)
                    (j (raw 0)))
      (if (raw= i end)
          j
          (char-next (raw+ i (raw 1)) (raw+ j (store-utf8! (raw+ p j) (string-code s i)))))))))

;; Whether the n words at p and q are equal
(define (words=? (raw p) (raw q) (raw n))
  (declare unsafe noalloc)
  (let word-next ((i (raw 0)))
    (cond
     ((raw= i n) #t)
     ((raw= (load p i) (load q i)) (word-next (raw+ i (raw 1))))
     (else #f))))

;; A string literal of nchars chars, from its nbytes UTF-8 bytes at the fixnum address bytes
;; (compile-literal-init); out of line, since each literal of a compilation calls it
(define (literal-string bytes nbytes nchars)
  (declare unsafe)
  (let ((s (make-text nchars nbytes)))
    (copy-bytes! (string-units (as-raw s)) (fixnum-value bytes) (fixnum-value nbytes))
    s))

(define (string-length s)
  (declare unsafe inline public)
  (check-string s string-length)
  (make-fixnum (string-nchars s)))

;; i is a fixnum in [0, length); one unsigned compare also rejects negatives
(define (string-ref s i)
  (declare unsafe inline public)
  (check-string s string-ref)
  (unless (and (fixnum? i) (raw<u (fixnum-value i) (string-nchars s)))
    (index-error string-ref i s))
  (code->char (string-code s (fixnum-value i))))

;; Same kind, length and byte count, then the units, whose padding is zero; a UTF-32 string,
;; whose kind does not follow its content, char by char; ASCII and UTF-8 strings differ
(define (same-string? a b)
  (declare unsafe noalloc)
  (let ((ha (bit-and (load (as-raw a) (raw 0)) (raw -513)))
        (hb (bit-and (load (as-raw b) (raw 0)) (raw -513))))
    (cond
     ((not (raw= (string-nchars a) (string-nchars b))) #f)
     ((or (raw= ha utf32-string-type) (raw= hb utf32-string-type))
      (let char-next ((i (raw 0)))
        (cond
         ((raw= i (string-nchars a)) #t)
         ((raw= (string-code a i) (string-code b i)) (char-next (raw+ i (raw 1))))
         (else #f))))
     ((not (raw= ha hb)) #f)
     ((raw= ha utf8-string-type)
      (and (raw= (utf8-nbytes-of a) (utf8-nbytes-of b))
           (words=? (raw+ (as-raw a) (raw 40)) (raw+ (as-raw b) (raw 40))
                    (raw- (string-words (as-raw a)) (raw 5)))))
     (else
      (words=? (raw+ (as-raw a) (raw 16)) (raw+ (as-raw b) (raw 16))
               (raw- (string-words (as-raw a)) (raw 2)))))))

(define (string=? a b)
  (declare public)
  (check-string a string=?)
  (check-string b string=?)
  (same-string? a b))

(define (string-append . strings)
  (declare unsafe public)
  (let count ((l strings)
              (n 0)
              (nbytes 0))
    (if (pair? l)
        (begin
          (check-string (car l) string-append)
          (count (cdr l) (+ n (string-length (car l)))
                 (+ nbytes (make-fixnum (utf8-range-nbytes (car l) (raw 0) (string-nchars (car l)))))))
        (let ((s (make-text n nbytes)))
          (let copy-next ((l strings)
                          (at (string-units (as-raw s))))
            (if (pair? l)
                (copy-next (cdr l) (raw+ at (put-utf8-range! at (car l) (raw 0) (string-nchars (car l)))))
                s))))))

;; ASCII when the chars are, even from a UTF-8 or UTF-32 string
(define (substring s start end)
  (declare unsafe public)
  (check-string s substring)
  (unless (and (fixnum? end) (raw<=u (fixnum-value end) (string-nchars s))
               (fixnum? start) (raw<=u (fixnum-value start) (fixnum-value end)))
    (range-error substring start end s))
  (let ((r (make-text (- end start)
                      (make-fixnum (utf8-range-nbytes s (fixnum-value start) (fixnum-value end))))))
    (put-utf8-range! (string-units (as-raw r)) s (fixnum-value start) (fixnum-value end))
    r))

;; Stores the UTF-8 encoding of the list of characters l in the string s
(define (fill-chars! s l)
  (declare unsafe noalloc (result void))
  (let char-next ((l l)
                  (p (string-units (as-raw s))))
    (if (pair? l)
        (char-next (pair-cdr l) (raw+ p (store-utf8! p (char-code (pair-car l)))))
        undef)))

(define (list->string l)
  (declare unsafe public)
  (let count ((x l)
              (n 0)
              (nbytes 0))
    (if (pair? x)
        (begin
          (unless (char? (car x))
            (argument-error list->string 'char (car x)))
          (count (cdr x) (+ n 1) (+ nbytes (make-fixnum (utf8-nbytes (char-code (car x)))))))
        (begin
          (unless (null? x)
            (argument-error list->string 'list l))
          (let ((s (make-text n nbytes)))
            (fill-chars! s l)
            s)))))

(define (string . chars)
  (declare public)
  (list->string chars))

(define (string->list s)
  (declare public)
  (let loop ((i (string-length s))
             (acc '()))
    (if (= i 0)
        acc
        (loop (- i 1) (cons (string-ref s (- i 1)) acc)))))

;; By code points
(define (string-less? a b)
  (declare unsafe noalloc)
  (let char-next ((i (raw 0)))
    (cond
     ((raw= i (string-nchars b)) #f)
     ((raw= i (string-nchars a)) #t)
     ((raw< (string-code a i) (string-code b i)) #t)
     ((raw> (string-code a i) (string-code b i)) #f)
     (else (char-next (raw+ i (raw 1)))))))

(define (string<? a b)
  (declare public)
  (check-string a string<?)
  (check-string b string<?)
  (string-less? a b))
