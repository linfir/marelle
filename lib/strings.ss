;; Strings. The header is the kind, chosen by content for a new string: ASCII, [header, nchars,
;; bytes...], when every char is below 128, else UTF-8, [header, nchars, nbytes, cache-index,
;; cache-offset, bytes...]; nchars, nbytes and the cache are raw words, the bytes padded with
;; zeros to a whole word. A UTF-8 string caches the byte offset of the last char indexed, so
;; indexing near it is fast (utf8-offset). UTF-32, [header, nchars, capacity, units...], 4 bytes
;; per char, zero past nchars, comes from mutation; 51 is kept for a later kind. ASCII and UTF-8
;; strings share their bytes' encoding, so building one writes UTF-8 bytes (put-utf8-range!).
;; New strings are mutable (R7RS); literals, symbol names and the static strings of
;; embedded-datum have the immutable types, 64 more (header bit 9; the static UTF-8 ones in
;; .data, since indexing writes their cache): every test of the kind masks that bit. A mutation
;; is in place for ASCII with an ASCII char and for UTF-32; otherwise the string is relocated
;; to UTF-32 first (widen!, relocate! in lib/objects.ss), so every entry point resolves it
;; (string-body).

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

(define (string-pointer? x)
  (declare unsafe inline)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (string-header? (load (as-raw x) (raw 0)))))

;; The body of x if it is a relocated string, else #f
(define (moved-string-body x)
  (declare unsafe noalloc)
  (and (moved? x)
       (let ((b (resolve x)))
         (and (string-header? (load (as-raw b) (raw 0))) b))))

(define (string? x)
  (declare unsafe inline public)
  (or (string-pointer? x)
      (if (moved-string-body x) #t #f)))

(define (check-string s who)
  (declare inline (result void))
  (unless (string? s)
    (argument-error who 'string s)))

;; The body of the string s, checked
(define (string-body s who)
  (declare unsafe inline)
  (if (string-pointer? s)
      s
      (or (moved-string-body s)
          (argument-error who 'string s))))

(define (mutable-header? (raw h))
  (declare unsafe inline)
  (raw= (bit-and h (raw 512)) (raw 0)))

;; The body of the mutable string s, checked
(define (mutable-string-body s who)
  (declare unsafe inline)
  (let ((b (if (string-pointer? s) s (moved-string-body s))))
    (if (and b (mutable-header? (load (as-raw b) (raw 0))))
        b
        (argument-error who 'mutable-string s))))

(define (string-nchars s)
  (declare unsafe inline (result raw))
  (load (as-raw s) (raw 1)))

(define (ascii-string? s)
  (declare unsafe inline)
  (ascii-header? (load (as-raw s) (raw 0))))

(define (utf8-string? s)
  (declare unsafe inline)
  (utf8-header? (load (as-raw s) (raw 0))))

(define (utf32-string? s)
  (declare unsafe inline)
  (raw= (bit-and (load (as-raw s) (raw 0)) (raw -513)) utf32-string-type))

;; The address of the units (bytes, or UTF-32 codes) of the string at p
(define (string-units (raw p))
  (declare unsafe inline (result raw))
  (raw+ p (cond
           ((ascii-header? (load p (raw 0))) (raw 16))
           ((utf8-header? (load p (raw 0))) (raw 40))
           (else (raw 24)))))

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
   (else (raw+ (raw 3) (raw>>u (raw+ (load p (raw 2)) (raw 1)) (raw 1))))))

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

;; A mutable UTF-32 string of the fixnum nchars chars, zero, with room for the fixnum capacity
(define (make-utf32 nchars capacity)
  (declare unsafe inline)
  (let ((p (alloc (raw+ (raw 3) (raw>>u (raw+ (fixnum-value capacity) (raw 1)) (raw 1))))))
    (store p (raw 0) utf32-string-type)
    (store p (raw 1) (fixnum-value nchars))
    (store p (raw 2) (fixnum-value capacity))
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
   (else (load32 (as-raw s) (raw+ (raw<< i (raw 2)) (raw 24))))))

;; Sequential reading: a position is a byte offset in a UTF-8 string, else a char index, from 0
;; to string-end

(define (string-end s)
  (declare unsafe inline (result raw))
  (if (utf8-string? s) (utf8-nbytes-of s) (string-nchars s)))

;; The code of the char at the position pos of s
(define (string-code-at s (raw pos))
  (declare unsafe inline (result raw))
  (cond
   ((ascii-string? s) (load8 (as-raw s) (raw+ pos (raw 16))))
   ((utf8-string? s) (decode-at (raw+ (as-raw s) (raw+ pos (raw 40)))))
   (else (load32 (as-raw s) (raw+ (raw<< pos (raw 2)) (raw 24))))))

;; The position after the char at pos
(define (string-next s (raw pos))
  (declare unsafe inline (result raw))
  (if (utf8-string? s)
      (raw+ pos (lead-length (load8 (as-raw s) (raw+ pos (raw 40)))))
      (raw+ pos (raw 1))))

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

;; Makes the new string s immutable
(define (freeze! s)
  (declare unsafe inline (result void))
  (store (as-raw s) (raw 0) (bit-or (load (as-raw s) (raw 0)) (raw 512))))

;; The string body s if it is immutable, else an immutable copy, ASCII or UTF-8 (symbol names)
(define (immutable-string s)
  (declare unsafe)
  (if (mutable-header? (load (as-raw s) (raw 0)))
      (let ((r (make-text (make-fixnum (string-nchars s))
                          (make-fixnum (utf8-range-nbytes s (raw 0) (string-nchars s))))))
        (put-utf8-range! (string-units (as-raw r)) s (raw 0) (string-nchars s))
        (freeze! r)
        r)
      s))

;; Stores the codes of the chars of the string s from char at of the UTF-32 string w
(define (store-codes! w (raw at) s)
  (declare unsafe noalloc (result void))
  (let char-next ((pos (raw 0))
                  (i at))
    (if (raw= pos (string-end s))
        undef
        (begin
          (store32 (as-raw w) (raw+ (raw<< i (raw 2)) (raw 24)) (string-code-at s pos))
          (char-next (string-next s pos) (raw+ i (raw 1)))))))

;; The UTF-32 string of the chars of the string body b, with room for the fixnum capacity, at
;; least its length; b is relocated to it
(define (widen! b capacity)
  (declare unsafe)
  (let ((w (make-utf32 (make-fixnum (string-nchars b)) capacity)))
    (store-codes! w (raw 0) b)
    (relocate! b w)
    w))

(define (string-set! s i c)
  (declare unsafe public)
  (let ((b (mutable-string-body s string-set!)))
    (unless (and (fixnum? i) (raw<u (fixnum-value i) (string-nchars b)))
      (index-error string-set! i s))
    (unless (char? c)
      (argument-error string-set! 'char c))
    (cond
     ((and (ascii-string? b) (raw<u (char-code c) (raw 128)))
      (store8 (as-raw b) (raw+ (fixnum-value i) (raw 16)) (char-code c)))
     ((utf32-string? b)
      (store32 (as-raw b) (raw+ (raw<< (fixnum-value i) (raw 2)) (raw 24)) (char-code c)))
     (else
      (let ((w (widen! b (make-fixnum (string-nchars b)))))
        (store32 (as-raw w) (raw+ (raw<< (fixnum-value i) (raw 2)) (raw 24)) (char-code c)))))
    undef))

;; k chars c, #\nul by default as in Chez: ASCII if c is, else UTF-32, since such a string is
;; made to be mutated
(define (make-string k . fill)
  (declare unsafe public)
  (unless (and (fixnum? k) (raw>= (fixnum-value k) (raw 0)))
    (argument-error make-string 'string-length k))
  (unless (or (null? fill) (null? (cdr fill)))
    (argument-count-error make-string (+ 1 (length fill))))
  (let ((c (if (null? fill) #\nul (car fill))))
    (unless (char? c)
      (argument-error make-string 'char c))
    (let ((s (if (raw<u (char-code c) (raw 128)) (make-text k k) (make-utf32 k k))))
      (fill-chars-range! s c (raw 0) (fixnum-value k))
      s)))

;; Sets the chars [start, end) of the string s, ASCII if c is, else UTF-32, to c
(define (fill-chars-range! s c (raw start) (raw end))
  (declare unsafe noalloc (result void))
  (let char-next ((i start))
    (if (raw= i end)
        undef
        (begin
          (if (ascii-string? s)
              (store8 (as-raw s) (raw+ i (raw 16)) (char-code c))
              (store32 (as-raw s) (raw+ (raw<< i (raw 2)) (raw 24)) (char-code c)))
          (char-next (raw+ i (raw 1)))))))

(define (string-length s)
  (declare unsafe inline public)
  (make-fixnum (string-nchars (string-body s string-length))))

;; i is a fixnum in [0, length); one unsigned compare also rejects negatives
(define (string-ref s i)
  (declare unsafe inline public)
  (let ((b (string-body s string-ref)))
    (unless (and (fixnum? i) (raw<u (fixnum-value i) (string-nchars b)))
      (index-error string-ref i s))
    (code->char (string-code b (fixnum-value i)))))

;; Same kind, length and byte count, then the units, whose padding is zero; a UTF-32 string,
;; whose kind does not follow its content, char by char; ASCII and UTF-8 strings differ
(define (same-string? a b)
  (declare unsafe noalloc)
  (let ((ha (bit-and (load (as-raw a) (raw 0)) (raw -513)))
        (hb (bit-and (load (as-raw b) (raw 0)) (raw -513))))
    (cond
     ((not (raw= (string-nchars a) (string-nchars b))) #f)
     ((or (raw= ha utf32-string-type) (raw= hb utf32-string-type))
      (let char-next ((i (raw 0))
                      (j (raw 0)))
        (cond
         ((raw= i (string-end a)) #t)
         ((raw= (string-code-at a i) (string-code-at b j))
          (char-next (string-next a i) (string-next b j)))
         (else #f))))
     ((not (raw= ha hb)) #f)
     ((raw= ha utf8-string-type)
      (and (raw= (utf8-nbytes-of a) (utf8-nbytes-of b))
           (words=? (raw+ (as-raw a) (raw 40)) (raw+ (as-raw b) (raw 40))
                    (raw- (string-words (as-raw a)) (raw 5)))))
     (else
      (words=? (raw+ (as-raw a) (raw 16)) (raw+ (as-raw b) (raw 16))
               (raw- (string-words (as-raw a)) (raw 2)))))))

(define string=?
  (case-lambda
   (declare public)
   ((a) (declare unsafe) (string-body a string=?) #t)
   ((a b)
    (declare unsafe)
    (same-string? (string-body a string=?) (string-body b string=?)))
   ((a b . rest) (declare inline) (and (string=? a b) (apply string=? b rest)))))

(define (string-append . strings)
  (declare unsafe public)
  (let count ((l strings)
              (n 0)
              (nbytes 0))
    (if (pair? l)
        (begin
          (let ((b (string-body (car l) string-append)))
            (count (cdr l) (+ n (make-fixnum (string-nchars b)))
                   (+ nbytes (make-fixnum (utf8-range-nbytes b (raw 0) (string-nchars b)))))))
        (let ((s (make-text n nbytes)))
          (let copy-next ((l strings)
                          (at (string-units (as-raw s))))
            (if (pair? l)
                (let ((b (resolve (car l))))
                  (copy-next (cdr l) (raw+ at (put-utf8-range! at b (raw 0) (string-nchars b)))))
                s))))))

;; ASCII when the chars are, even from a UTF-8 or UTF-32 string
(define (substring s start end)
  (declare unsafe public)
  (let ((b (string-body s substring)))
    (unless (and (fixnum? end) (raw<=u (fixnum-value end) (string-nchars b))
                 (fixnum? start) (raw<=u (fixnum-value start) (fixnum-value end)))
      (range-error substring start end s))
    (let ((r (make-text (- end start)
                        (make-fixnum (utf8-range-nbytes b (fixnum-value start) (fixnum-value end))))))
      (put-utf8-range! (string-units (as-raw r)) b (fixnum-value start) (fixnum-value end))
      r)))

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
  (let char-next ((i (raw 0))
                  (j (raw 0)))
    (cond
     ((raw= j (string-end b)) #f)
     ((raw= i (string-end a)) #t)
     ((raw< (string-code-at a i) (string-code-at b j)) #t)
     ((raw> (string-code-at a i) (string-code-at b j)) #f)
     (else (char-next (string-next a i) (string-next b j))))))

(define string<?
  (case-lambda
   (declare public)
   ((a) (declare unsafe) (string-body a string<?) #t)
   ((a b)
    (declare unsafe)
    (string-less? (string-body a string<?) (string-body b string<?)))
   ((a b . rest) (declare inline) (and (string<? a b) (apply string<? b rest)))))

;; Copying and filling, with R7RS's optional ranges (not Chez's R6RS arguments)

;; (string-copy s [start [end]])
(define string-copy
  (case-lambda
   (declare public)
   ((s)
    (substring s 0 (string-length s)))
   ((s . rest)
    (optional-range s (string-length s) rest string-copy
                    (lambda (start end) (substring s start end))))))

;; Whether the chars [start, end) of the string s are all ASCII
(define (ascii-range? s (raw start) (raw end))
  (declare unsafe noalloc)
  (or (ascii-string? s)
      (let char-next ((i start))
        (cond
         ((raw= i end) #t)
         ((raw<u (string-code s i) (raw 128)) (char-next (raw+ i (raw 1))))
         (else #f)))))

;; The body of the mutable string s, relocated to UTF-32 unless it is ASCII and fit? is true
(define (fitting-body s fit? who)
  (declare unsafe)
  (let ((b (mutable-string-body s who)))
    (if (or (utf32-string? b) (and fit? (ascii-string? b)))
        b
        (widen! b (make-fixnum (string-nchars b))))))

;; (string-fill! s c [start [end]])
(define (string-fill! s c . rest)
  (declare unsafe public)
  (let ((n (string-length (mutable-string-body s string-fill!))))
    (unless (char? c)
      (argument-error string-fill! 'char c))
    (optional-range s n rest string-fill!
                    (lambda (start end)
                      (fill-chars-range! (fitting-body s (raw<u (char-code c) (raw 128)) string-fill!)
                                         c (fixnum-value start) (fixnum-value end))
                      undef))))

;; Copies the chars [start, end) of the string from to the string to from char at: ASCII to
;; ASCII by bytes, else to UTF-32 by codes; backward when they overlap with at past start
(define (copy-chars! to (raw at) from (raw start) (raw end))
  (declare unsafe noalloc (result void))
  (cond
   ((ascii-string? to)
    (if (ascii-string? from)
        (move-bytes! (raw+ (as-raw to) (raw+ at (raw 16))) (raw+ (as-raw from) (raw+ start (raw 16)))
                     (raw- end start))
        (let char-next ((i start))
          (if (raw= i end)
              undef
              (begin
                (store8 (as-raw to) (raw+ (raw+ at (raw- i start)) (raw 16)) (string-code from i))
                (char-next (raw+ i (raw 1))))))))
   ((and (eq? to from) (raw>u at start))
    (let char-next ((i end))
      (if (raw= i start)
          undef
          (begin
            (store32 (as-raw to) (raw+ (raw<< (raw+ at (raw- (raw- i (raw 1)) start)) (raw 2)) (raw 24))
                     (string-code from (raw- i (raw 1))))
            (char-next (raw- i (raw 1)))))))
   (else
    (let char-next ((i start))
      (if (raw= i end)
          undef
          (begin
            (store32 (as-raw to) (raw+ (raw<< (raw+ at (raw- i start)) (raw 2)) (raw 24))
                     (string-code from i))
            (char-next (raw+ i (raw 1)))))))))

;; (string-copy! to at from [start [end]]); to is relocated to UTF-32 first unless both are
;; ASCII for the range
(define (string-copy! to at from . rest)
  (declare unsafe public)
  (mutable-string-body to string-copy!)
  (optional-range from (string-length from) rest string-copy!
                  (lambda (start end)
                    (unless (and (fixnum? at) (>= at 0) (<= (+ at (- end start)) (string-length to)))
                      (index-error string-copy! at to))
                    (let ((b (fitting-body to (ascii-range? (resolve from) (fixnum-value start) (fixnum-value end))
                                           string-copy!)))
                      (copy-chars! b (fixnum-value at) (resolve from) (fixnum-value start) (fixnum-value end))
                      undef))))

;; The chars of the n UTF-8 bytes at p: the bytes that are not continuation bytes
(define (utf8-nchars (raw p) (raw n))
  (declare unsafe noalloc (result raw))
  (let byte-next ((i (raw 0))
                  (k (raw 0)))
    (cond
     ((raw= i n) k)
     ((raw= (bit-and (load8 p i) (raw 192)) (raw 128)) (byte-next (raw+ i (raw 1)) k))
     (else (byte-next (raw+ i (raw 1)) (raw+ k (raw 1)))))))

;; Appends the chars of the n valid UTF-8 bytes at p to the UTF-32 string s, which has room
;; for them (string output ports)
(define (append-utf8! s (raw p) (raw n))
  (declare unsafe noalloc (result void))
  (let ((b (resolve s)))
    (let byte-next ((i (raw 0))
                    (k (string-nchars b)))
      (if (raw= i n)
          (store (as-raw b) (raw 1) k)
          (begin
            (store32 (as-raw b) (raw+ (raw<< k (raw 2)) (raw 24)) (decode-at (raw+ p i)))
            (byte-next (raw+ i (lead-length (load8 p i))) (raw+ k (raw 1))))))))

;; Growable strings, as Rust's String: a UTF-32 body appends in place while its capacity
;; allows, else relocates to one of twice the capacity; an ASCII or UTF-8 body is relocated to
;; UTF-32 with room first, so later appends are amortized O(1)

;; An empty mutable string with room for k chars
(define (string-with-capacity k)
  (declare unsafe public)
  (unless (and (fixnum? k) (>= k 0))
    (argument-error string-with-capacity 'string-length k))
  (make-utf32 0 k))

(define (string-capacity b)
  (declare unsafe inline (result raw))
  (if (utf32-string? b) (load (as-raw b) (raw 2)) (string-nchars b)))

;; The UTF-32 body of the mutable string body b with room for the fixnum need chars: b if it
;; has it, else b relocated to a copy of capacity max(2 capacity, need, 4)
(define (room-body b need)
  (declare unsafe)
  (if (and (utf32-string? b) (raw<=u (fixnum-value need) (string-capacity b)))
      b
      (let ((cap (* 2 (make-fixnum (string-capacity b)))))
        (widen! b (cond
                   ((< cap need) (if (< need 4) 4 need))
                   ((< cap 4) 4)
                   (else cap))))))

(define (string-push! s c)
  (declare unsafe public)
  (let ((b (mutable-string-body s string-push!)))
    (unless (char? c)
      (argument-error string-push! 'char c))
    (let ((w (room-body b (+ (make-fixnum (string-nchars b)) 1))))
      (store32 (as-raw w) (raw+ (raw<< (string-nchars w) (raw 2)) (raw 24)) (char-code c))
      (store (as-raw w) (raw 1) (raw+ (string-nchars w) (raw 1)))
      undef)))

;; Appends the strings, which may include s itself, as it was
(define (string-append! s . strings)
  (declare unsafe public)
  (let ((b (mutable-string-body s string-append!)))
    (let count ((l strings)
                (n (make-fixnum (string-nchars b))))
      (if (pair? l)
          (count (cdr l) (+ n (make-fixnum (string-nchars (string-body (car l) string-append!)))))
          (let ((w (room-body b n)))
            ;; The length is set last, so that s, among the strings, keeps its own
            (let copy-next ((l strings)
                            (at (make-fixnum (string-nchars w))))
              (if (pair? l)
                  (begin
                    (store-codes! w (fixnum-value at) (resolve (car l)))
                    (copy-next (cdr l) (+ at (make-fixnum (string-nchars (resolve (car l)))))))
                  (begin
                    (store (as-raw w) (raw 1) (fixnum-value n))
                    undef))))))))

;; Keeps the first k chars; the units past them are zeroed, as string=? compares the padding
(define (string-truncate! s k)
  (declare unsafe public)
  (let ((b (mutable-string-body s string-truncate!)))
    (unless (and (fixnum? k) (raw<=u (fixnum-value k) (string-nchars b)))
      (index-error string-truncate! k s))
    (let ((w (if (utf8-string? b) (widen! b (make-fixnum (string-nchars b))) b)))
      (if (ascii-string? w)
          (fill-bytes! (raw+ (as-raw w) (raw+ (fixnum-value k) (raw 16))) (raw- (string-nchars w) (fixnum-value k))
                       (raw 0))
          (fill-bytes! (raw+ (as-raw w) (raw+ (raw<< (fixnum-value k) (raw 2)) (raw 24)))
                       (raw<< (raw- (string-nchars w) (fixnum-value k)) (raw 2)) (raw 0)))
      (store (as-raw w) (raw 1) (fixnum-value k))
      undef)))
