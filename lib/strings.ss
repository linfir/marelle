;; Strings, immutable: [header, nchars, units...], nchars a raw word, the units
;; padded to a whole word; the header is the kind, chosen by content: ASCII, a byte per char,
;; when every char is below 128, else UTF-32, 4 bytes per char, so equal strings have the same
;; kind; 50 and 51 are kept for later kinds

;; Exactly the four words (48 to 51) << 3: the GC also reads a pair's car as a header
(define (string-header? (raw h))
  (declare unsafe inline)
  (raw= (bit-and h (raw -25)) ascii-string-type))

(define (string? x)
  (declare unsafe inline public)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (string-header? (load (as-raw x) (raw 0)))))

(define (check-string s (raw msg))
  (declare unsafe inline (result void))
  (unless (string? s)
    (scheme-error msg s)))

(define (string-nchars s)
  (declare unsafe inline (result raw))
  (load (as-raw s) (raw 1)))

(define (ascii-string? s)
  (declare unsafe inline)
  (raw= (load (as-raw s) (raw 0)) ascii-string-type))

(define (string-type ascii?)
  (declare unsafe inline (result raw))
  (if ascii? ascii-string-type utf32-string-type))

;; The words of a string of n chars with the header h
(define (string-words (raw h) (raw n))
  (declare unsafe inline (result raw))
  (raw+ (raw 2) (if (raw= h ascii-string-type)
                    (raw>>u (raw+ n (raw 7)) (raw 3))
                    (raw>>u (raw+ n (raw 1)) (raw 1)))))

;; A string of the fixnum n chars, zero, to fill before the next allocation
(define (make-string-object ascii? n)
  (declare unsafe inline)
  (let ((p (alloc (string-words (string-type ascii?) (fixnum-value n)))))
    (store p (raw 0) (string-type ascii?))
    (store p (raw 1) (fixnum-value n))
    (as-val p)))

;; The code of char i of the string s
(define (string-code s (raw i))
  (declare unsafe inline (result raw))
  (if (ascii-string? s)
      (load8 (as-raw s) (raw+ i (raw 16)))
      (load32 (as-raw s) (raw+ (raw<< i (raw 2)) (raw 16)))))

;; Sets char i of the string at p to the code n, which fits its kind
(define (string-set-code! (raw p) (raw i) (raw n))
  (declare unsafe inline (result void))
  (if (raw= (load p (raw 0)) ascii-string-type)
      (store8 p (raw+ i (raw 16)) n)
      (store32 p (raw+ (raw<< i (raw 2)) (raw 16)) n)))

;; Copies the chars [start, end) of the string s to the string d, from char at
(define (copy-chars! d (raw at) s (raw start) (raw end))
  (declare unsafe noalloc (result void))
  (let char-next ((i start)
                  (j at))
    (if (raw= i end)
        undef
        (begin
          (string-set-code! (as-raw d) j (string-code s i))
          (char-next (raw+ i (raw 1)) (raw+ j (raw 1)))))))

;; Whether the chars [start, end) of the string s are all ASCII
(define (ascii-chars? s (raw start) (raw end))
  (declare unsafe noalloc)
  (let char-next ((i start))
    (cond
     ((raw= i end) #t)
     ((raw<u (string-code s i) (raw 128)) (char-next (raw+ i (raw 1))))
     (else #f))))

;; Whether the n words at p and q are equal
(define (words=? (raw p) (raw q) (raw n))
  (declare unsafe noalloc)
  (let word-next ((i (raw 0)))
    (cond
     ((raw= i n) #t)
     ((raw= (load p i) (load q i)) (word-next (raw+ i (raw 1))))
     (else #f))))

;; n extended by the UTF-8 continuation bytes [i, end) at p
(define (continue-utf8 (raw p) (raw i) (raw end) (raw n))
  (declare unsafe noalloc (result raw))
  (if (raw= i end)
      n
      (continue-utf8 p (raw+ i (raw 1)) end (bit-or (raw<< n (raw 6)) (bit-and (load8 p i) (raw 63))))))

;; Stores the chars of the nbytes UTF-8 bytes at p, valid, in the string s, which fits them
(define (decode-utf8! s (raw p) (raw nbytes))
  (declare unsafe noalloc (result void))
  (let byte-next ((i (raw 0))
                  (j (raw 0)))
    (if (raw= i nbytes)
        undef
        (let* ((b (load8 p i))
               (len (cond
                     ((raw<u b (raw 128)) (raw 1))
                     ((raw<u b (raw 224)) (raw 2))
                     ((raw<u b (raw 240)) (raw 3))
                     (else (raw 4))))
               ;; The lead byte's payload: 255 >> (len + 1) is 31, 15 or 7
               (lead (if (raw= len (raw 1)) b (bit-and b (raw>>u (raw 255) (raw+ len (raw 1)))))))
          (string-set-code! (as-raw s) j (continue-utf8 p (raw+ i (raw 1)) (raw+ i len) lead))
          (byte-next (raw+ i len) (raw+ j (raw 1)))))))

;; A string literal of nchars chars, from its nbytes UTF-8 bytes at bytes (compile-literal-init)
(define (literal-string (raw bytes) (raw nbytes) (raw nchars) (raw ascii?))
  (declare unsafe inline)
  (let ((s (make-string-object (raw= ascii? (raw 1)) (make-fixnum nchars))))
    (decode-utf8! s bytes nbytes)
    s))

(define (string-length s)
  (declare unsafe inline public)
  (check-string s "`string-length` expected a string")
  (make-fixnum (string-nchars s)))

;; i is a fixnum in [0, length); one unsigned compare also rejects negatives
(define (string-ref s i)
  (declare unsafe inline public)
  (check-string s "`string-ref` expected a string")
  (unless (and (fixnum? i) (raw<u (fixnum-value i) (string-nchars s)))
    (scheme-error "`string-ref` index out of range" i))
  (code->char (string-code s (fixnum-value i))))

;; Same kind and length, then the units, whose padding is zero
(define (same-string? a b)
  (declare unsafe noalloc)
  (and (raw= (load (as-raw a) (raw 0)) (load (as-raw b) (raw 0)))
       (raw= (string-nchars a) (string-nchars b))
       (words=? (raw+ (as-raw a) (raw 16)) (raw+ (as-raw b) (raw 16))
                (raw- (string-words (load (as-raw a) (raw 0)) (string-nchars a)) (raw 2)))))

(define (string=? a b)
  (declare unsafe public)
  (check-string a "`string=?` expected a string")
  (check-string b "`string=?` expected a string")
  (same-string? a b))

(define (string-append . strings)
  (declare unsafe public)
  (let count ((l strings)
              (n 0)
              (ascii? #t))
    (if (pair? l)
        (begin
          (check-string (car l) "`string-append` expected a string")
          (count (cdr l) (+ n (string-length (car l))) (and ascii? (ascii-string? (car l)))))
        (let ((s (make-string-object ascii? n)))
          (let copy-next ((l strings)
                          (at (raw 0)))
            (if (pair? l)
                (begin
                  (copy-chars! s at (car l) (raw 0) (string-nchars (car l)))
                  (copy-next (cdr l) (raw+ at (string-nchars (car l)))))
                s))))))

;; ASCII when the chars are, even from a UTF-32 string
(define (substring s start end)
  (declare unsafe public)
  (check-string s "`substring` expected a string")
  (unless (and (fixnum? end) (raw<=u (fixnum-value end) (string-nchars s)))
    (scheme-error "`substring` end out of range" end))
  (unless (and (fixnum? start) (raw<=u (fixnum-value start) (fixnum-value end)))
    (scheme-error "`substring` start out of range" start))
  (let ((r (make-string-object (or (ascii-string? s)
                                   (ascii-chars? s (fixnum-value start) (fixnum-value end)))
                               (- end start))))
    (copy-chars! r (raw 0) s (fixnum-value start) (fixnum-value end))
    r))

;; Sets the chars of the string s from the list of characters l
(define (fill-chars! s l)
  (declare unsafe noalloc (result void))
  (let char-next ((l l)
                  (i (raw 0)))
    (if (pair? l)
        (begin
          (string-set-code! (as-raw s) i (char-code (car l)))
          (char-next (cdr l) (raw+ i (raw 1))))
        undef)))

(define (list->string l)
  (declare unsafe public)
  (let count ((x l)
              (n 0)
              (ascii? #t))
    (if (pair? x)
        (begin
          (unless (char? (car x))
            (scheme-error "`list->string` expected a character" (car x)))
          (count (cdr x) (+ n 1) (and ascii? (raw<u (char-code (car x)) (raw 128)))))
        (begin
          (unless (null? x)
            (scheme-error "`list->string` expected a list" l))
          (let ((s (make-string-object ascii? n)))
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
  (declare unsafe public)
  (check-string a "`string<?` expected a string")
  (check-string b "`string<?` expected a string")
  (string-less? a b))
