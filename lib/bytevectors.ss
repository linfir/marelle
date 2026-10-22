;; Bytevectors (R7RS, with Chez's bytevector=?, bytevector->u8-list and u8-list->bytevector):
;; [bytevector-type, nbytes, bytes...], all raw, the bytes padded with zeros to a whole word;
;; immutable-bytevector-type for the static ones of embedded-file, which only the readers take

(define (bytevector-words (raw n))
  (declare unsafe inline (result raw))
  (raw+ (raw 2) (raw>>u (raw+ n (raw 7)) (raw 3))))

;; A bytevector of the fixnum n bytes, zero
(define (alloc-bytevector n)
  (declare unsafe inline)
  (let ((p (alloc (bytevector-words (fixnum-value n)))))
    (store p (raw 0) bytevector-type)
    (store p (raw 1) (fixnum-value n))
    (as-val p)))

(define (bytevector-header? (raw h))
  (declare unsafe inline)
  (raw= (bit-and h (raw -17)) bytevector-type))

(define (bytevector? x)
  (declare unsafe inline public)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (bytevector-header? (load (as-raw x) (raw 0)))))

(define (check-bytevector bv who)
  (declare inline (result void))
  (unless (bytevector? bv)
    (argument-error who 'bytevector bv)))

(define (check-mutable-bytevector bv who)
  (declare unsafe inline (result void))
  (unless (and (tag=? bv (raw 0))
               (not (raw= (as-raw bv) (raw 0)))
               (raw= (load (as-raw bv) (raw 0)) bytevector-type))
    (argument-error who 'mutable-bytevector bv)))

(define (bytevector-nbytes bv)
  (declare unsafe inline (result raw))
  (load (as-raw bv) (raw 1)))

;; The address of byte i
(define (byte-address bv (raw i))
  (declare unsafe inline (result raw))
  (raw+ (as-raw bv) (raw+ i (raw 16))))

(define (byte? x)
  (declare unsafe inline)
  (and (fixnum? x) (raw<u (fixnum-value x) (raw 256))))

(define (check-byte b who)
  (declare inline (result void))
  (unless (byte? b)
    (argument-error who 'value b)))

;; start and end, fixnums with start <= end <= n
(define (check-range x start end (raw n) who)
  (declare unsafe inline (result void))
  (unless (and (fixnum? end) (raw<=u (fixnum-value end) n)
               (fixnum? start) (raw<=u (fixnum-value start) (fixnum-value end)))
    (range-error who start end x)))

(define (bytevector-length bv)
  (declare unsafe inline public)
  (check-bytevector bv bytevector-length)
  (make-fixnum (bytevector-nbytes bv)))

(define (bytevector-u8-ref bv i)
  (declare unsafe inline public)
  (check-bytevector bv bytevector-u8-ref)
  (unless (and (fixnum? i) (raw<u (fixnum-value i) (bytevector-nbytes bv)))
    (index-error bytevector-u8-ref i bv))
  (make-fixnum (load8 (byte-address bv (fixnum-value i)) (raw 0))))

(define (bytevector-u8-set! bv i b)
  (declare unsafe inline public)
  (check-mutable-bytevector bv bytevector-u8-set!)
  (unless (and (fixnum? i) (raw<u (fixnum-value i) (bytevector-nbytes bv)))
    (index-error bytevector-u8-set! i bv))
  (check-byte b bytevector-u8-set!)
  (store8 (byte-address bv (fixnum-value i)) (raw 0) (fixnum-value b))
  undef)

;; Sets the n bytes at p to b: the whole words, then the last bytes, so the padding stays zero
(define (fill-bytes! (raw p) (raw n) (raw b))
  (declare unsafe noalloc (result void))
  (let ((words (raw>>u n (raw 3))))
    (fill-words p words (raw* b (raw 72340172838076673))) ; 0x0101010101010101
    (let byte-next ((i (raw<< words (raw 3))))
      (if (raw= i n)
          undef
          (begin
            (store8 p i b)
            (byte-next (raw+ i (raw 1))))))))

(define (make-bytevector k fill)
  (declare unsafe public)
  (unless (and (fixnum? k) (raw>= (fixnum-value k) (raw 0)))
    (argument-error make-bytevector 'bytevector-length k))
  (unless (byte? fill)
    (argument-error make-bytevector 'fill-value fill))
  (let ((bv (alloc-bytevector k)))
    (fill-bytes! (byte-address bv (raw 0)) (fixnum-value k) (fixnum-value fill))
    bv))

(define (variadic-make-bytevector k . fill)
  (cond
   ((null? fill) (make-bytevector k 0))
   ((null? (cdr fill)) (make-bytevector k (car fill)))
   (else (argument-count-error make-bytevector (+ 1 (length fill))))))

;; A bytevector of the bytes of the list l, for who
(define (list->bytevector l who)
  (declare unsafe)
  (let count ((x l)
              (n 0))
    (if (pair? x)
        (begin
          (check-byte (car x) who)
          (count (cdr x) (+ n 1)))
        (begin
          (unless (null? x)
            (argument-error who 'list l))
          (let ((bv (alloc-bytevector n)))
            (let byte-next ((x l)
                            (i (raw 0)))
              (if (pair? x)
                  (begin
                    (store8 (byte-address bv i) (raw 0) (fixnum-value (car x)))
                    (byte-next (cdr x) (raw+ i (raw 1))))
                  bv)))))))

(define (u8-list->bytevector l)
  (declare public)
  (list->bytevector l u8-list->bytevector))

(define (bytevector . bytes)
  (declare public)
  (list->bytevector bytes bytevector))

(define (bytevector->u8-list bv)
  (declare public)
  (let loop ((i (bytevector-length bv))
             (acc '()))
    (if (= i 0)
        acc
        (loop (- i 1) (cons (bytevector-u8-ref bv (- i 1)) acc)))))

;; Copies the n bytes at src to dst, backwards when dst is inside them, so they may overlap
(define (move-bytes! (raw dst) (raw src) (raw n))
  (declare unsafe noalloc (result void))
  (if (and (raw>u dst src) (raw<u dst (raw+ src n)))
      (let copy-next ((i n))
        (if (raw= i (raw 0))
            undef
            (begin
              (store8 dst (raw- i (raw 1)) (load8 src (raw- i (raw 1))))
              (copy-next (raw- i (raw 1))))))
      (copy-bytes! dst src n)))

;; A new bytevector of the bytes [start, end) of bv, checked
(define (sub-bytevector bv start end)
  (declare unsafe inline)
  (let* ((n (- end start))
         (c (alloc-bytevector n)))
    (copy-bytes! (byte-address c (raw 0)) (byte-address bv (fixnum-value start)) (fixnum-value n))
    c))

(define (bytevector-copy bv)
  (declare unsafe public)
  (check-bytevector bv bytevector-copy)
  (sub-bytevector bv 0 (bytevector-length bv)))

;; The optional start and end of rest, the range [0, n) by default, checked for x, then f of them
(define (optional-range x n rest who f)
  (declare unsafe)
  (cond
   ((null? rest) (f 0 n))
   ((null? (cdr rest))
    (check-range x (car rest) n (fixnum-value n) who)
    (f (car rest) n))
   ((null? (cddr rest))
    (check-range x (car rest) (cadr rest) (fixnum-value n) who)
    (f (car rest) (cadr rest)))
   (else (argument-count-error who (+ 1 (length rest))))))

;; (bytevector-copy bv [start [end]])
(define (variadic-bytevector-copy bv . rest)
  (check-bytevector bv bytevector-copy)
  (optional-range bv (bytevector-length bv) rest bytevector-copy
                  (lambda (start end) (sub-bytevector bv start end))))

;; R7RS's argument order, not R6RS's (Chez)
(define (bytevector-copy! to at from)
  (declare public)
  (variadic-bytevector-copy! to at from))

;; (bytevector-copy! to at from [start [end]])
(define (variadic-bytevector-copy! to at from . rest)
  (declare unsafe)
  (check-mutable-bytevector to bytevector-copy!)
  (check-bytevector from bytevector-copy!)
  (optional-range from (bytevector-length from) rest bytevector-copy!
                  (lambda (start end)
                    (let ((n (- end start)))
                      (unless (and (fixnum? at) (>= at 0) (<= (+ at n) (bytevector-length to)))
                        (index-error bytevector-copy! at to))
                      (move-bytes! (byte-address to (fixnum-value at)) (byte-address from (fixnum-value start))
                                   (fixnum-value n))
                      undef))))

(define (bytevector-append . bvs)
  (declare unsafe public)
  (let count ((l bvs)
              (n 0))
    (if (pair? l)
        (begin
          (check-bytevector (car l) bytevector-append)
          (count (cdr l) (+ n (bytevector-length (car l)))))
        (let ((r (alloc-bytevector n)))
          (let copy-next ((l bvs)
                          (at (raw 0)))
            (if (pair? l)
                (begin
                  (copy-bytes! (byte-address r at) (byte-address (car l) (raw 0)) (bytevector-nbytes (car l)))
                  (copy-next (cdr l) (raw+ at (bytevector-nbytes (car l)))))
                r))))))

(define (bytevector=? a b)
  (declare unsafe public)
  (check-bytevector a bytevector=?)
  (check-bytevector b bytevector=?)
  (and (raw= (bytevector-nbytes a) (bytevector-nbytes b))
       (words=? (byte-address a (raw 0)) (byte-address b (raw 0))
                (raw- (bytevector-words (bytevector-nbytes a)) (raw 2)))))

;; UTF-8

;; The bytes of the sequence at byte i of bv, before end: the lead byte and the continuation
;; bytes after it, at most as many as it announces
(define (utf8-span bv (raw i) (raw end))
  (declare unsafe noalloc (result raw))
  (let* ((b (load8 (byte-address bv i) (raw 0)))
         (len (cond
               ((raw<u b (raw 192)) (raw 1))
               ((raw<u b (raw 224)) (raw 2))
               ((raw<u b (raw 240)) (raw 3))
               ((raw<u b (raw 245)) (raw 4))
               (else (raw 1)))))
    (let byte-next ((n (raw 1)))
      (if (and (raw<u n len)
               (raw<u (raw+ i n) end)
               (raw= (bit-and (load8 (byte-address bv (raw+ i n)) (raw 0)) (raw 192)) (raw 128)))
          (byte-next (raw+ n (raw 1)))
          n))))

;; The code of the n-byte span at byte i of bv, U+FFFD if it is not a valid sequence
(define (utf8-span-code bv (raw i) (raw n))
  (declare unsafe noalloc (result raw))
  (let ((b (load8 (byte-address bv i) (raw 0))))
    (cond
     ((raw= n (raw 1)) (if (raw<u b (raw 128)) b (raw 65533)))
     ((raw= n (utf8-length b))
      (let ((code (utf8-code (byte-address bv i) n)))
        (if (raw= code (raw -1)) (raw 65533) code)))
     (else (raw 65533)))))

;; The chars of the bytes [start, end) of bv, an invalid or truncated sequence one U+FFFD, as in
;; Chez's utf8->string: their number when bytes? is 0, else the bytes of their encoding, stored
;; at p unless it is 0
(define (decode-bytes! (raw p) bv (raw start) (raw end) (raw bytes?))
  (declare unsafe noalloc (result raw))
  (let byte-next ((i start)
                  (j (raw 0)))
    (if (raw= i end)
        j
        (let* ((n (utf8-span bv i end))
               (code (utf8-span-code bv i n)))
          (byte-next (raw+ i n)
                     (raw+ j (cond
                              ((raw= bytes? (raw 0)) (raw 1))
                              ((raw= p (raw 0)) (utf8-nbytes code))
                              (else (store-utf8! (raw+ p j) code)))))))))

;; The chars of the bytes [start, end) of bv if they are valid UTF-8, else -1
(define (valid-utf8-chars bv (raw start) (raw end))
  (declare unsafe noalloc (result raw))
  (let byte-next ((i start)
                  (n (raw 0)))
    (if (raw= i end)
        n
        (let ((len (utf8-span bv i end)))
          (if (or (raw= len (raw 1))
                  (and (raw= len (utf8-length (load8 (byte-address bv i) (raw 0))))
                       (not (raw= (utf8-code (byte-address bv i) len) (raw -1)))))
              (if (and (raw= len (raw 1)) (raw>=u (load8 (byte-address bv i) (raw 0)) (raw 128)))
                  (raw -1)
                  (byte-next (raw+ i len) (raw+ n (raw 1))))
              (raw -1))))))

;; The string of the UTF-8 bytes [start, end) of bv, fixnums: a copy of valid bytes, else
;; decoded
(define (utf8-range->string bv start end)
  (declare unsafe)
  (let ((nbytes (- end start))
        (nchars (make-fixnum (valid-utf8-chars bv (fixnum-value start) (fixnum-value end)))))
    (if (eq? nchars -1)
        (let ((s (make-text (make-fixnum (decode-bytes! (raw 0) bv (fixnum-value start) (fixnum-value end) (raw 0)))
                            (make-fixnum (decode-bytes! (raw 0) bv (fixnum-value start) (fixnum-value end) (raw 1))))))
          (decode-bytes! (string-units (as-raw s)) bv (fixnum-value start) (fixnum-value end) (raw 1))
          s)
        (let ((s (make-text nchars nbytes)))
          (copy-bytes! (string-units (as-raw s)) (byte-address bv (fixnum-value start)) (fixnum-value nbytes))
          s))))

(define (utf8->string bv)
  (declare public)
  (check-bytevector bv utf8->string)
  (utf8-range->string bv 0 (bytevector-length bv)))

;; (utf8->string bv [start [end]])
(define (variadic-utf8->string bv . rest)
  (check-bytevector bv utf8->string)
  (optional-range bv (bytevector-length bv) rest utf8->string
                  (lambda (start end) (utf8-range->string bv start end))))

;; The UTF-8 bytevector of the chars [start, end) of s, fixnums
(define (string-range->utf8 s start end)
  (declare unsafe)
  (let* ((s (resolve s))
         (bv (alloc-bytevector (make-fixnum (utf8-range-nbytes s (fixnum-value start) (fixnum-value end))))))
    (put-utf8-range! (byte-address bv (raw 0)) s (fixnum-value start) (fixnum-value end))
    bv))

(define (string->utf8 s)
  (declare public)
  (check-string s string->utf8)
  (string-range->utf8 s 0 (string-length s)))

;; (string->utf8 s [start [end]])
(define (variadic-string->utf8 s . rest)
  (check-string s string->utf8)
  (optional-range s (string-length s) rest string->utf8
                  (lambda (start end) (string-range->utf8 s start end))))
