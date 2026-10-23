;; Room for the digits of any raw, written backwards
(define-data digits 3)

;; Writes the nonnegative n in base radix, at most 16, with uppercase letters,
;; zero-padded to width digits (at most 24)
(define (put-digits (raw out) (raw n) (raw radix) (raw width))
  (declare unsafe noalloc (result void))
  (let digit-next ((m n)
                   (i (raw 24)))
    (let ((j (raw- i (raw 1)))
          (d (raw% m radix)))
      (store8 digits j (raw+ d (if (raw< d (raw 10)) (raw 48) (raw 55)))) ; 0, A - 10
      (if (and (raw= (raw/ m radix) (raw 0)) (raw>= (raw- (raw 24) j) width))
          (write-bytes out (raw+ digits j) (raw- (raw 24) j))
          (digit-next (raw/ m radix) j)))))

(define (put-int (raw out) (raw n))
  (declare unsafe noalloc (result void))
  (when (raw< n (raw 0))
    (put-string out "-"))
  (put-digits out (if (raw< n (raw 0)) (raw- (raw 0) n) n) (raw 10) (raw 1)))

;; Stores the UTF-8 encoding of the code point n at p; returns its length
;; The length of the UTF-8 encoding of the code point n
(define (utf8-nbytes (raw n))
  (declare unsafe inline (result raw))
  (cond
   ((raw<u n (raw 128)) (raw 1))
   ((raw<u n (raw 2048)) (raw 2))
   ((raw<u n (raw 65536)) (raw 3))
   (else (raw 4))))

(define (store-utf8! (raw p) (raw n))
  (declare unsafe noalloc (result raw))
  (let ((nbytes (utf8-nbytes n)))
    (let byte-next ((m n)
                    (i (raw- nbytes (raw 1))))
      (if (raw= i (raw 0))
          (begin
            ;; The lead byte: 0, else 0xFF00 >> nbytes is 0xC0, 0xE0 or 0xF0
            (store8 p i (if (raw= nbytes (raw 1))
                            m
                            (bit-or m (bit-and (raw>> (raw 65280) nbytes) (raw 255)))))
            nbytes)
          (begin
            (store8 p i (bit-or (bit-and m (raw 63)) (raw 128)))
            (byte-next (raw>> m (raw 6)) (raw- i (raw 1))))))))

;; Writes the UTF-8 encoding of the code point n
(define (put-utf8 (raw out) (raw n))
  (declare unsafe noalloc (result void))
  (write-bytes out digits (store-utf8! digits n)))

;; Prints x to the port out, like write when write? is 1, else like display: strings and
;; characters bare
(define (put-datum (raw out) (raw write?) x)
  (declare unsafe noalloc (result void))
  (cond
   ((fixnum? x) (put-int out (raw>> (as-raw x) (raw 3))))
   ((char? x) (display-char out write? x))
   ((tag=? x (raw 2))
    (put-string out (cond
                     ((eq? x #f) "#f")
                     ((eq? x #t) "#t")
                     ((null? x) "()")
                     ((eq? x undef) "#<undef>")
                     ((eof-object? x) "#!eof")
                     (else "#<err>"))))
   ((pair? x) (display-pair out write? x))
   ((tag=? x (raw 0)) (display-pointer out write? (resolve x)))
   (else (put-string out "#<err>"))))

;; The name of the character n, or 0
(define (char-name (raw n))
  (declare unsafe noalloc (result raw))
  (cond
   ((raw= n (raw 0))   "nul")
   ((raw= n (raw 7))   "alarm")
   ((raw= n (raw 8))   "backspace")
   ((raw= n (raw 9))   "tab")
   ((raw= n (raw 10))  "newline")
   ((raw= n (raw 11))  "vtab")
   ((raw= n (raw 12))  "page")
   ((raw= n (raw 13))  "return")
   ((raw= n (raw 27))  "esc")
   ((raw= n (raw 32))  "space")
   ((raw= n (raw 127)) "delete")
   (else (raw 0))))

;; Prints c like Chez's write: a name, hexadecimal for the other control
;; characters and the line separators U+0085 and U+2028, else UTF-8; bare UTF-8 for display
(define (display-char (raw out) (raw write?) c)
  (declare unsafe noalloc (result void))
  (let* ((n (raw>> (as-raw c) (raw 8)))
         (name (char-name n)))
    (cond
     ((raw= write? (raw 0)) (put-utf8 out n))
     ((not (raw= name (raw 0)))
      (put-string out "#\\")
      (put-string out name))
     ((or (raw< n (raw 32)) (raw= n (raw 133)) (raw= n (raw 8232)))
      (put-string out "#\\x")
      (put-digits out n (raw 16) (raw 1)))
     (else
      (put-string out "#\\")
      (put-utf8 out n)))))

(define (display-pair (raw out) (raw write?) x)
  (declare unsafe noalloc (result void))
  (put-string out "(")
  (put-datum out write? (pair-car x))
  (let more ((rest (pair-cdr x)))
    (if (pair? rest)
        (begin
          (put-string out " ")
          (put-datum out write? (pair-car rest))
          (more (pair-cdr rest)))
        (begin
          (unless (null? rest)
            (put-string out " . ")
            (put-datum out write? rest))
          (put-string out ")")))))

(define (display-pointer (raw out) (raw write?) x)
  (declare unsafe noalloc (result void))
  (cond
   ((raw= (as-raw x) (raw 0)) (put-string out "#<null>"))
   ((raw= (load (as-raw x) (raw 0)) bignum-type) (display-bignum out (as-raw x)))
   ((raw= (load (as-raw x) (raw 0)) ratnum-type)
    (put-datum out write? (as-val (load (as-raw x) (raw 1))))
    (put-string out "/")
    (put-datum out write? (as-val (load (as-raw x) (raw 2)))))
   ((vector-header? (load (as-raw x) (raw 0))) (display-vector out write? x))
   ((bytevector-header? (load (as-raw x) (raw 0))) (display-bytevector out x))
   ((raw= (load (as-raw x) (raw 0)) record-type) (display-record out write? x))
   ((raw= (load (as-raw x) (raw 0)) rtd-type)
    (put-string out "#<record type ")
    (display-symbol out (as-val (load (as-raw x) (raw 1))))
    (put-string out ">"))
   ((string-header? (load (as-raw x) (raw 0))) (display-string out write? x))
   ((symbol-header? (load (as-raw x) (raw 0))) (put-symbol out write? x))
   ((raw= (load (as-raw x) (raw 0)) flexvector-type) (put-string out "#<flexvector>"))
   ((raw= (load (as-raw x) (raw 0)) hashtable-type) (display-hashtable out x))
   ((raw= (load (as-raw x) (raw 0)) values-type) (display-values out write? x))
   ((raw= (load (as-raw x) (raw 0)) port-type)
    (put-string out (if (binary-port? x) "#<binary " "#<"))
    (put-string out (if (input-port? x) "input port>" "output port>")))
   ((raw= (load (as-raw x) (raw 0)) procedure-type)
    (let ((name (load (as-raw x) (raw 3))))
      (if (raw= name (raw 0))
          (put-string out "#<procedure>")
          (begin
            (put-string out "#<procedure ")
            (put-string out name)
            (put-string out ">")))))
   (else (put-string out "#<unknown ptr>"))))

;; Quoted when write? is 1, else bare UTF-8
(define (display-string (raw out) (raw write?) s)
  (declare unsafe noalloc (result void))
  (cond
   ((raw= write? (raw 1)) (put-quoted-string out s))
   ((not (utf32-string? s))
    (write-bytes out (string-units (as-raw s)) (string-end s)))
   (else
    (let char-next ((i (raw 0)))
      (if (raw= i (string-end s))
          undef
          (begin
            (put-utf8 out (string-code-at s i))
            (char-next (string-next s i))))))))

;; Like Chez's write: in quotes, with escapes; hexadecimal for the other control characters,
;; delete and the line separators U+0085 and U+2028, else UTF-8
(define (put-quoted-string (raw out) s)
  (declare unsafe noalloc (result void))
  (put-string out "\"")
  (let char-next ((i (raw 0)))
    (if (raw= i (string-end s))
        undef
        (let* ((n (string-code-at s i))
               (escape (string-escape n)))
          (cond
           ((not (raw= escape (raw 0))) (put-string out escape))
           ((or (raw< n (raw 32)) (raw= n (raw 127)) (raw= n (raw 133)) (raw= n (raw 8232)))
            (put-string out "\\x")
            (put-digits out n (raw 16) (raw 1))
            (put-string out ";"))
           (else (put-utf8 out n)))
          (char-next (string-next s i)))))
  (put-string out "\""))

;; Its name, bare; a gensym of no name g and its number
(define (display-symbol (raw out) sym)
  (declare unsafe noalloc (result void))
  (if (symbol-name sym)
      (display-string out (raw 0) (symbol-name sym))
      (begin
        (put-string out "g")
        (put-digits out (symbol-hash-word sym) (raw 10) (raw 1)))))

;; A gensym written as #{name number} when print-gensym is #t, #:name when it is pretty
(define (put-symbol (raw out) (raw write?) sym)
  (declare unsafe noalloc (result void))
  (let ((mode (load print-gensym-mode (raw 0))))
    (cond
     ((or (raw= write? (raw 0)) (raw= mode (raw 0)) (not (gensym? sym)))
      (display-symbol out sym))
     ((raw= mode (raw 1))
      (put-string out "#{")
      (display-symbol out sym)
      (put-string out " ")
      (put-digits out (symbol-hash-word sym) (raw 10) (raw 1))
      (put-string out "}"))
     (else
      (put-string out "#:")
      (display-symbol out sym)))))

;; The escape of the character n in a string, or 0
(define (string-escape (raw n))
  (declare unsafe noalloc (result raw))
  (cond
   ((raw= n (raw 7))  "\\a")
   ((raw= n (raw 8))  "\\b")
   ((raw= n (raw 9))  "\\t")
   ((raw= n (raw 10)) "\\n")
   ((raw= n (raw 11)) "\\v")
   ((raw= n (raw 12)) "\\f")
   ((raw= n (raw 13)) "\\r")
   ((raw= n (raw 34)) "\\\"")
   ((raw= n (raw 92)) "\\\\")
   (else (raw 0))))

;; One per line, like Chez's REPL
(define (display-values (raw out) (raw write?) x)
  (declare unsafe noalloc (result void))
  (let next ((l (as-val (load (as-raw x) (raw 1))))
             (first #t))
    (if (pair? l)
        (begin
          (unless first
            (put-string out "\n"))
          (put-datum out write? (pair-car l))
          (next (pair-cdr l) #f))
        undef)))

(define (display-vector (raw out) (raw write?) v)
  (declare unsafe noalloc (result void))
  (let ((n (load (as-raw v) (raw 1))))
    (put-string out "#(")
    (let elem-next ((i (raw 0)))
      (if (raw= i n)
          undef
          (begin
            (unless (raw= i (raw 0))
              (put-string out " "))
            (put-datum out write? (as-val (load (as-raw v) (raw+ i (raw 2)))))
            (elem-next (raw+ i (raw 1))))))
    (put-string out ")")))

;; #vu8(1 2), as in Chez
(define (display-bytevector (raw out) bv)
  (declare unsafe noalloc (result void))
  (put-string out "#vu8(")
  (let byte-next ((i (raw 0)))
    (if (raw= i (bytevector-nbytes bv))
        undef
        (begin
          (unless (raw= i (raw 0))
            (put-string out " "))
          (put-int out (load8 (byte-address bv i) (raw 0)))
          (byte-next (raw+ i (raw 1))))))
  (put-string out ")"))

;; Like Chez's, without the gensym of the type's name
(define (display-record (raw out) (raw write?) r)
  (declare unsafe noalloc (result void))
  (let ((n (load (as-raw r) (raw 1))))
    (put-string out "#[")
    (display-symbol out (as-val (load (load (as-raw r) (raw 2)) (raw 1))))
    (let field-next ((i (raw 0)))
      (if (raw= i n)
          undef
          (begin
            (put-string out " ")
            (put-datum out write? (as-val (load (as-raw r) (raw+ i (raw 3)))))
            (field-next (raw+ i (raw 1))))))
    (put-string out "]")))

;; The number of limbs at q once its leading zero limbs are dropped
(define (significant-limbs (raw q) (raw m))
  (declare unsafe noalloc (result raw))
  (if (and (raw>u m (raw 0)) (raw= (load q (raw- m (raw 1))) (raw 0)))
      (significant-limbs q (raw- m (raw 1)))
      m))

;; Stores the decimal digits of n at p + j, at least width of them, zero-padded; returns the
;; index past them
(define (store-digits! (raw p) (raw j) (raw n) (raw width))
  (declare unsafe noalloc (result raw))
  (let count ((m (raw/ n (raw 10)))
              (d (raw 1)))
    (if (raw= m (raw 0))
        (let ((end (raw+ j (if (raw> d width) d width))))
          (let digit-next ((i end)
                           (m n))
            (if (raw= i j)
                end
                (begin
                  (store8 p (raw- i (raw 1)) (raw+ (raw% m (raw 10)) (raw 48)))
                  (digit-next (raw- i (raw 1)) (raw/ m (raw 10)))))))
        (count (raw/ m (raw 10)) (raw+ d (raw 1))))))

;; Up to this many chunks of 18 digits, the digits come from repeated division by 10^18
(define (print-leaf-chunks)
  (declare unsafe inline)
  16)

;; Stores the digits of the na limbs at a, below 10^(18c), at p: 18c of them, zero-padded;
;; levels holds 4 words per power 10^(18 * 2^k): its limbs, their count, the normalized
;; limbs and their shift; t is the scratch
(define (store-limbs-digits! (raw p) (raw a) (raw na) (raw c) (raw levels) (raw t))
  (declare unsafe noalloc (result void))
  (let ((na (significant-limbs a na)))
    (if (raw<=u c (fixnum-value (print-leaf-chunks)))
        (begin
          (copy-words t a na)
          (let chunk-next ((i c)
                           (m na))
            (if (raw= i (raw 0))
                undef
                (begin
                  (store-digits! p (raw* (raw- i (raw 1)) (raw 18))
                                 (divide-limbs-by! t t m (raw 1000000000000000000)) (raw 18))
                  (chunk-next (raw- i (raw 1)) (significant-limbs t m))))))
        ;; Split at the largest power 10^(18h) below 10^(18c)
        (let level-next ((h (raw 1))
                         (e levels))
          (if (raw<u (raw<< h (raw 1)) c)
              (level-next (raw<< h (raw 1)) (word-address e (raw 4)))
              (let ((m (load e (raw 1)))
                    (low (raw+ p (raw* (raw- c h) (raw 18)))))
                (if (raw<u na m)
                    (begin
                      (store-limbs-digits! p a (raw 0) (raw- c h) levels t)
                      (store-limbs-digits! low a na h levels t))
                    ;; a < 10^(36h), so a shifted fits in 2m limbs; t gets na + 1 of them
                    (let ((s (load e (raw 3)))
                          (k (if (raw<u (raw- na m) m) (raw+ (raw- na m) (raw 1)) m))
                          (rest (word-address t (raw+ na (raw 1)))))
                      (store t na (shift-limbs-left! t a na s))
                      (divide-limbs! t (load e (raw 2)) m k rest)
                      (shift-limbs-right! t m s)
                      (store-limbs-digits! p (word-address t m) k (raw- c h) levels rest)
                      (store-limbs-digits! low t m h levels rest)))))))))

;; Fills levels from the power 10^(18h), the np limbs at f, squaring it until 10^(36h) is at
;; least 10^(18c); returns the end of the last normalized one
(define (store-powers! (raw levels) (raw f) (raw np) (raw h) (raw c))
  (declare unsafe noalloc (result raw))
  (let ((s (leading-zeros (load f (raw- np (raw 1)))))
        (v (word-address f np))
        (sq (word-address f (raw<< np (raw 1)))))
    (shift-limbs-left! v f np s)
    (store levels (raw 0) f)
    (store levels (raw 1) np)
    (store levels (raw 2) v)
    (store levels (raw 3) s)
    (if (raw<u (raw<< h (raw 1)) c)
        (begin
          (fill-words sq (raw<< np (raw 1)) (raw 0))
          (karatsuba! sq f np f np (word-address sq (raw<< np (raw 1))))
          (store-powers! (word-address levels (raw 4)) sq (significant-limbs sq (raw<< np (raw 1)))
                         (raw<< h (raw 1)) c))
        sq)))

;; Prints the bignum at x in decimal, its digits stored, then written at once; a big one is
;; split by powers 10^(18 * 2^k) recursively, in subquadratic time; the room comes from
;; spare-mem (unused outside a collection), zeroed again after, else from a fresh mapping
(define (display-bignum (raw out) (raw x))
  (declare unsafe noalloc (result void))
  (let* ((n (load x (raw 1)))
         ;; At most 64n log10(2) / 18 + 1 chunks; text starts with a byte for the sign
         (c (raw+ (raw>>u (raw* n (raw 70208)) (raw 16)) (raw 2)))
         (nt (raw+ (raw>>u (raw+ (raw* c (raw 18)) (raw 1)) (raw 3)) (raw 1)))
         (leaf? (raw<=u c (fixnum-value (print-leaf-chunks))))
         ;; Then the powers, the recursion's limbs and the division's scratch, about 9n
         (words (raw+ nt (if leaf? (raw+ n (raw 1)) (raw+ (raw* n (raw 20)) (raw 2304)))))
         (mapped? (raw>u words (load scheme-cap (raw 0))))
         (text (if mapped? (map-words words) (load spare-mem (raw 0))))
         (levels (word-address text nt))
         (p (raw+ text (raw 1)))
         (nd (raw* c (raw 18))))
    (if leaf?
        (store-limbs-digits! p (raw+ x (raw 24)) n c levels levels)
        (let ((powers (word-address levels (raw 256))))
          (store powers (raw 0) (raw 1000000000000000000))
          (store-limbs-digits! p (raw+ x (raw 24)) n c levels
                               (store-powers! levels powers (raw 1) (raw 1) c))))
    (let zero-next ((i (raw 0)))
      (if (and (raw<u i (raw- nd (raw 1))) (raw= (load8 p i) (raw 48))) ; 0
          (zero-next (raw+ i (raw 1)))
          (let ((i (if (raw= (load x (raw 2)) (raw 1))
                       (begin (store8 text i (raw 45)) i) ; -
                       (raw+ i (raw 1)))))
            (write-bytes out (raw+ text i) (raw- (raw+ nd (raw 1)) i))
            (if mapped?
                (unmap-words text words)
                (fill-words text words (raw 0))))))))
