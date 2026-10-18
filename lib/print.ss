;; Room for the digits of any raw, written backwards
(define-data digits 3)

;; Writes the nonnegative n in base radix, at most 16, with uppercase letters,
;; zero-padded to width digits (at most 24)
(define (put-digits (raw fd) (raw n) (raw radix) (raw width))
  (declare unsafe noalloc (result void))
  (let digit-next ((m n)
                   (i (raw 24)))
    (let ((j (raw- i (raw 1)))
          (d (raw% m radix)))
      (store8 digits j (raw+ d (if (raw< d (raw 10)) (raw 48) (raw 55)))) ; 0, A - 10
      (if (and (raw= (raw/ m radix) (raw 0)) (raw>= (raw- (raw 24) j) width))
          (write-bytes fd (raw+ digits j) (raw- (raw 24) j))
          (digit-next (raw/ m radix) j)))))

(define (put-int (raw fd) (raw n))
  (declare unsafe noalloc (result void))
  (when (raw< n (raw 0))
    (put-string fd "-"))
  (put-digits fd (if (raw< n (raw 0)) (raw- (raw 0) n) n) (raw 10) (raw 1)))

;; Writes the UTF-8 encoding of the code point n
(define (put-utf8 (raw fd) (raw n))
  (declare unsafe noalloc (result void))
  (let ((nbytes (cond
                 ((raw<u n (raw 128)) (raw 1))
                 ((raw<u n (raw 2048)) (raw 2))
                 ((raw<u n (raw 65536)) (raw 3))
                 (else (raw 4)))))
    (let byte-next ((m n)
                    (i (raw- nbytes (raw 1))))
      (if (raw= i (raw 0))
          (begin
            ;; The lead byte: 0, else 0xFF00 >> nbytes is 0xC0, 0xE0 or 0xF0
            (store8 digits i (if (raw= nbytes (raw 1))
                                 m
                                 (bit-or m (bit-and (raw>> (raw 65280) nbytes) (raw 255)))))
            (write-bytes fd digits nbytes))
          (begin
            (store8 digits i (bit-or (bit-and m (raw 63)) (raw 128)))
            (byte-next (raw>> m (raw 6)) (raw- i (raw 1))))))))

;; Prints x to the file descriptor out
(define (display (raw out) x)
  (declare unsafe noalloc (result void))
  (cond
   ((fixnum? x) (put-int out (raw>> (as-raw x) (raw 3))))
   ((char? x) (display-char out x))
   ((tag=? x (raw 2))
    (put-string out (cond
                     ((eq? x #f) "#f")
                     ((eq? x #t) "#t")
                     ((null? x) "()")
                     ((eq? x undef) "#<undef>")
                     (else "#<err>"))))
   ((pair? x) (display-pair out x))
   ((tag=? x (raw 0)) (display-pointer out x))
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
;; characters and the line separators U+0085 and U+2028, else UTF-8
(define (display-char (raw out) c)
  (declare unsafe noalloc (result void))
  (let* ((n (raw>> (as-raw c) (raw 8)))
         (name (char-name n)))
    (put-string out "#\\")
    (cond
     ((not (raw= name (raw 0))) (put-string out name))
     ((or (raw< n (raw 32)) (raw= n (raw 133)) (raw= n (raw 8232)))
      (put-string out "x")
      (put-digits out n (raw 16) (raw 1)))
     (else (put-utf8 out n)))))

(define (display-pair (raw out) x)
  (declare unsafe noalloc (result void))
  (put-string out "(")
  (display out (car x))
  (let more ((rest (cdr x)))
    (if (pair? rest)
        (begin
          (put-string out " ")
          (display out (car rest))
          (more (cdr rest)))
        (begin
          (unless (null? rest)
            (put-string out " . ")
            (display out rest))
          (put-string out ")")))))

(define (display-pointer (raw out) x)
  (declare unsafe noalloc (result void))
  (cond
   ((raw= (as-raw x) (raw 0)) (put-string out "#<null>"))
   ((raw= (load (as-raw x) (raw 0)) (bignum-type)) (display-bignum out (as-raw x)))
   ((raw= (load (as-raw x) (raw 0)) (vector-type)) (display-vector out x))
   ((string-header? (load (as-raw x) (raw 0))) (display-string out x))
   ((raw= (load (as-raw x) (raw 0)) (symbol-type)) (display-symbol out x))
   ((raw= (load (as-raw x) (raw 0)) (flexvector-type)) (put-string out "#<flexvector>"))
   ((raw= (load (as-raw x) (raw 0)) (values-type)) (display-values out x))
   ((raw= (load (as-raw x) (raw 0)) (procedure-type))
    (let ((name (load (as-raw x) (raw 3))))
      (if (raw= name (raw 0))
          (put-string out "#<procedure>")
          (begin
            (put-string out "#<procedure ")
            (put-string out name)
            (put-string out ">")))))
   (else (put-string out "#<unknown ptr>"))))

;; Like Chez's write: in quotes, with escapes; hexadecimal for the other control characters,
;; delete and the line separators U+0085 and U+2028, else UTF-8
(define (display-string (raw out) s)
  (declare unsafe noalloc (result void))
  (put-string out "\"")
  (let char-next ((i (raw 0)))
    (if (raw= i (string-nchars s))
        undef
        (let* ((n (string-code s i))
               (escape (string-escape n)))
          (cond
           ((not (raw= escape (raw 0))) (put-string out escape))
           ((or (raw< n (raw 32)) (raw= n (raw 127)) (raw= n (raw 133)) (raw= n (raw 8232)))
            (put-string out "\\x")
            (put-digits out n (raw 16) (raw 1))
            (put-string out ";"))
           (else (put-utf8 out n)))
          (char-next (raw+ i (raw 1))))))
  (put-string out "\""))

;; Its name, bare
(define (display-symbol (raw out) sym)
  (declare unsafe noalloc (result void))
  (let ((s (symbol-name sym)))
    (let char-next ((i (raw 0)))
      (if (raw= i (string-nchars s))
          undef
          (begin
            (put-utf8 out (string-code s i))
            (char-next (raw+ i (raw 1))))))))

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
(define (display-values (raw out) x)
  (declare unsafe noalloc (result void))
  (let next ((l (as-val (load (as-raw x) (raw 1))))
             (first #t))
    (if (pair? l)
        (begin
          (unless first
            (put-string out "\n"))
          (display out (car l))
          (next (cdr l) #f))
        undef)))

(define (display-vector (raw out) v)
  (declare unsafe noalloc (result void))
  (let ((n (load (as-raw v) (raw 1))))
    (put-string out "#(")
    (let elem-next ((i (raw 0)))
      (if (raw= i n)
          undef
          (begin
            (unless (raw= i (raw 0))
              (put-string out " "))
            (display out (as-val (load (as-raw v) (raw+ i (raw 2)))))
            (elem-next (raw+ i (raw 1))))))
    (put-string out ")")))

;; Divides the m limbs at q by 10^9 in place, returns the remainder; each limb is two
;; 32-bit halves, so rem << 32 | half < 2^62 and the signed raw/ works
(define (divide-limbs! (raw q) (raw m))
  (declare unsafe noalloc (result raw))
  (let limb-next ((i m)
                  (rem (raw 0)))
    (if (raw= i (raw 0))
        rem
        (let* ((limb (load q (raw- i (raw 1))))
               (hi (bit-or (raw<< rem (raw 32)) (raw>>u limb (raw 32))))
               (lo (bit-or (raw<< (raw% hi (raw 1000000000)) (raw 32))
                           (bit-and limb (raw 4294967295)))))
          (store q (raw- i (raw 1)) (bit-or (raw<< (raw/ hi (raw 1000000000)) (raw 32))
                                            (raw/ lo (raw 1000000000))))
          (limb-next (raw- i (raw 1)) (raw% lo (raw 1000000000)))))))

;; The number of limbs at q once its leading zero limbs are dropped
(define (significant-limbs (raw q) (raw m))
  (declare unsafe noalloc (result raw))
  (if (and (raw>u m (raw 0)) (raw= (load q (raw- m (raw 1))) (raw 0)))
      (significant-limbs q (raw- m (raw 1)))
      m))

;; Prints the bignum at x in decimal: its limbs, copied to spare-mem (unused outside a
;; collection), are divided by 10^9 until zero, the remainders stored after them, then printed
;; most significant first, 9 digits each but the first; spare-mem is zeroed again after
(define (display-bignum (raw out) (raw x))
  (declare unsafe noalloc (result void))
  (let* ((n (load x (raw 1)))
         (q (load spare-mem (raw 0)))
         (chunks (raw+ q (raw<< n (raw 3)))))
    ;; A chunk per 29.9 bits: at most 2.2 n + 1 of them
    (when (raw>u (raw+ (raw<< n (raw 2)) (raw 1)) (load scheme-cap (raw 0)))
      (fatal "Out of memory\n"))
    (when (raw= (load x (raw 2)) (raw 1))
      (put-string out "-"))
    (copy-words q (raw+ x (raw 24)) n)
    (let chunk-next ((k (raw 0))
                     (m n))
      (if (raw>u m (raw 0))
          (begin
            (store chunks k (divide-limbs! q m))
            (chunk-next (raw+ k (raw 1)) (significant-limbs q m)))
          (begin
            (put-digits out (load chunks (raw- k (raw 1))) (raw 10) (raw 1))
            (let print-next ((i (raw- k (raw 1))))
              (if (raw= i (raw 0))
                  undef
                  (begin
                    (put-digits out (load chunks (raw- i (raw 1))) (raw 10) (raw 9))
                    (print-next (raw- i (raw 1))))))
            (fill-words q (raw+ n k) (raw 0)))))))
