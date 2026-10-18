;; Integers: fixnums, (n << 3) | 1 for n in [-2^60, 2^60), else bignums
;; [bignum-type, nlimbs, sign, limb 0, ..., limb nlimbs - 1]: raw words, little-endian unsigned
;; 64-bit limbs, sign 0 or 1; normalized, so no leading zero limb and never in the fixnum range

(define (bignum? x)
  (declare unsafe inline)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) (bignum-type))))

;; Limb i of the bignum at p with n limbs, 0 past them
(define (limb (raw p) (raw n) (raw i))
  (declare unsafe inline (result raw))
  (if (raw<u i n) (load p (raw+ i (raw 3))) (raw 0)))

;; Bit 0 of the integer x; a bignum has the parity of its magnitude, so of limb 0
(define (integer-bit-0 x)
  (declare unsafe inline (result raw))
  (bit-and (if (fixnum? x) (fixnum-value x) (load (as-raw x) (raw 3))) (raw 1)))

;; A bignum of the fixnum n zeroed limbs
(define (make-bignum n negative?)
  (declare unsafe inline)
  (let ((r (alloc (raw+ (fixnum-value n) (raw 3)))))
    (store r (raw 0) (bignum-type))
    (store r (raw 1) (fixnum-value n))
    (store r (raw 2) (if negative? (raw 1) (raw 0)))
    (as-val r)))

(define (bignum-limbs x)
  (declare unsafe inline)
  (make-fixnum (load (as-raw x) (raw 1))))

;; Limb 0 of the bignum r gets the magnitude of the fixnum x
(define (set-magnitude! r x)
  (declare unsafe inline (result void))
  (store (as-raw r) (raw 3) (if (raw< (fixnum-value x) (raw 0))
                                (raw- (raw 0) (fixnum-value x))
                                (fixnum-value x))))

;; The 1-limb bignum of the fixnum x, not normalized
(define (fixnum->bignum x)
  (let ((r (make-bignum 1 (< x 0))))
    (set-magnitude! r x)
    r))

(define (->bignum x)
  (if (fixnum? x) (fixnum->bignum x) x))

;; -1, 0 or 1 as the magnitude of the bignum a is below, equal to or above b's;
;; a 1-limb operand may have a zero limb
(define (compare-magnitudes (raw a) (raw b))
  (declare unsafe noalloc (result raw))
  (let ((na (load a (raw 1)))
        (nb (load b (raw 1))))
    (cond
     ((raw<u na nb) (raw -1))
     ((raw>u na nb) (raw 1))
     (else
      (let limb-next ((i na))
        (if (raw= i (raw 0))
            (raw 0)
            (let ((x (load a (raw+ i (raw 2))))
                  (y (load b (raw+ i (raw 2)))))
              (cond
               ((raw<u x y) (raw -1))
               ((raw>u x y) (raw 1))
               (else (limb-next (raw- i (raw 1))))))))))))

;; -1, 0 or 1 as x is below, equal to or above y; both normalized integers, else an error;
;; a bignum is beyond every fixnum, so its sign decides against one
(define (compare-integers x y (raw msg))
  (declare unsafe noalloc (result raw))
  (check-integer x msg)
  (check-integer y msg)
  (cond
   ((and (fixnum? x) (fixnum? y))
    (cond
     ((raw< (as-raw x) (as-raw y)) (raw -1))
     ((raw= (as-raw x) (as-raw y)) (raw 0))
     (else (raw 1))))
   ((fixnum? x) (if (raw= (load (as-raw y) (raw 2)) (raw 1)) (raw 1) (raw -1)))
   ((fixnum? y) (if (raw= (load (as-raw x) (raw 2)) (raw 1)) (raw -1) (raw 1)))
   (else
    (let ((sx (load (as-raw x) (raw 2)))
          (sy (load (as-raw y) (raw 2))))
      (cond
       ((raw< sx sy) (raw 1))
       ((raw> sx sy) (raw -1))
       ((raw= sx (raw 0)) (compare-magnitudes (as-raw x) (as-raw y)))
       (else (compare-magnitudes (as-raw y) (as-raw x))))))))

;; The limbs of r: |a| + |b|, max(na, nb) + 1 of them
(define (add-magnitudes! (raw r) (raw a) (raw b))
  (declare unsafe noalloc (result void))
  (let* ((na (load a (raw 1)))
         (nb (load b (raw 1)))
         (n (if (raw>u na nb) na nb)))
    (store r (raw 1) (raw+ n (raw 1)))
    (let limb-next ((i (raw 0))
                    (carry (raw 0)))
      (if (raw= i n)
          (store r (raw+ i (raw 3)) carry)
          (let* ((x (limb a na i))
                 (s (raw+ x (limb b nb i)))
                 (t (raw+ s carry)))
            (store r (raw+ i (raw 3)) t)
            (limb-next (raw+ i (raw 1))
                       (raw+ (if (raw<u s x) (raw 1) (raw 0))
                             (if (raw<u t s) (raw 1) (raw 0)))))))))

;; The limbs of r: |a| - |b|, with |a| >= |b|, na of them
(define (subtract-magnitudes! (raw r) (raw a) (raw b))
  (declare unsafe noalloc (result void))
  (let ((na (load a (raw 1)))
        (nb (load b (raw 1))))
    (store r (raw 1) na)
    (let limb-next ((i (raw 0))
                    (borrow (raw 0)))
      (if (raw= i na)
          undef
          (let* ((x (limb a na i))
                 (y (limb b nb i))
                 (d (raw- x y)))
            (store r (raw+ i (raw 3)) (raw- d borrow))
            (limb-next (raw+ i (raw 1))
                       (raw+ (if (raw<u x y) (raw 1) (raw 0))
                             (if (raw<u d borrow) (raw 1) (raw 0)))))))))

;; The limbs of the zeroed r: |a| * |b|, na + nb of them (schoolbook);
;; x * y + r[k] + carry < 2^128, so the high word never overflows
(define (multiply-magnitudes! r a b)
  (declare unsafe noalloc (result void))
  (let* ((r (as-raw r))
         (a (as-raw a))
         (b (as-raw b))
         (na (load a (raw 1)))
         (nb (load b (raw 1))))
    (store r (raw 1) (raw+ na nb))
    (let row-next ((i (raw 0)))
      (if (raw= i na)
          undef
          (let ((x (load a (raw+ i (raw 3)))))
            (let column-next ((j (raw 0))
                              (carry (raw 0)))
              (if (raw= j nb)
                  (begin
                    (store r (raw+ (raw+ i nb) (raw 3)) carry)
                    (row-next (raw+ i (raw 1))))
                  (let* ((y (load b (raw+ j (raw 3))))
                         (k (raw+ (raw+ i j) (raw 3)))
                         (lo (raw* x y))
                         (lo1 (raw+ lo (load r k)))
                         (lo2 (raw+ lo1 carry)))
                    (store r k lo2)
                    (column-next (raw+ j (raw 1))
                                 (raw+ (raw+ (raw*hu x y)
                                             (if (raw<u lo1 lo) (raw 1) (raw 0)))
                                       (if (raw<u lo2 lo1) (raw 1) (raw 0))))))))))))

;; The bignum r without its leading zero limbs, or its fixnum when in range
(define (normalize r)
  (declare unsafe noalloc)
  (let ((r (as-raw r)))
    (let limb-next ((n (load r (raw 1))))
      (if (and (raw>u n (raw 1)) (raw= (load r (raw+ n (raw 2))) (raw 0)))
          (limb-next (raw- n (raw 1)))
          (let ((top (load r (raw 3)))
                (negative? (raw= (load r (raw 2)) (raw 1))))
            (store r (raw 1) n)
            (cond
             ((not (raw= n (raw 1))) (as-val r))
             ((raw<u top (raw 1152921504606846976)) ; 2^60
              (make-fixnum (if negative? (raw- (raw 0) top) top)))
             ((and negative? (raw= top (raw 1152921504606846976)))
              (make-fixnum (raw- (raw 0) top)))
             (else (as-val r))))))))

;; r = a + b, or a - b when negate-b? is true, on bignums
(define (add-signed! r a b negate-b?)
  (declare unsafe noalloc (result void))
  (let* ((r (as-raw r))
         (a (as-raw a))
         (b (as-raw b))
         (sa (load a (raw 2)))
         (sb (if negate-b? (raw- (raw 1) (load b (raw 2))) (load b (raw 2)))))
    (cond
     ((raw= sa sb)
      (store r (raw 2) sa)
      (add-magnitudes! r a b))
     ((raw< (compare-magnitudes a b) (raw 0))
      (store r (raw 2) sb)
      (subtract-magnitudes! r b a))
     (else
      (store r (raw 2) sa)
      (subtract-magnitudes! r a b)))))

;; a + b, or a - b when negate-b? is true; the room for a carry may stay unused
(define (add-bignums a b negate-b?)
  (let* ((na (bignum-limbs a))
         (nb (bignum-limbs b))
         (r (make-bignum (+ (if (> na nb) na nb) 1) #f)))
    (add-signed! r a b negate-b?)
    (normalize r)))

(define (multiply-bignums a b)
  (let ((r (make-bignum (+ (bignum-limbs a) (bignum-limbs b))
                        (not (eq? (< a 0) (< b 0))))))
    (multiply-magnitudes! r a b)
    (normalize r)))

;; Division: Knuth's algorithm D on 64-bit limbs, with raw/wu for the 2-by-1 limb steps

(define (word-address (raw p) (raw i))
  (declare unsafe inline (result raw))
  (raw+ p (raw<< i (raw 3))))

;; x is nonzero
(define (leading-zeros (raw x))
  (declare unsafe noalloc (result raw))
  (let bit-next ((n (raw 0))
                 (x x))
    (if (raw< x (raw 0))
        n
        (bit-next (raw+ n (raw 1)) (raw<< x (raw 1))))))

;; The bits of x a left shift by s, below 64, pushes out
(define (shifted-out (raw x) (raw s))
  (declare unsafe inline (result raw))
  (if (raw= s (raw 0)) (raw 0) (raw>>u x (raw- (raw 64) s))))

;; The n limbs at dst = those at src << s, s below 64; returns the bits pushed out
(define (shift-limbs-left! (raw dst) (raw src) (raw n) (raw s))
  (declare unsafe noalloc (result raw))
  (let limb-next ((i (raw 0))
                  (carry (raw 0)))
    (if (raw= i n)
        carry
        (let ((x (load src i)))
          (store dst i (bit-or (raw<< x s) carry))
          (limb-next (raw+ i (raw 1)) (shifted-out x s))))))

;; The n limbs at p >>= s, s below 64
(define (shift-limbs-right! (raw p) (raw n) (raw s))
  (declare unsafe noalloc (result void))
  (let limb-next ((i (raw 0)))
    (if (raw= i n)
        undef
        (let ((next (if (raw= (raw+ i (raw 1)) n) (raw 0) (load p (raw+ i (raw 1))))))
          (store p i (bit-or (raw>>u (load p i) s)
                             (if (raw= s (raw 0)) (raw 0) (raw<< next (raw- (raw 64) s)))))
          (limb-next (raw+ i (raw 1)))))))

;; The n limbs at q = those at a divided by d; returns the remainder
(define (divide-limbs-by! (raw q) (raw a) (raw n) (raw d))
  (declare unsafe noalloc (result raw))
  (let limb-next ((i n)
                  (rem (raw 0)))
    (if (raw= i (raw 0))
        rem
        (let* ((x (load a (raw- i (raw 1))))
               (y (raw/wu rem x d)))
          (store q (raw- i (raw 1)) y)
          (limb-next (raw- i (raw 1)) (raw- x (raw* y d)))))))

;; Whether q * v > r * 2^64 + x, unsigned
(define (product-above? (raw q) (raw v) (raw r) (raw x))
  (declare unsafe inline)
  (let ((hi (raw*hu q v)))
    (or (raw>u hi r)
        (and (raw= hi r) (raw>u (raw* q v) x)))))

;; The estimate of the next quotient limb: u[n]:u[n-1]:u[n-2] over the normalized divisor
;; v1:v2:..., at most 1 too large (Knuth's step D3)
(define (estimate-limb (raw u) (raw n) (raw v1) (raw v2))
  (declare unsafe noalloc (result raw))
  (let* ((hi (load u n))
         (lo (load u (raw- n (raw 1))))
         (x (load u (raw- n (raw 2))))
         (q (if (raw>=u hi v1) (raw -1) (raw/wu hi lo v1)))
         (r (raw- lo (raw* q v1))))
    ;; carry is 1 once r is 2^64 or more, so the estimate is right
    (let refine ((q q)
                 (r r)
                 (carry (if (and (raw>=u hi v1) (raw<u r v1)) (raw 1) (raw 0))))
      (if (and (raw= carry (raw 0)) (product-above? q v2 r x))
          (let ((r (raw+ r v1)))
            (refine (raw- q (raw 1)) r (if (raw<u r v1) (raw 1) (raw 0))))
          q))))

;; The n + 1 limbs at u -= q * the n limbs at v; returns 1 when the result is negative
(define (multiply-subtract! (raw u) (raw v) (raw n) (raw q))
  (declare unsafe noalloc (result raw))
  (let limb-next ((i (raw 0))
                  (carry (raw 0)))
    (if (raw= i n)
        (let ((x (load u n)))
          (store u n (raw- x carry))
          (if (raw<u x carry) (raw 1) (raw 0)))
        (let* ((x (load u i))
               (y (load v i))
               (lo (raw* q y))
               (d (raw- x lo)))
          (store u i (raw- d carry))
          (limb-next (raw+ i (raw 1))
                     (raw+ (raw*hu q y)
                           (raw+ (if (raw<u x lo) (raw 1) (raw 0))
                                 (if (raw<u d carry) (raw 1) (raw 0)))))))))

;; The n + 1 limbs at u += the n limbs at v, dropping the carry out
(define (add-back! (raw u) (raw v) (raw n))
  (declare unsafe noalloc (result void))
  (let limb-next ((i (raw 0))
                  (carry (raw 0)))
    (if (raw= i n)
        (store u n (raw+ (load u n) carry))
        (let* ((x (load u i))
               (s (raw+ x (load v i)))
               (t (raw+ s carry)))
          (store u i t)
          (limb-next (raw+ i (raw 1))
                     (raw+ (if (raw<u s x) (raw 1) (raw 0))
                           (if (raw<u t s) (raw 1) (raw 0))))))))

(define (quotient-limbs (raw na) (raw nb))
  (declare unsafe inline (result raw))
  (if (raw<u na nb) (raw 1) (raw+ (raw- na nb) (raw 1))))

;; The zeroed q gets |a| / |b|, quotient-limbs of them; u, na + nb + 1 zeroed words,
;; gets |a| mod |b| in its first nb
(define (divide-magnitudes! (raw q) (raw u) (raw a) (raw b))
  (declare unsafe noalloc (result void))
  (let ((na (load a (raw 1)))
        (nb (load b (raw 1))))
    (cond
     ((raw<u na nb)
      (copy-words u (word-address a (raw 3)) na))
     ((raw= nb (raw 1))
      (store u (raw 0) (divide-limbs-by! q (word-address a (raw 3)) na (load b (raw 3)))))
     (else
      ;; Shifted so that the divisor's top limb has its top bit set
      (let* ((v (word-address u (raw+ na (raw 1))))
             (s (leading-zeros (load b (raw+ nb (raw 2))))))
        (shift-limbs-left! v (word-address b (raw 3)) nb s)
        (store u na (shift-limbs-left! u (word-address a (raw 3)) na s))
        (let limb-next ((j (raw+ (raw- na nb) (raw 1))))
          (if (raw= j (raw 0))
              (shift-limbs-right! u nb s)
              (let* ((uj (word-address u (raw- j (raw 1))))
                     (y (estimate-limb uj nb (load v (raw- nb (raw 1))) (load v (raw- nb (raw 2))))))
                (if (raw= (multiply-subtract! uj v nb y) (raw 0))
                    (store q (raw- j (raw 1)) y)
                    (begin
                      (add-back! uj v nb)
                      (store q (raw- j (raw 1)) (raw- y (raw 1)))))
                (limb-next (raw- j (raw 1)))))))))))

;; The limbs divide! needs: the result, then room for the quotient and divide-magnitudes!'s u
(define (division-limbs a b quotient?)
  (declare unsafe inline)
  (let* ((na (load (as-raw a) (raw 1)))
         (nb (load (as-raw b) (raw 1)))
         (nq (quotient-limbs na nb)))
    (make-fixnum (raw+ (if quotient? nq nb)
                       (raw+ nq (raw+ (raw+ na nb) (raw 1)))))))

;; Fills the bignum r, of division-limbs limbs, with a / b, or a mod b, truncating
(define (divide! r a b quotient?)
  (declare unsafe noalloc (result void))
  (let* ((r (as-raw r))
         (a (as-raw a))
         (b (as-raw b))
         (na (load a (raw 1)))
         (nb (load b (raw 1)))
         (nq (quotient-limbs na nb))
         (n (if quotient? nq nb))
         (q (word-address r (raw+ n (raw 3))))
         (u (word-address q nq)))
    (divide-magnitudes! q u a b)
    (store r (raw 0) (bignum-type))
    (store r (raw 1) n)
    (store r (raw 2) (cond
                      ((not quotient?) (load a (raw 2)))
                      ((raw= (load a (raw 2)) (load b (raw 2))) (raw 0))
                      (else (raw 1))))
    (copy-words (word-address r (raw 3)) (if quotient? q u) n)))

;; The truncated quotient of the bignums a and b, or the remainder, with a's sign
(define (divide-bignums a b quotient?)
  (let ((r (make-bignum (division-limbs a b quotient?) #f)))
    (divide! r a b quotient?)
    (normalize r)))

(define (check-division x y (raw msg) (raw zero-msg))
  (declare unsafe inline (result void))
  (check-integer x msg)
  (check-integer y msg)
  (when (raw= (as-raw y) (as-raw 0))
    (scheme-error zero-msg y)))

;; The slow paths, out of line: a bignum operand or result
(define (add-integers x y)
  (declare unsafe)
  (check-integer x "`+` expected an integer")
  (check-integer y "`+` expected an integer")
  (add-bignums (->bignum x) (->bignum y) #f))

(define (subtract-integers x y)
  (declare unsafe)
  (check-integer x "`-` expected an integer")
  (check-integer y "`-` expected an integer")
  (add-bignums (->bignum x) (->bignum y) #t))

(define (multiply-integers x y)
  (declare unsafe)
  (check-integer x "`*` expected an integer")
  (check-integer y "`*` expected an integer")
  (multiply-bignums (->bignum x) (->bignum y)))

(define (quotient-integers x y)
  (declare unsafe)
  (check-division x y "`quotient` expected an integer" "`quotient` expected a nonzero divisor")
  (divide-bignums (->bignum x) (->bignum y) #t))

;; The remainder has x's sign; the modulo y's
(define (modulo-integers x y)
  (declare unsafe)
  (check-division x y "`modulo` expected an integer" "`modulo` expected a nonzero divisor")
  (let ((r (divide-bignums (->bignum x) (->bignum y) #f)))
    (if (or (= r 0) (eq? (< r 0) (< y 0)))
        r
        (+ r y))))
