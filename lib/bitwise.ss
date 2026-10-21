;; Bitwise operations on integers as infinite two's complement; bignums are sign and magnitude,
;; so their limbs are turned into two's complement on the fly

;; The and and the or of two tagged fixnums keep tag 1; their xor needs it again
(define (bitwise-and x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y))
      (as-val (bit-and (as-raw x) (as-raw y)))
      (and-integers x y)))

(define (bitwise-ior x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y))
      (as-val (bit-or (as-raw x) (as-raw y)))
      (ior-integers x y)))

(define (bitwise-xor x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y))
      (as-val (bit-or (bit-xor (as-raw x) (as-raw y)) (raw 1)))
      (xor-integers x y)))

;; ~(n << 3) is (~n << 3) | 7, so flipping all bits but the tag's gives ~n
(define (bitwise-not x)
  (declare unsafe inline public)
  (if (fixnum? x)
      (as-val (bit-xor (as-raw x) (raw -8)))
      (not-integer x)))

(define (limb-op (raw op) (raw x) (raw y))
  (declare unsafe inline (result raw))
  (cond
   ((raw= op (raw 0)) (bit-and x y))
   ((raw= op (raw 1)) (bit-or x y))
   (else (bit-xor x y))))

;; Limb i of the bignum at p in two's complement, given the borrow of |p| - 1 so far
(define (twos-limb (raw p) (raw i) (raw borrow))
  (declare unsafe inline (result raw))
  (let ((m (limb p (load p (raw 1)) i)))
    (if (raw= (load p (raw 2)) (raw 0))
        m
        (bit-xor (raw- m borrow) (raw -1)))))

;; The borrow out of limb i of |p| - 1, given the one into it
(define (next-borrow (raw p) (raw i) (raw borrow))
  (declare unsafe inline (result raw))
  (if (raw<u (limb p (load p (raw 1)) i) borrow) (raw 1) (raw 0)))

;; The limbs of r, max(na, nb) + 1 of them: a op b (0 and, 1 or, 2 xor); the sign of the
;; result is the op on the signs, and a negative result is turned back into its magnitude,
;; ~r + 1
(define (bitwise-limbs! r a b (raw op))
  (declare unsafe noalloc (result void))
  (let* ((r (as-raw r))
         (a (as-raw a))
         (b (as-raw b))
         (n (load r (raw 1)))
         (negative? (raw= (limb-op op (load a (raw 2)) (load b (raw 2))) (raw 1))))
    (store r (raw 2) (if negative? (raw 1) (raw 0)))
    (let limb-next ((i (raw 0))
                    (borrow-a (raw 1))
                    (borrow-b (raw 1))
                    (carry (raw 1)))
      (if (raw= i n)
          undef
          (let ((x (limb-op op (twos-limb a i borrow-a) (twos-limb b i borrow-b))))
            (if negative?
                (let ((m (raw+ (bit-xor x (raw -1)) carry)))
                  (store r (raw+ i (raw 3)) m)
                  (limb-next (raw+ i (raw 1))
                             (next-borrow a i borrow-a)
                             (next-borrow b i borrow-b)
                             (if (and (raw= carry (raw 1)) (raw= m (raw 0))) (raw 1) (raw 0))))
                (begin
                  (store r (raw+ i (raw 3)) x)
                  (limb-next (raw+ i (raw 1))
                             (next-borrow a i borrow-a)
                             (next-borrow b i borrow-b)
                             carry))))))))

(define (bitwise-bignums a b (raw op))
  (declare unsafe inline)
  (let* ((na (bignum-limbs a))
         (nb (bignum-limbs b))
         (r (make-bignum (+ (if (> na nb) na nb) 1) #f)))
    (bitwise-limbs! r a b op)
    (normalize r)))

(define (and-integers x y)
  (declare unsafe)
  (check-exact-integer x bitwise-and)
  (check-exact-integer y bitwise-and)
  (bitwise-bignums (->bignum x) (->bignum y) (raw 0)))

(define (ior-integers x y)
  (declare unsafe)
  (check-exact-integer x bitwise-ior)
  (check-exact-integer y bitwise-ior)
  (bitwise-bignums (->bignum x) (->bignum y) (raw 1)))

(define (xor-integers x y)
  (declare unsafe)
  (check-exact-integer x bitwise-xor)
  (check-exact-integer y bitwise-xor)
  (bitwise-bignums (->bignum x) (->bignum y) (raw 2)))

(define (not-integer x)
  (check-exact-integer x bitwise-not)
  (- -1 x))

(define (variadic-bitwise-and . xs)
  (fold-left (lambda (a b) (bitwise-and a b)) -1 xs))

(define (variadic-bitwise-ior . xs)
  (fold-left (lambda (a b) (bitwise-ior a b)) 0 xs))

(define (variadic-bitwise-xor . xs)
  (fold-left (lambda (a b) (bitwise-xor a b)) 0 xs))

;; The limbs of r from k / 64: |a| << k
(define (shift-magnitude-left! r a k)
  (declare unsafe noalloc (result void))
  (let* ((r (as-raw r))
         (a (as-raw a))
         (k (fixnum-value k))
         (na (load a (raw 1)))
         (dst (word-address r (raw+ (raw>>u k (raw 6)) (raw 3)))))
    (store dst na (shift-limbs-left! dst (word-address a (raw 3)) na (bit-and k (raw 63))))))

;; The limbs of r: |a| >> k, na - k / 64 of them
(define (shift-magnitude-right! r a k)
  (declare unsafe noalloc (result void))
  (let* ((r (as-raw r))
         (a (as-raw a))
         (k (fixnum-value k))
         (n (load r (raw 1))))
    (copy-words (word-address r (raw 3)) (word-address a (raw+ (raw>>u k (raw 6)) (raw 3))) n)
    (shift-limbs-right! (word-address r (raw 3)) n (bit-and k (raw 63)))))

;; x << k, k a nonnegative fixnum: a fixnum when shifting the tagged word back gives it again
(define (shift-left x k)
  (declare unsafe)
  (let ((t (raw<< (raw- (as-raw x) (raw 1)) (fixnum-value k))))
    (if (and (fixnum? x)
             (raw<u (fixnum-value k) (raw 64))
             (raw= (raw>> t (fixnum-value k)) (raw- (as-raw x) (raw 1))))
        (as-val (bit-or t (raw 1)))
        (let* ((a (->bignum x))
               (r (make-bignum (+ (bignum-limbs a) (quotient k 64) 1) (< x 0))))
          (shift-magnitude-left! r a k)
          (normalize r)))))

;; x >> k rounding down, k a positive fixnum; for a negative x, ~(~x >> k)
(define (shift-right x k)
  (declare unsafe)
  (cond
   ((fixnum? x)
    (make-fixnum (raw>> (fixnum-value x)
                        (if (raw<u (fixnum-value k) (raw 64)) (fixnum-value k) (raw 63)))))
   ((< x 0) (bitwise-not (shift-right (bitwise-not x) k)))
   (else
    (let ((n (- (bignum-limbs x) (quotient k 64))))
      (if (<= n 0)
          0
          (let ((r (make-bignum n #f)))
            (shift-magnitude-right! r x k)
            (normalize r)))))))

(define (check-shift x k who)
  (declare inline (result void))
  (check-exact-integer x who)
  (unless (fixnum? k)
    (argument-error who 'fixnum k)))

(define (shift-integer x k)
  (cond
   ((> k 0) (shift-left x k))
   ((< k 0) (shift-right x (- 0 k)))
   (else x)))

(define (bitwise-arithmetic-shift x k)
  (declare public)
  (check-shift x k bitwise-arithmetic-shift)
  (shift-integer x k))

(define (bitwise-arithmetic-shift-left x k)
  (declare unsafe public)
  (check-shift x k bitwise-arithmetic-shift-left)
  (when (< k 0)
    (argument-error bitwise-arithmetic-shift-left 'nonnegative-exact-integer k))
  (shift-left x k))

(define (bitwise-arithmetic-shift-right x k)
  (declare unsafe public)
  (check-shift x k bitwise-arithmetic-shift-right)
  (when (< k 0)
    (argument-error bitwise-arithmetic-shift-right 'nonnegative-exact-integer k))
  (shift-integer x (- 0 k)))

;; x is nonzero
(define (trailing-zeros (raw x))
  (declare unsafe noalloc (result raw))
  (if (raw= (bit-and x (raw 1)) (raw 0))
      (raw+ (trailing-zeros (raw>>u x (raw 1))) (raw 1))
      (raw 0)))

;; The index of the lowest set bit, -1 for 0; -x has the lowest set bit of x
(define (bitwise-first-bit-set x)
  (declare unsafe public)
  (check-exact-integer x bitwise-first-bit-set)
  (cond
   ((eq? x 0) -1)
   ((fixnum? x) (make-fixnum (trailing-zeros (fixnum-value x))))
   (else
    (let limb-next ((i 0))
      (let ((m (load (as-raw x) (raw+ (fixnum-value i) (raw 3)))))
        (if (raw= m (raw 0))
            (limb-next (+ i 1))
            (make-fixnum (raw+ (raw<< (fixnum-value i) (raw 6)) (trailing-zeros m)))))))))

;; Bit k of the bignum x in two's complement; the borrow of |x| - 1 into limb k / 64 is 1 when
;; the limbs below it are all zero, and the lowest nonzero limb is below the length
(define (bignum-bit-set? x (raw k))
  (declare unsafe noalloc)
  (let* ((p (as-raw x))
         (i (raw>>u k (raw 6))))
    (let limb-next ((j (raw 0)))
      (if (and (raw<u j i) (raw= (limb p (load p (raw 1)) j) (raw 0)))
          (limb-next (raw+ j (raw 1)))
          (raw= (bit-and (raw>>u (twos-limb p i (if (raw= j i) (raw 1) (raw 0))) (bit-and k (raw 63)))
                         (raw 1))
                (raw 1))))))

(define (logbit? k n)
  (declare unsafe public)
  (unless (and (fixnum? k) (>= k 0))
    (argument-error logbit? 'bit-index k))
  (check-exact-integer n logbit?)
  (if (fixnum? n)
      (raw= (bit-and (raw>> (fixnum-value n)
                            (if (raw<u (fixnum-value k) (raw 64)) (fixnum-value k) (raw 63)))
                     (raw 1))
            (raw 1))
      (bignum-bit-set? n (fixnum-value k))))

;; Chez's names, which the compiler uses: inline 2-operand defs like the bitwise ones
(define (logand x y)
  (declare inline public)
  (bitwise-and x y))

(define (logior x y)
  (declare inline public)
  (bitwise-ior x y))

(define (logor x y)
  (declare inline public)
  (bitwise-ior x y))

(define (logxor x y)
  (declare inline public)
  (bitwise-xor x y))

(define (variadic-logand . xs)
  (fold-left (lambda (a b) (bitwise-and a b)) -1 xs))

(define (variadic-logior . xs)
  (fold-left (lambda (a b) (bitwise-ior a b)) 0 xs))

(define (variadic-logor . xs)
  (fold-left (lambda (a b) (bitwise-ior a b)) 0 xs))

(define (variadic-logxor . xs)
  (fold-left (lambda (a b) (bitwise-xor a b)) 0 xs))

(define (lognot x)
  (declare public)
  (bitwise-not x))

(define (ash x k)
  (declare public)
  (bitwise-arithmetic-shift x k))
