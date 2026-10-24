(define (check-integer x who)
  (declare unsafe inline (result void))
  (unless (or (fixnum? x) (bignum? x))
    (argument-error who 'integer x)))

;; For the bitwise operations, whose errors say exact integer
(define (check-exact-integer x who)
  (declare unsafe inline (result void))
  (unless (or (fixnum? x) (bignum? x))
    (argument-error who 'exact-integer x)))

(define (integer? x)
  (declare unsafe inline public)
  (or (fixnum? x) (bignum? x)))

;; Exact integers and ratnums only, no flonums
(define (number? x)
  (declare unsafe inline public)
  (or (integer? x) (ratnum? x)))

(define (exact-integer? x)
  (declare public)
  (integer? x))

(define (rational? x)
  (declare public)
  (number? x))

(define (real? x)
  (declare public)
  (number? x))

(define (complex? x)
  (declare public)
  (number? x))

(define (check-number x who)
  (declare inline (result void))
  (unless (number? x)
    (argument-error who 'number x)))

;; Every number is real, but the errors say real number
(define (check-real x who)
  (declare inline (result void))
  (unless (number? x)
    (argument-error who 'real x)))

(define (exact? x)
  (declare public)
  (check-number x exact?)
  #t)

(define (inexact? x)
  (declare public)
  (check-number x inexact?)
  #f)

(define (exact x)
  (declare public)
  (check-number x exact)
  x)

(define (fixnum-value x)
  (declare unsafe inline (result raw))
  (raw>> (as-raw x) (raw 3)))

(define (make-fixnum (raw n))
  (declare unsafe inline)
  (as-val (bit-or (raw<< n (raw 3)) (raw 1))))

;; A tagged fixnum is its value times 8, plus 1, so the fixnum range is the signed word range:
;; a tagged sum overflows when the 64-bit sum of the words does, and that one
;; when the operands have the same sign and the result the other
(define (sum-overflow? (raw a) (raw b) (raw s))
  (declare unsafe inline)
  (if (raw< a (raw 0))
      (and (raw< b (raw 0)) (raw>= s (raw 0)))
      (and (raw>= b (raw 0)) (raw< s (raw 0)))))

;; a - b: when the operands have different signs and the result has b's
(define (difference-overflow? (raw a) (raw b) (raw d))
  (declare unsafe inline)
  (if (raw< a (raw 0))
      (and (raw>= b (raw 0)) (raw>= d (raw 0)))
      (and (raw< b (raw 0)) (raw< d (raw 0)))))

;; The value of the fixnum x is in (-2^30, 2^30), so the product of two fits a fixnum:
;; its tagged word is in [-(2^30 - 1) * 8 + 1, (2^30 - 1) * 8 + 1]
(define (small-fixnum? x)
  (declare unsafe inline)
  (raw<u (raw+ (as-raw x) (raw 8589934583)) (raw 17179869169)))

;; Inline fast paths on tagged fixnums: (a << 3) | 1 plus (b << 3) | 1, minus 1,
;; is ((a + b) << 3) | 1
(define +
  (case-lambda
   (declare public)
   (() (declare inline) 0)
   ((x) (declare inline) (check-number x +) x)
   ((x y)
    (declare unsafe inline)
    (let ((s (raw- (raw+ (as-raw x) (as-raw y)) (raw 1))))
      (if (and (fixnum? x) (fixnum? y) (not (sum-overflow? (as-raw x) (as-raw y) s)))
          (as-val s)
          (add-numbers x y))))
   ((x y . rest) (declare inline) (apply + (+ x y) rest))))

(define -
  (case-lambda
   (declare public)
   ((x) (declare inline) (- 0 x))
   ((x y)
    (declare unsafe inline)
    (let ((d (raw- (as-raw x) (as-raw y))))
      (if (and (fixnum? x) (fixnum? y) (not (difference-overflow? (as-raw x) (as-raw y) d)))
          (as-val (raw+ d (raw 1)))
          (subtract-numbers x y))))
   ((x y . rest) (declare inline) (apply - (- x y) rest))))

;; (n << 3) | 1 times m is (n * m) << 3 plus m, so subtract the tag first
(define *
  (case-lambda
   (declare public)
   (() (declare inline) 1)
   ((x) (declare inline) (check-number x *) x)
   ((x y)
    (declare unsafe inline)
    (if (and (fixnum? x) (fixnum? y) (small-fixnum? x) (small-fixnum? y))
        (as-val (raw+ (raw* (raw- (as-raw x) (raw 1)) (fixnum-value y)) (raw 1)))
        (multiply-numbers x y)))
   ((x y . rest) (declare inline) (apply * (* x y) rest))))

;; Fixnums, the divisor neither 0 nor -1 (-2^60 / -1 is a bignum)
(define (quotient x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y) (raw>u (raw+ (fixnum-value y) (raw 1)) (raw 1)))
      (make-fixnum (raw/ (fixnum-value x) (fixnum-value y)))
      (quotient-integers x y)))

;; The truncated remainder plus y when their signs differ
(define (modulo x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y) (not (raw= (as-raw y) (as-raw 0))))
      (let ((r (raw% (fixnum-value x) (fixnum-value y))))
        (make-fixnum (if (or (raw= r (raw 0))
                             (raw= (raw>> r (raw 63)) (raw>> (fixnum-value y) (raw 63))))
                         r
                         (raw+ r (fixnum-value y)))))
      (modulo-integers x y)))

;; Comparing tagged fixnums compares their values
(define =
  (case-lambda
   (declare public)
   ((x) (declare inline) (check-number x =) #t)
   ((x y)
    (declare unsafe inline)
    (if (and (fixnum? x) (fixnum? y))
        (raw= (as-raw x) (as-raw y))
        (raw= (compare-numbers x y =) (raw 0))))
   ((x y . rest) (declare inline) (and (= x y) (apply = y rest)))))

(define <
  (case-lambda
   (declare public)
   ((x) (declare inline) (check-real x <) #t)
   ((x y)
    (declare unsafe inline)
    (if (and (fixnum? x) (fixnum? y))
        (raw< (as-raw x) (as-raw y))
        (raw< (compare-numbers x y <) (raw 0))))
   ((x y . rest) (declare inline) (and (< x y) (apply < y rest)))))

(define <=
  (case-lambda
   (declare public)
   ((x) (declare inline) (check-real x <=) #t)
   ((x y)
    (declare unsafe inline)
    (if (and (fixnum? x) (fixnum? y))
        (raw<= (as-raw x) (as-raw y))
        (raw<= (compare-numbers x y <=) (raw 0))))
   ((x y . rest) (declare inline) (and (<= x y) (apply <= y rest)))))

(define >
  (case-lambda
   (declare public)
   ((x) (declare inline) (check-real x >) #t)
   ((x y)
    (declare unsafe inline)
    (if (and (fixnum? x) (fixnum? y))
        (raw> (as-raw x) (as-raw y))
        (raw> (compare-numbers x y >) (raw 0))))
   ((x y . rest) (declare inline) (and (> x y) (apply > y rest)))))

(define >=
  (case-lambda
   (declare public)
   ((x) (declare inline) (check-real x >=) #t)
   ((x y)
    (declare unsafe inline)
    (if (and (fixnum? x) (fixnum? y))
        (raw>= (as-raw x) (as-raw y))
        (raw>= (compare-numbers x y >=) (raw 0))))
   ((x y . rest) (declare inline) (and (>= x y) (apply >= y rest)))))

(define (zero? n)
  (declare public)
  (= n 0))

(define (positive? n)
  (declare public)
  (> n 0))

(define (negative? n)
  (declare public)
  (< n 0))

(define (abs n)
  (declare public)
  (if (< n 0) (- 0 n) n))

;; A lone x is compared with itself, so it is checked
(define (max x . xs)
  (declare public)
  (fold-left (lambda (a b) (if (< a b) b a)) (if (< x x) x x) xs))

(define (min x . xs)
  (declare public)
  (fold-left (lambda (a b) (if (> a b) b a)) (if (< x x) x x) xs))

;; The modulo has n's sign, the remainder m's
(define (remainder m n)
  (declare public)
  (let ((r (modulo m n)))
    (if (or (zero? r) (eq? (< r 0) (< m 0)))
        r
        (- r n))))

(define (even? n)
  (declare unsafe public)
  (check-integer n even?)
  (raw= (integer-bit-0 n) (raw 0)))

(define (odd? n)
  (declare unsafe public)
  (check-integer n odd?)
  (raw= (integer-bit-0 n) (raw 1)))

;; By squaring; a negative exponent gives the inverse
(define (expt b n)
  (declare public)
  (check-number b expt)
  (check-number n expt)
  (check-integer n expt)
  (if (< n 0)
      (/ 1 (expt-by-squaring b (- 0 n)))
      (expt-by-squaring b n)))

(define (expt-by-squaring b n)
  (cond
   ((zero? n) 1)
   ((even? n) (let ((h (expt-by-squaring b (quotient n 2))))
                (* h h)))
   (else (* b (expt-by-squaring b (- n 1))))))

;; R7RS integer division: floor rounds the quotient down, truncate toward zero
(define (floor/ n m)
  (declare public)
  (check-division n m floor/)
  (values (floor-quotient-integers n m) (modulo n m)))

(define (floor-quotient n m)
  (declare public)
  (check-division n m floor-quotient)
  (floor-quotient-integers n m))

(define (floor-remainder n m)
  (declare public)
  (check-division n m floor-remainder)
  (modulo n m))

(define (truncate/ n m)
  (declare public)
  (check-division n m truncate/)
  (values (quotient n m) (remainder n m)))

(define (truncate-quotient n m)
  (declare public)
  (check-division n m truncate-quotient)
  (quotient n m))

(define (truncate-remainder n m)
  (declare public)
  (check-division n m truncate-remainder)
  (remainder n m))

;; The number of bits of the nonnegative integer n
(define (bit-length n)
  (declare unsafe)
  (cond
   ((eq? n 0) 0)
   ((fixnum? n) (make-fixnum (raw- (raw 64) (leading-zeros (fixnum-value n)))))
   (else
    (let ((k (load (as-raw n) (raw 1))))
      (make-fixnum (raw- (raw<< k (raw 6)) (leading-zeros (load (as-raw n) (raw+ k (raw 2))))))))))

;; s and n - s^2, s the integer square root: Newton's method from a power of two above it
(define (exact-integer-sqrt n)
  (declare unsafe public)
  (unless (and (integer? n) (>= n 0))
    (argument-error exact-integer-sqrt 'nonnegative-exact-integer n))
  (if (< n 2)
      (values n 0)
      (let next ((x (bitwise-arithmetic-shift-left 1 (quotient (+ (bit-length n) 1) 2))))
        (let ((y (quotient (+ x (quotient n x)) 2)))
          (if (>= y x)
              (values x (- n (* x x)))
              (next y))))))

(define (square x)
  (declare public)
  (* x x))

;; R6RS: q such that x = qy + m, 0 <= m < |y|; the truncated quotient, moved by one
;; when the remainder is negative
(define (div x y)
  (declare public)
  (check-division x y div)
  (let ((q (quotient x y)))
    (cond
     ((>= (remainder x y) 0) q)
     ((> y 0) (- q 1))
     (else (+ q 1)))))

(define (digit-char d)
  (integer->char (+ d (if (< d 10) 48 55))))

;; The digits of the nonnegative fixnum n consed onto acc, zero-padded to width
(define (fixnum-digits n radix width acc)
  (if (and (eq? n 0) (<= width 0))
      acc
      (fixnum-digits (quotient n radix) radix (- width 1) (cons (digit-char (modulo n radix)) acc))))

;; The largest power of radix that is a fixnum, and its exponent
(define (radix-chunk radix)
  (let next ((p radix)
             (k 1))
    (if (< (* p radix) 1152921504606846976)
        (next (* p radix) (+ k 1))
        (values p k))))

;; Those of the nonnegative integer n, a fixnum chunk at a time
(define (integer-digits n radix acc)
  (if (< n 1152921504606846976)
      (fixnum-digits n radix 1 acc)
      (let-values (((chunk k) (radix-chunk radix)))
        (let next ((n n)
                   (acc acc))
          (if (< n chunk)
              (fixnum-digits n radix 1 acc)
              (next (quotient n chunk) (fixnum-digits (modulo n chunk) radix k acc)))))))

(define (signed-digits n radix acc)
  (if (< n 0)
      (cons #\- (integer-digits (- 0 n) radix acc))
      (integer-digits n radix acc)))

;; Radixes 2 to 36, like Chez, uppercase
(define number->string
  (case-lambda
   (declare public)
   ((x) (declare inline) (number->string x 10))
   ((x radix)
    (declare unsafe)
    (check-number x number->string)
    (unless (and (fixnum? radix) (>= radix 2) (<= radix 36))
      (argument-error number->string 'radix radix))
    (list->string (signed-digits (number-numerator x) radix
                                 (if (ratnum? x)
                                     (cons #\/ (integer-digits (number-denominator x) radix '()))
                                     '()))))))
