(define (check-integer x (raw msg))
  (declare unsafe inline (result void))
  (unless (or (fixnum? x) (bignum? x))
    (scheme-error msg x)))

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

(define (check-number x (raw msg))
  (declare unsafe inline (result void))
  (unless (number? x)
    (scheme-error msg x)))

(define (exact? x)
  (declare unsafe public)
  (check-number x "`exact?` expected a number")
  #t)

(define (inexact? x)
  (declare unsafe public)
  (check-number x "`inexact?` expected a number")
  #f)

(define (exact x)
  (declare unsafe public)
  (check-number x "`exact` expected a number")
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
(define (+ x y)
  (declare unsafe inline public)
  (let ((s (raw- (raw+ (as-raw x) (as-raw y)) (raw 1))))
    (if (and (fixnum? x) (fixnum? y) (not (sum-overflow? (as-raw x) (as-raw y) s)))
        (as-val s)
        (add-numbers x y))))

(define (- x y)
  (declare unsafe inline public)
  (let ((d (raw- (as-raw x) (as-raw y))))
    (if (and (fixnum? x) (fixnum? y) (not (difference-overflow? (as-raw x) (as-raw y) d)))
        (as-val (raw+ d (raw 1)))
        (subtract-numbers x y))))

;; (n << 3) | 1 times m is (n * m) << 3 plus m, so subtract the tag first
(define (* x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y) (small-fixnum? x) (small-fixnum? y))
      (as-val (raw+ (raw* (raw- (as-raw x) (raw 1)) (fixnum-value y)) (raw 1)))
      (multiply-numbers x y)))

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
(define (= x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw= (as-raw x) (as-raw y))
      (raw= (compare-numbers x y "`=` expected a number") (raw 0))))

(define (< x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw< (as-raw x) (as-raw y))
      (raw< (compare-numbers x y "`<` expected a number") (raw 0))))

(define (<= x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw<= (as-raw x) (as-raw y))
      (raw<= (compare-numbers x y "`<=` expected a number") (raw 0))))

(define (> x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw> (as-raw x) (as-raw y))
      (raw> (compare-numbers x y "`>` expected a number") (raw 0))))

(define (>= x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw>= (as-raw x) (as-raw y))
      (raw>= (compare-numbers x y "`>=` expected a number") (raw 0))))

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
  (check-integer n "`even?` expected an integer")
  (raw= (integer-bit-0 n) (raw 0)))

(define (odd? n)
  (declare unsafe public)
  (check-integer n "`odd?` expected an integer")
  (raw= (integer-bit-0 n) (raw 1)))

;; By squaring; a negative exponent gives the inverse
(define (expt b n)
  (declare unsafe public)
  (check-number b "`expt` expected a number")
  (check-integer n "`expt` expected an integer exponent")
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
  (declare unsafe public)
  (check-division n m "`floor/` expected an integer" "`floor/` expected a nonzero divisor")
  (values (floor-quotient-integers n m) (modulo n m)))

(define (floor-quotient n m)
  (declare unsafe public)
  (check-division n m "`floor-quotient` expected an integer"
                  "`floor-quotient` expected a nonzero divisor")
  (floor-quotient-integers n m))

(define (floor-remainder n m)
  (declare unsafe public)
  (check-division n m "`floor-remainder` expected an integer"
                  "`floor-remainder` expected a nonzero divisor")
  (modulo n m))

(define (truncate/ n m)
  (declare unsafe public)
  (check-division n m "`truncate/` expected an integer" "`truncate/` expected a nonzero divisor")
  (values (quotient n m) (remainder n m)))

(define (truncate-quotient n m)
  (declare unsafe public)
  (check-division n m "`truncate-quotient` expected an integer"
                  "`truncate-quotient` expected a nonzero divisor")
  (quotient n m))

(define (truncate-remainder n m)
  (declare unsafe public)
  (check-division n m "`truncate-remainder` expected an integer"
                  "`truncate-remainder` expected a nonzero divisor")
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
    (scheme-error "`exact-integer-sqrt` expected a nonnegative integer" n))
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

;; Any number of operands: the calls the inline defs do not take, unless folded by the
;; compiler, and the procedure values of +, *, - and the comparisons
(define (variadic-+ . xs)
  (fold-left (lambda (a b) (+ a b)) 0 xs))

(define (variadic-* . xs)
  (fold-left (lambda (a b) (* a b)) 1 xs))

(define (variadic-- x . xs)
  (if (null? xs)
      (- 0 x)
      (fold-left (lambda (a b) (- a b)) x xs)))

;; Every pair is compared, so every operand is checked; a lone x is compared with itself
(define (compare-all ok? x xs)
  (if (null? xs)
      (begin (ok? x x) #t)
      (let next ((x x)
                 (xs xs)
                 (result #t))
        (if (null? xs)
            result
            (next (car xs) (cdr xs) (and (ok? x (car xs)) result))))))

(define (variadic-= x . xs)
  (compare-all (lambda (a b) (= a b)) x xs))

(define (variadic-< x . xs)
  (compare-all (lambda (a b) (< a b)) x xs))

(define (variadic-> x . xs)
  (compare-all (lambda (a b) (> a b)) x xs))

(define (variadic-<= x . xs)
  (compare-all (lambda (a b) (<= a b)) x xs))

(define (variadic->= x . xs)
  (compare-all (lambda (a b) (>= a b)) x xs))

;; R6RS: q such that x = qy + m, 0 <= m < |y|; the truncated quotient, moved by one
;; when the remainder is negative
(define (div x y)
  (declare unsafe public)
  (check-division x y "`div` expected an integer" "`div` expected a nonzero divisor")
  (let ((q (quotient x y)))
    (cond
     ((>= (remainder x y) 0) q)
     ((> y 0) (- q 1))
     (else (+ q 1)))))

;; Bit k of n in two's complement
(define (logbit? k n)
  (declare unsafe public)
  (unless (and (fixnum? k) (>= k 0))
    (scheme-error "`logbit?` expected a nonnegative fixnum" k))
  (check-integer n "`logbit?` expected an integer")
  (if (fixnum? n)
      (raw= (bit-and (raw>> (fixnum-value n)
                            (if (raw<u (fixnum-value k) (raw 64)) (fixnum-value k) (raw 63)))
                     (raw 1))
            (raw 1))
      (odd? (shift-integer n (- 0 k)))))

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

(define (number->string x)
  (declare public)
  (variadic-number->string x 10))

;; Radixes 2 to 36, like Chez, uppercase
(define (variadic-number->string x . radix)
  (declare unsafe)
  (check-number x "`number->string` expected a number")
  (unless (and (pair? radix) (null? (cdr radix)))
    (scheme-error "`number->string` expected 1 or 2 arguments" radix))
  (unless (and (fixnum? (car radix)) (>= (car radix) 2) (<= (car radix) 36))
    (scheme-error "`number->string` expected a radix from 2 to 36" (car radix)))
  (let ((radix (car radix)))
    (list->string (signed-digits (number-numerator x) radix
                                 (if (ratnum? x)
                                     (cons #\/ (integer-digits (number-denominator x) radix '()))
                                     '())))))
