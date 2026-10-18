(define (check-integer x (raw msg))
  (declare unsafe inline (result void))
  (unless (or (fixnum? x) (bignum? x))
    (scheme-error msg x)))

(define (integer? x)
  (declare unsafe inline public)
  (or (fixnum? x) (bignum? x)))

;; Exact integers only, until rationals
(define (number? x)
  (declare public)
  (integer? x))

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
        (add-integers x y))))

(define (- x y)
  (declare unsafe inline public)
  (let ((d (raw- (as-raw x) (as-raw y))))
    (if (and (fixnum? x) (fixnum? y) (not (difference-overflow? (as-raw x) (as-raw y) d)))
        (as-val (raw+ d (raw 1)))
        (subtract-integers x y))))

;; (n << 3) | 1 times m is (n * m) << 3 plus m, so subtract the tag first
(define (* x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y) (small-fixnum? x) (small-fixnum? y))
      (as-val (raw+ (raw* (raw- (as-raw x) (raw 1)) (fixnum-value y)) (raw 1)))
      (multiply-integers x y)))

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
      (raw= (compare-integers x y "`=` expected an integer") (raw 0))))

(define (< x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw< (as-raw x) (as-raw y))
      (raw< (compare-integers x y "`<` expected an integer") (raw 0))))

(define (<= x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw<= (as-raw x) (as-raw y))
      (raw<= (compare-integers x y "`<=` expected an integer") (raw 0))))

(define (> x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw> (as-raw x) (as-raw y))
      (raw> (compare-integers x y "`>` expected an integer") (raw 0))))

(define (>= x y)
  (declare unsafe inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw>= (as-raw x) (as-raw y))
      (raw>= (compare-integers x y "`>=` expected an integer") (raw 0))))

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

;; By squaring; no rationals, so the exponent is nonnegative
(define (expt b n)
  (declare unsafe public)
  (cond
   ((< n 0) (scheme-error "`expt` expected a nonnegative exponent" n))
   ((zero? n) 1)
   ((even? n) (let ((h (expt b (quotient n 2))))
                (* h h)))
   (else (* b (expt b (- n 1))))))

(define (fibo n)
  (declare public)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (else (+ (fibo (- n 1)) (fibo (- n 2))))))

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
