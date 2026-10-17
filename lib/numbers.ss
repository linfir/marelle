(define (check-integer x (raw msg))
  (declare inline (result void))
  (unless (or (fixnum? x) (bignum? x))
    (scheme-error msg x)))

(define (fixnum-value x)
  (declare inline (result raw))
  (raw>> (as-raw x) (raw 3)))

(define (make-fixnum (raw n))
  (declare inline)
  (as-val (bit-or (raw<< n (raw 3)) (raw 1))))

;; A tagged fixnum is its value times 8, plus 1, so the fixnum range is the signed word range:
;; a tagged sum overflows when the 64-bit sum of the words does, and that one
;; when the operands have the same sign and the result the other
(define (sum-overflow? (raw a) (raw b) (raw s))
  (declare inline)
  (if (raw< a (raw 0))
      (and (raw< b (raw 0)) (raw>= s (raw 0)))
      (and (raw>= b (raw 0)) (raw< s (raw 0)))))

;; a - b: when the operands have different signs and the result has b's
(define (difference-overflow? (raw a) (raw b) (raw d))
  (declare inline)
  (if (raw< a (raw 0))
      (and (raw>= b (raw 0)) (raw>= d (raw 0)))
      (and (raw< b (raw 0)) (raw< d (raw 0)))))

;; The value of the fixnum x is in (-2^30, 2^30), so the product of two fits a fixnum:
;; its tagged word is in [-(2^30 - 1) * 8 + 1, (2^30 - 1) * 8 + 1]
(define (small-fixnum? x)
  (declare inline)
  (raw<u (raw+ (as-raw x) (raw 8589934583)) (raw 17179869169)))

;; Inline fast paths on tagged fixnums: (a << 3) | 1 plus (b << 3) | 1, minus 1,
;; is ((a + b) << 3) | 1
(define (+ x y)
  (declare inline public)
  (let ((s (raw- (raw+ (as-raw x) (as-raw y)) (raw 1))))
    (if (and (fixnum? x) (fixnum? y) (not (sum-overflow? (as-raw x) (as-raw y) s)))
        (as-val s)
        (add-integers x y))))

(define (- x y)
  (declare inline public)
  (let ((d (raw- (as-raw x) (as-raw y))))
    (if (and (fixnum? x) (fixnum? y) (not (difference-overflow? (as-raw x) (as-raw y) d)))
        (as-val (raw+ d (raw 1)))
        (subtract-integers x y))))

;; (n << 3) | 1 times m is (n * m) << 3 plus m, so subtract the tag first
(define (* x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y) (small-fixnum? x) (small-fixnum? y))
      (as-val (raw+ (raw* (raw- (as-raw x) (raw 1)) (fixnum-value y)) (raw 1)))
      (multiply-integers x y)))

;; Fixnums, the divisor neither 0 nor -1 (-2^60 / -1 is a bignum)
(define (quotient x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y) (raw>u (raw+ (fixnum-value y) (raw 1)) (raw 1)))
      (make-fixnum (raw/ (fixnum-value x) (fixnum-value y)))
      (quotient-integers x y)))

;; The truncated remainder plus y when their signs differ
(define (modulo x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y) (not (raw= (as-raw y) (as-raw 0))))
      (let ((r (raw% (fixnum-value x) (fixnum-value y))))
        (make-fixnum (if (or (raw= r (raw 0))
                             (raw= (raw>> r (raw 63)) (raw>> (fixnum-value y) (raw 63))))
                         r
                         (raw+ r (fixnum-value y)))))
      (modulo-integers x y)))

;; Comparing tagged fixnums compares their values
(define (= x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw= (as-raw x) (as-raw y))
      (raw= (compare-integers x y "`=` expected an integer") (raw 0))))

(define (< x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw< (as-raw x) (as-raw y))
      (raw< (compare-integers x y "`<` expected an integer") (raw 0))))

(define (<= x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw<= (as-raw x) (as-raw y))
      (raw<= (compare-integers x y "`<=` expected an integer") (raw 0))))

(define (> x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw> (as-raw x) (as-raw y))
      (raw> (compare-integers x y "`>` expected an integer") (raw 0))))

(define (>= x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw>= (as-raw x) (as-raw y))
      (raw>= (compare-integers x y "`>=` expected an integer") (raw 0))))

(define (zero? n)
  (declare public)
  (= n 0))

;; The modulo has n's sign, the remainder m's
(define (remainder m n)
  (declare public)
  (let ((r (modulo m n)))
    (if (or (zero? r) (eq? (< r 0) (< m 0)))
        r
        (- r n))))

(define (even? n)
  (declare public)
  (check-integer n "`even?` expected an integer")
  (raw= (integer-bit-0 n) (raw 0)))

(define (odd? n)
  (declare public)
  (check-integer n "`odd?` expected an integer")
  (raw= (integer-bit-0 n) (raw 1)))

;; By squaring; no rationals, so the exponent is nonnegative
(define (expt b n)
  (declare public)
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
