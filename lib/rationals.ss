;; Exact rationals: ratnums [ratnum-type, numerator, denominator], both integers, normalized:
;; the denominator above 1, the gcd 1, so a ratnum is never an integer

(define (ratnum? x)
  (declare unsafe inline)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) (ratnum-type))))

(define (make-ratnum n d)
  (declare unsafe inline)
  (let ((r (alloc (raw 3))))
    (store r (raw 0) (ratnum-type))
    (store r (raw 1) n)
    (store r (raw 2) d)
    (as-val r)))

;; The numerator and denominator of a number, unchecked
(define (number-numerator x)
  (declare unsafe inline)
  (if (ratnum? x) (as-val (load (as-raw x) (raw 1))) x))

(define (number-denominator x)
  (declare unsafe inline)
  (if (ratnum? x) (as-val (load (as-raw x) (raw 2))) 1))

;; Nonnegative integers
(define (gcd-magnitudes a b)
  (if (eq? b 0)
      a
      (gcd-magnitudes b (modulo a b))))

;; n / d in lowest terms, d a nonzero integer
(define (make-rational n d)
  (let* ((g (gcd-magnitudes (abs n) (abs d)))
         (n (if (< d 0) (- 0 (quotient n g)) (quotient n g)))
         (d (abs (quotient d g))))
    (if (eq? d 1) n (make-ratnum n d))))

;; x + y, or x - y when negate-y? is true, numbers
(define (add-rationals x y negate-y?)
  (let ((a (* (number-numerator x) (number-denominator y)))
        (b (* (number-numerator y) (number-denominator x))))
    (make-rational (if negate-y? (- a b) (+ a b))
                   (* (number-denominator x) (number-denominator y)))))

(define (multiply-rationals x y)
  (make-rational (* (number-numerator x) (number-numerator y))
                 (* (number-denominator x) (number-denominator y))))

;; y is nonzero
(define (divide-rationals x y)
  (make-rational (* (number-numerator x) (number-denominator y))
                 (* (number-denominator x) (number-numerator y))))

;; -1, 0 or 1, numbers: the denominators are positive, so cross products compare alike
(define (compare-rationals x y)
  (let ((a (* (number-numerator x) (number-denominator y)))
        (b (* (number-numerator y) (number-denominator x))))
    (cond
     ((< a b) -1)
     ((= a b) 0)
     (else 1))))

;; compare-integers, which says 2 for a ratnum operand, else compare-rationals
(define (compare-numbers x y (raw msg))
  (declare unsafe inline (result raw))
  (let ((c (compare-integers x y msg)))
    (if (raw= c (raw 2))
        (fixnum-value (compare-rationals x y))
        c)))

(define (/ x y)
  (declare unsafe public)
  (check-number x "`/` expected a number")
  (check-number y "`/` expected a number")
  (when (eq? y 0)
    (scheme-error "`/` expected a nonzero divisor" y))
  (if (and (integer? x) (integer? y))
      (make-rational x y)
      (divide-rationals x y)))

(define (variadic-/ x . ys)
  (if (null? ys)
      (/ 1 x)
      (fold-left (lambda (a b) (/ a b)) x ys)))

(define (numerator x)
  (declare unsafe public)
  (check-number x "`numerator` expected a number")
  (number-numerator x))

(define (denominator x)
  (declare unsafe public)
  (check-number x "`denominator` expected a number")
  (number-denominator x))

;; Integers, m nonzero: the quotient rounded down
(define (floor-quotient-integers n m)
  (let ((q (quotient n m)))
    (if (and (not (eq? (modulo n m) 0)) (not (eq? (< n 0) (< m 0))))
        (- q 1)
        q)))

;; The denominator of a ratnum is above 1, so its quotient is never exact
(define (floor x)
  (declare unsafe public)
  (check-number x "`floor` expected a number")
  (if (ratnum? x)
      (floor-quotient-integers (number-numerator x) (number-denominator x))
      x))

(define (ceiling x)
  (declare unsafe public)
  (check-number x "`ceiling` expected a number")
  (if (ratnum? x)
      (+ (floor-quotient-integers (number-numerator x) (number-denominator x)) 1)
      x))

(define (truncate x)
  (declare unsafe public)
  (check-number x "`truncate` expected a number")
  (if (ratnum? x)
      (quotient (number-numerator x) (number-denominator x))
      x))

;; floor(x + 1/2) = floor((2n + d) / 2d)
(define (floor-half-up x)
  (let ((d (number-denominator x)))
    (floor-quotient-integers (+ (* 2 (number-numerator x)) d) (* 2 d))))

;; Ties, a denominator of 2, go to the even neighbor
(define (round x)
  (declare unsafe public)
  (check-number x "`round` expected a number")
  (cond
   ((not (ratnum? x)) x)
   ((eq? (number-denominator x) 2)
    (let ((f (floor-quotient-integers (number-numerator x) 2)))
      (if (even? f) f (+ f 1))))
   (else (floor-half-up x))))

;; Ties go up, toward positive infinity
(define (round-half-up x)
  (declare unsafe public)
  (check-number x "`round-half-up` expected a number")
  (if (ratnum? x) (floor-half-up x) x))

(define (gcd . xs)
  (declare unsafe public)
  (fold-left (lambda (g x)
               (check-integer x "`gcd` expected an integer")
               (gcd-magnitudes g (abs x)))
             0 xs))

(define (lcm . xs)
  (declare unsafe public)
  (fold-left (lambda (l x)
               (check-integer x "`lcm` expected an integer")
               (if (or (eq? l 0) (eq? x 0))
                   0
                   (abs (* (quotient l (gcd-magnitudes l (abs x))) x))))
             1 xs))
