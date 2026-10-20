;; Characters: the immediate (code << 8) | 34
(define (char? x)
  (declare unsafe inline public)
  (raw= (bit-and (as-raw x) (raw 255)) (raw 34)))

(define (char-code c)
  (declare unsafe inline (result raw))
  (raw>> (as-raw c) (raw 8)))

(define (code->char (raw n))
  (declare unsafe inline)
  (as-val (bit-or (raw<< n (raw 8)) (raw 34))))

;; ((code << 8) | 34) >> 5 is (code << 3) | 1, the fixnum code
(define (char->integer x)
  (declare unsafe inline public)
  (unless (char? x)
    (argument-error char->integer 'char x))
  (as-val (raw>> (as-raw x) (raw 5))))

(define (integer->char x)
  (declare unsafe inline public)
  (unless (and (fixnum? x)
               (let ((n (raw>> (as-raw x) (raw 3))))
                 (and (raw<=u n (raw 1114111))
                      (not (and (raw>= n (raw 55296)) (raw<= n (raw 57343))))))) ; surrogates
    (argument-error integer->char 'scalar-value x))
  (as-val (bit-or (raw<< (as-raw x) (raw 5)) (raw 2))))

(define (check-chars x y who)
  (declare inline (result void))
  (unless (char? x)
    (argument-error who 'char x))
  (unless (char? y)
    (argument-error who 'char y)))

;; Comparing characters compares their codes
(define (char=? x y)
  (declare unsafe inline public)
  (check-chars x y char=?)
  (raw= (as-raw x) (as-raw y)))

(define (char<? x y)
  (declare unsafe inline public)
  (check-chars x y char<?)
  (raw< (as-raw x) (as-raw y)))

(define (char<=? x y)
  (declare unsafe inline public)
  (check-chars x y char<=?)
  (raw<= (as-raw x) (as-raw y)))

(define (char>? x y)
  (declare unsafe inline public)
  (check-chars x y char>?)
  (raw> (as-raw x) (as-raw y)))

(define (char>=? x y)
  (declare unsafe inline public)
  (check-chars x y char>=?)
  (raw>= (as-raw x) (as-raw y)))

(define (variadic-char=? x . xs)
  (compare-all (lambda (a b) (char=? a b)) x xs))

(define (variadic-char<? x . xs)
  (compare-all (lambda (a b) (char<? a b)) x xs))

(define (variadic-char<=? x . xs)
  (compare-all (lambda (a b) (char<=? a b)) x xs))

(define (variadic-char>? x . xs)
  (compare-all (lambda (a b) (char>? a b)) x xs))

(define (variadic-char>=? x . xs)
  (compare-all (lambda (a b) (char>=? a b)) x xs))

;; Characters: ASCII only
(define (char-upper-case? c)
  (declare public)
  (and (char<=? #\A c) (char<=? c #\Z)))

(define (char-lower-case? c)
  (declare public)
  (and (char<=? #\a c) (char<=? c #\z)))

(define (char-alphabetic? c)
  (declare public)
  (or (char-upper-case? c) (char-lower-case? c)))

(define (char-numeric? c)
  (declare public)
  (and (char<=? #\0 c) (char<=? c #\9)))

(define (char-whitespace? c)
  (declare public)
  (or (char=? c #\space)
      (and (char<=? #\tab c) (char<=? c #\return))))

(define (char-upcase c)
  (declare public)
  (if (char-lower-case? c)
      (integer->char (- (char->integer c) 32))
      c))

(define (char-downcase c)
  (declare public)
  (if (char-upper-case? c)
      (integer->char (+ (char->integer c) 32))
      c))
