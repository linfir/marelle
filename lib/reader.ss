;; The reader: read and string->number, in Chez's syntax and wording, exact numbers only

(define (read port)
  (declare unsafe public)
  (check-port port input-flag read)
  (read-datum port))

;; The EOF object at the end
(define (read-datum port)
  (let ((x (read-item port)))
    (if (eq? x (end-marker))
        (eof-object)
        (required x "datum"))))

(define (variadic-read . port)
  (declare unsafe)
  (read (optional-port port (raw 0) read 0)))

(define (read-error fmt args)
  (declare (result never))
  (library-errorf read fmt args))

;; The port is checked by read
(define (next-char port)
  (declare unsafe inline)
  (port-read-char port (raw 0)))

(define (peek port)
  (declare unsafe inline)
  (port-read-char port (raw 1)))

;; What read-item gives besides a datum, each its own object
(define (end-marker)
  (declare inline)
  '#(end))

(define (close-paren-marker)
  (declare inline)
  '#(close-paren))

(define (close-bracket-marker)
  (declare inline)
  '#(close-bracket))

(define (dot-marker)
  (declare inline)
  '#(dot))

(define (marker? x)
  (or (eq? x (end-marker)) (eq? x (close-paren-marker)) (eq? x (close-bracket-marker))
      (eq? x (dot-marker))))

(define (delimiter? c)
  (or (eof-object? c)
      (char-whitespace? c)
      (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\, #\#))))

;; Whitespace and line comments
(define (skip-atmosphere port)
  (let ((c (peek port)))
    (cond
     ((eof-object? c))
     ((char-whitespace? c)
      (next-char port)
      (skip-atmosphere port))
     ((eq? c #\;)
      (let skip ()
        (let ((c (next-char port)))
          (unless (or (eof-object? c) (eq? c #\newline))
            (skip))))
      (skip-atmosphere port)))))

;; The next datum, or a marker
(define (read-item port)
  (skip-atmosphere port)
  (let ((c (next-char port)))
    (cond
     ((eof-object? c) (end-marker))
     ((eq? c #\() (read-list port #\) "list"))
     ((eq? c #\[) (read-list port #\] "list"))
     ((eq? c #\)) (close-paren-marker))
     ((eq? c #\]) (close-bracket-marker))
     ((eq? c #\') (list 'quote (read-required port "quote")))
     ((eq? c #\`) (list 'quasiquote (read-required port "quasiquote")))
     ((eq? c #\,)
      (if (eq? (peek port) #\@)
          (begin
            (next-char port)
            (list 'unquote-splicing (read-required port "unquote-splicing")))
          (list 'unquote (read-required port "unquote"))))
     ((eq? c #\") (read-string-literal port))
     ((eq? c #\#) (read-sharp port))
     ((memv c '(#\{ #\})) (string->symbol (string c)))
     (else (read-atom port c)))))

;; A datum, after a prefix read while reading what
(define (read-required port what)
  (required (read-item port) what))

;; x, an item that must be a datum, read while reading what
(define (required x what)
  (cond
   ((eq? x (end-marker)) (read-error "unexpected end-of-file reading ~a" (list what)))
   ((eq? x (close-paren-marker)) (read-error "unexpected close parenthesis" '()))
   ((eq? x (close-bracket-marker)) (read-error "unexpected close bracket" '()))
   ((eq? x (dot-marker)) (read-error "unexpected dot (.)" '()))
   (else x)))

;; The items up to close, ) or ], what "list", with a dotted tail, or "vector"
(define (read-list port close what)
  (define (closes? x)
    (eq? x (if (eq? close #\)) (close-paren-marker) (close-bracket-marker))))

  (define (check-close x)
    (cond
     ((closes? x))
     ((eq? x (end-marker)) (read-error "unexpected end-of-file reading ~a" (list what)))
     ((equal? what "vector") (read-error "unexpected close bracket" '()))
     ((eq? close #\)) (read-error "parenthesized list terminated by bracket" '()))
     (else (read-error "bracketed list terminated by parenthesis" '()))))

  (let item-next ((items '()))
    (let ((x (read-item port)))
      (cond
       ((not (marker? x)) (item-next (cons x items)))
       ((eq? x (dot-marker))
        (when (or (null? items) (equal? what "vector"))
          (read-error "unexpected dot (.)" '()))
        (let ((tail (read-item port)))
          (cond
           ((eq? tail (end-marker)) (read-error "unexpected end-of-file reading list" '()))
           ((eq? tail (dot-marker)) (read-error "unexpected dot (.)" '()))
           ((marker? tail) (read-error "expected one item after dot (.)" '())))
          (let ((end (read-item port)))
            (cond
             ((not (marker? end)) (read-error "more than one item found after dot (.)" '()))
             ((eq? end (dot-marker)) (read-error "unexpected dot" '()))
             (else (check-close end)))
            (append (reverse items) tail))))
       (else
        (check-close x)
        (reverse items))))))

(define (read-sharp port)
  (let ((c (next-char port)))
    (cond
     ((eof-object? c) (read-error "unexpected end-of-file reading # prefix" '()))
     ((eq? c #\() (list->vector (read-list port #\) "vector")))
     ((eq? c #\\) (read-char-literal port))
     ((eq? c #\|)
      (skip-block-comment port)
      (read-item port))
     ((eq? c #\;)
      (read-required port "s-expression comment")
      (read-item port))
     ((memv c '(#\t #\f #\T #\F)) (read-boolean port c))
     ((memv c '(#\x #\X #\b #\B #\o #\O #\d #\D #\e #\E #\i #\I))
      (let ((chars (cons #\# (cons c (read-token port #t)))))
        (when (null? (cddr chars))
          (read-error "unexpected end-of-file reading number" '()))
        (let ((n (parse-number chars 10)))
          (cond
           ((number? n) n)
           (else (number-error n chars))))))
     ((eq? c #\!) (read-directive port))
     (else (read-error "invalid sharp-sign prefix #~a" (list c))))))

;; The chars up to a delimiter, but # when sharp?, for the prefixes of a number
(define (read-token port sharp?)
  (let char-next ((chars '()))
    (let ((c (peek port)))
      (if (and (delimiter? c) (not (and sharp? (eq? c #\#))))
          (reverse chars)
          (char-next (cons (next-char port) chars))))))

;; n, what parse-number gave for the token of chars: #f, inexact or zero
(define (number-error n chars)
  (declare (result never))
  (case n
    ((inexact) (read-error "inexact numbers are not supported ~a" (list (list->string chars))))
    ((zero) (read-error "cannot represent ~a" (list (list->string chars))))
    (else (read-error "invalid number syntax ~a" (list (list->string chars))))))

(define (skip-block-comment port)
  (let char-next ((depth 1))
    (let ((c (next-char port)))
      (cond
       ((eof-object? c) (read-error "unexpected end-of-file reading block comment" '()))
       ((and (eq? c #\|) (eq? (peek port) #\#))
        (next-char port)
        (unless (= depth 1)
          (char-next (- depth 1))))
       ((and (eq? c #\#) (eq? (peek port) #\|))
        (next-char port)
        (char-next (+ depth 1)))
       (else (char-next depth))))))

;; #t, #true, #f or #false, in any case
(define (read-boolean port c)
  (let* ((chars (cons c (read-alphabetic port)))
         (name (list->string (map char-downcase chars))))
    (unless (delimiter? (peek port))
      (read-error "invalid delimiter ~a for boolean" (list (peek port))))
    (cond
     ((member name '("t" "true")) #t)
     ((member name '("f" "false")) #f)
     ((and (eof-object? (peek port)) (or (prefix? name "true") (prefix? name "false")))
      (read-error "unexpected end-of-file reading boolean" '()))
     (else (read-error "invalid boolean #~a" (list (list->string chars)))))))

(define (prefix? s t)
  (and (< (string-length s) (string-length t))
       (string=? s (substring t 0 (string-length s)))))

(define (read-alphabetic port)
  (let char-next ((chars '()))
    (let ((c (peek port)))
      (if (and (char? c) (char-alphabetic? c))
          (char-next (cons (next-char port) chars))
          (reverse chars)))))

;; #!eof, the EOF object, and #!no-fold-case, the default, which reads the next datum
(define (read-directive port)
  (let ((chars (read-token port #f)))
    (when (and (null? chars) (eof-object? (peek port)))
      (read-error "unexpected end-of-file reading #! syntax" '()))
    (let ((name (list->string chars)))
      (cond
       ((equal? name "eof") (eof-object))
       ((equal? name "no-fold-case") (read-item port))
       (else (read-error "invalid syntax #!~a" (list name)))))))

(define (char-names)
  (declare inline)
  '(("nul" . 0) ("alarm" . 7) ("backspace" . 8) ("tab" . 9) ("newline" . 10) ("linefeed" . 10)
    ("vtab" . 11) ("page" . 12) ("return" . 13) ("esc" . 27) ("space" . 32) ("delete" . 127)
    ("rubout" . 127)))

;; After #\: one char, a name, or x and hex digits
(define (read-char-literal port)
  (let* ((c (next-char port))
         (next (peek port)))
    (cond
     ((eof-object? c) (read-error "unexpected end-of-file reading character" '()))
     ((delimiter? next) c)
     ((and (memv c '(#\x #\X)) (digit-value next 16))
      (let* ((chars (read-token port #f))
             (n (parse-digits chars 16)))
        (unless n
          (read-error "invalid delimiter ~a for character"
                      (list (find (lambda (c) (not (digit-value c 16))) chars))))
        (unless (scalar-value? n)
          (read-error "invalid hex character escape x~a" (list (list->string chars))))
        (integer->char n)))
     ((and (char-alphabetic? c) (char-alphabetic? next) (not (memv c '(#\x #\X))))
      (let* ((name (list->string (cons c (read-token port #f))))
             (entry (assoc name (char-names))))
        (unless entry
          (read-error "invalid character name #\\~a" (list name)))
        (integer->char (cdr entry))))
     (else (read-error "invalid delimiter ~a for character" (list next))))))

(define (scalar-value? n)
  (and (<= 0 n 1114111) (not (<= 55296 n 57343))))

(define (read-string-literal port)
  (let char-next ((chars '()))
    (let ((c (next-char port)))
      (cond
       ((eof-object? c) (read-error "unexpected end-of-file reading string" '()))
       ((eq? c #\") (list->string (reverse chars)))
       ((eq? c #\\) (char-next (read-string-escape port chars)))
       (else (char-next (cons c chars)))))))

;; chars with the escape after a \ in a string
(define (read-string-escape port chars)
  (let ((c (next-char port)))
    (case c
      ((#\n) (cons #\newline chars))
      ((#\t) (cons #\tab chars))
      ((#\r) (cons #\return chars))
      ((#\a) (cons (integer->char 7) chars))
      ((#\b) (cons (integer->char 8) chars))
      ((#\v) (cons (integer->char 11) chars))
      ((#\f) (cons (integer->char 12) chars))
      ((#\0) (cons (integer->char 0) chars))
      ((#\\ #\" #\|) (cons c chars))
      ((#\x #\X) (cons (read-hex-escape port "string") chars))
      (else
       (cond
        ((eof-object? c) (read-error "unexpected end-of-file reading string" '()))
        ((or (eq? c #\newline) (memv c '(#\space #\tab)))
         (skip-line-continuation port c)
         chars)
        (else (read-error "invalid string character \\~a" (list c))))))))

;; \<intraline whitespace>*<newline><intraline whitespace>*, after its first char c
(define (skip-line-continuation port c)
  (define (skip-blanks)
    (when (memv (peek port) '(#\space #\tab))
      (next-char port)
      (skip-blanks)))

  (unless (eq? c #\newline)
    (skip-blanks)
    (unless (eq? (next-char port) #\newline)
      (read-error "invalid string character \\~a" (list c))))
  (skip-blanks))

;; The char of hex digits up to a ;, after \x in a string or a symbol, what
(define (read-hex-escape port what)
  (let char-next ((n 0)
                  (digits 0))
    (let ((c (next-char port)))
      (cond
       ((eof-object? c) (read-error "unexpected end-of-file reading ~a" (list what)))
       ((and (eq? c #\;) (> digits 0))
        (unless (scalar-value? n)
          (read-error "invalid code point value ~a in ~a hex escape" (list n what)))
        (integer->char n))
       ((digit-value c 16) => (lambda (d) (char-next (+ (* n 16) d) (+ digits 1))))
       (else (read-error "invalid character ~a in ~a hex escape" (list c what)))))))

;; A number, a symbol or the dot, from c on: |...| verbatim and \ escapes make it a symbol
(define (read-atom port c)
  (let char-next ((c c)
                  (chars '())
                  (symbol? #f))
    (cond
     ((eq? c #\|)
      (let bar-next ((chars chars))
        (let ((c (next-char port)))
          (cond
           ((eof-object? c) (read-error "unexpected end-of-file reading symbol" '()))
           ((eq? c #\|) (char-next (next-atom-char port) chars #t))
           (else (bar-next (cons c chars)))))))
     ((eq? c #\\)
      (let* ((e (next-char port))
             (e (cond
                 ((eof-object? e) (read-error "unexpected end-of-file reading symbol" '()))
                 ((memv e '(#\x #\X)) (read-hex-escape port "symbol"))
                 (else e))))
        (char-next (next-atom-char port) (cons e chars) #t)))
     ((not c)
      (let ((chars (reverse chars)))
        (cond
         (symbol? (string->symbol (list->string chars)))
         ((equal? chars '(#\.)) (dot-marker))
         (else
          (let ((n (parse-number chars 10)))
            (cond
             ((number? n) n)
             ((memq n '(inexact zero)) (number-error n chars))
             (else (string->symbol (list->string chars)))))))))
     (else (char-next (next-atom-char port) (cons c chars) symbol?)))))

;; The next char of an atom, else #f at a delimiter
(define (next-atom-char port)
  (if (delimiter? (peek port))
      #f
      (next-char port)))

;; Numbers: string->number with an optional radix, 2 to 36 like Chez
(define (string->number s)
  (declare public)
  (variadic-string->number s 10))

(define (variadic-string->number s . radix)
  (check-string s string->number)
  (unless (or (null? radix) (null? (cdr radix)))
    (argument-count-error string->number (+ 1 (length radix))))
  (let ((radix (if (null? radix) 10 (car radix))))
    (unless (and (fixnum? radix) (<= 2 radix 36))
      (argument-error string->number 'radix radix))
    (let ((n (parse-number (string->list s) radix)))
      (and (number? n) n))))

;; The digit c in radix, else #f
(define (digit-value c radix)
  (let* ((n (and (char? c) (char->integer (char-downcase c))))
         (d (cond
             ((not n) #f)
             ((<= 48 n 57) (- n 48))
             ((<= 97 n 122) (- n 87))
             (else #f))))
    (and d (< d radix) d)))

;; The value of the digits chars, else #f; none is #f
(define (parse-digits chars radix)
  (and (pair? chars)
       (let digit-next ((chars chars)
                        (n 0))
         (cond
          ((null? chars) n)
          ((digit-value (car chars) radix) => (lambda (d) (digit-next (cdr chars) (+ (* n radix) d))))
          (else #f)))))

;; The longest run of digits at the start of chars: its value, its length and the rest
(define (digit-run chars radix)
  (let digit-next ((chars chars)
                   (n 0)
                   (k 0))
    (let ((d (and (pair? chars) (digit-value (car chars) radix))))
      (if d
          (digit-next (cdr chars) (+ (* n radix) d) (+ k 1))
          (values n k chars)))))

;; The number of chars, its #x #b #o #d #e #i prefixes first, else #f; inexact for a valid
;; inexact number and zero for a zero denominator
(define (parse-number chars radix)
  (let prefix-next ((chars chars)
                    (radix radix)
                    (radix? #f)
                    (exactness #f))
    (if (and (pair? chars) (eq? (car chars) #\#) (pair? (cdr chars)))
        (let ((p (char-downcase (cadr chars))))
          (cond
           ((and (not radix?) (assv p '((#\x . 16) (#\b . 2) (#\o . 8) (#\d . 10))))
            => (lambda (r) (prefix-next (cddr chars) (cdr r) #t exactness)))
           ((and (not exactness) (memv p '(#\e #\i)))
            (prefix-next (cddr chars) radix radix? p))
           (else #f)))
        (let* ((sign (and (pair? chars) (memv (car chars) '(#\+ #\-)) (car chars)))
               (n (parse-real (if sign (cdr chars) chars) radix (eq? exactness #\e))))
          (cond
           ((not (number? n)) n)
           ((eq? exactness #\i) 'inexact)
           ((eq? sign #\-) (- 0 n))
           (else n))))))

;; An unsigned integer, ratio, or decimal in radix 10, exact when exact?
(define (parse-real chars radix exact?)
  (let-values (((n k rest) (digit-run chars radix)))
    (cond
     ((and (null? rest) (> k 0)) n)
     ((and (pair? rest) (eq? (car rest) #\/) (> k 0))
      (let ((d (parse-digits (cdr rest) radix)))
        (cond
         ((not d) #f)
         ((eq? d 0) 'zero)
         (else (/ n d)))))
     ((= radix 10) (parse-decimal n k rest exact?))
     (else #f))))

;; n of k digits, then [.digits][e[sign]digits], at least one digit before the exponent
(define (parse-decimal n k chars exact?)
  (let-values (((f j rest) (if (and (pair? chars) (eq? (car chars) #\.))
                               (digit-run (cdr chars) 10)
                               (values 0 0 chars))))
    (let ((dot? (and (pair? chars) (eq? (car chars) #\.))))
      (cond
       ((= (+ k j) 0) #f)
       ((and (not dot?) (null? rest)) n)
       (else
        (let ((e (cond
                  ((null? rest) 0)
                  ((memv (car rest) '(#\e #\E)) (parse-exponent (cdr rest)))
                  (else #f))))
          (cond
           ((not e) #f)
           ((not exact?) 'inexact)
           (else (* (+ n (/ f (expt 10 j))) (expt 10 e))))))))))

(define (parse-exponent chars)
  (let* ((sign (and (pair? chars) (memv (car chars) '(#\+ #\-)) (car chars)))
         (e (parse-digits (if sign (cdr chars) chars) 10)))
    (and e (if (eq? sign #\-) (- 0 e) e))))
