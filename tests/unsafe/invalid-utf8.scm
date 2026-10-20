;; Invalid UTF-8 reads as U+FFFD per bad byte: a lone lead byte, overlong, a surrogate, past
;; U+10FFFF, a lone continuation byte, a bad continuation byte, then a truncated sequence
(define-data bad 3)

(define (write-bad bytes)
  (declare unsafe)
  (let byte-next ((l bytes)
                  (i 0))
    (when (pair? l)
      (store8 bad (fixnum-value i) (fixnum-value (car l)))
      (byte-next (cdr l) (+ i 1))))
  (let* ((n (length bytes))
         (fd (syscall 2 "bad.txt" (raw 577) (raw 438))))
    (syscall 1 fd bad (fixnum-value n))
    (syscall 3 fd))
  0)

(write-bad '(#x61 #xFF #x62 #xC0 #x80 #xED #xA0 #x80 #xF4 #x90 #x80 #x80
                  #xC3 #xA9 #xF0 #x9F #x98 #x80 #x80 #xC3 #x41 #xC3))

(define (codes p)
  (let ((c (read-char p)))
    (if (eof-object? c)
        '()
        (cons (char->integer c) (codes p)))))
(call-with-input-file "bad.txt" codes)
