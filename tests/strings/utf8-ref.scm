;; string-ref on a string of 1- to 4-byte chars: forward, backward, then jumping, so the
;; cached position is reused near and far
(define (make-string-of n) (list->string (map (lambda (i) #\λ) (iota n))))
(define s (string-append "aé€😀" "bÿ中🎉" (make-string-of 3)))
(define (codes order)
  (map (lambda (i) (char->integer (string-ref s i))) order))
(define n (string-length s))
(define forward (iota n))
(list n
      (codes forward)
      (codes (reverse forward))
      (codes (list 5 0 10 1 9 2 8 3 7 4 6 5 10 0)))
