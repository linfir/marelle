;; The R7RS string procedures Chez defines with R6RS arguments
(define r6rs-string-copy! string-copy!)
(define r6rs-string-fill! string-fill!)

(define string-copy
  (case-lambda
   ((s) (substring s 0 (string-length s)))
   ((s start) (substring s start (string-length s)))
   ((s start end) (substring s start end))))

(define string-copy!
  (case-lambda
   ((to at from) (string-copy! to at from 0 (string-length from)))
   ((to at from start) (string-copy! to at from start (string-length from)))
   ((to at from start end) (r6rs-string-copy! from start to at (- end start)))))

(define string-fill!
  (case-lambda
   ((s c) (r6rs-string-fill! s c))
   ((s c start) (string-fill! s c start (string-length s)))
   ((s c start end)
    (do ((i start (+ i 1)))
        ((= i end))
      (string-set! s i c)))))
