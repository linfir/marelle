;; The R7RS bytevector procedures Chez lacks or defines with R6RS arguments
(define r6rs-bytevector-copy! bytevector-copy!)
(define r6rs-utf8->string utf8->string)
(define r6rs-string->utf8 string->utf8)

(define (sub-bytevector bv start end)
  (let ((c (make-bytevector (- end start))))
    (r6rs-bytevector-copy! bv start c 0 (- end start))
    c))

(define bytevector-copy
  (case-lambda
   ((bv) (sub-bytevector bv 0 (bytevector-length bv)))
   ((bv start) (sub-bytevector bv start (bytevector-length bv)))
   ((bv start end) (sub-bytevector bv start end))))

(define bytevector-copy!
  (case-lambda
   ((to at from) (bytevector-copy! to at from 0 (bytevector-length from)))
   ((to at from start) (bytevector-copy! to at from start (bytevector-length from)))
   ((to at from start end) (r6rs-bytevector-copy! from start to at (- end start)))))

(define (bytevector-append . bvs)
  (u8-list->bytevector (apply append (map bytevector->u8-list bvs))))

(define utf8->string
  (case-lambda
   ((bv) (r6rs-utf8->string bv))
   ((bv start) (r6rs-utf8->string (bytevector-copy bv start)))
   ((bv start end) (r6rs-utf8->string (bytevector-copy bv start end)))))

(define string->utf8
  (case-lambda
   ((s) (r6rs-string->utf8 s))
   ((s start) (r6rs-string->utf8 (substring s start (string-length s))))
   ((s start end) (r6rs-string->utf8 (substring s start end)))))
