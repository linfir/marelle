;; Copies a string with store8, which keeps the low byte, then sums its bytes
(define-data byte-buffer 2)

(define (copy-bytes (raw src))
  (declare unsafe noalloc (result raw))
  (let loop ((i (raw 0)))
    (if (raw= (load8 src i) (raw 0))
        i
        (begin
          (store8 byte-buffer i (raw+ (load8 src i) (raw 256)))
          (loop (raw+ i (raw 1)))))))

(define (sum-bytes (raw n))
  (declare unsafe noalloc (result raw))
  (let loop ((i (raw 0))
             (sum (raw 0)))
    (if (raw= i n)
        sum
        (loop (raw+ i (raw 1)) (raw+ sum (load8 byte-buffer i))))))

;; "\xff;" is c3 bf in UTF-8, so 97 + 98 + 99 + 195 + 191
(define (entry)
  (declare unsafe)
  (as-val (raw+ (raw<< (sum-bytes (copy-bytes "abc\xff;")) (raw 3)) (raw 1))))
(entry)
---
stdout = "680"
