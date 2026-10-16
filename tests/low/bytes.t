;; Copies a string with store8, which keeps the low byte, then sums its bytes
(data byte-buffer 2 ())

(def copy-bytes ((raw src)) -> (raw) (:native)
     (let loop ((i (raw 0)))
       (if (raw= (load8 src i) (raw 0))
           i
           (begin
             (store8 byte-buffer i (raw+ (load8 src i) (raw 256)))
             (loop (raw+ i (raw 1)))))))

(def sum-bytes ((raw n)) -> (raw) (:native)
     (let loop ((i (raw 0))
                (sum (raw 0)))
       (if (raw= i n)
           sum
           (loop (raw+ i (raw 1)) (raw+ sum (load8 byte-buffer i))))))

;; "\xff;" is c3 bf in UTF-8, so 97 + 98 + 99 + 195 + 191
(def main () -> (val) ()
     (as-val (raw+ (raw<< (sum-bytes (copy-bytes "abc\xff;")) (raw 3)) (raw 1))))
---
stdout = "680"
