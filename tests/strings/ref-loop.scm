;; Sequential and random access to a long UTF-32 string
(define (repeat s n) (if (= n 0) "" (string-append s (repeat s (- n 1)))))
(define s (repeat "aé😀" 100))
(define (sum i acc)
  (if (= i (string-length s))
      acc
      (sum (+ i 1) (+ acc (char->integer (string-ref s i))))))
(list (string-length s) (sum 0 0) (string-ref s 299) (string-ref s 151) (string-ref s 0))
