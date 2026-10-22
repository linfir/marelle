;; An internal define capturing locals is lifted (its captures parameters), then contified:
;; no closure, no capture load
(define (f n k)
  (define (loop i acc)
    (if (< i n) (loop (+ i 1) (+ acc k)) acc))
  (loop 0 0))
(f 3 10)
