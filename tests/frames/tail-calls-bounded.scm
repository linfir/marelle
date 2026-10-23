;; Tail calls through procedure values, never contified: each moves its frame down, so the
;; frame region stays bounded
(define (ping n other)
  (if (= n 0) 'done (other (- n 1) ping)))
(define (pong n other)
  (if (= n 0) 'done (other (- n 1) pong)))
(ping 20000000 pong)
