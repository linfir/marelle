;; A vector larger than the first semispace (2^20 words) grows the heap at once
(let ((v (make-vector 3000000 7)))
  (+ (vector-length v) (vector-ref v 2999999)))
---
stdout = "3000007"
