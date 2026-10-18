(do ((i 0 (+ i 1)) (fs (quote ()) (cons (lambda () i) fs))) ((= i 3) (map (lambda (f) (f)) fs)))
---
stdout = "(2 1 0)"
