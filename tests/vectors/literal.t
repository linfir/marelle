;; Self-evaluating, as in R7RS; Chez requires a quote
#(1 #t #\x ())
---
stdout = "#(1 #t #\\x ())"
chez = "skip"
