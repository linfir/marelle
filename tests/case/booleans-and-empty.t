(cons (case #f ((#t) 1) ((#f) 2)) (case '() ((()) 3) (else 4)))
---
stdout = "(2 . 3)"
