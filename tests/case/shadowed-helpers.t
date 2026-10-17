(let ((if 1) (eq? 2) (or 3)) (case 2 ((1) if) ((2) eq?) (else or)))
---
stdout = "2"
