(call-with-values (lambda () (apply values (list 1 2 3))) list)
---
stdout = "(1 2 3)"
