(list (flexvector? (flexvector))
      (flexvector? (vector))
      (vector? (make-flexvector 0))
      (flexvector? '())
      (flexvector? 0))
---
stdout = "(#t #f #f #f #f)"
