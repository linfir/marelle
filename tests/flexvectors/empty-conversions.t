(list (flexvector->vector (flexvector))
      (flexvector->list (vector->flexvector (vector)))
      (flexvector-length (list->flexvector '())))
---
stdout = "(#() () 0)"
