(define (kind x)
  (case x
    ((a e i o u) 'vowel)
    ((y) 'sometimes)
    (else 'consonant)))
(list (kind 'e) (kind 'y) (kind 'z))
---
stdout = '(vowel sometimes consonant)'
