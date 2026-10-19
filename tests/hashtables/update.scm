(define t (make-eq-hashtable))
(for-each (lambda (s) (hashtable-update! t s (lambda (n) (+ n 1)) 0)) '(a b a c a b))
(list (hashtable-ref t 'a 0) (hashtable-ref t 'b 0) (hashtable-ref t 'c 0))
