(define t (make-eq-hashtable))
(define gs (map (lambda (i) (gensym "t")) (iota 1000)))
(for-each (lambda (g i) (hashtable-set! t g i)) gs (iota 1000))
(list (hashtable-ref t (list-ref gs 500) #f) (hashtable-ref t 't #f) (hashtable-size t))
