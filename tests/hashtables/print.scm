(list (make-eq-hashtable) (make-eqv-hashtable 10) (make-hashtable equal-hash equal? 10)
      (hashtable? (make-eq-hashtable)) (hashtable? (vector)))
