(let ((t (make-hashtable equal-hash equal?)))
  (hashtable-set! t (bytevector 1 2) 3)
  (hashtable-ref t (bytevector 1 2) #f))
