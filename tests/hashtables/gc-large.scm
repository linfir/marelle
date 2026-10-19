(define t (make-eq-hashtable))
(define (keys n acc)
  (if (= n 0)
      acc
      (keys (- n 1) (cons (list n) acc))))
(define ks (keys 20000 '()))
(for-each (lambda (k) (hashtable-set! t k (car k))) ks)
(define (churn n)
  (if (= n 0)
      0
      (begin (make-vector 100 0) (churn (- n 1)))))
(define (total round)
  (if (= round 0)
      0
      (begin
        (churn 20000)
        (+ (fold-left + 0 (map (lambda (k) (hashtable-ref t k 0)) ks))
           (total (- round 1))))))
(total 3)
