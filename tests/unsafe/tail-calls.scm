(define (even? n)
  (declare unsafe)
  (if (= n 0) #t (odd? (- n 1))))

(define (odd? n)
  (declare unsafe)
  (if (= n 0) #f (even? (- n 1))))

(define (step n)
  (declare unsafe inline)
  (count (- n 1)))

;; Tail position through let, let*, begin and an inline def
(define (count n)
  (declare unsafe)
  (if (= n 0)
      0
      (let ((m n))
        (let* ((k m))
          (begin
            (car (cons 1 2))
            (step k))))))

(define (entry)
  (declare unsafe)
  (count 1000000)
  (cons (even? 1000000) (odd? 1000001)))
(entry)
