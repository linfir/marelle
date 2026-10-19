(let loop ((i 1000000) (acc 0))
  (if (= i 0)
      acc
      (let-values (((a b) (values i 1)))
        (loop (- a b) (+ acc 1)))))
