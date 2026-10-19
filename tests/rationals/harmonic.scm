(let loop ((k 1) (sum 0))
  (if (> k 300) sum (loop (+ k 1) (+ sum (/ 1 k)))))
