;; prev gets the old cur, not the new one
(let loop ((i 0) (prev 0) (cur 1))
  (if (= i 10)
      cur
      (loop (+ i 1) cur (+ prev cur))))
