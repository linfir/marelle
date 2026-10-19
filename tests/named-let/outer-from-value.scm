(let outer ((i 0))
  (if (= i 3)
      i
      (+ 100 (let inner ((j 0))
               (if (< j 2)
                   (inner (+ j 1))
                   (if (= i 0) (outer (+ i 1)) j))))))
