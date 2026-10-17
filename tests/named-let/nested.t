(let outer ((i 0) (acc 0))
  (if (= i 3)
      acc
      (outer (+ i 1)
             (let inner ((j 0) (acc acc))
               (if (= j 4) acc (inner (+ j 1) (+ acc (* i j))))))))
---
stdout = "18"
