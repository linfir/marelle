(let loop ((i 0) (acc (list)))
  (if (= i 100)
      (length acc)
      (loop (+ i 1) `(,i ,@(list i "s") #(,i) . ,acc))))
