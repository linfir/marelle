(let* ((a 1)) (let* ((g (lambda () (let* ((b a)) (+ b 1))))) (g)))
