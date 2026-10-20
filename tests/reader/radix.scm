;; Radix and exactness prefixes, in either order
(let ((p (open-input-string "#x-ff #XFF #b101 #o17 #d10 #e#x10 #x#e10")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
