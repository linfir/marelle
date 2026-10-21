;; A name of 512 chars, longer than memfd_create(2) takes
(execute-bytevector (get-bytevector-all (open-file-input-port "/bin/echo"))
                    (let double ((s "a") (i 0))
                      (if (= i 9) s (double (string-append s s) (+ i 1)))))
