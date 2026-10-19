(define p (open-output-string))
(format p "~s ~a" #\a #\a)
(format p "!")
(get-output-string p)
