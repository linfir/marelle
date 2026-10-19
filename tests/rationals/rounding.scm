(define xs (list 5/2 7/2 -5/2 -7/2 1/3 2/3 -1/3 -2/3 3 -3))
(list (map floor xs) (map ceiling xs) (map truncate xs) (map round xs) (map round-half-up xs))
