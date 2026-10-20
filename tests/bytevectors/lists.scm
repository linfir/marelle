(list (bytevector->u8-list (bytevector 1 2 255))
      (u8-list->bytevector (list 3 4))
      (bytevector->u8-list (bytevector)))
