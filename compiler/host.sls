;; Under Chez, what stage1 has and Chez lacks: the files schemec --embed embedded, none
;; (embedded-file of lib/system.ss); not flattened

(library (host)
  (export embedded-file)
  (import (chezscheme))


  (define (embedded-file name)
    #f))
