#!/usr/bin/env -S scheme --script

;; scheme --script compiler.ss < SOURCE > LOW, run from the project root

(import (utils))
(load "frontend.ss")
(load "scheme-low.ss")


(parameterize ((print-brackets #f))
  (for-each pretty-print
            (program->low (with-input-from-file "prelude.ss" read-all) (read-all))))
