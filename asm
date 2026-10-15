#!/usr/bin/env scheme-script

;; scheme-script asm < IR > ASM

(import (chezscheme)
        (utils)
        (asm))


(compile-asm (read-all))
