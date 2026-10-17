#!/usr/bin/env -S scheme --libdirs compiler::build/chez --program

;; ./asm < IR > ASM, run from the project root

(import (chezscheme)
        (utils)
        (asm))


(compile-asm (read-all))
