;; A static ELF64 executable for x86-64, of the library image and a program after it (layout.sls):
;; no section headers, 4 program headers.
;;
;; The file is the ELF header (64 bytes) and the program headers (56 bytes each),
;; at the start of the read-only segment,
;; then each segment from a new page of the file, its offset congruent to its address modulo 4096:
;; the library's bytes, then the program's.
;; A program header: type, flags (4 read, 2 write, 1 execute), file offset, address,
;; physical address (the same), size in the file, size in memory and alignment.
;; PT_LOAD (1) maps a segment;
;; PT_GNU_STACK (#x6474e551), without execute, makes the stack not executable.

(library (elf)
  (export executable-file)
  (import (chezscheme)
          (bytes)
          (layout))


  ;; The executable of the library's segments and the program's (bytevectors in the order of
  ;; segment-bases), entering at entry
  (define (executable-file library program entry)
    (let* ((sizes (map (lambda (l p) (+ (bytevector-length l) (bytevector-length p))) library program))
           (rodata-size (+ headers-size (car sizes)))
           (data-offset (align-up rodata-size 4096))
           (text-offset (align-up (+ data-offset (cadr sizes)) 4096))
           (b (new-byte-buffer))
           (file (make-bytevector (+ text-offset (caddr sizes)) 0)))
      ;; Magic, 64-bit, little-endian, version 1, System V ABI, padding; type ET_EXEC, machine
      ;; EM_X86_64, version, entry, program header offset, section header offset (none), flags,
      ;; header size, program header entry size and count, section header entry size, count and
      ;; index of the names
      (pack! b 4 #x464c457f 1 2 1 1 1 1 1 0 8 0
             2 2 2 62 4 1 8 entry 8 64 8 0 4 0
             2 64 2 56 2 4 2 0 2 0 2 0)
      (pack! b 4 1 4 4 8 0 8 (- (car segment-bases) headers-size) 8 (- (car segment-bases) headers-size)
             8 rodata-size 8 rodata-size 8 4096)
      (pack! b 4 1 4 6 8 data-offset 8 (cadr segment-bases) 8 (cadr segment-bases)
             8 (cadr sizes) 8 (cadr sizes) 8 4096)
      (pack! b 4 1 4 5 8 text-offset 8 (caddr segment-bases) 8 (caddr segment-bases)
             8 (caddr sizes) 8 (caddr sizes) 8 4096)
      (pack! b 4 #x6474e551 4 6 8 0 8 0 8 0 8 0 8 0 8 16)
      (copy-bytes! file 0 (buffer->bytevector b) 0 (buffer-size b))
      (for-each (lambda (offset l p)
                  (copy-bytes! file offset l 0 (bytevector-length l))
                  (copy-bytes! file (+ offset (bytevector-length l)) p 0 (bytevector-length p)))
                (list headers-size data-offset text-offset) library program)
      file)))
