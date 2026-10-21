;; A unit of encode.sls as an ELF64 relocatable object, GNU as's but for the order of the symbols:
;; for asmcheck.py, which compares it with GNU as's.
;;
;; A reference to a local symbol of the same section is resolved, but an
;; absolute one, and so is a jmp to a global one (a call is not); the
;; others become relocations, against the symbol for a
;; global or undefined one, against the section otherwise.
;; Calls and jumps to a symbol use R_X86_64_PLT32, RIP-relative operands R_X86_64_PC32, quads
;; R_X86_64_64.
;;
;; The object has the unit's sections, their type and flags from their
;; names (section-kinds), then their .rela sections, .symtab, .strtab and
;; .shstrtab.
;; The symbols: one per section, the labels but the .L ones (local), then the
;; defined globals and the undefined symbols.
;;
;; The file is a 64-byte ELF header, the contents of the sections, then the
;; section header table, of 64-byte entries.
;; Entry 0 is null, index 0 meaning no section, so the sections are numbered
;; from 1.
;; A section header: name, type, flags, address (0 in an object), file offset,
;; size, link, info, alignment and entry size (of a table, else 0); link and
;; info depend on the type.
;; The names are offsets in .shstrtab, a string table the ELF header gives the
;; index of.
;; A string table is the names, each followed by a 0 byte, after a 0 byte,
;; offset 0 being the empty name.
;;
;; .symtab is an array of 24-byte symbols: name (an offset in .strtab), info,
;; other (the visibility, 0 the default), section index (0 undefined), value
;; (an offset in the section) and size.
;; info is the binding (0 local, 1 global) * 16 + the type (0 none, 3 section).
;; Entry 0 is null, then come the locals: the header's info is the index of
;; the first global, its link that of .strtab.
;;
;; .rela.X holds the relocations of section X, of 24 bytes: offset in X, info
;; (the symbol's index * 2^32 + the type) and addend.
;; Its link is the index of .symtab, its info that of X (flag #x40, SHF_INFO_LINK).
;; For each type, the linker stores at the offset: S + A for R_X86_64_64 (1),
;; S + A - P for R_X86_64_PC32 (2) and L + A - P for R_X86_64_PLT32 (4), S
;; being the symbol's address, A the addend, P the address of the offset and L
;; that of the symbol's PLT entry, or of the symbol when there is none.
;; A relocation to a local label is against its section's symbol, the label's
;; offset added to the addend.

(library (object)
  (export object-file)
  (import (chezscheme)
          (bytes)
          (encode))


  (define (reloc-type type)
    (case type
      ((abs64) 1)
      ((pc32) 2)
      (else 4)))


  ;; Labels left out of the symbol table, as GNU as does
  (define (local-label? name)
    (and (>= (string-length name) 2) (string=? (substring name 0 2) ".L")))


  ;; A string table: (values add! contents), add! giving the offset of a name, contents
  ;; the table once all are added
  (define (string-table)
    (let ((offsets (make-hashtable string-hash string=?))
          (b (new-byte-buffer)))
      (buffer-u8! b 0)
      (values
       (lambda (name)
         (or (hashtable-ref offsets name #f)
             (let ((offset (buffer-size b)))
               (hashtable-set! offsets name offset)
               (buffer-bytevector! b (string->utf8 name))
               (buffer-u8! b 0)
               offset)))
       (lambda () (buffer->bytevector b)))))


  (define sht-progbits 1)
  (define sht-symtab 2)
  (define sht-strtab 3)
  (define sht-rela 4)


  ;; The type and flags of the sections the compiler emits (flags: 1 write, 2 alloc, 4 exec).
  ;; The empty .note.GNU-stack asks for a non-executable stack.
  (define section-kinds
    `((".text" ,sht-progbits 6)
      (".data" ,sht-progbits 3)
      (".data.rel.ro" ,sht-progbits 3)
      (".rodata" ,sht-progbits 2)
      (".note.GNU-stack" ,sht-progbits 0)))


  (define-record-type elf-section
    (fields name type flags link info align entsize bytes))


  (define (align-up n a)
    (* a (quotient (+ n a -1) a)))


  ;; Writes the fields to b: size n ..., n taken modulo 2^(8 size)
  (define (pack! b . fields)
    (let loop ((l fields))
      (unless (null? l)
        (buffer-le! b (cadr l) (car l))
        (loop (cddr l)))))


  ;; The file offset of each section, then the end of the last
  (define (section-offsets sections)
    (let loop ((l sections) (end 64))
      (if (null? l)
          (list end)
          (let ((offset (align-up end (elf-section-align (car l)))))
            (cons offset
                  (loop (cdr l) (+ offset (bytevector-length (elf-section-bytes (car l))))))))))


  ;; An ELF64 relocatable object for x86-64 of the sections, at indices from 1, and
  ;; .shstrtab after them
  (define (elf-object sections)
    (let-values (((name-offset names) (string-table)))
      (for-each (lambda (s) (name-offset (elf-section-name s))) sections)
      (name-offset ".shstrtab")
      (let* ((sections (append sections (list (make-elf-section ".shstrtab" sht-strtab 0 0 0 1 0 (names)))))
             (n (length sections))
             (offsets (section-offsets sections))
             (end (list-ref offsets n))
             (shoff (align-up end 8))
             (b (new-byte-buffer)))
        ;; Magic, 64-bit, little-endian, version 1, System V ABI, padding; type ET_REL,
        ;; machine EM_X86_64, version, entry, program header offset (none), section
        ;; header offset, flags, header size, program header entry size and count,
        ;; section header entry size and count, index of .shstrtab
        (pack! b 4 #x464c457f 1 2 1 1 1 1 1 0 8 0
               2 1 2 62 4 1 8 0 8 0 8 shoff 4 0
               2 64 2 0 2 0 2 64 2 (+ n 1) 2 n)
        (for-each (lambda (s offset)
                    (buffer-zeros! b (- offset (buffer-size b)))
                    (buffer-bytevector! b (elf-section-bytes s)))
                  sections (list-head offsets n))
        (buffer-zeros! b (- shoff end))
        (buffer-zeros! b 64)
        ;; The section headers
        (for-each (lambda (s offset)
                    (pack! b 4 (name-offset (elf-section-name s)) 4 (elf-section-type s)
                           8 (elf-section-flags s) 8 0 8 offset
                           8 (bytevector-length (elf-section-bytes s))
                           4 (elf-section-link s) 4 (elf-section-info s)
                           8 (elf-section-align s) 8 (elf-section-entsize s)))
                  sections (list-head offsets n))
        (buffer->bytevector b))))


  ;; The relocations, (section offset type target addend), target a section index or a name, in
  ;; the unit's fixup order; the fixups resolved in the sections' bytes, which it changes
  (define (resolve-fixups! unit)
    (let ((labels (make-hashtable string-hash string=?))
          (globals (make-hashtable string-hash string=?))
          (sections (list->vector (unit-sections unit))))
      (for-each (lambda (l) (hashtable-set! labels (unit-label-name l) l)) (unit-labels unit))
      (for-each (lambda (name) (hashtable-set! globals name #t)) (unit-globals unit))
      (let loop ((fixups (unit-fixups unit)) (relocs '()))
        (if (null? fixups)
            (reverse relocs)
            (let* ((f (car fixups))
                   (section (fixup-section f))
                   (offset (fixup-offset f))
                   (name (fixup-symbol f))
                   (def (and (or (eq? (fixup-type f) 'jmp32) (not (hashtable-contains? globals name)))
                             (hashtable-ref labels name #f))))
              (cond
               ((and def (= (unit-label-section def) section) (not (eq? (fixup-type f) 'abs64)))
                (bytevector-le-set! (section-bytes (vector-ref sections section)) offset
                                    (+ (unit-label-offset def) (fixup-addend f) (- offset)) 4)
                (loop (cdr fixups) relocs))
               (def
                (loop (cdr fixups)
                      (cons (list section offset (reloc-type (fixup-type f))
                                  (unit-label-section def) (+ (unit-label-offset def) (fixup-addend f)))
                            relocs)))
               (else
                (loop (cdr fixups)
                      (cons (list section offset (reloc-type (fixup-type f)) name (fixup-addend f))
                            relocs)))))))))


  ;; The first occurrence of each string of l, in order; a loop, since filter and map may call
  ;; their procedure in any order
  (define (unique-strings l)
    (let ((seen (make-hashtable string-hash string=?)))
      (let loop ((l l) (acc '()))
        (cond
         ((null? l) (reverse acc))
         ((hashtable-contains? seen (car l)) (loop (cdr l) acc))
         (else
          (hashtable-set! seen (car l) #t)
          (loop (cdr l) (cons (car l) acc)))))))


  ;; The unit as an ELF object, a bytevector
  (define (object-file unit)
    (let* ((relocs (resolve-fixups! unit))
           (sections (unit-sections unit))
           (n (length sections))
           (globals (let ((t (make-hashtable string-hash string=?)))
                      (for-each (lambda (name) (hashtable-set! t name #t)) (unit-globals unit))
                      t))
           (labels (let ((t (make-hashtable string-hash string=?)))
                     (for-each (lambda (l) (hashtable-set! t (unit-label-name l) l)) (unit-labels unit))
                     t))
           (locals (filter (lambda (l)
                             (not (or (hashtable-contains? globals (unit-label-name l))
                                      (local-label? (unit-label-name l)))))
                           (unit-labels unit)))
           (exported (filter (lambda (x) x)
                             (map (lambda (name) (hashtable-ref labels name #f))
                                  (unique-strings (unit-globals unit)))))
           (undefined (unique-strings
                       (filter (lambda (name)
                                 (and (string? name)
                                      (not (and (hashtable-contains? globals name)
                                                (hashtable-contains? labels name)))))
                               (map cadddr relocs))))
           (first-global (+ 1 n (length locals)))
           (symbol-indices (make-hashtable string-hash string=?))
           (with-relocs (filter (lambda (i) (exists (lambda (r) (= (car r) i)) relocs)) (iota n)))
           (symtab-index (+ n (length with-relocs) 1)))
      (let loop ((names (append (map unit-label-name exported) undefined)) (i first-global))
        (unless (null? names)
          (hashtable-set! symbol-indices (car names) i)
          (loop (cdr names) (+ i 1))))
      (let-values (((name-offset names) (string-table)))
        (define (sym! b name info shndx value)
          (pack! b 4 (if name (name-offset name) 0) 1 info 1 0 2 shndx 8 value 8 0))
        ;; The symbol of section i is at index i + 1, as the section
        (define (rela! b r)
          (let ((target (cadddr r)))
            (pack! b 8 (cadr r)
                   4 (caddr r)
                   4 (if (string? target) (hashtable-ref symbol-indices target #f) (+ target 1))
                   8 (car (cddddr r)))))
        (let* ((program (map (lambda (s)
                               (let ((kind (or (assoc (section-name s) section-kinds)
                                               (error 'object-file "unknown section" (section-name s)))))
                                 (make-elf-section (section-name s) (cadr kind) (caddr kind) 0 0
                                                   (section-align s) 0 (section-bytes s))))
                             sections))
               (rela-sections
                (map (lambda (i)
                       (let ((b (new-byte-buffer)))
                         (for-each (lambda (r) (when (= (car r) i) (rela! b r))) relocs)
                         (make-elf-section (string-append ".rela" (section-name (list-ref sections i)))
                                           sht-rela #x40 symtab-index (+ i 1) 8 24
                                           (buffer->bytevector b))))
                     with-relocs))
               ;; Local (bind 0) section symbols (type 3) and labels, then the globals (bind 1)
               (symtab (let ((b (new-byte-buffer)))
                         (sym! b #f 0 0 0)
                         (for-each (lambda (i) (sym! b #f 3 (+ i 1) 0)) (iota n))
                         (for-each (lambda (l) (sym! b (unit-label-name l) 0 (+ (unit-label-section l) 1) (unit-label-offset l)))
                                   locals)
                         (for-each (lambda (l) (sym! b (unit-label-name l) #x10 (+ (unit-label-section l) 1) (unit-label-offset l)))
                                   exported)
                         (for-each (lambda (name) (sym! b name #x10 0 0)) undefined)
                         (buffer->bytevector b))))
          (elf-object (append program
                              rela-sections
                              (list (make-elf-section ".symtab" sht-symtab 0 (+ symtab-index 1)
                                                      first-global 8 24 symtab)
                                    (make-elf-section ".strtab" sht-strtab 0 0 0 1 0 (names))))))))))
