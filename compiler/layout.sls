;; Placing units at fixed addresses: three segments, the library first in each, then the program.
;;
;; | Segment | Base       | Contents                                         |
;; | rodata  | 0x400000   | ELF header, program headers; .data.rel.ro, .rodata |
;; | data    | 0x10000000 | .data; the program's starts with scheme_program  |
;; | text    | 0x20000000 | .text                                            |
;;
;; Everything is within 2 GiB of everything else, so a rel32 always reaches.
;; The library image (lib.img) is its three segments, after a stamp, the hash of the image and of
;; the interface, which must come from the same library build.

(library (layout)
  (export segment-bases headers-size place-unit placement-ends link-unit
          linked-segments linked-globals linked-symbols
          stamp image-bytes read-image image-stamp image-segments image-ends)
  (import (chezscheme)
          (bytes)
          (encode))


  ;; The 64-byte ELF header and 4 program headers of 56 bytes, before the library's rodata
  (define headers-size (+ 64 (* 4 56)))

  (define segment-bases (list (+ #x400000 headers-size) #x10000000 #x20000000))


  ;; The segment of each section, in the order of the segments' contents
  (define section-segments
    '((".data.rel.ro" . 0) (".rodata" . 0) (".data" . 1) (".text" . 2)))


  ;; The address of each section of a unit (a vector, by index) and the segments' starts and
  ;; ends
  (define-record-type placement
    (fields addresses starts ends))


  ;; The unit's sections placed in the segments from the addresses starts, each aligned
  (define (place-unit unit starts)
    (let* ((sections (list->vector (unit-sections unit)))
           (addresses (make-vector (vector-length sections) #f)))
      (do ((i 0 (+ i 1)))
          ((= i (vector-length sections)))
        (let ((s (vector-ref sections i)))
          (unless (or (assoc (section-name s) section-segments)
                      (= (bytevector-length (section-bytes s)) 0))
            (error 'place-unit "unknown section" (section-name s)))))
      (let loop ((l section-segments) (ends starts))
        (if (null? l)
            (make-placement addresses starts ends)
            (let ((i (section-index sections (caar l)))
                  (k (cdar l)))
              (if i
                  (let* ((s (vector-ref sections i))
                         (address (align-up (list-ref ends k) (section-align s))))
                    (vector-set! addresses i address)
                    (loop (cdr l)
                          (replace-nth ends k (+ address (bytevector-length (section-bytes s))))))
                  (loop (cdr l) ends)))))))


  (define (section-index sections name)
    (let loop ((i 0))
      (cond
       ((= i (vector-length sections)) #f)
       ((string=? (section-name (vector-ref sections i)) name) i)
       (else (loop (+ i 1))))))


  (define (replace-nth l k x)
    (if (= k 0)
        (cons x (cdr l))
        (cons (car l) (replace-nth (cdr l) (- k 1) x))))


  ;; A linked unit: the bytes of its three segments, from their starts, the global symbols and
  ;; then all the labels, ((name . address) ...) in order
  (define-record-type linked
    (fields segments globals symbols))


  ;; The unit placed by p, its fixups resolved against its labels, then known (a hashtable of
  ;; names to addresses); unknown symbols and rel32s out of range are errors, all named at once
  (define (link-unit unit p known)
    (let* ((addresses (placement-addresses p))
           (sections (list->vector (unit-sections unit)))
           (labels (make-hashtable string-hash string=?))
           (symbols (map (lambda (l)
                           (cons (unit-label-name l)
                                 (+ (vector-ref addresses (unit-label-section l)) (unit-label-offset l))))
                         (unit-labels unit))))
      (for-each (lambda (x) (hashtable-set! labels (car x) (cdr x))) symbols)
      (let ((bytes (list->vector (map (lambda (s) (bytevector-copy (section-bytes s))) (unit-sections unit)))))
        (let loop ((fixups (unit-fixups unit)) (unknown '()) (far '()))
          (if (pair? fixups)
              (let* ((f (car fixups))
                     (name (fixup-symbol f))
                     (s (or (hashtable-ref labels name #f) (hashtable-ref known name #f)))
                     (at (+ (vector-ref addresses (fixup-section f)) (fixup-offset f))))
                (cond
                 ((not s)
                  (loop (cdr fixups) (if (member name unknown) unknown (cons name unknown)) far))
                 ((eq? (fixup-type f) 'abs64)
                  (bytevector-le-set! (vector-ref bytes (fixup-section f)) (fixup-offset f)
                                      (+ s (fixup-addend f)) 8)
                  (loop (cdr fixups) unknown far))
                 ((s32? (- (+ s (fixup-addend f)) at))
                  (bytevector-le-set! (vector-ref bytes (fixup-section f)) (fixup-offset f)
                                      (- (+ s (fixup-addend f)) at) 4)
                  (loop (cdr fixups) unknown far))
                 (else
                  (loop (cdr fixups) unknown (if (member name far) far (cons name far))))))
              (begin
                (unless (null? unknown)
                  (apply error 'link-unit "undefined symbols" (reverse unknown)))
                (unless (null? far)
                  (apply error 'link-unit "symbols out of rel32 range" (reverse far)))
                (make-linked (segment-bytes sections bytes p)
                             (globals-of unit labels)
                             symbols)))))))


  ;; The bytes of each segment from its start, the sections at their addresses, zeros between
  (define (segment-bytes sections bytes p)
    (map (lambda (k start end)
           (let ((b (new-byte-buffer)))
             (for-each (lambda (x)
                         (let ((i (and (= (cdr x) k) (section-index sections (car x)))))
                           (when i
                             (buffer-zeros! b (- (vector-ref (placement-addresses p) i)
                                                 (+ start (buffer-size b))))
                             (buffer-bytevector! b (vector-ref bytes i)))))
                       section-segments)
             (buffer-zeros! b (- end (+ start (buffer-size b))))
             (buffer->bytevector b)))
         '(0 1 2) (placement-starts p) (placement-ends p)))


  ;; The defined globals, once each, in order
  (define (globals-of unit labels)
    (let ((seen (make-hashtable string-hash string=?)))
      (let loop ((l (unit-globals unit)) (acc '()))
        (cond
         ((null? l) (reverse acc))
         ((or (hashtable-contains? seen (car l)) (not (hashtable-ref labels (car l) #f)))
          (loop (cdr l) acc))
         (else
          (hashtable-set! seen (car l) #t)
          (loop (cdr l) (cons (cons (car l) (hashtable-ref labels (car l) #f)) acc)))))))


  ;; FNV-1a, 32 bits, of the bytevectors: the products stay below 2^57, fixnums on both hosts
  (define (stamp bvs)
    (let loop ((bvs bvs) (h 2166136261))
      (if (null? bvs)
          h
          (let ((bv (car bvs)))
            (let next ((i 0) (h h))
              (if (= i (bytevector-length bv))
                  (loop (cdr bvs) h)
                  (next (+ i 1)
                        (logand (* (logxor h (bytevector-u8-ref bv i)) 16777619) 4294967295))))))))


  ;; The image file: the stamp and the segments' sizes, 8-byte words, then the segments
  (define (image-bytes stamp segments)
    (let ((b (new-byte-buffer)))
      (buffer-le! b stamp 8)
      (for-each (lambda (s) (buffer-le! b (bytevector-length s) 8)) segments)
      (for-each (lambda (s) (buffer-bytevector! b s)) segments)
      (buffer->bytevector b)))


  ;; An image: its stamp and segments, the library's, from the segment bases
  (define-record-type image
    (fields stamp segments))


  (define (le-ref bv offset size)
    (let loop ((i (- size 1)) (n 0))
      (if (< i 0)
          n
          (loop (- i 1) (+ (* n 256) (bytevector-u8-ref bv (+ offset i)))))))


  (define (read-image bv)
    (unless (and (bytevector? bv) (>= (bytevector-length bv) 32))
      (error 'read-image "truncated library image"))
    (let* ((sizes (map (lambda (k) (le-ref bv (+ 8 (* 8 k)) 8)) '(0 1 2)))
           (total (+ 32 (apply + sizes))))
      (unless (= (bytevector-length bv) total)
        (error 'read-image "library image of the wrong size" (bytevector-length bv) total))
      (make-image (le-ref bv 0 8)
                  (let loop ((sizes sizes) (offset 32))
                    (if (null? sizes)
                        '()
                        (cons (bytevector-slice bv offset (car sizes))
                              (loop (cdr sizes) (+ offset (car sizes)))))))))


  (define (bytevector-slice bv start n)
    (let ((b (new-byte-buffer)))
      (buffer-copy! b bv start n)
      (buffer->bytevector b)))


  ;; Where the program's segments start
  (define (image-ends image)
    (map (lambda (base s) (+ base (bytevector-length s))) segment-bases (image-segments image))))
