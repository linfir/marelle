;; Growable byte buffers, written in order, for the encoder and the file writers

(library (bytes)
  (export new-byte-buffer buffer-size buffer-u8! buffer-le! buffer-bytevector! buffer-zeros!
          buffer->bytevector buffer-copy! copy-bytes! bytevector-le-set! align-up pack!)
  (import (except (chezscheme) bytevector-copy!)
          (host))


  ;; The first size bytes of data are written
  (define-record-type byte-buffer
    (fields (mutable data) (mutable size)))


  (define (new-byte-buffer)
    (make-byte-buffer (make-bytevector 4096 0) 0))


  (define (buffer-size b)
    (byte-buffer-size b))


  ;; Makes room for n more bytes
  (define (reserve! b n)
    (let ((data (byte-buffer-data b))
          (need (+ (byte-buffer-size b) n)))
      (when (> need (bytevector-length data))
        (let ((new (make-bytevector (max need (* 2 (bytevector-length data))) 0)))
          (copy-bytes! new 0 data 0 (byte-buffer-size b))
          (byte-buffer-data-set! b new)))))


  ;; Copies the n bytes of from at start to to at at;
  ;; byte by byte when short, since a bytevector-copy! call costs as much as 10 bytes in stage1
  (define (copy-bytes! to at from start n)
    (if (< n 16)
        (do ((i 0 (+ i 1)))
            ((= i n))
          (bytevector-u8-set! to (+ at i) (bytevector-u8-ref from (+ start i))))
        (bytevector-copy! to at from start (+ start n))))


  (define (buffer-u8! b x)
    (reserve! b 1)
    (bytevector-u8-set! (byte-buffer-data b) (byte-buffer-size b) x)
    (byte-buffer-size-set! b (+ (byte-buffer-size b) 1)))


  ;; The size low bytes of the exact integer n, little-endian, so n modulo 2^(8 size)
  (define (buffer-le! b n size)
    (reserve! b size)
    (bytevector-le-set! (byte-buffer-data b) (byte-buffer-size b) n size)
    (byte-buffer-size-set! b (+ (byte-buffer-size b) size)))


  ;; n less its low byte divides exactly, so quotient shifts right on the inline fixnum paths
  (define (bytevector-le-set! bv offset n size)
    (do ((i 0 (+ i 1))
         (n n (quotient (- n (logand n 255)) 256)))
        ((= i size))
      (bytevector-u8-set! bv (+ offset i) (logand n 255))))


  (define (buffer-bytevector! b bv)
    (buffer-copy! b bv 0 (bytevector-length bv)))


  ;; The n bytes of bv from start
  (define (buffer-copy! b bv start n)
    (reserve! b n)
    (copy-bytes! (byte-buffer-data b) (byte-buffer-size b) bv start n)
    (byte-buffer-size-set! b (+ (byte-buffer-size b) n)))


  (define (buffer-zeros! b n)
    (reserve! b n)
    (byte-buffer-size-set! b (+ (byte-buffer-size b) n)))


  ;; Writes the fields to b: size n ..., n taken modulo 2^(8 size)
  (define (pack! b . fields)
    (let loop ((l fields))
      (unless (null? l)
        (buffer-le! b (cadr l) (car l))
        (loop (cddr l)))))


  ;; n rounded up to a multiple of a
  (define (align-up n a)
    (* a (quotient (+ n a -1) a)))


  (define (buffer->bytevector b)
    (let ((bv (make-bytevector (byte-buffer-size b) 0)))
      (copy-bytes! bv 0 (byte-buffer-data b) 0 (byte-buffer-size b))
      bv)))
