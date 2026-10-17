(library (emit)
  (export with-function emit! start-block! fresh-tmp fresh-label emit-op!)
  (import (chezscheme)
          (utils))


  ;; The function being emitted: its closed blocks, and the label and the
  ;; instructions of the open block, all newest first, and its counters
  (define-record-type fn
    (fields (mutable blocks) (mutable label) (mutable instrs)
            tmps raws labels)
    (protocol
     (lambda (new)
       (lambda ()
         (new '() 0 '() (make-counter) (make-counter) (make-counter))))))


  (define current-fn (make-parameter #f))


  ;; The blocks of the function whose body (emit) emits
  (define (with-function emit)
    (parameterize ((current-fn (make-fn)))
      (emit)
      (close-block!)
      (reverse (fn-blocks (current-fn)))))


  (define (emit! instr)
    (let ((fn (current-fn)))
      (fn-instrs-set! fn (cons instr (fn-instrs fn)))))


  (define (close-block!)
    (let ((fn (current-fn)))
      (fn-blocks-set! fn (cons `(block ,(fn-label fn) ,@(reverse (fn-instrs fn)))
                               (fn-blocks fn)))
      (fn-instrs-set! fn '())))


  (define (start-block! label)
    (close-block!)
    (fn-label-set! (current-fn) label))


  (define (fresh-tmp kind)
    (let ((fn (current-fn)))
      (list kind ((if (eq? kind 'raw) (fn-raws fn) (fn-tmps fn))))))


  (define (fresh-label)
    ((fn-labels (current-fn))))


  ;; (op dst src ...) with a fresh dst of kind k
  (define (emit-op! op k srcs)
    (let ((dst (fresh-tmp k)))
      (emit! `(,op ,dst ,@srcs))
      dst)))
