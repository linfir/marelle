;; Common subexpressions: a letprim whose op, its atoms substituted, equals one in scope is that
;; one's variable. One walk down the term, scope being dominance: each letcont is moved down to
;; the innermost term enclosing all its uses, so the letprims dominating its body enclose it.
;; - a pure op is shared while in scope, but a literal: reloading it is cheaper than keeping it
;; - a reading op only with no write, app or call between
;; - a raw result only with no safepoint between (the GC rule of cps-effect)
;; Writes and safepoints start new epochs, path by path; a continuation's body starts in the
;; epochs of its uses where they all agree, else in new ones (a return point always).
;; Cells, targets of a cell-set!, are never shared nor in a shared op.

(library (cse)
  (export cse)
  (import (chezscheme)
          (utils)
          (cps))


  ;; A letcont not placed yet: its body with the pending letconts used only there
  (define-record-type pending
    (fields k params body inner))


  (define (cse def fun)
    (let ((free (make-eq-hashtable))
          (cells (make-eq-hashtable))
          (kinds (cps-kinds fun))
          ;; Variables to variables
          (subst (make-eq-hashtable))
          ;; Ops to (x mem gc), mem and gc the epochs the entry needs, or #f
          (table (make-hashtable equal-hash equal?))
          ;; Continuations to the epochs (mem . gc) of their uses, each #f when they differ
          (uses (make-eq-hashtable))
          (next (make-counter)))
      (define (atom a)
        (cond
         ((symbol? a) (hashtable-ref subst a a))
         ((pair? a) (cons (car a) (map atom (cdr a))))
         (else a)))

      (define (cell-atom? a)
        (cond
         ((symbol? a) (hashtable-ref cells a #f))
         ((pair? a) (exists cell-atom? (cdr a)))
         (else #f)))

      (define (use! k mem gc)
        (let ((u (hashtable-ref uses k #f)))
          (hashtable-set! uses k (if u
                                     (cons (and (eqv? (car u) mem) mem) (and (eqv? (cdr u) gc) gc))
                                     (cons mem gc)))))

      (define (lookup op mem gc)
        (let ((e (hashtable-ref table op #f)))
          (and e
               (or (not (cadr e)) (eqv? (cadr e) mem))
               (or (not (caddr e)) (eqv? (caddr e) gc))
               (car e))))

      (define (walk t pending mem gc)
        (case (car t)
          ((letprim) (walk-letprim (cadr t) (map-op atom (caddr t)) (cadddr t) pending mem gc))
          ((letcont letcont-rec) (walk-letcont t pending mem gc))
          ;; The frame is allocated at the mark
          ((args) `(args ,(cadr t) ,(walk (caddr t) pending mem (next))))
          (else (place pending (lambda () (leaf t mem gc))))))

      (define (walk-letprim x op body pending mem gc)
        (let* ((effect (cps-effect op))
               (shared? (and x
                             (memq effect '(pure reads))
                             (not (eq? (car op) 'literal))
                             (not (hashtable-ref cells x #f))
                             (not (exists cell-atom? (op-atoms op))))))
          (cond
           ((and shared? (lookup op mem gc))
            => (lambda (y)
                 (hashtable-set! subst x y)
                 (walk body pending mem gc)))
           (else
            (let* ((mem (if (eq? effect 'writes) (next) mem))
                   (gc (if (eq? effect 'allocates) (next) gc))
                   (walk-body (lambda () (walk body pending mem gc))))
              `(letprim ,x ,op
                        ,(if shared?
                             (with-entry table op
                                         (list x
                                               (and (eq? effect 'reads) mem)
                                               (and (eq? (hashtable-ref kinds x #f) 'raw) gc))
                                         walk-body)
                             (walk-body))))))))

      ;; The pending letconts used in both k's body and the main term are placed here, those
      ;; used in one go with it; k is pending in the main term (a letcont-rec stays)
      (define (walk-letcont t pending mem gc)
        (let* ((b (car (cadr t)))
               (k (car b))
               (params (cadr b))
               (body (caddr b))
               (main (caddr t))
               (fs (hashtable-ref free t #f)))
          (let loop ((ps pending) (here '()) (to-body '()) (to-main '()))
            (if (pair? ps)
                (let* ((j (pending-k (car ps)))
                       (in-body? (memq j (car fs)))
                       (in-main? (memq j (cdr fs))))
                  (cond
                   ((and in-body? (not in-main?)) (loop (cdr ps) here (cons (car ps) to-body) to-main))
                   ((and in-main? (not in-body?)) (loop (cdr ps) here to-body (cons (car ps) to-main)))
                   (else (loop (cdr ps) (cons (car ps) here) to-body to-main))))
                (place here
                       (lambda ()
                         (cond
                          ((eq? (car t) 'letcont-rec)
                           (let ((main (walk main to-main mem gc)))
                             `(letcont-rec ((,k ,params ,(walk body to-body (next) (next)))) ,main)))
                          ((memq k (cdr fs))
                           (walk main (cons (make-pending k params body to-body) to-main) mem gc))
                          (else
                           (place (list (make-pending k params body to-body))
                                  (lambda () (walk main to-main mem gc)))))))))))

      ;; (make-term) with the letconts of pending around it, their bodies walked after it, in
      ;; the epochs of their uses
      (define (place pending make-term)
        (let ((t (make-term)))
          (fold-left (lambda (t p)
                       (let* ((u (hashtable-ref uses (pending-k p) #f))
                              (mem (or (and u (car u)) (next)))
                              (gc (or (and u (cdr u)) (next))))
                         `(letcont ((,(pending-k p) ,(pending-params p)
                                     ,(walk (pending-body p) (pending-inner p) mem gc)))
                                   ,t)))
                     t
                     pending)))

      (define (leaf t mem gc)
        (case (car t)
          ((if)
           (use! (caddr t) mem gc)
           (use! (cadddr t) mem gc)
           `(if (,(caadr t) ,@(map atom (cdadr t))) ,(caddr t) ,(cadddr t)))
          ((throw)
           (use! (cadr t) mem gc)
           `(throw ,(cadr t) ,@(map atom (cddr t))))
          ((call)
           (use! (cadr t) (next) (next))
           `(call ,(cadr t) ,(caddr t) ,(atom (cadddr t))))
          ((app)
           (use! (cadddr t) (next) (next))
           `(app ,(cadr t) ,(atom (caddr t)) ,(cadddr t) ,@(map atom (cddddr t))))
          ((selfcall) `(selfcall ,@(map atom (cdr t))))
          (else t)))

      (free-conts! (fun-term fun) free cells)
      `(,@(list-head fun 3) ,(walk (fun-term fun) '() 0 0))))


  ;; The continuations each subterm uses: for each letcont, (body-conts . main-conts) in free;
  ;; the targets of cell-set! in cells
  (define (free-conts! t free cells)
    (let walk ((t t))
      (case (car t)
        ((letprim)
         (when (eq? (car (caddr t)) 'cell-set!)
           (hashtable-set! cells (cadr (caddr t)) #t))
         (walk (cadddr t)))
        ((letcont letcont-rec)
         (let* ((b (car (cadr t)))
                (fb (walk (caddr b)))
                (fm (walk (caddr t))))
           (hashtable-set! free t (cons fb fm))
           (remq (car b) (fold-left (lambda (ks k) (adjoin k ks)) fb fm))))
        ((args) (walk (caddr t)))
        ((if) (list (caddr t) (cadddr t)))
        ((throw call) (list (cadr t)))
        ((app) (list (cadddr t)))
        (else '())))))
