;; Cells to continuation parameters (SSA by construction, like mem2reg): a cell, a variable bound
;; by a move and only read by moves and written by cell-set!, becomes the atom it holds on each
;; path. A read is that atom, a write a new one; a continuation it is live into takes it as a
;; parameter, which each throw passes. An if's branch or a call's continuation cannot take more
;; arguments, so it goes through a continuation that throws to it with them.

(library (cells)
  (export cells-to-params)
  (import (chezscheme)
          (utils)
          (cps))


  (define (cells-to-params fun counter)
    (let ((cells (find-cells fun)))
      (if (zero? (hashtable-size cells))
          fun
          (cells-convert fun cells counter))))


  ;; The cells of fun: to their number, in the order of their bindings
  (define (find-cells fun)
    (let ((set (make-eq-hashtable))
          (other (make-eq-hashtable))
          (order '()))
      (define (atom! a)
        (cond
         ((symbol? a) (hashtable-set! other a #t))
         ((pair? a) (for-each atom! (cdr a)))))

      (define (walk t)
        (case (car t)
          ((letprim)
           (let ((op (caddr t)))
             (case (car op)
               ((move)
                (when (cadr t)
                  (set! order (cons (cadr t) order)))
                (unless (symbol? (cadr op))
                  (atom! (cadr op))))
               ((cell-set!)
                (hashtable-set! set (cadr op) #t)
                (atom! (caddr op)))
               (else (for-each atom! (op-atoms op)))))
           (walk (cadddr t)))
          ((letcont letcont-rec)
           (walk (caddr (car (cadr t))))
           (walk (caddr t)))
          ((args) (walk (caddr t)))
          ((if) (for-each atom! (cdadr t)))
          ((throw) (for-each atom! (cddr t)))
          ((app)
           (atom! (caddr t))
           (for-each atom! (cddddr t)))
          ((selfcall) (for-each atom! (cdr t)))
          (else (void))))

      (walk (fun-term fun))
      (let ((cells (make-eq-hashtable)))
        (for-each (lambda (x i)
                    (cond
                     ((not (hashtable-ref other x #f)) (hashtable-set! cells x i))
                     ((hashtable-ref set x #f) (compile-error 'cells "cell used as a value" x))))
                  (reverse order) (iota (length order)))
        cells)))


  ;; Per continuation, the cells live into it, in their order
  (define (live-cells t cells)
    (let ((live (make-eq-hashtable))
          (changed? #t))
      (define (index x)
        (hashtable-ref cells x #f))

      (define (add x s)
        (cond
         ((null? s) (list x))
         ((eq? (car s) x) s)
         ((< (index x) (index (car s))) (cons x s))
         (else (cons (car s) (add x (cdr s))))))

      (define (union a b)
        (fold-left (lambda (s x) (add x s)) b a))

      (define (live-in k)
        (hashtable-ref live k '()))

      (define (walk t)
        (case (car t)
          ((letprim)
           (let ((s (walk (cadddr t)))
                 (op (caddr t)))
             (case (car op)
               ((move)
                (let ((s (if (index (cadr t)) (remq (cadr t) s) s)))
                  (if (index (cadr op)) (add (cadr op) s) s)))
               ((cell-set!) (if (index (cadr op)) (remq (cadr op) s) s))
               (else s))))
          ((letcont letcont-rec)
           (let* ((b (car (cadr t)))
                  (s (walk (caddr b))))
             (unless (equal? s (live-in (car b)))
               (hashtable-set! live (car b) s)
               (set! changed? #t))
             (walk (caddr t))))
          ((args) (walk (caddr t)))
          ((if) (union (live-in (caddr t)) (live-in (cadddr t))))
          ((throw) (live-in (cadr t)))
          ((app) (live-in (cadddr t)))
          (else '())))

      (let loop ()
        (when changed?
          (set! changed? #f)
          (walk t)
          (loop)))
      live))


  ;; A continuation takes a cell as a parameter only when a use may pass it another atom than
  ;; the one the cell holds where the continuation is bound: assumed not, then walks again with
  ;; the cells some use contradicts as parameters, until none does
  (define (cells-convert fun cells counter)
    (let ((live (live-cells (fun-term fun) cells))
          (kinds (cps-kinds fun))
          (passed (make-eq-hashtable))
          (subst #f)
          (held #f)
          (params (make-eq-hashtable))
          (changed? #f))
      (define (fresh x)
        (gensym (string-append (name-prefix x) (number->string (counter)))))

      (define (cell? x)
        (and (symbol? x) (hashtable-ref cells x #f)))

      (define (sub a)
        (cond
         ((symbol? a) (hashtable-ref subst a a))
         ((pair? a) (cons (car a) (map sub (cdr a))))
         (else a)))

      ;; The atom the cell c holds in env
      (define (value c env)
        (let ((entry (assq c env)))
          (unless entry
            (compile-error 'cells "cell read outside its scope" c))
          (cdr entry)))

      (define (passed-cells k)
        (hashtable-ref passed k '()))

      ;; The atoms of the cells k takes, from a use of k in env; a use holding another atom than
      ;; k's binding makes that cell a parameter of k
      (define (extra-atoms k env)
        (for-each (lambda (entry)
                    (unless (equal? (value (car entry) env) (cdr entry))
                      (hashtable-set! passed k (cons (car entry) (passed-cells k)))
                      (set! changed? #t)))
                  (hashtable-ref held k '()))
        (map (lambda (c) (value c env)) (passed-cells k)))

      ;; k, or a continuation of no extra parameters throwing to it with them, around (use k)
      (define (direct k env use)
        (let ((atoms (extra-atoms k env)))
          (if (null? atoms)
              (use k)
              (let ((j (fresh k))
                    (ps (map (lambda (p)
                               (let ((x (fresh (param-var p))))
                                 (if (pair? p) (list (car p) x) x)))
                             (hashtable-ref params k '()))))
                `(letcont ((,j ,ps (throw ,k ,@(map param-var ps) ,@atoms)))
                          ,(use j))))))

      (define (walk t env)
        (case (car t)
          ((letprim)
           (let ((x (cadr t))
                 (op (caddr t)))
             (cond
              ((and (eq? (car op) 'move) (cell? x))
               (walk (cadddr t)
                     (cons (cons x (if (cell? (cadr op)) (value (cadr op) env) (sub (cadr op)))) env)))
              ((and (eq? (car op) 'move) (cell? (cadr op)))
               (when x
                 (hashtable-set! subst x (value (cadr op) env)))
               (walk (cadddr t) env))
              ((and (eq? (car op) 'cell-set!) (cell? (cadr op)))
               (walk (cadddr t) (cons (cons (cadr op) (sub (caddr op))) env)))
              (else `(letprim ,x ,(map-op sub op) ,(walk (cadddr t) env))))))
          ((letcont letcont-rec)
           (let* ((b (car (cadr t)))
                  (k (car b)))
             ;; A live cell not held here is a parameter
             (for-each (lambda (c)
                         (unless (or (assq c env) (memq c (passed-cells k)))
                           (hashtable-set! passed k (cons c (passed-cells k)))))
                       (hashtable-ref live k '()))
             (let* ((cs (filter (lambda (c) (memq c (passed-cells k))) (hashtable-ref live k '())))
                    (kept (filter (lambda (c) (not (memq c cs))) (hashtable-ref live k '())))
                    (vars (map fresh cs)))
               (hashtable-set! passed k cs)
               (hashtable-set! held k (map (lambda (c) (cons c (value c env))) kept))
               (hashtable-set! params k (cadr b))
               `(,(car t) ((,k ,(append (cadr b)
                                        (map (lambda (c x)
                                               (if (eq? (hashtable-ref kinds c 'val) 'raw) `(raw ,x) x))
                                             cs vars))
                               ,(walk (caddr b) (append (map cons cs vars)
                                                        (hashtable-ref held k '())))))
                 ,(walk (caddr t) env)))))
          ((args) `(args ,(cadr t) ,(walk (caddr t) env)))
          ((if)
           (let ((test `(,(caadr t) ,@(map sub (cdadr t)))))
             (direct (caddr t) env
                     (lambda (k1)
                       (direct (cadddr t) env
                               (lambda (k2) `(if ,test ,k1 ,k2)))))))
          ((throw) `(throw ,(cadr t) ,@(map sub (cddr t)) ,@(extra-atoms (cadr t) env)))
          ((app)
           (direct (cadddr t) env
                   (lambda (k)
                     `(app ,(cadr t) ,(sub (caddr t)) ,k ,@(map sub (cddddr t))))))
          ((selfcall) `(selfcall ,@(map sub (cdr t))))
          (else t)))

      (let loop ()
        (set! changed? #f)
        (set! subst (make-eq-hashtable))
        (set! held (make-eq-hashtable))
        (let ((term (walk (fun-term fun) '())))
          (if changed?
              (loop)
              `(,@(list-head fun 3) ,term)))))))
