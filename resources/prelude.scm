(import (rename (scheme)
                (assoc scheme-assoc)
                (merge scheme-merge)
                (div   scheme-div)))

;; # Internal
;; ==========

;; ## Constants
;; ============
(define null
  '())

;; ## Errors
;; =========
(define (raise! ex-str)
  (raise
   (condition
    (make-error)
    (make-message-condition ex-str))))

;; ## Boolean
;; ==========
(define (falsey? x)
  (or (not x)
      (null? x)))

(define (truthy? x)
  (not (falsey? x)))

;; ## Symbols and Keywords
;; =======================
(define (string->keyword s)
  (string->symbol (string-append ":" s)))

(define (symbol->keyword s)
  (string->keyword (symbol->string s)))

(define (keyword s)
  (cond
   ((symbol? s)
    (symbol->keyword s))

   ((string? s)
    (string->keyword s))

   (else (raise! "non string/symbol passed to keyword"))))

;; ## Seq
;; ======
(define first car)

(define rest cdr)

(define nth list-ref)

(define (every? xs pred)
  (if (null? xs)
      #t
      (if (pred (car xs))
          (every? (cdr xs) pred)
          #f)))

(define (reduce f init coll)
  (if (null? coll)
      init
      (reduce f
              (f init (car coll))
              (cdr coll))))

(define (range start end)
  (let loop ((res '())
             (end (- end 1)))
    (if (< end start)
        res
        (loop (cons end res)
              (- end 1)))))

;; ## Maps
;; =======
(define-syntax list-map
  (syntax-rules ()
    ((_ (k1 v1) ...)
     (list (cons k1 v1) ...))))

(define (map? m)
  (and (list? m)
       (every? m pair?)))

(define (keys m)
  (map car m))

(define (vals m)
  (map cdr m))

(define (get m k)
  (let ((cell (scheme-assoc k m)))
    (if cell
        (cdr cell)
        '())))

(define (get-in m ks)
  (if (null? ks)
      m
      (get-in (get m (car ks))
              (cdr ks))))

(define (assoc* m k v)
  (cons (cons k v) m))

(define (assoc m k v)
  (assoc* (dissoc m k)
          k v))

(define (assoc-in m ks v)
  (let ((k  (car ks))
        (ks (cdr ks)))
    (if (null? ks)
        (assoc m k v)
        (assoc m k (assoc-in (get m k)
                             ks
                             v)))))

(define (update-in m ks f . args)
  (assoc-in m ks (apply f (get-in m ks) args)))

(define (update m k f . args)
  (apply update-in m (list k) f args))

(define (dissoc m k)
  (let loop ((new-m '())
             (remaining-pairs m))
    (if (null? remaining-pairs)
        new-m
        (let* ((cell (car remaining-pairs))
               (cell-k (car cell))
               (cell-v (cdr cell)))
          (if (equal? k cell-k)
              (loop new-m
                    (cdr remaining-pairs))
              (loop (assoc* new-m cell-k cell-v)
                    (cdr remaining-pairs)))))))

(define (contains? m k)
  (let loop ((ks (keys m)))
    (if (null? ks)
        #f
        (if (equal? k (car ks))
            #t
            (loop (cdr ks))))))

(define (zipmap ks vs)
  (let recur ((m '())
              (ks ks)
              (vs vs))
    (if (null? ks)
        m
        (recur (assoc m (car ks) (car vs))
               (cdr ks)
               (cdr vs)))))

(define (merge m1 m2)
  (let* ((ks (append (keys m1) (keys m2)))
         (vs (append (vals m1) (vals m2))))
    (zipmap ks vs)))

;; ## Atoms
;; ========
(define (atom x)
  (box x))

(define (deref atm)
  (unbox atm))

(define (swap! atm f . args)
  (let* ((x (deref atm))
         (v (apply f (cons x args))))
    (if (box-cas! atm x v)
        v
        (apply swap! atm f args))))

(define (reset! atm x)
  (swap! atm (lambda (_) x)))

;; ## IO
;; =====
(define (print! . args)
  (for-each display args))

(define (println! . args)
  (apply print! args)
  (newline))

;; ## Math
;; =======
(define add +)

(define sub -)

(define mul *)

(define div /)

(define pow expt)

(define ** pow)


;; # Core
;; ======
(define rudra-current-ns
  (atom 'rudra.core))

(define (rudra-set-ns! name)
  (reset! rudra-current-ns name))

(define (rudra-set-ns-opts! ns opts)
  (void))

(define rudra-env
  (atom (list-map)))

(define (rudra-extend-env env bindings)
  (atom (merge (list-map
                ('__parent__ env))
               bindings)))

(define (rudra-lookup-env _env ns k)
  (let ((env (deref _env)))
    (cond
     ((contains? (deref env) k)
      (get env (list ns k)))

     ((contains? env '__parent__)
      (rudra-lookup-env (get env '__parent__) ns k))

     ((contains? env (list 'rudra.core k))
      (rudra-lookup-env env (list 'rudra.core k)))

     (else (raise! (string-append "Not defined: " (symbol->string k)))))))

(define (rudra-def! name x)
  (let ((ns (deref rudra-current-ns)))
    (swap! rudra-env
           (lambda (e)
             (assoc e (list ns name))))))

(define (rudra-set-def-opts! name x)
  (void))


;; # User code
;; ===========
