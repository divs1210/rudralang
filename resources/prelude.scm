(import (rename (scheme)
                (assoc scheme-assoc)
                (merge scheme-merge)
                (div   scheme-div)
                (equal? scheme=)
                (atom? scheme-atom?)))

;; ## Internal
;; ==========
(define (type* x)
  (cond
   ((boolean? x)
    'Boolean)
   ((number? x)
    'Number)
   ((char? x)
    'Char)
   ((keyword? x)
    'Keyword)
   ((symbol? x)
    'Symbol)
   ((string? x)
    'String)
   ((fn? x)
    'Fn)
   ((atom? x)
    'Atom)
   ((map? x)
    'Map)
   ((list? x)
    'List)
   ((pair? x)
    'Pair)
   (else
    '<unknown>)))

(define (is-a? type x)
  (scheme= type (type* x)))

(define (any? x)
  #t)

(define (unknown? x)
  (is-a? '<unknown> x))

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

(define (same? x y)
  (eq? x y))

(define (equal? x y)
  (let ((t (type* x)))
    (if (not (scheme= t (type* y)))
        #f
        (case t
          ((Fn Atom)
           (same? x y))
          (Map
           (and
            (scheme= (length x) (length y))
            (every? (lambda (pair)
                      (let ((k (car pair))
                            (v (cdr pair)))
                        (and
                         (contains? y k)
                         (equal? v (get y k))
                         #t)))
                    x)))
          (List
           (let loop ((xs x)
                      (ys y))
             (cond
              ((and (null? xs) (null? ys))
               #t)
              ((or (null? xs) (null? ys))
               #f)
              (else
               (if (equal? (first xs)
                           (first ys))
                   (loop (rest xs)
                         (rest ys))
                   #f)))))
          (Pair
           (and
            (equal? (car x) (car y))
            (equal? (cdr x) (cdr y))))
          (else
           (scheme= x y))))))

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

(define (keyword? k)
  (and
   (symbol? k)
   (scheme= #\: (string-ref (symbol->string k) 0))))

(define (name sym-or-kw)
  (if (symbol? sym-or-kw)
      (let ((str (symbol->string sym-or-kw)))
        (if (equal? #\: (string-ref str 0))
            (list->string (rest (string->list str)))
            str))
      (raise! "non symbol/keyword passed to name")))

;; ## Seq
;; ======
(define (first xs)
  (if (null? xs)
      null
      (car xs)))

(define (rest xs)
  (if (null? xs)
      null
      (cdr xs)))

(define nth list-ref)

(define (take n xs)
  (if (> n 0)
      (append (list (first xs))
              (take (- n 1) (rest xs)))
      null))

(define (drop n xs)
  (if (> n 0)
      (drop (- n 1) (rest xs))
      xs))

(define (every? pred xs)
  (if (null? xs)
      #t
      (if (pred (car xs))
          (every? pred (cdr xs))
          #f)))

(define (find-first pred xs)
  (if (null? xs)
      null
      (let ((x (first xs)))
        (if (pred x)
            x
            (find-first pred (rest xs))))))

(define (reduce f init coll)
  (if (null? coll)
      init
      (reduce f
              (f init (car coll))
              (cdr coll))))

(define (range start end step)
  (let ((compare (if (positive? step) < >))
        (update  (lambda (x) (+ x step))))
    (if (compare start end)
        (cons start (range (update start) end step))
        null)))

;; ## Maps
;; =======
(define-syntax list-map
  (syntax-rules ()
    ((_ (k1 v1) ...)
     (list (cons k1 v1) ...))))

(define (map? m)
  (and (list? m)
       (every? pair? m)
       ;; check if list of maps
       (if (scheme= 1 (length m))
           (not (map? (car m)))
           #t)))

(define (keys m)
  (map car m))

(define (vals m)
  (map cdr m))

(define (get m k)
  (let ((pair (find-first (lambda (pair)
                            (equal? k (first pair)))
                          m)))
    (rest pair)))

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

(define (atom? x)
  (box? x))

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

;; ## Functions
;; ============
(define fn? procedure?)

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
