(import (rename (scheme)
                (assoc scheme-assoc)
                (merge scheme-merge)
                (div   scheme-div)
                (equal? scheme-equal?)
                (atom? scheme-atom?)))

;; ## Internal
;; ==========
(define Boolean 'Boolean)
(define Number  'Number)
(define Char    'Char)
(define Keyword 'Keyword)
(define Symbol  'Symbol)
(define String  'String)
(define Fn      'Fn)
(define Atom    'Atom)
(define Map     'Map)
(define List    'List)
(define Pair    'Pair)
(define <unknown> '<unknown>)
(define <default> '<default>)

(define (type* x)
  (cond
   ((number? x)
    Number)
   ((boolean? x)
    Boolean)
   ((char? x)
    Char)
   ((keyword? x)
    Keyword)
   ((symbol? x)
    Symbol)
   ((string? x)
    String)
   ((fn? x)
    Fn)
   ((atom? x)
    Atom)
   ((map? x)
    Map)
   ((list? x)
    List)
   ((pair? x)
    Pair)
   (else
    <unknown>)))

;; Util
;; ====
(define (identity x)
  x)

(define (comp . fs)
  (cond
   ((null? fs)
    identity)
   ((scheme-equal? 1 (length fs))
    (car fs))
   (else
    (lambda args
      (let* ((f (last fs))
             (acc (apply f args)))
        ((apply comp (drop-last 1 fs)) acc))))))

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
    (case t
      ((Fn Atom)
       (same? x y))
      (Map
       (and
        (scheme-equal? (length x) (length y))
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
       (scheme-equal? x y)))))

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

   (else (raise! (str "non string/symbol " s " passed to keyword")))))

(define (keyword? k)
  (and
   (symbol? k)
   (scheme-equal? #\: (string-ref (symbol->string k) 0))))

(define (name sym-or-kw)
  (if (symbol? sym-or-kw)
      (let ((str (symbol->string sym-or-kw)))
        (if (equal? #\: (string-ref str 0))
            (list->string (rest (string->list str)))
            str))
      (raise! (str "non symbol/keyword " sym-or-kw " passed to name"))))

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

(define (last xs)
  (cond
   ((null? xs)
    null)
   ((null? (cdr xs))
    (car xs))
   (else
    (last (cdr xs)))))

(define (take n xs)
  (if (> n 0)
      (append (list (first xs))
              (take (- n 1) (rest xs)))
      null))

(define (drop n xs)
  (if (> n 0)
      (drop (- n 1) (rest xs))
      xs))

(define (drop-last n xs)
  (reverse (drop n (reverse xs))))

(define (every? pred xs)
  (if (null? xs)
      #t
      (if (pred (car xs))
          (every? pred (cdr xs))
          #f)))

(define (find-first pred xs)
  (if (null? xs)
      null
      (let ((x (car xs)))
        (if (pred x)
            x
            (find-first pred (cdr xs))))))

(define (list-contains? xs x)
  (if (null? xs)
      #f
      (let ((y (car xs)))
        (if (equal? x y)
            #t
            (list-contains? (cdr xs) x)))))

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
(define (pair x y)
  (cons x y))

(define map-type-tag
  (pair '<map> #t))

(define-syntax list-map
  (syntax-rules ()
    ((_ (k1 v1) ...)
     (list
      map-type-tag
      (cons k1 v1)
      ...))))

(define (map? m)
  (and (list? m)
       (every? pair? m)
       (list-contains? m map-type-tag)))

(define (keys m)
  (map car (dissoc m '<map>)))

(define (vals m)
  (map cdr (dissoc m '<map>)))

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
  (let recur ((m (list-map))
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
  (display (apply str args)))

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

(define % mod)

(define ** pow)

(define (== x y)
  (if (number? x)
      (scheme-equal? x y)
      (raise! (str "non-number " x " passed to =="))))

(define lt <)

(define gt >)

(define lte <=)

(define gte >=)

;; ## Functions
;; ============
(define fn? procedure?)


;; ## Types and Protocols
;; ======================
(define (type x)
  (let ((t (type* x)))
    (if (and (scheme-equal? Map t)
             (contains? x '<type>))
        (get x '<type>)
        t)))

(define (methods protocol)
  (deref (get protocol 'methods)))

(define (add-method! protocol method)
  (let ((methods-atom (get protocol 'methods)))
    (when (not (list-contains? (deref methods-atom)
                               method))
        (swap! methods-atom
               (lambda (ms)
                 (cons method ms))))))

(define (implementors protocol method)
  (get (deref (get protocol 'implementors))
       method))

(define (add-implementor! protocol method type)
  (let* ((implementors-atom (get protocol 'implementors))
         (implementors-of-method (implementors protocol method)))
    (when (not (list-contains? implementors-of-method type))
      (swap! implementors-atom
             update method
             (lambda (ts)
               (cons type ts))))))

(define (implementation protocol method type)
  (let ((impl (get-in (deref (get protocol 'implementations))
                       (list method type))))
    (cond
     ((and (null? impl)
           (scheme-equal? '<default> type))
      (raise! (string-append
               "No default method "
               (name method)
               " of protocol "
               (name (get protocol 'name))
               " defined.")))
     ((null? impl)
      (implementation protocol method '<default>))
     (else
      impl))))

(define (add-implementation! protocol method type fn)
  (swap! (get protocol 'implementations)
         assoc-in (list method type) fn))

(define (method* protocol method-name)
  (lambda args
    (let* ((f (implementation protocol method-name (type (first args)))))
      (apply f args))))

(define-syntax method
  (syntax-rules ()
    ((_ protocol method)
     (method* protocol (quote method)))))

(define Protocol 'Protocol)

(define-syntax define-protocol
  (syntax-rules ()
    ((_ proto-name)
     (define proto-name
       (list-map
        ('<type> Protocol)
        ('name (quote proto-name))
        ('methods (atom null))
        ('implementors (atom (list-map)))
        ('implementations (atom (list-map))))))))

(define-syntax implement-method!
  (syntax-rules ()
    ((_ protocol method _type fn)
     (begin
       (add-method! protocol (quote method))
       (add-implementor! protocol (quote method) _type)
       (add-implementation! protocol (quote method) _type fn)))))


;; Rudra core methods
;; ==================
(define-protocol IRudra)

;; ->list
;; ======
(implement-method!
 IRudra ->list List
 identity)

(implement-method!
 IRudra ->list Map
 (lambda (m)
   (dissoc m '<map>)))

(implement-method!
 IRudra ->list String
 string->list)

(implement-method!
 IRudra ->list Pair
 (lambda (this)
   (list (car this) (cdr this))))

(define ->list
  (method IRudra ->list))

;; ->string
;; ========
(implement-method!
 IRudra ->string String
 identity)

(implement-method!
 IRudra ->string Boolean
 (lambda (this)
   (if this "true" "false")))

(implement-method!
 IRudra ->string Number
 number->string)

(implement-method!
 IRudra ->string Char
 string)

(implement-method!
 IRudra ->string Char
 string)

(implement-method!
 IRudra ->string Keyword
 symbol->string)

(implement-method!
 IRudra ->string Symbol
 symbol->string)

(implement-method!
 IRudra ->string Fn
 (lambda _ "<Fn>"))

(implement-method!
 IRudra ->string Atom
 (lambda (this)
   (str "<Atom(" (deref this) ")>")))

(implement-method!
 IRudra ->string Map
 (lambda (this)
   (str
    "{"
    (let loop ((pairs (->list this))
               (acc ""))
      (if (null? pairs)
          acc
          (let* ((p (car pairs))
                 (k (car p))
                 (v (cdr p))
                 (pairs (cdr pairs))
                 (delim (if (null? pairs)
                            ""
                            ", ")))
            (loop pairs
                  (str acc k " " v delim)))))
    "}")))

(implement-method!
 IRudra ->string List
 (lambda (this)
   (str
    "["
    (let loop ((xs this)
               (acc ""))
      (if (null? xs)
          acc
          (let* ((x  (car xs))
                 (xs (cdr xs))
                 (delim (if (null? xs)
                            ""
                            ", ")))
            (loop xs
                  (str acc x delim)))))
    "]")))

(implement-method!
 IRudra ->string Pair
 (lambda (this)
   (str "(" (car this) " . " (cdr this) ")")))

(implement-method!
 IRudra ->string <default>
 (lambda (obj)
   (if (and (map? obj)
            (contains? obj '<type>))
       (let ((impl (implementation IRudra '->string Map)))
         (impl obj))
       "<unknown>")))

(define ->string
  (method IRudra ->string))

(define (str . xs)
  (apply string-append
         (map ->string xs)))


;; # User code
;; ===========
