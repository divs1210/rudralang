;; gives the input expression to the Clojure-based compiler,
;; which returns a Scheme expression,
;; that is evaluated, and the result printed

(load "./resources/prelude.scm")

(define (read-string scheme-code)
  (let* ((port (open-input-string scheme-code))
         (scheme-exp (read port)))
    (close-input-port port)
    scheme-exp))

(define (rudra-read rudra-code)
  (println! "compiling...")
  (let* ((_ (spit-file! ".rudra-repl-in.rudra" rudra-code))
         (_ (system (str "lein -compile-exp .rudra-repl-in.rudra .rudra-repl-out.scm >/dev/null 2>&1")))
         (scheme-code (slurp-file ".rudra-repl-out.scm")))
    (delete-file! ".rudra-repl-out.scm")
    (read-string scheme-code)))

(define (rudra-eval rudra-code)
  (let ((scheme-exp (rudra-read rudra-code)))
    (println! "evaluating...")
    (eval scheme-exp)))

;; read text from stdin up till newline
(define (read-line)
  (get-line (current-input-port)))

;; read text from stdin till newline
;; and keep reading if it ends with a comma
;; or is empty
(define (read-lines)
  (let loop ((acc (read-line)))
    (let ((chars (string->list acc)))
      (if (or (zero? (length chars))
              (ends-with? chars '(#\,))
              (ends-with? chars '(#\())
              (ends-with? chars '(#\= #\>)))
          (loop (str acc (read-line)))
          acc))))

(define (rudra-repl)
  (print! "rudra> ")
  (guard (ex (else
              (display-condition ex)
              (newline)))
    (let* ((in (read-lines))
           (res (rudra-eval in)))
      (when (not (same? (void) res))
        (pprint! res))))
  (rudra-repl))

(rudra-repl)
