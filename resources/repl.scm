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
  (let* ((_ (system (str "lein -compile-exp '" rudra-code "' >/dev/null 2>&1")))
         (port (open-input-file ".rudra-repl"))
         (scheme-code (get-string-all port)))
    (close-input-port port)
    (system "rm .rudra-repl >/dev/null 2>&1")
    (read-string scheme-code)))

(define (rudra-eval rudra-code)
  (eval (rudra-read rudra-code)))

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
           (_ (println! "evaluating..."))
           (res (rudra-eval in)))
      (when (not (same? (void) res))
        (println! res))))
  (rudra-repl))

(rudra-repl)
