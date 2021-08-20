(ns rudralang.compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.core.match :refer [match]]))

(defn compile
  [exp]
  (match exp
   [:boolean "true"]
   (symbol "#t")

   [:boolean "false"]
   (symbol "#f")

   [:number n]
   n

   [:keyword k]
   (symbol (str "':" (name k)))

   [:symbol s]
   s

   [:string s]
   s

   [:vector & xs]
   (cons 'vector
         (map compile xs))

   [:map & kvs]
   (cons 'list-map
         (partition-all 2 (map compile kvs)))

   [:fn & args]
   (let [[argv & body] args
         [argv-type & argv-args] argv
         body (cons 'begin (map compile body))]
     (case argv-type
       :symbol
       (list 'lambda (first argv-args) body)

       :vector
       (list 'lambda (map compile argv-args) body)))

   [:form op & args]
   (let [op (compile op)]
     (case op
       ns!
       (let [[name opts & exps] (map compile args)
             name-sym (symbol (str "'" name))]
         (list
          'begin
          (list 'rudra-set-ns! name-sym)
          (list 'rudra-set-ns-opts! name-sym opts)
          (cons 'begin exps)))

       def!
       (let [[name opts val] (map compile args)
             name-sym (symbol (str "'" name))]
         (list
          'begin
          (list 'define name val)
          #_(list 'rudra-set-def! name val)
          (list 'rudra-set-def-opts! name-sym opts)))

       defn!
       (let [[name opts argv & body] args]
         (recur [:form
                 [:symbol 'def!]
                 name
                 opts
                 (vec (list* :fn argv body))]))

       cond
       (cons
        'cond
        (let [opts (partition-all 2 (map compile args))
              last-opt (last opts)
              last-opt-test (first last-opt)]
          (if (= (symbol "':else")
                 last-opt-test)
            (concat (butlast opts)
                    [(list 'else (last last-opt))])
            opts)))

       do
       (cons 'begin (map compile args))

       ;; default
       (cons op (map compile args))))))
