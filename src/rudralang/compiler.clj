(ns rudralang.compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.core.match :refer [match]]
            [rudralang.util :as u]))

(declare compile)

(defn destructuring-bind
  [[lhs-type & lhs-ks] rhs]
  (case lhs-type
    :symbol
    (list (list (first lhs-ks) (compile rhs)))

    :vector
    (let [rhs-name (gensym)]
      (list*
       (list rhs-name (compile rhs))
       (apply
        concat
        (map-indexed (fn [idx k]
                       (destructuring-bind
                        k
                        [:form
                         [:symbol 'nth]
                         [:symbol rhs-name]
                         [:number idx]]))
                     lhs-ks))))

    :map
    (let [rhs-name (gensym)]
      (list*
       (list rhs-name (compile rhs))
       (map (fn [k]
              (let [k-sym (compile k)]
                (list k-sym (list 'get rhs-name (symbol (str "':" k-sym))))))
            lhs-ks)))))

(defn compile-do
  [exp]
  (match exp
   [:do]
   '(begin)

   [:do & [[:symbol 'let] lhs [:keyword :=] rhs & others]]
   (list 'let* (destructuring-bind lhs rhs)
         (compile (vec (cons :do others))))

   [:do & [[:symbol 'let] & others]]
   (u/throw+ "Syntax error - let should be of the form:\n"
             "  let name := value")

   [:do e]
   (compile e)

   [:do & [e & others]]
   (list 'begin
         (compile e)
         (compile (vec (cons :do others))))))

(defn compile-fn
  [[_ & args]]
  (let [[argv & body] args
        [argv-type & argv-args] argv]
    (case argv-type
      :symbol
      (list 'lambda (first argv-args)
            (compile (vec (cons :do body))))

      :vector
      (let [argv-name (gensym)]
        (list 'lambda argv-name
              (compile
               (u/concatv
                [:do
                 [:symbol 'let]
                 (vec (cons :vector argv-args))
                 [:keyword :=]
                 [:symbol argv-name]]
                body)))))))

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

   [:do & _]
   (compile-do exp)

   [:fn & _]
   (compile-fn exp)

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

       fn
       (recur (vec (cons :fn args)))

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
       (recur (vec (cons :do args)))

       ;; default
       (cons op (map compile args))))))
