(ns rudralang.compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.core.match :refer [match]]
            [rudralang.util :as u]))

(defn- rudra-get
  [m k not-found]
  (let [kvs (rest m)]
    (u/after kvs k not-found)))

(declare compile)

(defn destructuring-bind
  [[lhs-type & lhs-ks] rhs]
  (case lhs-type
    :symbol
    (list (list (first lhs-ks) (compile rhs)))

    :list
    (let [rhs-name-node (u/after lhs-ks [:keyword :as] ::not-found)
          rhs-name (if (= ::not-found rhs-name-node)
                     (gensym)
                     (last rhs-name-node))
          lhs-ks (if (= ::not-found rhs-name-node)
                   lhs-ks
                   (drop-last 2 lhs-ks))
          rest-name-node (u/after lhs-ks [:keyword :and] ::not-found)
          rest-name (when (not= ::not-found rest-name-node)
                      (last rest-name-node))
          lhs-ks (if (= ::not-found rest-name-node)
                   lhs-ks
                   (drop-last 2 lhs-ks))
          rest-rhs (when (some? rest-name)
                     (list 'drop (count lhs-ks) rhs-name))]
      (list*
       (list rhs-name (compile rhs))
       (apply
        concat
        (when (some? rest-name)
          (list (list rest-name rest-rhs)))
        (map-indexed (fn [idx k]
                       (destructuring-bind
                        k
                        [:form
                         [:symbol 'list-get]
                         [:symbol rhs-name]
                         [:number idx]]))
                     lhs-ks))))

    :map
    (let [rhs-name-node (u/after lhs-ks [:keyword :as] ::not-found)
          rhs-name (if (= ::not-found rhs-name-node)
                     (gensym)
                     (last rhs-name-node))
          lhs-ks (if (= ::not-found rhs-name-node)
                   lhs-ks
                   (drop-last 2 lhs-ks))
          default-map-node (u/after lhs-ks [:keyword :or] ::not-found)
          default-bindings (when (not= ::not-found default-map-node)
                             (for [[k v] (partition-all 2 (rest default-map-node))
                                   :let [k (compile k)
                                         v (compile v)]]
                               (list
                                (list k (list 'if (list 'contains?
                                                        rhs-name
                                                        (symbol (str "':" k)))
                                              k
                                              v)))))
          lhs-ks (if (= ::not-found default-map-node)
                   lhs-ks
                   (drop-last 2 lhs-ks))]
      (apply
       concat
       (list (list rhs-name (compile rhs)))
       (map (fn [k]
              (let [k-sym (compile k)]
                (list k-sym (list 'map-get rhs-name (symbol (str "':" k-sym))))))
            lhs-ks)
       default-bindings))))

(defn compile-letfn
  [exp]
  (match exp
    [:do & [[:symbol 'letfn] [:symbol name] [:keyword :=] argv body & others]]
    (loop [bindings (list (list name (compile [:fn argv body])))
           [next-node & more :as remaining] others]
      (if (= [:keyword :and] next-node)
        (let [[[_ name] _ argv body & others] more]
          (recur (concat bindings
                         [(list name (compile [:fn argv body]))])
                 others))
        (list 'letrec bindings
              (compile (vec (cons :do remaining))))))))

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

   [:do & [[:symbol 'letfn] & _]]
   (compile-letfn exp)

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

      :list
      (let [argv-name (gensym)]
        (list 'lambda argv-name
              (compile
               (u/concatv
                [:do
                 [:symbol 'let]
                 (vec (cons :list argv-args))
                 [:keyword :=]
                 [:symbol argv-name]]
                body)))))))

(defn compile-loop
  [args]
  (let [[[_ & binding-nodes] & body] args
        bindings-list-name (gensym)
        binding-nodes (partition-all 3 binding-nodes)]
    (list
     'let 'recur (list
                  (list bindings-list-name
                        (compile
                         (u/concatv
                          [:form
                           [:symbol 'list]]
                          (for [[_ _ rhs-node] binding-nodes]
                            rhs-node)))))
     (list 'let* (destructuring-bind
                  (u/concatv
                   [:list]
                   (for [[lhs-node _ _] binding-nodes]
                     lhs-node))
                  [:symbol bindings-list-name])
           (compile (vec (cons :do body)))))))

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

   [:list & xs]
   (cons 'list
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
       (let [[_ _ & exps] args]
         (compile
          (vec
           (list*
            :form
            [:symbol 'do]
            exps))))

       declare!
       (let [syms (map compile args)]
         (cons
          'begin
          (map #(list 'define %
                      (list 'method 'IRudra %))
               syms)))

       defn!
       (let [[name opts argv & body] args
             for (rudra-get opts
                            [:keyword :for]
                            [:symbol '<default>])]
         (recur [:form
                 [:symbol 'implement-method!]
                 [:symbol 'IRudra]
                 name
                 [:symbol (compile for)]
                 (vec (list* :fn argv body))]))

       fn
       (recur (vec (cons :fn args)))

       loop
       (compile-loop args)

       recur
       (list 'recur
             (compile (u/concatv
                       [:form
                        [:symbol 'list]]
                       args)))

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

       quote
       (do
         (assert (= 1 (count args))
                 "more than 1 args passed to quote")
         (let [[arg] args
               [tag & xs] arg]
           (case tag
             :list
             (->> xs
                  (map #(vector :form [:symbol 'quote] %))
                  (u/concatv [:list])
                  compile)

             :map
             (->> xs
                  (map #(vector :form [:symbol 'quote] %))
                  (u/concatv [:map])
                  compile)

             (:symbol :form)
             (list 'quote (compile arg))

             ;; default
             (compile arg))))

       scheme
       (do
         (assert (= 1 (count args))
                 "more than 1 string passed to scheme")
         (let [[arg] args
               [tag s] arg]
           (assert (= :string tag)
                   "non string literal passed to scheme")
           (symbol s)))

       ;; default
       (cons op (map compile args))))))
