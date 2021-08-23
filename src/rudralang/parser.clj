(ns rudralang.parser
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as insta]))

(def parse*
  (insta/parser
   (slurp (io/resource "grammar.ebnf"))))

(defn tag
  [tag transformer]
  (fn [& args]
    (let [transformed (apply transformer args)]
      [tag transformed])))

(defn transform-string
  [& args]
  (->> args
       (map last)
       str/join))

(defn transform-infix-operation
  [x-node [_ op] y-node]
  [:form
   [:symbol (symbol op)]
   x-node
   y-node])

(def transform-options
  {:exp     identity
   :number  (tag :number read-string)
   :keyword (tag :keyword (comp keyword transform-string))
   :symbol  (tag :symbol  (comp symbol transform-string))
   :string  (tag :string transform-string)
   :infix_operation transform-infix-operation})

(defn parse
  [text]
  (->> (parse* text)
       (insta/transform transform-options)))
