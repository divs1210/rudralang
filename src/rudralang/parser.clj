(ns rudralang.parser
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [rudralang.util :as u]))

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
  [x-node op y-node]
  [:form op x-node y-node])

(def transform-options
  {:exp     identity
   :number  (tag :number read-string)
   :keyword (tag :keyword (comp keyword transform-string))
   :symbol  (tag :symbol  (comp symbol transform-string))
   :string  (tag :string transform-string)
   :infix_operation transform-infix-operation})

(defn parse
  [text]
  (let [parsed (insta/transform transform-options
                                (parse* text))]
    (if (map? parsed)
      (let [{:keys [line column text]} parsed
            pointer (str/join (concat (repeat (dec column) " ") ["^"]))]
        (u/throw+ "Parsing error at line: " line
                  ", column: " column
                  " in text:\n" text
                  "\n" pointer))
      parsed)))
