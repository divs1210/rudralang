(ns rudralang.expression-compiler
  (:gen-class)
  (:require [rudralang.compiler :as compiler]
            [rudralang.parser :as parser]))

(defn -main
  [exp-str]
  (spit ".rudra-repl"
        (try
          (compiler/compile (parser/parse exp-str))
          (catch Throwable e
            (str "(println! " (pr-str (.getMessage e)) ")")))))
