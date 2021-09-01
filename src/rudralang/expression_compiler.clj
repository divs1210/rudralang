(ns rudralang.expression-compiler
  (:gen-class)
  (:require [rudralang.compiler :as compiler]
            [rudralang.parser :as parser]))

(defn -main
  [in-file out-file]
  (spit out-file
        (try
          (compiler/compile (parser/parse (slurp in-file)))
          (catch Throwable e
            (str "(println! " (pr-str (.getMessage e)) ")")))))
