(ns rudralang.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [rudralang.compiler :as compiler]
            [rudralang.parser :as parser]))

(defn compile-raw-string
  [s]
  (-> s parser/parse compiler/compile))

(defn -main
  [& [filename]]
  (let [{:keys [chez-exe-path]}
        (edn/read-string (slurp "./config.edn"))

        native-compile-cmd
        (str chez-exe-path "/compile-chez-program")

        raw-code (slurp filename)
        filename-without-ext (first (str/split filename #"\."))
        scheme-filename (str filename-without-ext ".scm")

        prelude (slurp (io/resource "prelude.scm"))
        postlude (slurp (io/resource "postlude.scm"))]
    (let [_   (println "parsing" filename)
          AST (parser/parse raw-code)]
      (println "writing" scheme-filename)
      (spit scheme-filename
            (str prelude
                 (compiler/compile AST)
                 postlude)))

    (println "compiling with chez-exe")
    (let [res (sh native-compile-cmd "--optimize-level" "3" scheme-filename)
          out (:out res)
          out (if (seq out)
                out
                (:err res))]
      (println out))

    (shutdown-agents)))

(comment
  (def code
    (slurp "./samples/fact.rudra"))

  (compile-raw-string code)
  )
