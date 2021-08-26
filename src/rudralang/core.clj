(ns rudralang.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [rudralang.compiler :as compiler]
            [rudralang.parser :as parser]))

(defn -main
  [& [filepath]]
  (let [{:keys [chez-exe-path target-path]}
        (edn/read-string (slurp "./config.edn"))

        source-path (->> (str/split filepath #"/")
                         butlast
                         (str/join "/"))

        target-path (if (= :same-as-source target-path)
                      source-path
                      target-path)

        native-compile-cmd (str chez-exe-path "/compile-chez-program")

        filename-without-ext (-> filepath
                                 (str/split #"/")
                                 last
                                 (str/split #"\.")
                                 first)

        scheme-filename (str target-path "/" filename-without-ext ".scm")

        prelude (slurp (io/resource "prelude.scm"))
        postlude (slurp (io/resource "postlude.scm"))]
    (println "parsing" filepath)
    (let [AST (parser/parse (slurp filepath))
          _ (println "compiling to Scheme")
          scheme-code (compiler/compile AST)]
      (println "writing" scheme-filename)
      (spit scheme-filename
            (str prelude
                 "\n"
                 (with-out-str (pprint/pprint scheme-code))
                 "\n"
                 postlude)))

    (println "compiling with chez-exe")
    (let [{:keys [err out]}
          (sh native-compile-cmd "--optimize-level" "3" scheme-filename)]
      (println (if (seq err) err out)))

    (shutdown-agents)))
