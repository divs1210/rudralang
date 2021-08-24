(ns rudralang.util
  (:require [clojure.string :as str]))

(defn throw+
  [& msgs]
  (throw (Exception. (str/join msgs))))
