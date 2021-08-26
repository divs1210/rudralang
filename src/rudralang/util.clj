(ns rudralang.util
  (:require [clojure.string :as str]))

(defn throw+
  [& msgs]
  (throw (Exception. (str/join msgs))))

(defn concatv
  [& colls]
  (vec (apply concat colls)))

(defn after
  [xs v not-found]
  (if (seq xs)
    (let [[x & xs] xs]
      (if (= v x)
        (if (seq xs)
          (first xs)
          not-found)
        (recur xs v not-found)))
    not-found))
