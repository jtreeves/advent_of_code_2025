(ns parse)

;; Parsing utilities for Clojure solutions

(defn parse-ints [line]
  "Parse integers from a line of text."
  (->> line
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))))
