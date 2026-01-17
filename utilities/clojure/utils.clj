(ns utils)

;; Common utility functions for Clojure solutions

(defn read-input [file-path]
  (->> file-path
       slurp
       clojure.string/trim
       clojure.string/split-lines))

(defn read-input-raw [file-path]
  (slurp file-path))

(defn parse-ints [line]
  (->> line
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))))

;; Placeholder for additional common utilities
