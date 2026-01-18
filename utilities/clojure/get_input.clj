(ns get-input)

;; Input reading utilities for Clojure solutions

(defn get-input [day]
  "Read input from input.txt for the given day."
  (let [path (get-input-path day)]
    (read-input path)))

(defn get-test-input [day test-num]
  "Read input from test_N.txt for the given day and test number."
  (let [path (get-test-input-path day test-num)]
    (read-input path)))

(defn get-input-path [day]
  "Return the path to input.txt for the given day."
  "../data/input.txt")

(defn get-test-input-path [day test-num]
  "Return the path to test_N.txt for the given day and test number."
  (str "../data/test_" test-num ".txt"))

(defn read-input [file-path]
  "Read input file and return lines as a list."
  (->> file-path
       slurp
       clojure.string/trim
       clojure.string/split-lines))

(defn read-input-raw [file-path]
  "Read input file and return raw content."
  (slurp file-path))
