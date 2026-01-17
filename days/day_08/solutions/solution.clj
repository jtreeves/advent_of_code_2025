(require '[clojure.java.io :as io])

;; Placeholder for Day 8 Clojure solution
(defn solve [input-data]
  (println "Day 8 Clojure placeholder")
  (let [lines (clojure.string/split-lines (clojure.string/trim input-data))]
    ;; Part 1
    (let [part1-result "TODO"]
      ;; Part 2
      (let [part2-result "TODO"]
        [part1-result part2-result]))))

(defn -main []
  (let [data (slurp "../data/input.txt")
        [part1 part2] (solve data)]
    (println (str "Part 1: " part1))
    (println (str "Part 2: " part2))))

(-main)
