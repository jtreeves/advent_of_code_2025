;; Placeholder for Day 01 Clojure solution
;; Note: Clojure utility functions would be in utilities/clojure/get_input.clj
;; For now, using inline function - will be replaced with proper requires

(load-file "../../../utilities/clojure/get_input.clj")

(defn solve [input-data]
  (println "Day 01 Clojure placeholder")
  (let [lines (clojure.string/split-lines (clojure.string/trim input-data))]
    ;; Part 1
    (let [part1-result "TODO"]
      ;; Part 2
      (let [part2-result "TODO"]
        [part1-result part2-result]))))

(defn -main []
  ;; Use utility function to get input
  (let [data (read-input-raw "../data/input.txt")
        [part1 part2] (solve data)]
    (println (str "Part 1: " part1))
    (println (str "Part 2: " part2))))

(-main)
