(require '[clojure.string :as str])

;; Import utilities
(defn read-input [path]
  (->> path
       slurp
       str/trim
       str/split-lines))

(defn solve [lines]
  ;; Build graph: device -> list of outputs
  (let [graph (reduce (fn [g line]
                        (let [parts (str/split line #":")
                              device (str/trim (first parts))
                              outputs-str (if (> (count parts) 1)
                                            (str/trim (second parts))
                                            "")
                              outputs (if (empty? outputs-str)
                                       []
                                       (str/split outputs-str #"\s+"))]
                          (assoc g device outputs)))
                      {}
                      lines)]
    
    ;; Part 1: Count paths from "you" to "out"
    (letfn [(count-paths-part1 [node memo]
              (if (= node "out")
                1
                (if (contains? @memo node)
                  (@memo node)
                  (let [neighbors (get graph node [])
                        count (reduce (fn [cnt neighbor]
                                        (+ cnt (count-paths-part1 neighbor memo)))
                                      0
                                      neighbors)]
                    (swap! memo assoc node count)
                    count))))]
      (let [part1-memo (atom {})]
        (if (contains? graph "you")
          (let [part1-count (count-paths-part1 "you" part1-memo)]
            ;; Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
            (letfn [(count-paths-part2 [node visited-fft visited-dac memo]
                      (if (= node "out")
                        (if (and visited-fft visited-dac) 1 0)
                        (let [key [node visited-fft visited-dac]]
                          (if (contains? @memo key)
                            (@memo key)
                            (let [new-visited-fft (or visited-fft (= node "fft"))
                                  new-visited-dac (or visited-dac (= node "dac"))
                                  neighbors (get graph node [])
                                  count (reduce (fn [cnt neighbor]
                                                  (+ cnt (count-paths-part2 neighbor new-visited-fft new-visited-dac memo)))
                                                0
                                                neighbors)]
                              (swap! memo assoc key count)
                              count)))))]
              (let [part2-memo (atom {})]
                (if (contains? graph "svr")
                  (let [part2-count (count-paths-part2 "svr" false false part2-memo)]
                    [(str part1-count) (str part2-count)])
                  ["0" "0"]))))
          ["0" "0"])))))

(let [lines (read-input "../data/input.txt")
      [part1 part2] (solve lines)]
  (println (str "Part 1: " part1))
  (println (str "Part 2: " part2)))
