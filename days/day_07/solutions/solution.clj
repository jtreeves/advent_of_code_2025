(require '[clojure.string :as str])

(defn read-input-raw [path]
  (->> path slurp str/trim))

(defn solve [input-data]
  (let [lines (str/split-lines input-data)
        rows (count lines)
        cols (if (> rows 0) (count (first lines)) 0)
        
        ;; Find starting position S - strings can be indexed directly
        start-pos (first (for [r (range rows)
                               c (range cols)
                               :when (= (get (nth lines r) c) \S)]
                           [r c]))
        start-row (first start-pos)
        start-col (second start-pos)]
    
    (if (nil? start-pos)
      [0 0]
      (let [;; Part 1: Count total splits
            split-count (loop [r (inc start-row)
                               active-beams #{start-col}
                               cnt 0]
                          (if (>= r rows)
                            cnt
                            (let [row (nth lines r)
                                  next-beams (reduce (fn [acc col]
                                                       (case (get row col)
                                                         \. (conj acc col)
                                                         \^ (cond-> acc
                                                              (>= (dec col) 0) (conj (dec col))
                                                              (< (inc col) cols) (conj (inc col)))
                                                         acc))
                                                     #{} active-beams)
                                  new-cnt (+ cnt (count (filter #(= (get row %) \^) active-beams)))]
                              (recur (inc r) next-beams new-cnt))))
            
            ;; Part 2: Count beams reaching bottom row
            beam-counts (reduce (fn [counts r]
                                  (reduce (fn [acc c]
                                            (let [prev-count (get-in acc [(dec r) c])]
                                              (if (> prev-count 0)
                                                (let [row (nth lines r)]
                                                  (case (get row c)
                                                    \. (update-in acc [r c] + prev-count)
                                                    \^ (cond-> acc
                                                         (>= (dec c) 0) (update-in [r (dec c)] + prev-count)
                                                         (< (inc c) cols) (update-in [r (inc c)] + prev-count))
                                                    acc))
                                                acc)))
                                          counts (range cols)))
                                (assoc-in (vec (repeat rows (vec (repeat cols 0)))) [start-row start-col] 1)
                                (range (inc start-row) rows))
            bottom-beam-count (reduce + (get beam-counts (dec rows)))]
        
        [(str split-count) (str bottom-beam-count)]))))

(let [content (read-input-raw "../data/input.txt")
      [part1 part2] (solve content)]
  (println (str "Part 1: " part1))
  (println (str "Part 2: " part2)))
