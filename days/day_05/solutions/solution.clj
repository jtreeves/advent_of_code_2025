(require '[clojure.string :as str])

(defn read-input-raw [path]
  (->> path slurp str/trim))

(defn merge-ranges [ranges]
  (if (empty? ranges)
    []
    (let [sorted (sort-by first ranges)]
      (loop [result [(first sorted)]
             remaining (rest sorted)]
        (if (empty? remaining)
          result
          (let [[curr-start curr-end] (first remaining)
                [last-start last-end] (last result)]
            (if (<= curr-start (inc last-end))
              (recur (conj (vec (butlast result)) [last-start (max last-end curr-end)])
                     (rest remaining))
              (recur (conj result [curr-start curr-end])
                     (rest remaining)))))))))

(defn solve [input-data]
  (let [lines (str/split-lines input-data)
        blank-idx (or (first (keep-indexed #(when (str/blank? %2) %1) lines))
                      (count lines))
        ranges (->> (take blank-idx lines)
                    (filter (comp not str/blank?))
                    (map (fn [line]
                           (let [[start end] (str/split line #"-")]
                             [(Long/parseLong start) (Long/parseLong end)]))))
        ids (->> (drop (inc blank-idx) lines)
                 (filter (comp not str/blank?))
                 (map #(Long/parseLong %)))
        part1-count (count (filter (fn [id-val]
                                      (some (fn [[start end]]
                                              (<= start id-val end))
                                            ranges))
                                    ids))
        merged (merge-ranges ranges)
        part2-total (reduce + (map (fn [[start end]] (+ (- end start) 1)) merged))]
    [part1-count part2-total]))

(let [content (read-input-raw "../data/input.txt")
      [part1 part2] (solve content)]
  (println (str "Part 1: " part1))
  (println (str "Part 2: " part2)))
