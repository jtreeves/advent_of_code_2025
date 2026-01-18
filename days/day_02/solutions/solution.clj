(require '[clojure.string :as str])

;; Check if ID is invalid for Part 1: exactly two identical sequences
(defn invalid-part1? [id-str]
  (let [n (count id-str)]
    (and (even? n)
         (let [half (/ n 2)]
           (= (subs id-str 0 half)
              (subs id-str half))))))

;; Check if ID is invalid for Part 2: sequence repeated 2+ times
(defn invalid-part2? [id-str]
  (let [n (count id-str)]
    (some (fn [k]
            (when (zero? (mod n k))
              (let [seq-len (/ n k)
                    pattern (subs id-str 0 seq-len)
                    repeated (apply str (repeat k pattern))]
                (= id-str repeated))))
          (range 2 (inc n)))))

;; Parse a range string like "start-end"
(defn parse-range [range-str]
  (let [[start end] (str/split range-str #"-")]
    [(Long/parseLong (str/trim start))
     (Long/parseLong (str/trim end))]))

;; Parse a line of comma-separated ranges
(defn parse-ranges [line]
  (->> (str/split line #",")
       (map str/trim)
       (filter seq)
       (map parse-range)))

(defn solve [input-data]
  (let [lines (-> input-data str/trim str/split-lines)]
    (loop [lines lines
           part1-sum 0
           part2-sum 0]
      (if (empty? lines)
        [(str part1-sum) (str part2-sum)]
        (let [line (first lines)
              rest-lines (rest lines)]
          (if (str/blank? line)
            (recur rest-lines part1-sum part2-sum)
            (let [ranges (parse-ranges line)
                  [new-p1-sum new-p2-sum] (reduce (fn [acc [start end]]
                                                     (reduce (fn [[p1-sum p2-sum] num]
                                                               (let [id-str (str num)]
                                                                 [(+ p1-sum (if (invalid-part1? id-str) num 0))
                                                                  (+ p2-sum (if (invalid-part2? id-str) num 0))]))
                                                             acc
                                                             (range start (inc end))))
                                                   [part1-sum part2-sum]
                                                   ranges)]
              (recur rest-lines new-p1-sum new-p2-sum))))))))

(defn -main []
  (let [data (slurp "../data/input.txt")
        [part1 part2] (solve data)]
    (println (str "Part 1: " part1))
    (println (str "Part 2: " part2))))

(-main)
